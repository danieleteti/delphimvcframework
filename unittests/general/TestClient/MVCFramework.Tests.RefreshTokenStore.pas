// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit MVCFramework.Tests.RefreshTokenStore;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.JWT.RefreshToken;

type
  // One contract, exercised against any IMVCRefreshTokenStore implementation.
  // Descendants only override CreateStore (and CleanUp when they own resources).
  TRefreshTokenStoreContractTests = class
  protected
    fStore: IMVCRefreshTokenStore;
    function CreateStore: IMVCRefreshTokenStore; virtual; abstract;
    procedure CleanUp; virtual;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Issue_Then_Rotate_Succeeds_And_ChangesToken;
    [Test]
    procedure Rotate_UnknownToken_ReturnsNotFound;
    [Test]
    procedure Rotate_ExpiredToken_ReturnsExpired;
    [Test]
    procedure Reuse_Of_Rotated_Token_IsDetected;
    [Test]
    procedure Reuse_RevokesWholeFamily;
    [Test]
    procedure Revoke_KillsFamily;
  end;

  [TestFixture]
  TInMemoryRefreshTokenStoreTests = class(TRefreshTokenStoreContractTests)
  protected
    function CreateStore: IMVCRefreshTokenStore; override;
  end;

  [TestFixture]
  TActiveRecordRefreshTokenStoreTests = class(TRefreshTokenStoreContractTests)
  protected
    function CreateStore: IMVCRefreshTokenStore; override;
    procedure CleanUp; override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.DApt,
  FireDAC.Phys.SQLite,
  MVCFramework.ActiveRecord,
  MVCFramework.JWT.RefreshToken.ActiveRecord;

const
  TTL = 3600;
  AR_CONN_DEF = 'rt_test_conn';

{ TRefreshTokenStoreContractTests }

procedure TRefreshTokenStoreContractTests.Setup;
begin
  fStore := CreateStore;
end;

procedure TRefreshTokenStoreContractTests.CleanUp;
begin
  // overridden by stores that own external resources
end;

procedure TRefreshTokenStoreContractTests.TearDown;
begin
  fStore := nil;
  CleanUp;
end;

procedure TRefreshTokenStoreContractTests.Issue_Then_Rotate_Succeeds_And_ChangesToken;
var
  lT1: string;
  lRot: TMVCRefreshTokenRotation;
begin
  lT1 := fStore.Issue('alice', 'admin,user', TTL);
  lRot := fStore.Rotate(lT1, TTL);
  Assert.AreEqual(Ord(rtsOK), Ord(lRot.Status), 'rotation should succeed');
  Assert.AreEqual('alice', lRot.Username);
  Assert.AreEqual('admin,user', lRot.Roles);
  Assert.IsNotEmpty(lRot.NewRefreshToken);
  Assert.AreNotEqual(lT1, lRot.NewRefreshToken, 'token must change on rotation');
end;

procedure TRefreshTokenStoreContractTests.Rotate_UnknownToken_ReturnsNotFound;
var
  lRot: TMVCRefreshTokenRotation;
begin
  lRot := fStore.Rotate('does-not-exist', TTL);
  Assert.AreEqual(Ord(rtsNotFound), Ord(lRot.Status));
end;

procedure TRefreshTokenStoreContractTests.Rotate_ExpiredToken_ReturnsExpired;
var
  lT1: string;
  lRot: TMVCRefreshTokenRotation;
begin
  lT1 := fStore.Issue('bob', 'user', -1); // already expired
  lRot := fStore.Rotate(lT1, TTL);
  Assert.AreEqual(Ord(rtsExpired), Ord(lRot.Status));
end;

procedure TRefreshTokenStoreContractTests.Reuse_Of_Rotated_Token_IsDetected;
var
  lT1: string;
  lRot: TMVCRefreshTokenRotation;
begin
  lT1 := fStore.Issue('carol', 'user', TTL);
  fStore.Rotate(lT1, TTL);         // lT1 is now consumed
  lRot := fStore.Rotate(lT1, TTL); // replay
  Assert.AreEqual(Ord(rtsReuseDetected), Ord(lRot.Status));
end;

procedure TRefreshTokenStoreContractTests.Reuse_RevokesWholeFamily;
var
  lT1: string;
  lRot1, lAfter: TMVCRefreshTokenRotation;
begin
  lT1 := fStore.Issue('dave', 'user', TTL);
  lRot1 := fStore.Rotate(lT1, TTL);  // produces t2
  fStore.Rotate(lT1, TTL);           // replay of t1 => family revoked
  lAfter := fStore.Rotate(lRot1.NewRefreshToken, TTL); // t2 must be dead too
  Assert.AreEqual(Ord(rtsNotFound), Ord(lAfter.Status), 'whole family must be revoked');
end;

procedure TRefreshTokenStoreContractTests.Revoke_KillsFamily;
var
  lT1: string;
  lRot: TMVCRefreshTokenRotation;
begin
  lT1 := fStore.Issue('erin', 'user', TTL);
  fStore.Revoke(lT1);
  lRot := fStore.Rotate(lT1, TTL);
  Assert.AreEqual(Ord(rtsNotFound), Ord(lRot.Status));
end;

{ TInMemoryRefreshTokenStoreTests }

function TInMemoryRefreshTokenStoreTests.CreateStore: IMVCRefreshTokenStore;
begin
  Result := TMVCInMemoryRefreshTokenStore.Create;
end;

{ TActiveRecordRefreshTokenStoreTests }

function TActiveRecordRefreshTokenStoreTests.CreateStore: IMVCRefreshTokenStore;
var
  lParams: TStringList;
begin
  if not FDManager.IsConnectionDef(AR_CONN_DEF) then
  begin
    lParams := TStringList.Create;
    try
      lParams.Add('Database=' + TPath.Combine(TPath.GetTempPath, 'rt_test.db'));
      lParams.Add('LockingMode=Normal');
      FDManager.AddConnectionDef(AR_CONN_DEF, 'SQLite', lParams);
    finally
      lParams.Free;
    end;
  end;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(AR_CONN_DEF);
  try
    TMVCActiveRecord.CurrentConnection.ExecSQL('DROP TABLE IF EXISTS refresh_tokens');
    TMVCActiveRecord.CurrentConnection.ExecSQL(
      'CREATE TABLE refresh_tokens (' +
      ' id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      ' token_hash TEXT NOT NULL,' +
      ' family_id TEXT NOT NULL,' +
      ' username TEXT NOT NULL,' +
      ' roles TEXT,' +
      ' expires_at TIMESTAMP NOT NULL,' +
      ' used INTEGER DEFAULT 0)');
  finally
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
  end;

  Result := TMVCActiveRecordRefreshTokenStore.Create(AR_CONN_DEF);
end;

procedure TActiveRecordRefreshTokenStoreTests.CleanUp;
begin
  ActiveRecordConnectionsRegistry.AddDefaultConnection(AR_CONN_DEF);
  try
    TMVCActiveRecord.CurrentConnection.ExecSQL('DROP TABLE IF EXISTS refresh_tokens');
  finally
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TInMemoryRefreshTokenStoreTests);
TDUnitX.RegisterTestFixture(TActiveRecordRefreshTokenStoreTests);

end.
