// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Session.Database;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Commons,
  MVCFramework.ActiveRecord,
  MVCFramework.Nullables ,
  MVCFramework.Session,
  JsonDataObjects;

type
  [MVCTable('sessions')]
  TMVCSessionActiveRecord = class(TMVCActiveRecord)
  private
    [MVCTableField('session_id', [foPrimaryKey])]
    fSessionID: NullableString;
    [MVCTableField('session_data', [])]
    fData: string;
    [MVCTableField('session_expiration', [])]
    fSessionExpiration: TDateTime;
    //transient
    fTimeout: Cardinal;
    fJSONData: TJsonObject;
  protected
    procedure OnBeforeInsertOrUpdate; override;
    procedure OnAfterLoad; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Data: string read FData write FData;
  end;

  TMVCWebSessionDatabase = class(TMVCWebSession)
  private
    fSessionData: TMVCSessionActiveRecord;
  protected
    class var gLock: TObject;
    function GetItems(const AKey: string): string; override;
    procedure SetItems(const AKey, AValue: string); override;
    procedure LoadFromDB;
    procedure UpdateToDB;
    procedure InsertIntoDB;
    procedure InternalApplyChanges; override;
    procedure MarkAsUsed; override;
  public
    constructor Create; overload; override;
    constructor Create(const aSessionID: String; const aTimeout: UInt64); overload;
    constructor CreateFromSessionData(const aSessionData: TMVCSessionActiveRecord); overload;
    destructor Destroy; override;
    function Keys: TArray<String>; override;
    function Clone: TMVCWebSession; override;
    function ToString: string; override;
    property SessionData: TMVCSessionActiveRecord read fSessionData;
    class function CreateNewSession(const aSessionId: string; const ATimeout: UInt64): TMVCWebSession; override;
    class function CreateFromSessionID(const aSessionId: string): TMVCWebSession; override;
    class function TryFindSessionID(const ASessionID: String): Boolean; override;
    class procedure TryDeleteSessionID(const aSessionID: String); override;
    //
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  System.IOUtils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Logger,
  MVCFramework.SQLGenerators.PostgreSQL,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.SQLGenerators.Interbase,
  MVCFramework.SQLGenerators.MSSQL,
  MVCFramework.SQLGenerators.Sqlite,
  MVCFramework.SQLGenerators.MySQL, System.DateUtils
  ;

{ TMVCWebSessionDatabase }

const
  LOG_TAG = 'TMVCWebSessionDatabase';

constructor TMVCWebSessionDatabase.Create;
begin
  inherited;
  fSessionData := TMVCSessionActiveRecord.Create;
end;

destructor TMVCWebSessionDatabase.Destroy;
begin
  fSessionData.Free;
  inherited;
end;

class destructor TMVCWebSessionDatabase.Destroy;
begin

end;

function TMVCWebSessionDatabase.GetItems(const AKey: string): string;
begin
  Result := fSessionData.fJSONData.S[AKey];
end;

procedure TMVCWebSessionDatabase.InsertIntoDB;
begin
  MarkAsUsed;
  fSessionData.Insert;
end;

procedure TMVCWebSessionDatabase.InternalApplyChanges;
begin
  inherited;
  UpdateToDB;
end;

function TMVCWebSessionDatabase.Keys: TArray<String>;
begin
  Result := [''];
end;

procedure TMVCWebSessionDatabase.LoadFromDB;
var
  lFileName: String;
  lFile: TStreamReader;
  lLine: string;
  lPieces: TArray<System.string>;
begin
  Log.Info('Loading session %s from %s', [SessionId, lFileName], LOG_TAG);
  fSessionData := TMVCActiveRecord.GetByPK<TMVCSessionActiveRecord>(Self.SessionId, False);
  if not Assigned(fSessionData) then
  begin
    fSessionData := TMVCSessionActiveRecord.Create;
    fSessionData.fSessionID := Self.SessionId;
    fSessionData.fTimeout := Self.Timeout;
    MarkAsUsed;
    fSessionData.Insert;
  end
  else
  begin
    UpdateToDB;
  end;
end;

procedure TMVCWebSessionDatabase.MarkAsUsed;
begin
  inherited;
  fSessionData.fSessionExpiration := Now + OneMinute * Timeout;
end;

procedure TMVCWebSessionDatabase.UpdateToDB;
begin
  MarkAsUsed;
  fSessionData.Update(True);
end;

procedure TMVCWebSessionDatabase.SetItems(const AKey, AValue: string);
begin
  inherited;
  fSessionData.fJSONData.S[AKey] := AValue;
end;

function TMVCWebSessionDatabase.ToString: string;
begin

end;

class procedure TMVCWebSessionDatabase.TryDeleteSessionID(const ASessionID: String);
var
  lSessionFolder: string;
begin
  inherited;
  TMVCActiveRecord.DeleteRQL<TMVCSessionActiveRecord>('eq(session_id, "' + ASessionID + '")');
end;

class function TMVCWebSessionDatabase.TryFindSessionID(const aSessionID: String): Boolean;
var
  lSess: TMVCSessionActiveRecord;
begin
  inherited;
  lSess := TMVCActiveRecord.SelectOneByRQL<TMVCSessionActiveRecord>(Format('eq(session_id, "%s")', [aSessionID]), False);
  try
    Result := Assigned(lSess);
  finally
    lSess.Free;
  end;
end;


function TMVCWebSessionDatabase.Clone: TMVCWebSession;
begin

end;

class constructor TMVCWebSessionDatabase.Create;
begin

end;

constructor TMVCWebSessionDatabase.Create(const aSessionID: String; const aTimeout: UInt64);
begin
  Create;
  FSessionId := aSessionID;
  FTimeout := aTimeout;
  fSessionData.fSessionID := aSessionID;
  fSessionData.fTimeout := aTimeout;
end;

constructor TMVCWebSessionDatabase.CreateFromSessionData(const aSessionData: TMVCSessionActiveRecord);
begin
  inherited Create;
  fSessionData := aSessionData;
  fSessionId := fSessionData.fSessionID;
  fTimeout := fSessionData.fTimeout;
end;

class function TMVCWebSessionDatabase.CreateFromSessionID(const aSessionId: string): TMVCWebSession;
var
  lSessDB: TMVCSessionActiveRecord;
begin
  Result := nil;
  lSessDB := TMVCActiveRecord.GetByPK<TMVCSessionActiveRecord>(aSessionId, False);
  if lSessDB <> nil then
  begin
    Result := TMVCWebSessionDatabase.CreateFromSessionData(lSessDB);
  end;
end;

class function TMVCWebSessionDatabase.CreateNewSession(const aSessionId: string; const aTimeout: UInt64): TMVCWebSession;
begin
  Result := TMVCWebSessionDatabase.Create(aSessionId, aTimeout);
  TMVCWebSessionDatabase(Result).InsertIntoDB;
end;

{ TMVCSessionActiveRecord }

constructor TMVCSessionActiveRecord.Create;
begin
  inherited Create;
end;

destructor TMVCSessionActiveRecord.Destroy;
begin
  fJSONData.Free;
  inherited;
end;

procedure TMVCSessionActiveRecord.OnAfterLoad;
begin
  inherited;
  if fData.IsEmpty then
    fJSONData := TJsonObject.Create
  else
    fJSONData.pa := StrToJSONObject(fData);
end;

procedure TMVCSessionActiveRecord.OnBeforeInsertOrUpdate;
begin
  inherited;
  if Assigned(fJSONData) then
    fData := fJSONData.ToJSON(True)
  else
    fData := '{}';
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('dbactiverecord', TMVCWebSessionDatabase);

end.

