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
  [MVCTable('dmvc_sessions')]
  TMVCSessionActiveRecord = class(TMVCActiveRecord)
  strict private
    [MVCTableField('session_id', [foPrimaryKey])]
    fSessionID: NullableString;
    [MVCTableField('session_data', [])]
    fData: string;
    [MVCTableField('session_expiration', [])]
    fSessionExpiration: NullableTDateTime;
    //transient
    fTimeout: Cardinal;
    procedure SetTimeout(const Value: Cardinal);
  private
    fJSONData: TJsonObject;
  protected
    procedure OnBeforeInsertOrUpdate; override;
    procedure OnAfterLoad; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RefreshSessionExpiration;
    property Data: string read FData write FData;
    property SessionExpiration: NullableTDateTime read fSessionExpiration;
    property Timeout: Cardinal read fTimeout write SetTimeout;
    property SessionID: NullableString read fSessionID write fSessionID;
  end;

  TMVCWebSessionDatabase = class(TMVCWebSession)
  private
    fSessionData: TMVCSessionActiveRecord;
  protected
    class var gLock: TObject;
    function GetItems(const AKey: string): string; override;
    procedure SetItems(const AKey, AValue: string); override;
    procedure UpdateToDB;
    procedure InsertIntoDB;
    procedure InternalApplyChanges; override;
    function GetExpirationTimeStamp: NullableTDateTime; override;
  public
    constructor Create; overload; override;
    constructor Create(const aSessionID: String; const aTimeout: UInt64); reintroduce; overload;
    constructor CreateFromSessionData(const aSessionData: TMVCSessionActiveRecord; const aTimeout: UInt64); overload;
    destructor Destroy; override;
    procedure MarkAsUsed; override;
    function Keys: TArray<String>; override;
    function Clone: TMVCWebSession; override;
    function ToString: string; override;
    property SessionData: TMVCSessionActiveRecord read fSessionData;
    class function CreateNewSession(const aSessionId: string; const ATimeout: UInt64): TMVCWebSession; override;
    class function CreateFromSessionID(const aSessionId: string; const ATimeout: UInt64): TMVCWebSession; override;
    class function TryFindSessionID(const aSessionID: String): Boolean; override;
    class procedure TryDeleteSessionID(const aSessionID: String); override;
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
  MVCFramework.SQLGenerators.MySQL,
  System.DateUtils;

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

function TMVCWebSessionDatabase.GetExpirationTimeStamp: NullableTDateTime;
begin
  Result := fSessionData.SessionExpiration;
end;

function TMVCWebSessionDatabase.GetItems(const AKey: string): string;
begin
  Result := fSessionData.fJSONData.S[AKey];
end;

procedure TMVCWebSessionDatabase.InsertIntoDB;
begin
  MarkAsUsed;
  fSessionData.Insert;
  LogW('InsertIntoDB');
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

procedure TMVCWebSessionDatabase.MarkAsUsed;
var
  lFutureExpiration: TDateTime;
begin
  if fSessionData.SessionExpiration.HasValue then
  begin
    lFutureExpiration := Now() + OneMinute * Timeout;
    if FormatDateTime('yyyymmddhhnn', lFutureExpiration) > FormatDateTime('yyyymmddhhnn', fSessionData.SessionExpiration) then
    begin
      inherited;
      fSessionData.RefreshSessionExpiration;
    end;
  end;
end;

procedure TMVCWebSessionDatabase.UpdateToDB;
begin
  fSessionData.Update(True);
  LogW('UpdateToDB');
end;

procedure TMVCWebSessionDatabase.SetItems(const AKey, AValue: string);
begin
  inherited;
  fSessionData.fJSONData.S[AKey] := AValue;
end;

function TMVCWebSessionDatabase.ToString: string;
begin
  Result := fSessionData.fJSONData.ToJSON(True);
end;

class procedure TMVCWebSessionDatabase.TryDeleteSessionID(const ASessionID: String);
begin
  inherited;
  TMVCActiveRecord.DeleteRQL<TMVCSessionActiveRecord>('eq(session_id, "' + ASessionID + '")');
end;

class function TMVCWebSessionDatabase.TryFindSessionID(const aSessionID: String): Boolean;
var
  lSess: TMVCSessionActiveRecord;
begin
  inherited;
  LogW('ReadFromDB - EXISTS');
  lSess := TMVCActiveRecord.SelectOneByRQL<TMVCSessionActiveRecord>(Format('eq(session_id, "%s")', [aSessionID]), False);
  try
    Result := Assigned(lSess);
  finally
    lSess.Free;
  end;
end;


function TMVCWebSessionDatabase.Clone: TMVCWebSession;
begin
  raise EMVCSession.Create('Clone not allowed in ' + ClassName);
end;

constructor TMVCWebSessionDatabase.Create(const aSessionID: String; const aTimeout: UInt64);
begin
  Create;
  FSessionId := aSessionID;
  FTimeout := aTimeout;
  fSessionData.SessionID := aSessionID;
  fSessionData.Timeout := aTimeout;
end;

constructor TMVCWebSessionDatabase.CreateFromSessionData(const aSessionData: TMVCSessionActiveRecord; const aTimeout: UInt64);
begin
  inherited Create;
  fSessionData := aSessionData;
  fSessionId := fSessionData.SessionID;
  fSessionData.Timeout := aTimeout;
  SetTimeout(aTimeout);
end;

class function TMVCWebSessionDatabase.CreateFromSessionID(const aSessionId: string; const aTimeout: UInt64): TMVCWebSession;
var
  lSessDB: TMVCSessionActiveRecord;
begin
  Result := nil;
  LogW('ReadFromDB');
  lSessDB := TMVCActiveRecord.GetByPK<TMVCSessionActiveRecord>(aSessionId, False);
  if lSessDB <> nil then
  begin
    Result := TMVCWebSessionDatabase.CreateFromSessionData(lSessDB, aTimeout);
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
  fJSONData := TJsonObject.Create;
end;

destructor TMVCSessionActiveRecord.Destroy;
begin
  fJSONData.Free;
  inherited;
end;

procedure TMVCSessionActiveRecord.OnAfterLoad;
begin
  inherited;
  if not fData.IsEmpty then
  begin
    FreeAndNil(fJSONData);
    fJSONData := StrToJSONObject(fData);
  end
  else
  begin
    fJSONData.Clear;
  end;
end;

procedure TMVCSessionActiveRecord.OnBeforeInsertOrUpdate;
begin
  inherited;
  fData := fJSONData.ToJSON(True)
end;

procedure TMVCSessionActiveRecord.RefreshSessionExpiration;
begin
  if fTimeout = 0 then
    fSessionExpiration.Clear
  else
    fSessionExpiration := RecodeSecond(Now() + OneMinute * fTimeout, 0);
end;

procedure TMVCSessionActiveRecord.SetTimeout(const Value: Cardinal);
begin
  fTimeout := Value;
  RefreshSessionExpiration;
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('dbactiverecord', TMVCWebSessionDatabase);

end.

