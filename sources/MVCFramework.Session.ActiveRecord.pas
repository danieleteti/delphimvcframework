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

unit MVCFramework.Session.ActiveRecord;

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
  TMVCWebSessionActiveRecordStorage = class(TMVCActiveRecord)
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
    constructor Create(aSessionID: string; aTimeout: UInt32);
    destructor Destroy; override;
    property Data: string read FData write FData;
  end;

  TMVCWebSessionActiveRecord = class(TMVCWebSession)
  private
    fSessionID: String;
    fSessionData: TMVCWebSessionActiveRecordStorage;
  protected
    procedure LoadFromDB;
    procedure SaveToDB;
    procedure MarkAsUsed; override;
    function GetItems(const AKey: string): string; override;
    procedure SetItems(const AKey, AValue: string); override;
  public
    constructor Create(const SessionID: string; const Timeout: UInt64); override;
    destructor Destroy; override;
    class function TryFindSessionID(const ASessionID: String): Boolean; override;
    class procedure TryDeleteSessionID(const ASessionID: String); override;
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

{ TMVCWebSessionActiveRecord }

const
  LOG_TAG = 'TMVCWebSessionActiveRecord';

constructor TMVCWebSessionActiveRecord.Create(const SessionID: string; const Timeout: UInt64);
begin
  inherited Create(SessionID, Timeout);
  LoadFromDB;
end;

destructor TMVCWebSessionActiveRecord.Destroy;
begin
  fSessionData.Store;
  inherited;
end;

function TMVCWebSessionActiveRecord.GetItems(const AKey: string): string;
begin
  Result := fSessionData.fJSONData.S[AKey];
end;

procedure TMVCWebSessionActiveRecord.LoadFromDB;
var
  lFileName: String;
  lFile: TStreamReader;
  lLine: string;
  lPieces: TArray<System.string>;
begin
  Log.Info('Loading session %s from %s', [SessionId, lFileName], LOG_TAG);
  fSessionData := TMVCActiveRecord.GetByPK<TMVCWebSessionActiveRecordStorage>(Self.SessionId, False);
  if not Assigned(fSessionData) then
  begin
    fSessionData := TMVCWebSessionActiveRecordStorage.Create(Self.SessionId, Self.Timeout);
    MarkAsUsed;
    fSessionData.Insert;
  end
  else
  begin
    SaveToDB;
  end;
end;

procedure TMVCWebSessionActiveRecord.MarkAsUsed;
begin
  inherited;
  fSessionData.fSessionExpiration := Now + OneMinute * Timeout;
end;

procedure TMVCWebSessionActiveRecord.SaveToDB;
var
  lFileName: String;
  lPair: TPair<String, String>;
  lFile: TStreamWriter;
  lJSON: TJSONObject;
begin
  MarkAsUsed;
  fSessionData.Store;
end;

procedure TMVCWebSessionActiveRecord.SetItems(const AKey, AValue: string);
begin
  inherited;
  fSessionData.fJSONData.S[AKey] := AValue;
end;

class procedure TMVCWebSessionActiveRecord.TryDeleteSessionID(const ASessionID: String);
var
  lSessionFolder: string;
begin
  inherited;
  TMVCActiveRecord.DeleteRQL<TMVCWebSessionActiveRecordStorage>('eq(session_id, "' + ASessionID + '")');
end;

class function TMVCWebSessionActiveRecord.TryFindSessionID(const aSessionID: String): Boolean;
var
  lSess: TMVCWebSessionActiveRecordStorage;
begin
  inherited;
  lSess := TMVCActiveRecord.SelectOneByRQL<TMVCWebSessionActiveRecordStorage>(Format('eq(session_id, "%s")', [aSessionID]), False);
  try
    Result := Assigned(lSess);
  finally
    lSess.Free;
  end;
end;


{ TMVCWebSessionActiveRecordStorage }

constructor TMVCWebSessionActiveRecordStorage.Create(aSessionID: string; aTimeout: UInt32);
begin
  inherited Create;
  fSessionID := aSessionID;
  fTimeout := aTimeout;
end;

destructor TMVCWebSessionActiveRecordStorage.Destroy;
begin
  fJSONData.Free;
  inherited;
end;

procedure TMVCWebSessionActiveRecordStorage.OnAfterLoad;
begin
  inherited;
  if fData.IsEmpty then
    fJSONData := TJsonObject.Create
  else
    fJSONData := StrToJSONObject(fData);
end;

procedure TMVCWebSessionActiveRecordStorage.OnBeforeInsertOrUpdate;
begin
  inherited;
  if Assigned(fJSONData) then
    fData := fJSONData.ToJSON(True)
  else
    fData := '{}';
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('dbactiverecord', TMVCWebSessionActiveRecord);

end.

