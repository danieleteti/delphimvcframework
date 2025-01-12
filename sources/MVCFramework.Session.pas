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

unit MVCFramework.Session;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections, MVCFramework.Commons;

const
  DEFAULT_SESSION_INACTIVITY = 60; // in minutes

type
  EMVCSession = class(EMVCException)

  end;

  TMVCWebSession = class abstract
  private
    FSessionId: string;
    FLastAccess: TDateTime;
    FTimeout: UInt64;
    FChanged: Boolean;
  protected
    function GetItems(const AKey: string): string; virtual; abstract;
    procedure SetItems(const AKey, AValue: string); virtual;
    procedure SetLastAccess(Value: TDateTime);
    procedure InternalApplyChanges; virtual;
  public
    class function CreateNewSession(const ASessionId: string; const ATimeout: UInt64): TMVCWebSession; virtual; abstract;
    class function CreateFromSessionID(const ASessionId: string): TMVCWebSession; virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure MarkAsUsed; virtual;
    procedure ApplyChanges;
    function ToString: string; override;
    function IsExpired: Boolean; virtual;
    function Keys: TArray<String>; virtual;
    procedure StopSession; virtual;
    function Clone: TMVCWebSession; virtual; abstract;
    class function TryFindSessionID(const ASessionID: String): Boolean; virtual;
    class procedure TryDeleteSessionID(const ASessionID: String); virtual;
    property Items[const AKey: string]: string read GetItems write SetItems; default;
    property SessionId: string read FSessionId;
    property LastAccess: TDateTime read FLastAccess;
    property Timeout: UInt64 read FTimeout;
  end;

  TMVCWebSessionClass = class of TMVCWebSession;

  TMVCWebSessionMemory = class(TMVCWebSession)
  private
    FData: TDictionary<string, string>;
  protected
    function GetItems(const AKey: string): string; override;
    procedure SetItems(const AKey, AValue: string); override;
    procedure InternalApplyChanges; override;
    class function GlobalSessionList: TObjectDictionary<string, TMVCWebSessionMemory>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Clone: TMVCWebSession; override;
    function ToString: string; override;
    property Data: TDictionary<string, string> read FData;
    class function CreateNewSession(const aSessionId: string; const ATimeout: UInt64): TMVCWebSession; override;
    class function CreateFromSessionID(const aSessionId: string): TMVCWebSession; override;
    class function TryFindSessionID(const ASessionID: String): Boolean; override;
    class procedure TryDeleteSessionID(const aSessionID: String); override;
  end;


  TMVCWebSessionFile = class(TMVCWebSessionMemory)
  private
    fSessionFolder: String;
  protected
    procedure StartLoading;
    procedure EndLoading;
    function GetFileName: String; overload;
    class function GetFileName(const SessionFolder, SessionID: String): String; overload;
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure OnValueNotify(Sender: TObject; const Item: String; Action: TCollectionNotification);
  public
    destructor Destroy; override;
    function Keys: System.TArray<System.string>; override;
    class function TryFindSessionID(const ASessionID: String): Boolean; override;
    class procedure TryDeleteSessionID(const ASessionID: String); override;
  end;

  TMVCSessionFactory = class sealed
  private
    FSessionTypeClass: TMVCWebSessionClass;
    FRegisteredSessionTypes: TDictionary<string, TMVCWebSessionClass>;
  protected
    class var cInstance: TMVCSessionFactory;
    constructor Create;
  public
    destructor Destroy; override;
    procedure RegisterSessionType(const AName: string; AWebSessionClass: TMVCWebSessionClass);
    function CreateNewSession(const ATimeout: UInt64): TMVCWebSession;
    function LoadSessionBySessionID(const aSessionId: string): TMVCWebSession;
    function TryFindSessionID(const ASessionID: String): Boolean;
    procedure TryDeleteSessionID(const ASessionID: String);
    class function GetInstance: TMVCSessionFactory; static;
    // class procedure DestroyInstance; static;
    procedure SetSessionType(const SessionType: String);
  end;


implementation

uses
  System.IOUtils,
  MVCFramework.Serializer.Commons;

var
  GlSessionList: TObjectDictionary<string, TMVCWebSessionMemory> = nil;
  GlLastSessionListClear: TDateTime;
  GlCriticalSection: TCriticalSection;
  GSessionTypeLock: Int64 = 0;

function GenerateSessionID: string;
begin
  Result := StringReplace(StringReplace(StringReplace(
    'DT' + GUIDToString(TGUID.NewGuid) + GUIDToString(TGUID.NewGuid) + GUIDToString(TGUID.NewGuid),
    '}', '', [rfReplaceAll]),
    '{', '', [rfReplaceAll]),
    '-', '$', [rfReplaceAll]);
end;

class function TMVCWebSessionMemory.GlobalSessionList: TObjectDictionary<string, TMVCWebSessionMemory>;
var
  S: string;
begin
  if not Assigned(GlSessionList) then
  begin
    GlCriticalSection.Enter;
    try
      if not Assigned(GlSessionList) then
      begin
        GlSessionList := TObjectDictionary<string, TMVCWebSessionMemory>.Create([doOwnsValues], 1024);
      end;
    finally
      GlCriticalSection.Leave;
    end;
  end;

  if MinutesBetween(Now, GlLastSessionListClear) >= 1 then
  begin
    TMonitor.Enter(GlSessionList);
    try
      for S in GlSessionList.Keys do
        if TMVCWebSession(GlSessionList.Items[S]).IsExpired then
          GlSessionList.Remove(S);
      GlLastSessionListClear := Now;
    finally
      TMonitor.Exit(GlSessionList);
    end;
  end;

  Result := GlSessionList;
end;

procedure TMVCWebSessionMemory.InternalApplyChanges;
begin
  TMonitor.Enter(GlobalSessionList);
  try
    GlobalSessionList.AddOrSetValue(fSessionId, TMVCWebSessionMemory(Self.Clone));
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

procedure TMVCWebSession.ApplyChanges;
begin
  if FChanged then
  begin
    InternalApplyChanges;
    FChanged := False;
  end;
end;

constructor TMVCWebSession.Create;
begin
  inherited;
  FChanged := False;
end;

{ TWebSession }

destructor TMVCWebSession.Destroy;
begin
  inherited Destroy;
end;

procedure TMVCWebSession.InternalApplyChanges;
begin

end;

function TMVCWebSession.IsExpired: Boolean;
begin
  if (FTimeout = 0) then
    Result := MinutesBetween(Now, LastAccess) > DEFAULT_SESSION_INACTIVITY
  else
    Result := MinutesBetween(Now, LastAccess) > FTimeout;
end;

function TMVCWebSession.Keys: TArray<String>;
begin
  Result := ['<not implemented>'];
end;

procedure TMVCWebSession.MarkAsUsed;
begin
  FLastAccess := Now;
end;

procedure TMVCWebSession.SetItems(const AKey, AValue: string);
begin
  FChanged := True;
end;

procedure TMVCWebSession.SetLastAccess(Value: TDateTime);
begin
  FLastAccess := Value;
end;

procedure TMVCWebSession.StopSession;
begin
  //do nothing
end;

function TMVCWebSession.ToString: string;
begin
  Result := '';
end;

class procedure TMVCWebSession.TryDeleteSessionID(const ASessionID: String);
begin
  //do nothing
end;

class function TMVCWebSession.TryFindSessionID(const ASessionID: String): Boolean;
begin
  Result := False;
end;

function TMVCWebSessionMemory.Clone: TMVCWebSession;
var
  lMemSess: TMVCWebSessionMemory;
begin
  lMemSess := TMVCWebSessionMemory.Create;
  try
    lMemSess.FSessionId := Self.FSessionId;
    lMemSess.FLastAccess := Self.FLastAccess;
    lMemSess.FTimeout := Self.FTimeout;
    for var lItem in Self.Data do
    begin
      lMemSess.Data.Add(lItem.Key, lItem.Value);
    end;
  except
    lMemSess.Free;
    raise;
  end;
  Result := lMemSess;
end;

constructor TMVCWebSessionMemory.Create;
begin
  inherited;
  FData := TDictionary<String, String>.Create;
end;

class function TMVCWebSessionMemory.CreateFromSessionID(const ASessionId: string): TMVCWebSession;
var
  lSess: TMVCWebSessionMemory;
begin
  TMonitor.Enter(GlobalSessionList);
  try
    Result := nil;
    if GlobalSessionList.TryGetValue(ASessionId, lSess) then
    begin
      Result := lSess.Clone;
    end;
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

{ TWebSessionMemory }

class function TMVCWebSessionMemory.CreateNewSession(const aSessionId: string; const ATimeout: UInt64): TMVCWebSession;
var
  lSess: TMVCWebSessionMemory;
begin
  TMonitor.Enter(GlobalSessionList);
  try
    lSess := TMVCWebSessionMemory.Create;
    try
      lSess.fSessionId := ASessionId;
      lSess.fTimeout := ATimeout;
      lSess.MarkAsUsed;
      GlobalSessionList.Add(ASessionId, lSess);
      Result := lSess.Clone;
    except
      lSess.Free;
      raise;
    end;
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

destructor TMVCWebSessionMemory.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TMVCWebSessionMemory.GetItems(const AKey: string): string;
begin
  TMonitor.Enter(Self);
  try
    if not FData.TryGetValue(AKey, Result) then
      Result := '';
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TMVCWebSessionMemory.SetItems(const AKey, AValue: string);
begin
  inherited;
  TMonitor.Enter(Self);
  try
    FData.AddOrSetValue(AKey, AValue);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TMVCWebSessionMemory.ToString: string;
var
  LKey: string;
begin
  Result := '';
  for LKey in FData.Keys do
    Result := Result + LKey + '=' + QuotedStr(FData.Items[LKey]) + sLineBreak;
end;

class procedure TMVCWebSessionMemory.TryDeleteSessionID(const ASessionID: String);
begin
  inherited;
  TMonitor.Enter(GlobalSessionList);
  try
    GlobalSessionList.Remove(aSessionID);
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

class function TMVCWebSessionMemory.TryFindSessionID(const ASessionID: String): Boolean;
begin
  TMonitor.Enter(GlobalSessionList);
  try
    Result := GlobalSessionList.ContainsKey(ASessionId);
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

{ TMVCSessionFactory }

constructor TMVCSessionFactory.Create;
begin
  inherited Create;
  FRegisteredSessionTypes := TDictionary<string, TMVCWebSessionClass>.Create;
end;

function TMVCSessionFactory.CreateNewSession(const ATimeout: UInt64): TMVCWebSession;
begin
  Result := FSessionTypeClass.CreateNewSession(GenerateSessionID, ATimeout);
end;

function TMVCSessionFactory.LoadSessionBySessionID(const aSessionId: string): TMVCWebSession;
begin
  Result := FSessionTypeClass.CreateFromSessionID(ASessionId);
end;

destructor TMVCSessionFactory.Destroy;
begin
  FRegisteredSessionTypes.Free;
  inherited Destroy;
end;

class function TMVCSessionFactory.GetInstance: TMVCSessionFactory;
begin
  if not Assigned(cInstance) then
  begin
    cInstance := TMVCSessionFactory.Create;
  end;
  Result := cInstance;
end;

procedure TMVCSessionFactory.RegisterSessionType(const AName: string; AWebSessionClass: TMVCWebSessionClass);
begin
  if TInterlocked.Read(GSessionTypeLock) = 1 then
  begin
    raise EMVCSession.Create('Session factory already initialized');
  end;
  FRegisteredSessionTypes.AddOrSetValue(AName, AWebSessionClass);
end;

procedure TMVCSessionFactory.SetSessionType(const SessionType: String);
var
  Clazz: TMVCWebSessionClass;
begin
  if TInterlocked.CompareExchange(GSessionTypeLock, 1, 0) = 0 then
  begin
    if not FRegisteredSessionTypes.TryGetValue(SessionType, Clazz) then
      raise EMVCSession.Create('Unknown session type: ' + SessionType);
    FSessionTypeClass := Clazz;
  end;
end;

procedure TMVCSessionFactory.TryDeleteSessionID(const ASessionID: String);
begin
  FSessionTypeClass.TryDeleteSessionID(ASessionID);
end;

function TMVCSessionFactory.TryFindSessionID(const ASessionID: String): Boolean;
begin
  Result := FSessionTypeClass.TryFindSessionID(ASessionID);
end;

{ TWebSessionMemoryController }

//constructor TMVCWebSessionFile.CreateNewSession(const SessionID: string; const Timeout: UInt64);
//begin
//  inherited;
//  Data.OnValueNotify := OnValueNotify;
//  fSessionFolder := TPath.Combine(AppPath, 'sessions');
//  TDirectory.CreateDirectory(fSessionFolder);
//  LoadFromFile;
//  MarkAsUsed;
//  SaveToFile;
//end;

destructor TMVCWebSessionFile.Destroy;
begin
  inherited;
end;

procedure TMVCWebSessionFile.EndLoading;
begin
  Data.OnValueNotify := OnValueNotify;
end;

class function TMVCWebSessionFile.GetFileName(const SessionFolder,
  SessionID: String): String;
begin
  Result := TPath.Combine(SessionFolder, SessionId);
end;

function TMVCWebSessionFile.GetFileName: String;
begin
  Result := GetFileName(fSessionFolder, SessionId);
end;

function TMVCWebSessionFile.Keys: System.TArray<System.string>;
begin
  Result := Data.Keys.ToArray;
end;

procedure TMVCWebSessionFile.LoadFromFile;
var
  lFileName: String;
  lFile: TStreamReader;
  lLine: string;
  lPieces: TArray<System.string>;
begin
  lFileName := GetFileName;
  if not TFile.Exists(lFileName) then
  begin
    Exit;
  end;
  //Log.Info('Loading session %s from %s', [SessionId, lFileName], 'file_session_events');
  lFile := TFile.OpenText(lFileName);
  try
    StartLoading;
    try
      SetLastAccess(ISOTimeStampToDateTime(lFile.ReadLine));
      while not lFile.EndOfStream do
      begin
        lLine := lFile.ReadLine;
        lPieces := lLine.Split(['=']);
        Data.Add(lPieces[0], lPieces[1]);
      end;
    finally
      EndLoading;
    end;
  finally
    lFile.Free;
  end;
end;

procedure TMVCWebSessionFile.OnValueNotify(Sender: TObject; const Item: String;
  Action: TCollectionNotification);
begin
  if Action in [cnAdded, cnExtracted, cnRemoved] then
  begin
    //Log.Info('Saving session %s because item changed [%s]', [SessionId, Item], 'file_session_events');
    SaveToFile;
  end;
end;

procedure TMVCWebSessionFile.SaveToFile;
var
  lFileName: String;
  lPair: TPair<String, String>;
  lFile: TStreamWriter;
begin
  MarkAsUsed;
  lFileName := GetFileName;
  lFile := TFile.CreateText(lFileName);
  try
    lFile.WriteLine(DateTimeToISOTimeStamp(LastAccess));
    for lPair in Data do
    begin
      lFile.WriteLine(String.Join('=', [lPair.Key, lPair.Value]));
    end;
    lFile.Close;
  finally
    lFile.Free;
  end;
end;

procedure TMVCWebSessionFile.StartLoading;
begin
  Data.OnValueNotify := nil;
end;

class procedure TMVCWebSessionFile.TryDeleteSessionID(const ASessionID: String);
var
  lSessionFolder: string;
begin
  inherited;
  lSessionFolder := TPath.Combine(AppPath, 'sessions');
  if TFile.Exists(GetFileName(lSessionFolder, ASessionID)) then
  begin
    TFile.Delete(GetFileName(lSessionFolder, ASessionID));
  end;
end;

class function TMVCWebSessionFile.TryFindSessionID(
  const ASessionID: String): Boolean;
var
  lSessionFolder: string;
begin
  inherited;
  lSessionFolder := TPath.Combine(AppPath, 'sessions');
  Result := TFile.Exists(GetFileName(lSessionFolder, ASessionID));
end;


initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('memory', TMVCWebSessionMemory);
TMVCSessionFactory.GetInstance.RegisterSessionType('file', TMVCWebSessionFile);

GlCriticalSection := TCriticalSection.Create;

finalization

FreeAndNil(TMVCSessionFactory.cInstance);
FreeAndNil(GlCriticalSection);

if Assigned(GlSessionList) then
  FreeAndNil(GlSessionList);

end.

