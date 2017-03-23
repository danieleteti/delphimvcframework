// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
  System.SyncObjs,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections;

const

  DEFAULT_SESSION_INACTIVITY = 60; // in minutes

type

  TWebSession = class abstract
  private
    FSessionId: string;
    FLastAccess: TDateTime;
    FTimeout: UInt64;
  protected
    function GetItems(const AKey: string): string; virtual; abstract;
    procedure SetItems(const AKey, AValue: string); virtual; abstract;
  public
    constructor Create(const ASessionId: string; const ATimeout: UInt64); virtual;
    destructor Destroy; override;

    procedure MarkAsUsed;
    function ToString: string; override;
    function IsExpired: Boolean;

    property Items[const AKey: string]: string read GetItems write SetItems; default;
    property SessionId: string read FSessionId;
    property LastAccess: TDateTime read FLastAccess;
    property Timeout: UInt64 read FTimeout;
  end;

  TWebSessionClass = class of TWebSession;

  TWebSessionMemory = class(TWebSession)
  private
    FData: TDictionary<string, string>;
  protected
    function GetItems(const AKey: string): string; override;
    procedure SetItems(const AKey, AValue: string); override;
  public
    constructor Create(const ASessionId: string; const ATimeout: UInt64); override;
    destructor Destroy; override;

    function ToString: String; override;

    property Data: TDictionary<string, string> read FData;
  end;

  TMVCSessionFactory = class sealed
  private
    FRegisteredSessionTypes: TDictionary<String, TWebSessionClass>;
  private
    class var Instance: TMVCSessionFactory;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterSessionType(const AName: String; AWebSessionClass: TWebSessionClass);
    function CreateNewByType(const AName, ASessionId: string; const ATimeout: UInt64): TWebSession;

    class function GetInstance: TMVCSessionFactory; static;
    class procedure DestroyInstance; static;
  end;

function GlobalSessionList: TObjectDictionary<string, TWebSession>;

implementation

var
  GlSessionList: TObjectDictionary<string, TWebSession> = nil;
  GlLastSessionListClear: TDateTime;
  GlCriticalSection: TCriticalSection;

function GlobalSessionList: TObjectDictionary<string, TWebSession>;
var
  S: string;
begin
  if not Assigned(GlSessionList) then
  begin
    GlCriticalSection.Enter;
    try
      if not Assigned(GlSessionList) then
        GlSessionList := TObjectDictionary<string, TWebSession>.Create([doOwnsValues]);
    finally
      GlCriticalSection.Leave;
    end;
  end;

  if MinutesBetween(Now, GlLastSessionListClear) >= 1 then
  begin
    TMonitor.Enter(GlSessionList);
    try
      for S in GlSessionList.Keys do
        if TWebSession(GlSessionList.Items[S]).IsExpired then
          GlSessionList.Remove(S);
      GlLastSessionListClear := Now;
    finally
      TMonitor.Exit(GlSessionList);
    end;
  end;

  Result := GlSessionList;
end;

{ TWebSession }

constructor TWebSession.Create(const ASessionId: string; const ATimeout: UInt64);
begin
  inherited Create;
  FSessionId := ASessionId;
  FTimeout := ATimeout;
end;

destructor TWebSession.Destroy;
begin
  inherited Destroy;
end;

function TWebSession.IsExpired: Boolean;
begin
  if (FTimeout = 0) then
    Result := MinutesBetween(Now, LastAccess) > DEFAULT_SESSION_INACTIVITY
  else
    Result := MinutesBetween(Now, LastAccess) > FTimeout;
end;

procedure TWebSession.MarkAsUsed;
begin
  FLastAccess := Now;
end;

function TWebSession.ToString: string;
begin
  Result := '';
end;

{ TWebSessionMemory }

constructor TWebSessionMemory.Create(const ASessionId: string; const ATimeout: UInt64);
begin
  inherited Create(ASessionId, ATimeout);
  FData := TDictionary<string, string>.Create;
end;

destructor TWebSessionMemory.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TWebSessionMemory.GetItems(const AKey: string): string;
begin
  TMonitor.Enter(Self);
  try
    if not FData.TryGetValue(AKey, Result) then
      Result := '';
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TWebSessionMemory.SetItems(const AKey, AValue: string);
begin
  TMonitor.Enter(Self);
  try
    FData.AddOrSetValue(AKey, AValue);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TWebSessionMemory.ToString: String;
var
  LKey: string;
begin
  Result := '';
  for LKey in FData.Keys do
    Result := Result + LKey + '=' + QuotedStr(FData.Items[LKey]) + sLineBreak;
end;

{ TMVCSessionFactory }

constructor TMVCSessionFactory.Create;
begin
  inherited Create;
  FRegisteredSessionTypes := TDictionary<string, TWebSessionClass>.Create;
end;

function TMVCSessionFactory.CreateNewByType(const AName, ASessionId: string; const ATimeout: UInt64): TWebSession;
var
  Clazz: TWebSessionClass;
begin
  if not FRegisteredSessionTypes.TryGetValue(AName, Clazz) then
    raise Exception.Create('Unknown application session type');
  Result := Clazz.Create(ASessionId, ATimeout);
end;

destructor TMVCSessionFactory.Destroy;
begin
  FRegisteredSessionTypes.Free;
  inherited Destroy;
end;

class procedure TMVCSessionFactory.DestroyInstance;
begin
  if Assigned(Instance) then
    Instance.Free;
end;

class function TMVCSessionFactory.GetInstance: TMVCSessionFactory;
begin
  if not Assigned(Instance) then
    Instance := TMVCSessionFactory.Create;
  Result := Instance;
end;

procedure TMVCSessionFactory.RegisterSessionType(const AName: String; AWebSessionClass: TWebSessionClass);
begin
  FRegisteredSessionTypes.AddOrSetValue(AName, AWebSessionClass);
end;

initialization

TMVCSessionFactory.GetInstance.RegisterSessionType('memory', TWebSessionMemory);
GlCriticalSection := TCriticalSection.Create;

finalization

TMVCSessionFactory.DestroyInstance;
FreeAndNil(GlCriticalSection);

if Assigned(GlSessionList) then
  FreeAndNil(GlSessionList);

end.
