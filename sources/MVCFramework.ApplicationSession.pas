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
// *************************************************************************** }

unit MVCFramework.ApplicationSession;

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections;

type

  TWebApplicationSession = class abstract
  private
    { private declarations }
  protected
    function GetItems(const AKey: string): string; virtual; abstract;
    procedure SetItems(const AKey, AValue: string); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; override;

    property Items[const AKey: string]: string read GetItems write SetItems; default;
  end;

  TWebApplicationSessionClass = class of TWebApplicationSession;

  TWebApplicationSessionMemory = class(TWebApplicationSession)
  private
    FData: TDictionary<string, string>;
  protected
    function GetItems(const AKey: String): String; override;
    procedure SetItems(const AKey, AValue: String); override;

    property Data: TDictionary<string, string> read FData;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ToString: String; override;
  end;

  TMVCApplicationSessionFactory = class sealed
  private
    { private declarations }
  protected
    FRegisteredApplicationSessionTypes: TDictionary<String, TWebApplicationSessionClass>;
    class var FInstance: TMVCApplicationSessionFactory;
    constructor Create;
  public
    class function GetInstance: TMVCApplicationSessionFactory;
  public
    destructor Destroy; override;

    procedure RegisterSessionType(const AName: String; AWebApplicationSessionClass: TWebApplicationSessionClass);
    function CreateNewByType(const AName: String): TWebApplicationSession;
  end;

implementation

constructor TWebApplicationSession.Create;
begin
  inherited Create;
end;

destructor TWebApplicationSession.Destroy;
begin
  inherited;
end;

function TWebApplicationSession.ToString: string;
begin
  Result := '';
end;

constructor TWebApplicationSessionMemory.Create;
begin
  inherited;
  FData := TDictionary<string, string>.Create;
end;

destructor TWebApplicationSessionMemory.Destroy;
begin
  FData.Free;
  inherited;
end;

function TWebApplicationSessionMemory.GetItems(const AKey: String): String;
begin
  TMonitor.Enter(self);
  try
    if not FData.TryGetValue(AKey, Result) then
      Result := '';
  finally
    TMonitor.Exit(self);
  end;
end;

procedure TWebApplicationSessionMemory.SetItems(const AKey, AValue: String);
begin
  TMonitor.Enter(self);
  try
    FData.AddOrSetValue(AKey, AValue);
  finally
    TMonitor.Exit(self);
  end;
end;

function TWebApplicationSessionMemory.ToString: String;
var
  LKey: String;
begin
  TMonitor.Enter(self);
  try
    Result := '';
    for LKey in FData.Keys do
    begin
      Result := LKey + ' = ' + QuotedStr(FData.Items[LKey]) + sLineBreak;
    end;
  finally
    TMonitor.Exit(self);
  end;
end;

{ TMVCSessionManager }

constructor TMVCApplicationSessionFactory.Create;
begin
  inherited;
  FRegisteredApplicationSessionTypes := TDictionary<string, TWebApplicationSessionClass>.Create;
end;

function TMVCApplicationSessionFactory.CreateNewByType(const AName: String): TWebApplicationSession;
var
  clazz: TWebApplicationSessionClass;
begin
  if not FRegisteredApplicationSessionTypes.TryGetValue(AName, clazz) then
    raise Exception.Create('Unknown application session type');
  Result := clazz.Create;
end;

destructor TMVCApplicationSessionFactory.Destroy;
begin
  FRegisteredApplicationSessionTypes.Free;
  inherited;
end;

class
  function TMVCApplicationSessionFactory.GetInstance: TMVCApplicationSessionFactory;
begin
  if not Assigned(FInstance) then
    // doesnt require double-check because used for the first time at the unit initialization
    FInstance := TMVCApplicationSessionFactory.Create;
  Result := FInstance;
end;

procedure TMVCApplicationSessionFactory.RegisterSessionType(const AName: String;
  AWebApplicationSessionClass: TWebApplicationSessionClass);
begin
  FRegisteredApplicationSessionTypes.AddOrSetValue(AName, AWebApplicationSessionClass);
end;

initialization

TMVCApplicationSessionFactory.GetInstance.RegisterSessionType('memory', TWebApplicationSessionMemory);

finalization

TMVCApplicationSessionFactory.FInstance.Free;

end.
