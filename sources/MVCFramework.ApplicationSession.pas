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

uses System.SysUtils, System.Generics.Collections;

type
  TWebApplicationSession = class abstract
  strict protected
    function GetItems(const Key: string): string; virtual; abstract;
    procedure SetItems(const Key, Value: string); virtual; abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    property Items[const Key: string]: string read GetItems
      write SetItems; default;
  end;

  TWebApplicationSessionClass = class of TWebApplicationSession;

  TWebApplicationSessionMemory = class(TWebApplicationSession)
  strict protected
    FData: TDictionary<string, string>;
    function GetItems(const Key: String): String; override;
    procedure SetItems(const Key, Value: String); override;

  public
    function ToString: String; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TMVCApplicationSessionFactory = class sealed
  protected
    FRegisteredApplicationSessionTypes: TDictionary<String, TWebApplicationSessionClass>;
    class var FInstance: TMVCApplicationSessionFactory;
    constructor Create;
  public
    procedure RegisterSessionType(const AName: String;
      AWebApplicationSessionClass: TWebApplicationSessionClass);
    class function GetInstance: TMVCApplicationSessionFactory;
    function CreateNewByType(const AName: String): TWebApplicationSession;
    destructor Destroy; override;
  end;

implementation

uses
  System.dateutils;

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

function TWebApplicationSessionMemory.GetItems(const Key: String): String;
begin
  TMonitor.Enter(self);
  try
    if not FData.TryGetValue(Key, Result) then
      Result := '';
  finally
    TMonitor.Exit(self);
  end;
end;

procedure TWebApplicationSessionMemory.SetItems(const Key, Value: String);
begin
  TMonitor.Enter(self);
  try
    FData.AddOrSetValue(Key, Value);
  finally
    TMonitor.Exit(self);
  end;
end;

function TWebApplicationSessionMemory.ToString: String;
var
  Key: String;
begin
  TMonitor.Enter(self);
  try
    Result := '';
    for Key in FData.Keys do
    begin
      Result := Key + ' = ' + QuotedStr(FData.Items[Key]) + sLineBreak;
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

TMVCApplicationSessionFactory.GetInstance.RegisterSessionType('memory',
  TWebApplicationSessionMemory);

finalization

TMVCApplicationSessionFactory.FInstance.Free;

end.
