// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
// Contribution on this file: Copyright (c) 2018 - João Antônio Duarte
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

unit MVCFramework.Controllers.Register;

interface

uses
  System.Generics.Collections, MVCFramework;

type
  TControllerDelegateItem = record
    Clazz: TMVCControllerClazz;
    CreateAction: TMVCControllerCreateAction;
    ServerName: string;
    constructor Create(AClazz: TMVCControllerClazz; ACreateAction: TMVCControllerCreateAction; AServerName: string);
  end;

  TControllersRegister = class
  private
    FControllers: TDictionary<TMVCControllerClazz, string>;
    FControllersDelegate: TList<TControllerDelegateItem>;
    class var FInstance: TControllersRegister;
    class function GetInstance: TControllersRegister; static;
    class procedure ReleaseInstance;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Call this procedure after creating your MVCEngine on your Webmodule</summary>
    /// <param name="AEngine">TMVCEngine class</param>
    /// <param name="AServerName">Name of the controller server. Useful when you have multiple servers in a server container</param>
    procedure AddControllersInEngine(AEngine: TMVCEngine; const AServerName: string = '');

    /// <summary>register your controller using this procedure</summary>
    /// <param name="AController">Your controller class</param>
    /// <param name="AServerName">Name of the controller server. Useful when you have multiple servers in a server container</param>
    procedure RegisterController(AController: TMVCControllerClazz; const AServerName: string = ''); overload;

    /// <summary>register your controller using this procedure when need a create action</summary>
    /// <param name="AControllerClazz">Your controller class</param>
    /// <param name="ACreateAction">Your create action to initialize construtor class</param>
    /// <param name="AServerName">Name of the controller server. Useful when you have multiple servers in a server container</param>
    procedure RegisterController(const AControllerClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction; const AServerName: string = ''); overload;

    class property Instance: TControllersRegister read GetInstance;
  end;

implementation

uses
  System.SysUtils;

{ TControllerDelegateItem }

constructor TControllerDelegateItem.Create(AClazz: TMVCControllerClazz;
  ACreateAction: TMVCControllerCreateAction; AServerName: string);
begin
  Clazz := AClazz;
  CreateAction := ACreateAction;
  ServerName := AServerName;
end;

{ TControllersRegister }

procedure TControllersRegister.AddControllersInEngine(AEngine: TMVCEngine;
  const AServerName: string);
var
  LControllerClass: TMVCControllerClazz;
  delegate: TControllerDelegateItem;
begin
  FControllers.TrimExcess;

  for LControllerClass in FControllers.Keys do
  begin
    if SameText(FControllers.Items[LControllerClass], AServerName) then
    begin
      AEngine.AddController(LControllerClass);
    end;
  end;

  FControllersDelegate.TrimExcess;
  for delegate in FControllersDelegate do
  begin
    if SameText(delegate.ServerName, AServerName) then
    begin
      AEngine.AddController(delegate.Clazz, delegate.CreateAction);
    end;
  end;
end;

constructor TControllersRegister.Create;
begin
  inherited;
  FControllers := TDictionary<TMVCControllerClazz, string>.Create;
  FControllersDelegate := TList<TControllerDelegateItem>.Create;
end;

destructor TControllersRegister.Destroy;
begin
  FControllers.Free;
  FControllersDelegate.Free;
  inherited;
end;

class function TControllersRegister.GetInstance: TControllersRegister;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TControllersRegister.Create;
  end;
  Result := FInstance;
end;

procedure TControllersRegister.RegisterController(AController: TMVCControllerClazz;
  const AServerName: string);
begin
  FControllers.Add(AController, AServerName);
end;

procedure TControllersRegister.RegisterController(
  const AControllerClazz: TMVCControllerClazz;
  const ACreateAction: TMVCControllerCreateAction; const AServerName: string);
begin
  FControllersDelegate.Add(TControllerDelegateItem.Create(AControllerClazz, ACreateAction, AServerName));
end;

class procedure TControllersRegister.ReleaseInstance;
begin
  if Assigned(FInstance) then
  begin
    FreeAndNil(FInstance);
  end;
end;

initialization

finalization

TControllersRegister.ReleaseInstance;

end.
