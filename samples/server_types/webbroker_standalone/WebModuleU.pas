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

unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TSampleWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TSampleWebModule;

implementation

{$R *.dfm}

uses
  EngineConfigU;

procedure TSampleWebModule.WebModuleCreate(Sender: TObject);
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  FEngine := TMVCEngine.Create(Self);
  {$WARN SYMBOL_DEPRECATED ON}
  ConfigureEngine(FEngine);
end;

procedure TSampleWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
