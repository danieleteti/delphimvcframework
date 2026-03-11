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
// *************************************************************************** }

unit RangeMediaTestWebModuleU;

// WebModule used exclusively by RangeMediaMiddlewareTestsU.
// Registers UseRangeMediaMiddleware on the '/testmedia' prefix, serving
// files from a temporary directory ('testmedia_tmp') that the test fixture
// creates and destroys around each test run.

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TRangeMediaTestWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FEngine: TMVCEngine;
  end;

var
  RangeMediaTestWebModuleClass: TComponentClass = TRangeMediaTestWebModule;

implementation

uses
  MVCFramework.Middleware.RangeMedia;

{$R *.dfm}

procedure TRangeMediaTestWebModule.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self);
  FEngine.AddMiddleware(
    UseRangeMediaMiddleware('/testmedia', 'testmedia_tmp')
  );
end;

procedure TRangeMediaTestWebModule.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
