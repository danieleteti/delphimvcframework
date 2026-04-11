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

unit WebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  FireDAC.Stan.StorageJSON
{$IFDEF MSWINDOWS}
    ,MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes
{$ENDIF}
    ;

type
  TMainWebModule = class(TWebModule)
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    procedure WebModuleCreate(Sender: TObject);
  private
    MVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TMainWebModule;

implementation

{$R *.dfm}


uses
  MVCFramework.Commons,
  TestServerEngineConfigU,
  System.IOUtils;

procedure TMainWebModule.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.PathPrefix] := '';
      Config[TMVCConfigKey.ViewPath] := TPath.Combine(AppPath, '..\templates');
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end);
  ConfigureTestEngine(MVCEngine);
end;

end.
