// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano M�ller (ezequieljuliano@gmail.com)
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

unit App.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Serializer.JsonDataObjects;

type

  TAppWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TAppWebModule;

implementation

{$R *.dfm}

uses
  MVCFramework.Commons,
  Person.Controller;

procedure TAppWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(AConfig: TMVCConfig)
    begin
      // enable static files
      AConfig[TMVCConfigKey.DocumentRoot] := ExtractFilePath(GetModuleName(HInstance)) + '\www';
      // session timeout (0 means session cookie)
      AConfig[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      AConfig[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      AConfig[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      AConfig[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      AConfig[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      AConfig[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      AConfig[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Define a default URL for requests that don't map to a route or a file (useful for client side web app)
      AConfig[TMVCConfigKey.FallbackResource] := 'index.html';
    end);
  FMVCEngine.AddController(TPersonController);
  FMVCEngine.AddSerializer(TMVCMediaType.APPLICATION_JSON, TMVCJsonDataObjectsSerializer.Create);
end;

procedure TAppWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
