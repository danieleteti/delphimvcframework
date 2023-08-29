// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
  TestServerControllerU,
  TestServerControllerExceptionU,
  SpeedMiddlewareU,
  MVCFramework.Middleware.Authentication,
  MVCFramework.ActiveRecordController,
  System.Generics.Collections,
  MVCFramework.Commons,
  TestServerControllerPrivateU,
  AuthHandlersU,
  TestServerControllerJSONRPCU,
  {$IFNDEF LINUX}
  MVCFramework.View.Renderers.Mustache,
  {$ENDIF}
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.StaticFiles, FireDAC.Comp.Client,
  MVCFramework.ActiveRecord, FDConnectionConfigU;

procedure TMainWebModule.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      // no config here
      Config[TMVCConfigKey.SessionTimeout] := '0'; // setting cookie
      Config[TMVCConfigKey.PathPrefix] := '';
      Config[TMVCConfigKey.ViewPath] := '..\templates';
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end, nil);
  MVCEngine
    .AddController(TTestServerController)
    .AddController(TTestPrivateServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddController(TTestServerControllerActionFilters)
    .AddController(TTestPrivateServerControllerCustomAuth)
    .AddController(TTestMultiPathController)
    .AddController(TTestActionResultController)
    .AddController(TTestJSONRPCController, '/jsonrpc')
    .AddController(TTestJSONRPCControllerWithGet, '/jsonrpcwithget')
    .AddController(TMVCActiveRecordController,
        function: TMVCController
        begin
          Result := TMVCActiveRecordController.Create(CON_DEF_NAME);
        end, '/api/entities')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCClass.Create
    end, '/jsonrpcclass')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCClassWithGET.Create
    end, '/jsonrpcclasswithget')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClass.Create
    end, '/jsonrpcclass1')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClassWithGet.Create
    end, '/jsonrpcclass1withget')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCHookClassWithGet.Create
    end, '/jsonrpcclass1withget')
    .AddController(TTestFaultController) // this will raise an exception
    .AddController(TTestFault2Controller,
    function: TMVCController
    begin
      Result := TTestFault2Controller.Create; // this will raise an exception
    end)
    .AddMiddleware(TMVCSpeedMiddleware.Create)
    .AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(TCustomAuthHandler.Create, '/system/users/logged'))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www', 'index.html', False))
    .AddMiddleware(TMVCStaticFilesMiddleware.Create('/spa', 'www', 'index.html', True))
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TBasicAuthHandler.Create))
    .AddMiddleware(TMVCCompressionMiddleware.Create);
{$IFDEF MSWINDOWS}
  MVCEngine.SetViewEngine(TMVCMustacheViewEngine);
  RegisterOptionalCustomTypesSerializers(MVCEngine.Serializers[TMVCMediaType.APPLICATION_JSON]);
{$ENDIF}
end;

end.
