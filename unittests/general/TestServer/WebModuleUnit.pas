// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
  MVCFramework
{$IFDEF MSWINDOWS}
    ,
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes
{$ENDIF}
    ;

type
  Tbas = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    MVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = Tbas;

implementation

{$R *.dfm}


uses
  TestServerControllerU,
  TestServerControllerExceptionU,
  SpeedMiddlewareU,
  MVCFramework.Middleware.Authentication,
  System.Generics.Collections,
  MVCFramework.Commons,
  TestServerControllerPrivateU,
  AuthHandlersU,
  TestServerControllerJSONRPCU,
  MVCFramework.Middleware.Compression;

procedure Tbas.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      // no config here
      Config[TMVCConfigKey.SessionTimeout] := '0'; // setting cookie
      Config[TMVCConfigKey.PathPrefix] := '';
    end, nil);
  MVCEngine.AddController(TTestServerController)
    .AddController(TTestPrivateServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddController(TTestServerControllerActionFilters)
    .AddController(TTestPrivateServerControllerCustomAuth)
    .AddController(TTestJSONRPCController, '/jsonrpc')
    .PublishObject(
    function: TObject
    begin
      Result := TTestJSONRPCClass.Create
    end, '/jsonrpcclass')
    .AddController(TTestFaultController) // this will raise an exception
    .AddController(TTestFault2Controller,
    function: TMVCController
    begin
      Result := TTestFault2Controller.Create; // this will raise an exception
    end)
    .AddMiddleware(TMVCSpeedMiddleware.Create)
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TBasicAuthHandler.Create))
    .AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(TCustomAuthHandler.Create, '/system/users/logged'))
    .AddMiddleware(TMVCCompressionMiddleware.Create);
{$IFDEF MSWINDOWS}
  RegisterOptionalCustomTypesSerializers(MVCEngine.Serializers[TMVCMediaType.APPLICATION_JSON]);
{$ENDIF}
end;

end.
