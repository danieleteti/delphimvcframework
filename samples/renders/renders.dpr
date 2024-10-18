// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

program renders;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  MVCFramework.Commons,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Console,
  MVCFramework.Logger,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  RenderSampleControllerU in 'RenderSampleControllerU.pas',
  BusinessObjectsU in '..\commons\BusinessObjectsU.pas',
  MyDataModuleU in 'MyDataModuleU.pas' {MyDataModule: TDataModule},
  CustomTypesU in 'CustomTypesU.pas',
  CustomTypesSerializersU in 'CustomTypesSerializersU.pas',
  InMemoryDataU in 'InMemoryDataU.pas',
  MVCFramework.DataSet.Utils in '..\..\sources\MVCFramework.DataSet.Utils.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas',
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes in '..\..\sources\MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes.pas',
  MVCFramework.Serializer.JsonDataObjects.CustomTypes in '..\..\sources\MVCFramework.Serializer.JsonDataObjects.CustomTypes.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    TextColor(Red);
    LogI('DMVCFRAMEWORK VERSION: ' + DMVCFRAMEWORK_VERSION);
    ResetConsole;
    LogI('Press RETURN to stop the server');
    WaitForReturn;
    LogI('Stopping...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  MVCSerializeNulls := True;
  UseConsoleLogger := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, E.Message);
  end

end.
