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
// Apache 2.4 module variant of the test server. Built as a shared library
// (mod_dmvctest.dll on Windows, mod_dmvctest.so on Linux) and loaded into a
// portable Apache HTTPD instance set up by tasks.py for integration tests.
//
// httpd.conf entries (see invoke task tests32-apache):
//
//   LoadModule dmvc_module modules/mod_dmvctest.dll
//   <Location />
//     SetHandler mod_dmvc-handler
//   </Location>
//
// ***************************************************************************

library TestServerApache;

uses
  Winapi.ActiveX,
  Winapi.Windows,
  System.Win.ComObj,
  System.Classes,
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPDMethods,
  Web.HTTPD24Impl,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Text in '..\..\..\sources\MVCFramework.Serializer.Text.pas',
  MVCFramework.Serializer.HTML in '..\..\..\sources\MVCFramework.Serializer.HTML.pas',
  WebModuleUnit in 'WebModuleUnit.pas' {MainWebModule: TWebModule},
  TestServerControllerU in 'TestServerControllerU.pas',
  TestServerControllerExceptionU in 'TestServerControllerExceptionU.pas',
  SpeedMiddlewareU in 'SpeedMiddlewareU.pas',
  TestServerControllerPrivateU in 'TestServerControllerPrivateU.pas',
  AuthHandlersU in 'AuthHandlersU.pas',
  BusinessObjectsU in '..\..\..\samples\commons\BusinessObjectsU.pas',
  TestServerControllerJSONRPCU in 'TestServerControllerJSONRPCU.pas',
  RandomUtilsU in '..\..\..\samples\commons\RandomUtilsU.pas',
  MVCFramework.Tests.Serializer.Entities in '..\..\common\MVCFramework.Tests.Serializer.Entities.pas',
  FDConnectionConfigU in '..\..\common\FDConnectionConfigU.pas',
  Entities in '..\TestClient\Entities.pas',
  EntitiesProcessors in '..\TestClient\EntitiesProcessors.pas',
  MVCFramework.JSONRPC.Client in '..\..\..\sources\MVCFramework.JSONRPC.Client.pas',
  MVCFramework.JSONRPC in '..\..\..\sources\MVCFramework.JSONRPC.pas',
  MVCFramework in '..\..\..\sources\MVCFramework.pas';

var
  GModuleData: TApacheModuleData;

exports
  GModuleData name 'dmvc_module';

begin
  CoInitFlags := COINIT_MULTITHREADED;

  // Match the same Sqids alphabet used by the other TestServer variants so
  // that ID-encoding tests stay consistent across all test fixtures.
  TMVCSqids.SQIDS_ALPHABET := 'axDlw8dRnsPCrbZIAEMFG4TQ6gc3iWtOy9v5NBz0LfSmuKV71JHkUhYpej2Xqo';
  TMVCSqids.SQIDS_MIN_LENGTH := 6;

  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
