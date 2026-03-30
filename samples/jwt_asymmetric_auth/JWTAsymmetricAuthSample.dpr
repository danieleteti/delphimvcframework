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
// JWT Asymmetric Authentication Sample
//
// Demonstrates RS256 JWT authentication using TaurusTLS (OpenSSL 1.1.1+).
// The server signs tokens with a PRIVATE key and verifies them with a PUBLIC key.
// This is fundamentally different from HS256 where the same secret is used for both.
//
// IMPORTANT: This sample uses modern OpenSSL (1.1.1+) via TaurusTLS,
// NOT the old OpenSSL 1.0.x libraries bundled with Delphi/Indy.
// Make sure to deploy the correct OpenSSL DLLs with your application.
//
// ***************************************************************************

program JWTAsymmetricAuthSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Signal,
  IdContext,
  IdHTTPWebBrokerBridge,
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  AuthHandlerU in 'AuthHandlerU.pas',
  ControllerU in 'ControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.Active := True;
    LogI('Listening on http://localhost:' + APort.ToString);
    LogI('Open http://localhost:' + APort.ToString + ' in your browser');
    LogI('Press Ctrl+C to shut down.');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  IsMultiThread := True;
  UseConsoleLogger := True;

  LogI('** JWT Asymmetric Auth Sample (RS256) ** build ' + DMVCFRAMEWORK_VERSION);
  LogI('Using TaurusTLS for modern OpenSSL (1.1.1+) RSA operations');

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
