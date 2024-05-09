program TestServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  {$IFNDEF LINUX}
  Winapi.Windows,
  {$ENDIF }
  Web.WebBroker,
  MVCFramework.Commons,
  MVCFramework.Console,
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
  Entities in '..\Several\Entities.pas',
  EntitiesProcessors in '..\Several\EntitiesProcessors.pas',
  MVCFramework.JSONRPC.Client in '..\..\..\sources\MVCFramework.JSONRPC.Client.pas',
  MVCFramework.JSONRPC in '..\..\..\sources\MVCFramework.JSONRPC.pas',
  MVCFramework.Serializer.Commons,
  MVCFramework in '..\..\..\sources\MVCFramework.pas',
  MVCFramework.Serializer.Text in '..\..\..\sources\MVCFramework.Serializer.Text.pas',
  MVCFramework.Serializer.HTML in '..\..\..\sources\MVCFramework.Serializer.HTML.pas';

{$R *.res}

procedure Logo;
begin
  ResetConsole();
  Writeln;
  TextBackground(TConsoleColor.Black);
  TextColor(TConsoleColor.Red);
  Writeln(' ██████╗ ███╗   ███╗██╗   ██╗ ██████╗    ███████╗███████╗██████╗ ██╗   ██╗███████╗██████╗');
  Writeln(' ██╔══██╗████╗ ████║██║   ██║██╔════╝    ██╔════╝██╔════╝██╔══██╗██║   ██║██╔════╝██╔══██╗');
  Writeln(' ██║  ██║██╔████╔██║██║   ██║██║         ███████╗█████╗  ██████╔╝██║   ██║█████╗  ██████╔╝');
  Writeln(' ██║  ██║██║╚██╔╝██║╚██╗ ██╔╝██║         ╚════██║██╔══╝  ██╔══██╗╚██╗ ██╔╝██╔══╝  ██╔══██╗');
  Writeln(' ██████╔╝██║ ╚═╝ ██║ ╚████╔╝ ╚██████╗    ███████║███████╗██║  ██║ ╚████╔╝ ███████╗██║  ██║');
  Writeln(' ╚═════╝ ╚═╝     ╚═╝  ╚═══╝   ╚═════╝    ╚══════╝╚══════╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝╚═╝  ╚═╝');
  Writeln(' ');
  TextColor(TConsoleColor.White);
  Write('PLATFORM: ');
  {$IF Defined(Win32)} Writeln('WIN32'); {$ENDIF}
  {$IF Defined(Win64)} Writeln('WIN64'); {$ENDIF}
  {$IF Defined(Linux64)} Writeln('Linux64'); {$ENDIF}
  TextColor(TConsoleColor.Yellow);
  Writeln('DMVCFRAMEWORK VERSION: ', DMVCFRAMEWORK_VERSION);
  TextColor(TConsoleColor.White);
end;

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Logo;
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication :=
      TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LServer.MaxConnections := 0;
    LServer.ListenQueue := 200;
    Writeln('Press RETURN to stop the server');
    WaitForReturn;
    TextColor(TConsoleColor.Red);
    Writeln('Server stopped');
    ResetConsole();
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  gLocalTimeStampAsUTC := False;
  UseConsoleLogger := False;
  TMVCSqids.SQIDS_ALPHABET := dotEnv.Env('dmvc.sqids.alphabet', 'axDlw8dRnsPCrbZIAEMFG4TQ6gc3iWtOy9v5NBz0LfSmuKV71JHkUhYpej2Xqo');
  TMVCSqids.SQIDS_MIN_LENGTH := dotEnv.Env('dmvc.sqids.min_length', 6);
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8888);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
