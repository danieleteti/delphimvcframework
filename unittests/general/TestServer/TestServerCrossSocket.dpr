program TestServerCrossSocket;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  FireDAC.Stan.StorageJSON,
  MVCFramework.Logger,
  MVCFramework.Commons,
  {$IFNDEF LINUX}
  Winapi.Windows,
  {$ENDIF}
  MVCFramework.Console,
  MVCFramework.Server.Intf,
  MVCFramework.Server.CrossSocket,
  MVCFramework,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Text in '..\..\..\sources\MVCFramework.Serializer.Text.pas',
  MVCFramework.Serializer.HTML in '..\..\..\sources\MVCFramework.Serializer.HTML.pas',
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
  TestServerEngineConfigU in 'TestServerEngineConfigU.pas';

{$R *.res}

const
  gPLATFORM = {$IF Defined(Win32)} 'WIN32' {$ENDIF}
  {$IF Defined(Win64)} 'WIN64' {$ENDIF}
  {$IF Defined(Linux64)} 'Linux64' {$ENDIF}
  ;

procedure Logo;
begin
  Box('DMVCFramework TEST SERVER (CrossSocket)',
  [
      'PLATFORM'.PadRight(25) + gPLATFORM,
      'DMVCFRAMEWORK VERSION'.PadRight(25) + DMVCFRAMEWORK_VERSION,
      'SERVER TYPE'.PadRight(25) + 'TMVCCrossSocketServer (IOCP)',
      'OS Version'.PadRight(25) + TOSVersion.ToString
  ], 100);
end;

procedure RunServer(APort: Integer);
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
  LConfigAction: TProc<TMVCConfig>;
begin
  Logo;
  WriteLn(Format('Starting CrossSocket HTTP Server on port %d', [APort]));

  LConfigAction :=
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.PathPrefix] := '';
      Config[TMVCConfigKey.ViewPath] := TPath.Combine(AppPath, '..\templates');
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
    end;
  LEngine := TMVCEngine.Create(LConfigAction);
  try
    ConfigureTestEngine(LEngine);
    LServer := TMVCCrossSocketServer.Create(LEngine);
    LServer.Listen(APort);
    WriteLn('Press RETURN to stop the server');
    while GetCh() <> Char(KEY_ENTER) do;
    LServer.Stop;
    LServer := nil;
  finally
    LEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  gLocalTimeStampAsUTC := False;
  UseConsoleLogger := False;
  EnableUTF8Console;
  TMVCSqids.SQIDS_ALPHABET := dotEnv.Env('dmvc.sqids.alphabet', 'axDlw8dRnsPCrbZIAEMFG4TQ6gc3iWtOy9v5NBz0LfSmuKV71JHkUhYpej2Xqo');
  TMVCSqids.SQIDS_MIN_LENGTH := dotEnv.Env('dmvc.sqids.min_length', 6);
  try
    RunServer(8888);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
