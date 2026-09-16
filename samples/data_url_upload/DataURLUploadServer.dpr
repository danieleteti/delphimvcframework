program DataURLUploadServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.Logger,
  MVCFramework.Middleware.StaticFiles,
  DataURLTypeU in 'DataURLTypeU.pas',
  UploadDTOU in 'UploadDTOU.pas',
  UploadControllerU in 'UploadControllerU.pas';

{$R *.res}

const
  PORT = 8080;

procedure RunServer;
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  LogI('** DMVCFramework Data URL Upload Sample **');

  LEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
      // Data URLs balloon by ~33% over the raw bytes; raise the cap accordingly.
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(20 * 1024 * 1024);
    end);
  try
    LEngine.AddController(TUploadController);
    LEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/static', 'www'));

    // Plug TDataURL into JSON (de)serialization.
    LEngine.Serializers.Items[TMVCMediaType.APPLICATION_JSON]
      .RegisterTypeSerializer(TypeInfo(TDataURL), TDataURLSerializer.Create);

    LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
    LServer.Listen(PORT);
    try
      LogI('Server started on port ' + PORT.ToString + ' (Indy Direct)');
      LogI('Open your browser at:');
      LogI('  ==> http://localhost:' + PORT.ToString + '/static/index.html');
      LogI('Drop a file on the page to POST it as a JSON-embedded data URL.');
      LogI('The server decodes it via TDataURL custom type serializer and');
      LogI('saves the bytes under the "uploaded" folder next to the executable.');
      LogI('Press Ctrl+C to stop.');
      WaitForTerminationSignal;
      LogI('Shutting down...');
    finally
      LServer.Stop;
      LServer := nil;
    end;
  finally
    LEngine.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    RunServer;
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end;
end.
