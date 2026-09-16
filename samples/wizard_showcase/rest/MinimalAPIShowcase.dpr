// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************


program MinimalAPIShowcase;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Signal,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Factory,
  MVCFramework.MinimalAPI,
  MVCFramework.Container,
  RoutesU in 'RoutesU.pas',
  ShowcaseModelsU in 'ShowcaseModelsU.pas',
  EntitiesU in 'EntitiesU.pas',
  ServicesU in 'ServicesU.pas',
  BootConfigU in 'BootConfigU.pas',
  EngineConfigU in 'EngineConfigU.pas';

{$R *.res}

procedure RunServer(aPort: Integer);
var
  LEngine: TMVCEngine;
  LServer: IMVCServer;
begin
  LEngine := TMVCEngine.Create(
    procedure(Config: TMVCConfig)
    begin
      //default content-type
      Config[TMVCConfigKey.DefaultContentType] := dotEnv.Env('dmvc.default.content_type', TMVCConstants.DEFAULT_CONTENT_TYPE);
      //default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := dotEnv.Env('dmvc.default.content_charset', TMVCConstants.DEFAULT_CONTENT_CHARSET);
      //unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := dotEnv.Env('dmvc.allow_unhandled_actions', 'false');
      //enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := dotEnv.Env('dmvc.load_system_controllers', 'true');
      //default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := dotEnv.Env('dmvc.default.view_file_extension', 'html');
      //view path
      Config[TMVCConfigKey.ViewPath] := dotEnv.Env('dmvc.view_path', TPath.Combine(AppPath, 'templates'));
      //Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := dotEnv.Env('dmvc.max_entities_record_count', IntToStr(TMVCConstants.MAX_RECORD_COUNT));
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := dotEnv.Env('dmvc.expose_server_signature', 'false');
      //Enable X-Powered-By Header in response
      Config[TMVCConfigKey.ExposeXPoweredBy] := dotEnv.Env('dmvc.expose_x_powered_by', 'true');
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := dotEnv.Env('dmvc.max_request_size', IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE));
    end);
  try
    ConfigureEngine(LEngine);
    // Routes are intentionally separated from ConfigureEngine: this is the
    // single line a developer scans first to find/edit URL handlers.
    ConfigureRoutes(LEngine.Root);
    LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
    LogI('Starting on http://localhost:' + APort.ToString +
      ' (Indy Direct). Press Ctrl+C to shut down.');
    // RunAndWait: Listen + WaitForTerminationSignal + EnterInShutdownState +
    // Stop. Console-only. For VCL/FMX/test hosts use Listen() + Stop().
    LServer.RunAndWait(APort);
    LServer := nil;
  finally
    LEngine.Free;
  end;
end;

begin
  { Enable ReportMemoryLeaksOnShutdown during debug }
  // ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  // DMVCFramework Specific Configurations
  MVCSerializeNulls := True;

  // BootConfigU.Boot: dotEnv + logger + profiler (+ TemplatePro context if any).
  // Must run before the first LogI. Edit BootConfigU to tune anything.
  Boot;

  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);

  try


    RegisterServices(DefaultMVCServiceContainer);
    DefaultMVCServiceContainer.Build;

    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
