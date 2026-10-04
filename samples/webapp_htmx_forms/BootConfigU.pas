// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit BootConfigU;

// Startup configuration: dotEnv, LoggerPro, DMVC profiler, TemplatePro context.
// Call Boot once, before any LogI/LogW/LogE call.

interface

procedure Boot;

implementation

uses
  System.SysUtils,  
  LoggerPro,
  LoggerPro.Builder,
  LoggerPro.ConsoleAppender,
  MVCFramework.Logger.ColorConsoleRenderer,
  TemplateProHelpersU,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Logger;

{ --- private ---------------------------------------------------------------- }

procedure ConfigDotEnv;
begin
  // .UseLogger intentionally omitted: the dotEnv fallback calls LogI, which would
  // create a default logger before ConfigLogger installs the configured one.
  dotEnvConfigure(
    function: IMVCDotEnv
    begin
      Result := NewDotEnv
                 .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                                     //if available, by default, loads default environment (.env)
                 .UseProfile('test') //if available loads the test environment (.env.test)
                 .UseProfile('prod') //if available loads the prod environment (.env.prod)
                 .Build(AppPath);    //uses the executable folder to look for .env* files
    end);
end;

procedure ConfigLogger;
var
  lBuilder: ILoggerProBuilder;
  
begin
  lBuilder := LoggerProBuilder
    .WithDefaultMinimumLevel(TLogType.Debug)
    .WriteToConsole
      .WithUTF8Output
      .WithRenderer(TMVCColorConsoleRenderer.Create)
      .Done
    ;


  SetDefaultLogger(lBuilder.Build);
end;

procedure ConfigProfiler;
begin
{$IF CompilerVersion >= 34} //SYDNEY+
  if dotEnv.Env('dmvc.profiler.enabled', False) then
  begin
    Profiler.ProfileLogger := Log;
    Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 1000);
    Profiler.LogsOnlyIfOverThreshold := dotEnv.Env('dmvc.profiler.logs_only_over_threshold', True);
  end;
{$ENDIF}
end;

{ --- public ----------------------------------------------------------------- }

procedure Boot;
begin
  ConfigDotEnv;
  ConfigLogger;
  ConfigProfiler;
  TemplateProContextConfigure;
end;

end.
