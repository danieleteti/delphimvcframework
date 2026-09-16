// ***************************************************************************
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// ***************************************************************************

unit LoggerPro.ExeWatchAppender;

(*
  ExeWatch appender for LoggerPro.

  THIS UNIT IS INTENTIONALLY OUTSIDE THE loggerproRT PACKAGE.

  Why: the ExeWatch SDK (https://exewatch.com) is a third-party dependency.
  Including this unit in the shared runtime package would force every
  LoggerPro user to have the SDK on their search path. By keeping the
  unit out of the .dpk / .dproj files, only users who actually want
  ExeWatch integration pick it up - they add this single .pas to their
  project and the SDK directory (DelphiCommons) to their search path.

  What this appender does at runtime:
    - Setup   -> InitializeExeWatch(apiKey, customerId, appVersion) ONLY if
                 the SDK is not already initialized (respects an app that
                 initialized the SDK directly elsewhere).
    - WriteLog -> EW.Debug/Info/Warning/Error/Fatal(message, tag)
                  (level mapping: TLogType -> SDK severity)
    - TearDown -> EW.Flush (ship any buffered events before returning).
                 The appender does NOT call FinalizeExeWatch: the SDK's
                 own "finalization" section handles process-exit cleanup,
                 and tearing down the SDK here would kill any other
                 component that relies on direct EW.* calls.

  Three ways to use this appender once the unit is in your project:

  1) Imperative (lowest friction):

       uses LoggerPro, LoggerPro.Builder, LoggerPro.ExeWatchAppender;

       Log := LoggerProBuilder
         .WriteToAppender(NewExeWatchAppender('ew_win_xxxxxx', 'SampleCustomer'))
         .Build;

  2) Fluent configurator:

       Log := WithExeWatch(LoggerProBuilder)
         .WithAPIKey('ew_win_xxxxxx')
         .WithCustomerId('SampleCustomer')
         .WithAppVersion('2.0.0')
         .Done
         .Build;

  3) JSON configuration file (factory auto-registered on unit load):

       {
         "appenders": [
           {
             "type": "ExeWatch",
             "apiKey": "ew_win_xxxxxx",
             "customerId": "SampleCustomer",
             "appVersion": "2.0.0"
           }
         ]
       }

  Caveats:
    - InitializeExeWatch is a process-global singleton. Creating multiple
      TLoggerProExeWatchAppender instances in the same process is
      undefined; the last Setup wins. For GUI apps that also use the
      ExeWatch SDK directly, prefer one or the other, not both.
    - Only the standard LoggerPro log-item fields flow through (timestamp,
      level, tag, message). To access the richer SDK features (breadcrumbs,
      timings, user identity, gauges, tags, periodic metrics), call the
      SDK directly via the EW global (e.g. EW.AddBreadcrumb, EW.SetUser)
      alongside LoggerPro - the two layers coexist freely.
*)

interface

uses
  System.SysUtils,
  System.Classes,
  LoggerPro,
  LoggerPro.Builder;

type
  /// <summary>
  /// Routes LoggerPro log items to the ExeWatch cloud via the official
  /// SDK. On Setup calls <c>InitializeExeWatch</c>; on WriteLog dispatches
  /// to <c>EW.Debug/Info/Warning/Error/Fatal</c> based on the log item's
  /// <c>TLogType</c>; on TearDown calls <c>FinalizeExeWatch</c>.
  /// </summary>
  TLoggerProExeWatchAppender = class(TLoggerProAppenderBase)
  private
    FAPIKey: string;
    FCustomerId: string;
    FAppVersion: string;
    FAnonymizeDeviceId: Boolean;
    FInitialized: Boolean;
  public
    constructor Create(const aAPIKey: string;
      const aCustomerId: string = '';
      const aAppVersion: string = '';
      aAnonymizeDeviceId: Boolean = False;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    property APIKey: string read FAPIKey;
    property CustomerId: string read FCustomerId;
    property AppVersion: string read FAppVersion;
    property AnonymizeDeviceId: Boolean read FAnonymizeDeviceId;
  end;

  /// <summary>Fluent configurator returned by <c>WithExeWatch(builder)</c>.
  /// <c>Done</c> adds the appender to the builder and returns the builder
  /// so the chain continues naturally.</summary>
  IExeWatchConfigurator = interface
    ['{9A6E3F9C-1B3E-4D5A-9F8D-8E2A4C0D1F0E}']
    function WithAPIKey(const aAPIKey: string): IExeWatchConfigurator;
    function WithCustomerId(const aCustomerId: string): IExeWatchConfigurator;
    function WithAppVersion(const aAppVersion: string): IExeWatchConfigurator;
    function WithAnonymizeDeviceId(aValue: Boolean): IExeWatchConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IExeWatchConfigurator;
    function Done: ILoggerProBuilder;
  end;

/// <summary>Imperative factory - useful with .WriteToAppender when you
/// already have the configuration values in variables.</summary>
function NewExeWatchAppender(const aAPIKey: string;
  const aCustomerId: string = '';
  const aAppVersion: string = '';
  aAnonymizeDeviceId: Boolean = False): ILogAppender;

/// <summary>Fluent extension - plug into an ILoggerProBuilder and chain
/// WithAPIKey / WithCustomerId / WithAppVersion / Done like any built-in
/// appender.</summary>
function WithExeWatch(const aBuilder: ILoggerProBuilder): IExeWatchConfigurator;

implementation

uses
  LoggerPro.Config,
  System.JSON,
  ExeWatchSDKv1;

type
  TExeWatchConfigurator = class(TInterfacedObject, IExeWatchConfigurator)
  private
    FBuilder: ILoggerProBuilder;
    FAPIKey: string;
    FCustomerId: string;
    FAppVersion: string;
    FAnonymizeDeviceId: Boolean;
    FLogLevel: TLogType;
    FLogLevelSet: Boolean;
  public
    constructor Create(const aBuilder: ILoggerProBuilder);
    function WithAPIKey(const aAPIKey: string): IExeWatchConfigurator;
    function WithCustomerId(const aCustomerId: string): IExeWatchConfigurator;
    function WithAppVersion(const aAppVersion: string): IExeWatchConfigurator;
    function WithAnonymizeDeviceId(aValue: Boolean): IExeWatchConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IExeWatchConfigurator;
    function Done: ILoggerProBuilder;
  end;

{ TLoggerProExeWatchAppender }

constructor TLoggerProExeWatchAppender.Create(const aAPIKey, aCustomerId,
  aAppVersion: string; aAnonymizeDeviceId: Boolean;
  aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  if aAPIKey.Trim.IsEmpty then
    raise ELoggerPro.Create('ExeWatch: apiKey cannot be empty');
  FAPIKey := aAPIKey;
  FCustomerId := aCustomerId;
  FAppVersion := aAppVersion;
  FAnonymizeDeviceId := aAnonymizeDeviceId;
  FInitialized := False;
end;

procedure TLoggerProExeWatchAppender.Setup;
begin
  inherited;
  // ExeWatch maintains a process-global singleton. If another part of
  // the application already initialized it, skip to avoid resetting state.
  if not ExeWatchIsInitialized then
  begin
    InitializeExeWatch(FAPIKey, FCustomerId, FAppVersion, FAnonymizeDeviceId);
    FInitialized := True;
  end;
end;

procedure TLoggerProExeWatchAppender.TearDown;
begin
  // Flush any queued events so they make it to the server before the
  // appender stops receiving items. We do NOT call FinalizeExeWatch:
  //   1. The SDK's own "finalization" section finalizes on process exit.
  //   2. If the app uses the SDK directly elsewhere, finalizing here
  //      would pull the rug from under those callers.
  // On a logger reconfigure (destroy + rebuild), Setup's
  //   "if not ExeWatchIsInitialized" guard keeps the existing SDK state
  //   intact - which is almost always what the user wants.
  if ExeWatchIsInitialized then
    EW.Flush;
  FInitialized := False;
  inherited;
end;

procedure TLoggerProExeWatchAppender.WriteLog(const aLogItem: TLogItem);
begin
  if not ExeWatchIsInitialized then
    Exit;
  // Map LoggerPro TLogType -> ExeWatch severity. Tag flows through
  // verbatim; ExeWatch uses it the same way LoggerPro does.
  case aLogItem.LogType of
    TLogType.Debug:   EW.Debug(aLogItem.LogMessage, aLogItem.LogTag);
    TLogType.Info:    EW.Info(aLogItem.LogMessage, aLogItem.LogTag);
    TLogType.Warning: EW.Warning(aLogItem.LogMessage, aLogItem.LogTag);
    TLogType.Error:   EW.Error(aLogItem.LogMessage, aLogItem.LogTag);
    TLogType.Fatal:   EW.Fatal(aLogItem.LogMessage, aLogItem.LogTag);
  end;
end;

{ TExeWatchConfigurator }

constructor TExeWatchConfigurator.Create(const aBuilder: ILoggerProBuilder);
begin
  inherited Create;
  FBuilder := aBuilder;
  FAnonymizeDeviceId := False;
  FLogLevel := TLogType.Debug;
  FLogLevelSet := False;
end;

function TExeWatchConfigurator.WithAPIKey(const aAPIKey: string): IExeWatchConfigurator;
begin
  FAPIKey := aAPIKey;
  Result := Self;
end;

function TExeWatchConfigurator.WithCustomerId(const aCustomerId: string): IExeWatchConfigurator;
begin
  FCustomerId := aCustomerId;
  Result := Self;
end;

function TExeWatchConfigurator.WithAppVersion(const aAppVersion: string): IExeWatchConfigurator;
begin
  FAppVersion := aAppVersion;
  Result := Self;
end;

function TExeWatchConfigurator.WithAnonymizeDeviceId(aValue: Boolean): IExeWatchConfigurator;
begin
  FAnonymizeDeviceId := aValue;
  Result := Self;
end;

function TExeWatchConfigurator.WithMinimumLevel(aLogLevel: TLogType): IExeWatchConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TExeWatchConfigurator.Done: ILoggerProBuilder;
var
  lAppender: TLoggerProExeWatchAppender;
begin
  lAppender := TLoggerProExeWatchAppender.Create(
    FAPIKey, FCustomerId, FAppVersion, FAnonymizeDeviceId);
  if FLogLevelSet then
    lAppender.SetMinimumLevel(FLogLevel);
  FBuilder.WriteToAppender(lAppender);
  Result := FBuilder;
end;

function NewExeWatchAppender(const aAPIKey, aCustomerId, aAppVersion: string;
  aAnonymizeDeviceId: Boolean): ILogAppender;
begin
  Result := TLoggerProExeWatchAppender.Create(
    aAPIKey, aCustomerId, aAppVersion, aAnonymizeDeviceId);
end;

function WithExeWatch(const aBuilder: ILoggerProBuilder): IExeWatchConfigurator;
begin
  Result := TExeWatchConfigurator.Create(aBuilder);
end;

{ JSON config auto-registration }

procedure ExeWatchConfigFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IExeWatchConfigurator;
  lValue: TJSONValue;
  lLogLevel: TLogType;
begin
  lCfg := WithExeWatch(aBuilder);

  lValue := aConfig.GetValue('apiKey');
  if lValue is TJSONString then
    lCfg := lCfg.WithAPIKey(TJSONString(lValue).Value);

  lValue := aConfig.GetValue('customerId');
  if lValue is TJSONString then
    lCfg := lCfg.WithCustomerId(TJSONString(lValue).Value);

  lValue := aConfig.GetValue('appVersion');
  if lValue is TJSONString then
    lCfg := lCfg.WithAppVersion(TJSONString(lValue).Value);

  lValue := aConfig.GetValue('anonymizeDeviceId');
  if lValue is TJSONBool then
    lCfg := lCfg.WithAnonymizeDeviceId(TJSONBool(lValue).AsBoolean);

  if TLoggerProConfig.TryGetJSONLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);

  lCfg.Done;
end;

initialization
  // Auto-register the JSON factory on unit load. The mere presence of
  // "uses LoggerPro.ExeWatchAppender" in the main program enables the
  // "ExeWatch" appender type in config JSON files - no explicit call
  // from user code is required.
  TLoggerProConfig.RegisterAppenderType(
    'ExeWatch',
    ExeWatchConfigFactory,
    ['apiKey', 'customerId', 'appVersion', 'anonymizeDeviceId', 'minimumLevel']);

end.
