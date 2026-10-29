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
//
// The ActiveRecord filters and the database session filters. A companion of
// MVCFramework.Filters, in a unit of its own so that the filters do not depend
// on FireDAC and TMVCActiveRecord.

unit MVCFramework.Filters.ActiveRecord;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Filters;                        // TMVCSessionCookieFlags

// -------------------------------------------------------------------------
// EndpointFilter helpers (per route group, attached via group.Use).
// -------------------------------------------------------------------------

// ActiveRecord lifecycle filter. Acquires a FireDAC connection from the
// FDManager pool keyed by ADefaultConnectionDefName before the handler
// runs and releases it inside a `finally` so the connection is returned
// to the pool even when the handler raises. The connection definitions are
// read from FDConnectionDefs.ini the first time a request arrives.
function ActiveRecordEndpointFilter(
  const ADefaultConnectionDefName: string): TMVCEndpointFilter;

// Database-backed session factory EndpointFilter — sibling of MemorySession.
// Requires an ActiveRecord connection in scope on the same group (i.e. the
// dmvc_sessions table is read/written via TMVCActiveRecord on the ambient
// connection). Combine with ActiveRecordEndpointFilter(...) earlier in the filter chain.
function DatabaseSessionEndpointFilter(const ATimeoutInMinutes: Integer = 0;
  const AConnectionDefName: string = '';
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCEndpointFilter;

// -------------------------------------------------------------------------
// HTTPFilter helpers (engine-wide, attached via lEngine.Use).
// Engine-wide, so controllers use them too.
// -------------------------------------------------------------------------

// Reads/writes the dmvc_sessions table through TMVCActiveRecord on the ambient
// connection: register ActiveRecordHTTPFilter before it.
function DatabaseSessionHTTPFilter(const ATimeoutInMinutes: Integer = 0;
  const AFlags: TMVCSessionCookieFlags = [scfHttpOnly]): TMVCHTTPFilter;

// ActiveRecord lifecycle HTTPFilter: acquires the connection(s) before the
// request is dispatched and releases them in a `finally`, so an exception in
// the action does not leak the connection. The connection definitions file is
// loaded once per process (an empty AConnectionDefFileName skips it). The
// second overload adds connections: AAdditionalARConnectionNames are the names
// the code uses, AAdditionalConnectionDefNames the FireDAC definitions behind
// them, same length.
//
//   lEngine.Use(ActiveRecordHTTPFilter('mydb', 'FDConnectionDefs.ini'));
function ActiveRecordHTTPFilter(const ADefaultConnectionDefName: string;
  const AConnectionDefFileName: string = 'FDConnectionDefs.ini'): TMVCHTTPFilter; overload;
function ActiveRecordHTTPFilter(const ADefaultConnectionDefName: string;
  const AAdditionalARConnectionNames: TArray<string>;
  const AAdditionalConnectionDefNames: TArray<string>;
  const AConnectionDefFileName: string = 'FDConnectionDefs.ini'): TMVCHTTPFilter; overload;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client,                         // FDManager
  MVCFramework.Commons,                        // EMVCConfigException
  MVCFramework.Logger,                         // LogE
  MVCFramework.ActiveRecord,                   // ActiveRecordConnectionsRegistry
  MVCFramework.Session.Database;               // TMVCWebSessionDatabaseFactory

type
  // The connections of one ActiveRecord filter: loads the definitions the
  // first time a request arrives, then adds the connections to the registry
  // for the request and takes them out again. Interface-managed, so the filter
  // closure that captures it frees it with the filter.
  IActiveRecordConnections = interface
    ['{4C1B6B1E-5D55-4F0B-9E0C-3B8B9D6C3A11}']
    procedure Open;
    procedure Close;
  end;

  TActiveRecordConnections = class(TInterfacedObject, IActiveRecordConnections)
  strict private
    fDefaultConnectionDefName: string;
    fConnectionDefFileName: string;
    fARConnectionNames: TArray<string>;
    fConnectionDefNames: TArray<string>;
    fLoaded: Boolean;
    procedure EnsureLoaded;
    procedure CheckConnectionDef(const AConnectionDefName: string);
  public
    constructor Create(const ADefaultConnectionDefName: string;
      const AARConnectionNames, AConnectionDefNames: TArray<string>;
      const AConnectionDefFileName: string);
    procedure Open;
    procedure Close;
  end;

constructor TActiveRecordConnections.Create(const ADefaultConnectionDefName: string;
  const AARConnectionNames, AConnectionDefNames: TArray<string>;
  const AConnectionDefFileName: string);
begin
  inherited Create;
  fDefaultConnectionDefName := ADefaultConnectionDefName;
  fARConnectionNames := Copy(AARConnectionNames);
  fConnectionDefNames := Copy(AConnectionDefNames);
  fConnectionDefFileName := AConnectionDefFileName;
end;

procedure TActiveRecordConnections.CheckConnectionDef(const AConnectionDefName: string);
begin
  if not FDManager.IsConnectionDef(AConnectionDefName) then
  begin
    // the full path goes to the log, the client gets the file name
    LogE('Connection definitions file: ' + FDManager.ActualConnectionDefFileName);
    raise EMVCConfigException.CreateFmt(
      'ConnectionDefName "%s" not found in config file "%s" - or config file not present',
      [AConnectionDefName, ExtractFileName(FDManager.ActualConnectionDefFileName)]);
  end;
end;

procedure TActiveRecordConnections.EnsureLoaded;
var
  lConnectionDefName: string;
begin
  if fLoaded then
    Exit;
  // FDManager is shared by every filter of the process: it is the lock
  TMonitor.Enter(FDManager);
  try
    if fLoaded then
      Exit;
    if fConnectionDefFileName <> '' then
    begin
      FDManager.ConnectionDefFileAutoLoad := False;
      FDManager.ConnectionDefFileName := fConnectionDefFileName;
      if not FDManager.ConnectionDefFileLoaded then
        FDManager.LoadConnectionDefFile;
    end;
    if fDefaultConnectionDefName <> '' then
      CheckConnectionDef(fDefaultConnectionDefName);
    if Length(fARConnectionNames) <> Length(fConnectionDefNames) then
      raise EMVCConfigException.Create(
        'AdditionalARConnectionNames must have the same length of AdditionalConnectionDefNames');
    for lConnectionDefName in fConnectionDefNames do
      CheckConnectionDef(lConnectionDefName);
    fLoaded := True;
  finally
    TMonitor.Exit(FDManager);
  end;
end;

procedure TActiveRecordConnections.Open;
var
  I: Integer;
begin
  EnsureLoaded;
  if fDefaultConnectionDefName <> '' then
    ActiveRecordConnectionsRegistry.AddDefaultConnection(fDefaultConnectionDefName);
  for I := 0 to High(fConnectionDefNames) do
    ActiveRecordConnectionsRegistry.AddConnection(fARConnectionNames[I], fConnectionDefNames[I]);
end;

procedure TActiveRecordConnections.Close;
var
  I: Integer;
begin
  if fDefaultConnectionDefName <> '' then
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection(False);
  for I := 0 to High(fConnectionDefNames) do
    ActiveRecordConnectionsRegistry.RemoveConnection(fARConnectionNames[I], False);
end;

function ActiveRecordEndpointFilter(const ADefaultConnectionDefName: string): TMVCEndpointFilter;
var
  lConnections: IActiveRecordConnections;
begin
  lConnections := TActiveRecordConnections.Create(ADefaultConnectionDefName, [], [], 'FDConnectionDefs.ini');
  Result :=
    function (const AContext: TWebContext;
              const ANext: TMVCEndpointFilterNext): IMVCResponse
    begin
      // Acquire BEFORE Next(), release in a finally so the connection goes back
      // to the pool even if the handler raises. The classic OnBeforeRouting /
      // OnAfterRouting hooks cannot express this: an exception skips the second.
      lConnections.Open;
      try
        Result := ANext();
      finally
        lConnections.Close;
      end;
    end;
end;

function ActiveRecordHTTPFilter(const ADefaultConnectionDefName: string;
  const AConnectionDefFileName: string): TMVCHTTPFilter;
begin
  Result := ActiveRecordHTTPFilter(ADefaultConnectionDefName, [], [], AConnectionDefFileName);
end;

function ActiveRecordHTTPFilter(const ADefaultConnectionDefName: string;
  const AAdditionalARConnectionNames: TArray<string>;
  const AAdditionalConnectionDefNames: TArray<string>;
  const AConnectionDefFileName: string): TMVCHTTPFilter;
var
  lConnections: IActiveRecordConnections;
begin
  lConnections := TActiveRecordConnections.Create(ADefaultConnectionDefName,
    AAdditionalARConnectionNames, AAdditionalConnectionDefNames, AConnectionDefFileName);
  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    begin
      lConnections.Open; // raises when a connection definition is missing
      try
        ANext();
      finally
        // OnAfterRouting of the classic middleware is skipped when the action raises: here it is not
        lConnections.Close;
      end;
    end;
end;

function DatabaseSessionEndpointFilter(const ATimeoutInMinutes: Integer;
  const AConnectionDefName: string;
  const AFlags: TMVCSessionCookieFlags): TMVCEndpointFilter;
begin
  // AConnectionDefName is currently inert in TMVCWebSessionDatabaseFactory
  // (it relies on the ambient ActiveRecord default connection) — kept on the
  // signature to document the dependency on ActiveRecordEndpointFilter(...) earlier in the
  // filter chain.
  Result := SessionEndpointFilter(TMVCWebSessionDatabaseFactory.Create(
    scfHttpOnly in AFlags, ATimeoutInMinutes, AConnectionDefName, scfSecure in AFlags));
end;

function DatabaseSessionHTTPFilter(const ATimeoutInMinutes: Integer;
  const AFlags: TMVCSessionCookieFlags): TMVCHTTPFilter;
begin
  // the connection definition name is not used by the factory: it takes the ambient ActiveRecord connection
  Result := SessionHTTPFilter(TMVCWebSessionDatabaseFactory.Create(
    scfHttpOnly in AFlags, ATimeoutInMinutes, 'notused', scfSecure in AFlags));
end;

end.
