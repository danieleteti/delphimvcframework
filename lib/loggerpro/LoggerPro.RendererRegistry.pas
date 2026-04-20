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
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

// Process-wide registry mapping string names to TLogItemRenderer class
// references. Enables JSON configuration files to select a renderer by name
// via the Console appender's "renderer" field, and lets application or library
// code add custom renderers without forking LoggerPro.
//
// Built-in renderers declared in LoggerPro.Renderers self-register from that
// unit's initialization section. Additional renderers shipped by other units
// (including third-party code such as DMVCFramework's gin-style console
// renderer) follow the same pattern:
//
//     initialization
//       RegisterRenderer('MyRenderer', TMyLogItemRenderer);
//
// The "renderer" field in loggerpro.json then resolves any registered name:
//
//     "type": "Console", "renderer": "MyRenderer", "utf8Output": true
//
// Lookup is case-insensitive. Unknown names raise ELoggerProConfigError at
// config-load time, so typos fail fast instead of silently falling back.

unit LoggerPro.RendererRegistry;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  LoggerPro;

/// Adds or replaces a registry entry. Safe to call from multiple units'
/// initialization sections; later calls win (last-registration wins).
/// Raises EArgumentException if aName is empty or aClass is nil.
procedure RegisterRenderer(const aName: string; aClass: TLogItemRendererClass);

/// Removes a registry entry. No-op if the name isn't registered.
procedure UnregisterRenderer(const aName: string);

/// True iff aName is registered (case-insensitive).
function IsRendererRegistered(const aName: string): Boolean;

/// Returns True and an ILogItemRenderer instance when aName is registered.
/// Returns False and leaves aRenderer=nil otherwise.
/// Lookup is case-insensitive. The caller owns the returned instance via the
/// interface reference.
function TryCreateRenderer(const aName: string; out aRenderer: ILogItemRenderer): Boolean;

/// Enumerates registered renderer names in lower-case. Intended for
/// diagnostics / error messages, not hot paths.
function RegisteredRendererNames: TArray<string>;

implementation

uses
  System.SysUtils,
  System.Generics.Collections;

var
  gLock: TObject;
  gRegistry: TDictionary<string, TLogItemRendererClass>;

procedure RegisterRenderer(const aName: string; aClass: TLogItemRendererClass);
begin
  if aName.IsEmpty then
    raise EArgumentException.Create('RegisterRenderer: name cannot be empty');
  if aClass = nil then
    raise EArgumentException.Create('RegisterRenderer: class reference cannot be nil');
  TMonitor.Enter(gLock);
  try
    gRegistry.AddOrSetValue(aName.ToLower, aClass);
  finally
    TMonitor.Exit(gLock);
  end;
end;

procedure UnregisterRenderer(const aName: string);
begin
  if aName.IsEmpty then
    Exit;
  TMonitor.Enter(gLock);
  try
    gRegistry.Remove(aName.ToLower);
  finally
    TMonitor.Exit(gLock);
  end;
end;

function IsRendererRegistered(const aName: string): Boolean;
begin
  if aName.IsEmpty then
    Exit(False);
  TMonitor.Enter(gLock);
  try
    Result := gRegistry.ContainsKey(aName.ToLower);
  finally
    TMonitor.Exit(gLock);
  end;
end;

function TryCreateRenderer(const aName: string; out aRenderer: ILogItemRenderer): Boolean;
var
  lClass: TLogItemRendererClass;
begin
  aRenderer := nil;
  if aName.IsEmpty then
    Exit(False);
  TMonitor.Enter(gLock);
  try
    Result := gRegistry.TryGetValue(aName.ToLower, lClass);
  finally
    TMonitor.Exit(gLock);
  end;
  if Result then
    aRenderer := lClass.Create;
end;

function RegisteredRendererNames: TArray<string>;
begin
  TMonitor.Enter(gLock);
  try
    Result := gRegistry.Keys.ToArray;
  finally
    TMonitor.Exit(gLock);
  end;
end;

initialization
  gLock := TObject.Create;
  gRegistry := TDictionary<string, TLogItemRendererClass>.Create;

finalization
  gRegistry.Free;
  gLock.Free;

end.
