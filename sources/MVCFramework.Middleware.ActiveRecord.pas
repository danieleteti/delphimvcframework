// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit MVCFramework.Middleware.ActiveRecord;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type

  TMVCActiveRecordMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fDefaultConnectionDefName: string;
    fConnectionDefFileName: string;
    fConnectionLoaded: Boolean;
    fAdditionalARConnectionNames: TArray<String>;
    fAdditionalConnectionDefNames: TArray<String>;
    fAdditionalARConnectionNamesCount: Integer;
    fAdditionalConnectionDefNamesCount: Integer;
  protected
    procedure EnsureConnection;
    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean);

    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      );

    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      const AHandled: Boolean
      );

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean);
  public
    constructor Create(
      const DefaultConnectionDefName: string); overload; virtual;
    constructor Create(
      const DefaultConnectionDefName: string;
      const ConnectionDefFileName: string{ = 'FDConnectionDefs.ini'}); overload; virtual;
    constructor Create(
      const DefaultConnectionDefName: string;
      const AdditionalARConnectionNames: TArray<String>;
      const AdditionalConnectionDefNames: TArray<String>;
      const ConnectionDefFileName: string{ = 'FDConnectionDefs.ini'}); overload; virtual;
  end;

implementation

uses
  MVCFramework.ActiveRecord,
  System.SyncObjs,
  FireDAC.Comp.Client;

var
  gCONNECTION_DEF_FILE_LOADED: Integer = 0;

{ TMVCActiveRecordMiddleware }

constructor TMVCActiveRecordMiddleware.Create(const DefaultConnectionDefName: string;
  const ConnectionDefFileName: string);
begin
  Create(DefaultConnectionDefName, [], [], ConnectionDefFileName);
end;

constructor TMVCActiveRecordMiddleware.Create(
  const DefaultConnectionDefName: string;
  const AdditionalARConnectionNames, AdditionalConnectionDefNames: TArray<String>;
  const ConnectionDefFileName: string);
begin
  inherited Create;
  fConnectionLoaded := False;
  fDefaultConnectionDefName := DefaultConnectionDefName;
  fConnectionDefFileName := ConnectionDefFileName;
  fAdditionalARConnectionNames := AdditionalARConnectionNames;
  fAdditionalConnectionDefNames := AdditionalConnectionDefNames;
end;

constructor TMVCActiveRecordMiddleware.Create(
  const DefaultConnectionDefName: string);
begin
  Create(DefaultConnectionDefName, 'FDConnectionDefs.ini');
end;

procedure TMVCActiveRecordMiddleware.EnsureConnection;
var
  I: Integer;
begin
  if fConnectionLoaded then
  begin
    Exit;
  end;

  TMonitor.Enter(Self);
  try
    if fConnectionLoaded then
    begin
      Exit;
    end;
    if TInterlocked.CompareExchange(gCONNECTION_DEF_FILE_LOADED, 1, 0) = 0 then
    begin
      if not fConnectionDefFileName.IsEmpty then
      begin
        FDManager.ConnectionDefFileAutoLoad := False;
        FDManager.ConnectionDefFileName := fConnectionDefFileName;
        if not FDManager.ConnectionDefFileLoaded then
        begin
          FDManager.LoadConnectionDefFile;
        end;
      end;

      //loading default connection
      if not fDefaultConnectionDefName.IsEmpty then
      begin
        if not FDManager.IsConnectionDef(fDefaultConnectionDefName) then
        begin
          raise EMVCConfigException.CreateFmt('ConnectionDefName "%s" not found in config file "%s" - or config file not present',
            [fDefaultConnectionDefName, FDManager.ActualConnectionDefFileName]);
        end;
      end;

      //loading additional connections
      fAdditionalARConnectionNamesCount := Length(fAdditionalARConnectionNames);
      fAdditionalConnectionDefNamesCount := Length(fAdditionalConnectionDefNames);
      if (fAdditionalARConnectionNamesCount > 0) or (fAdditionalConnectionDefNamesCount > 0) then
      begin
        if fAdditionalARConnectionNamesCount <> fAdditionalConnectionDefNamesCount then
        begin
          raise EMVCConfigException.Create('AdditionalARConnectionNames must have the same length of AdditionalConnectionDefNames');
        end;
        for I := 0 to fAdditionalConnectionDefNamesCount - 1 do
        begin
          if not FDManager.IsConnectionDef(fAdditionalConnectionDefNames[I]) then
          begin
            raise EMVCConfigException.CreateFmt('ConnectionDefName "%s" not found in config file "%s"',
              [fAdditionalConnectionDefNames[I], FDManager.ActualConnectionDefFileName]);
          end;
        end;
      end;
    end;
    fConnectionLoaded := True;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TMVCActiveRecordMiddleware.OnAfterControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCActiveRecordMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
var
  I: Integer;
begin
  if not fDefaultConnectionDefName.IsEmpty then
  begin
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection(False);
  end;
  for I := 0 to fAdditionalConnectionDefNamesCount - 1 do
  begin
    ActiveRecordConnectionsRegistry.RemoveConnection(fAdditionalARConnectionNames[I], False);
  end;
end;

procedure TMVCActiveRecordMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string;
  var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCActiveRecordMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  I: Integer;
begin
  EnsureConnection;
  if not fDefaultConnectionDefName.IsEmpty then
  begin
    ActiveRecordConnectionsRegistry.AddDefaultConnection(fDefaultConnectionDefName);
  end;
  for I := 0 to fAdditionalConnectionDefNamesCount - 1 do
  begin
    ActiveRecordConnectionsRegistry.AddConnection(fAdditionalARConnectionNames[I], fAdditionalConnectionDefNames[I]);
  end;
  AHandled := False;
end;

end.
