// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
    fConnectionDefName: string;
  protected
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
      const AActionName: string;
      const AHandled: Boolean
      );

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean);
  public
    constructor Create(
      const ConnectionDefName: string;
      const ConnectionDefFileName: string = 'FDConnectionDefs.ini'); virtual;
  end;

implementation

uses
  MVCFramework.ActiveRecord,
  FireDAC.Comp.Client;

{ TMVCActiveRecordMiddleware }

constructor TMVCActiveRecordMiddleware.Create(const ConnectionDefName: string;
  const ConnectionDefFileName: string);
begin
  inherited Create;
  if not FDManager.ConnectionDefFileLoaded then
  begin
    FDManager.ConnectionDefFileName := ConnectionDefFileName;
    FDManager.LoadConnectionDefFile;
  end;
  if not FDManager.IsConnectionDef(ConnectionDefName) then
  begin
    raise EMVCConfigException.CreateFmt('ConnectionDefName "%s" not found in config file "%s"',
      [ConnectionDefName, ConnectionDefFileName]);
  end;
  fConnectionDefName := ConnectionDefName;
end;

procedure TMVCActiveRecordMiddleware.OnAfterControllerAction(
  AContext: TWebContext;
  const AActionName: string;
  const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCActiveRecordMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
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
  lConn: TFDConnection;
begin
  lConn := TFDConnection.Create(nil);
  lConn.ConnectionDefName := fConnectionDefName;
  ActiveRecordConnectionsRegistry.AddDefaultConnection(lConn, True);
  AHandled := False;
end;

end.
