// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.Session.Internal;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Session,
  MVCFramework.Logger;

type
  TMVCSessionMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fSessionFactory: TMVCWebSessionFactory;
  protected
    procedure OnAfterControllerAction(aContext: TWebContext; const aControllerQualifiedClassName: string;
      const aActionName: string; const AHandled: Boolean);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(const SessionFactory: TMVCWebSessionFactory); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.ZLib,
  System.Classes,
  MVCFramework.Session.Database,
  MVCFramework.Commons;

{ TMVCSalutationMiddleware }

constructor TMVCSessionMiddleware.Create(const SessionFactory: TMVCWebSessionFactory);
begin
  inherited Create;
  fSessionFactory := SessionFactory;
end;

destructor TMVCSessionMiddleware.Destroy;
begin
  fSessionFactory.Free;
  inherited;
end;

procedure TMVCSessionMiddleware.OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSessionMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin
end;

procedure TMVCSessionMiddleware.OnBeforeControllerAction
  (AContext: TWebContext; const AControllerQualifiedClassName,
  AActionNAme: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSessionMiddleware.OnBeforeRouting(aContext: TWebContext;
  var AHandled: Boolean);
begin
  aContext.SetSessionFactory(fSessionFactory);
end;


end.
