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

unit MVCFramework.Middleware.SecurityHeaders;

{$I dmvcframework.inc}

interface

uses
  MVCFramework;

type
  TMVCSecurityHeadersMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

{ TMVCSecurityHeadersMiddleware }

procedure TMVCSecurityHeadersMiddleware.OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSecurityHeadersMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSecurityHeadersMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCSecurityHeadersMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
begin
  AContext.Response.SetCustomHeader('X-XSS-Protection', '1; mode = block');
  AContext.Response.SetCustomHeader('X-Content-Type-Options', 'nosniff');
end;

end.
