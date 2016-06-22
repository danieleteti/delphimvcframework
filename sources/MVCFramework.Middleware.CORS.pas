// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.CORS;

interface

uses
  MVCFramework;

type
  TCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AActionNAme: string; const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string;
      var Handled: Boolean);
  end;

implementation

{ TCORSMiddleware }

procedure TCORSMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin
end;

procedure TCORSMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string;
  var Handled: Boolean);
begin
  // do nothing
end;

procedure TCORSMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  Context.Response.RawWebResponse.CustomHeaders.Values
    ['Access-Control-Allow-Origin'] := '*';
  Context.Response.RawWebResponse.CustomHeaders.Values
    ['Access-Control-Allow-Methods'] :=
    'POST, GET, OPTIONS, PUT, DELETE';
  Context.Response.RawWebResponse.CustomHeaders.Values
    ['Access-Control-Allow-Headers'] := 'content-type';

  if Context.Request.HTTPMethod = httpOPTIONS then
  begin
    Context.Response.StatusCode := 200;
    Handled := True;
  end;
end;

end.
