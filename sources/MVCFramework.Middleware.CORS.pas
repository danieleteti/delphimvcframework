// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.StrUtils,
  MVCFramework,
  MVCFramework.Commons;

type
  TMVCCORSDefaults = class sealed
  public
    const
    ALLOWS_ORIGIN_URL = '*';
    ALLOWS_CREDENTIALS = True;
    /// <summary>
    /// The Access-Control-Expose-Headers response header indicates which headers can be exposed as part of the response by listing their names.
    /// By default, only the 6 simple response headers are exposed: Cache-Control, Content-Language, Content-Type, Expires, Last-Modified, Pragma
    /// If you want clients to be able to access other headers, you have to list them using the Access-Control-Expose-Headers header.
    /// Source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Expose-Headers
    /// </summary>
    EXPOSE_HEADERS = '';
    /// <summary>
    /// The Access-Control-Allow-Headers response header is used in response to a preflight request which includes the Access-Control-Request-Headers to indicate which HTTP headers can be used during the actual request.
    /// The simple headers, Accept, Accept-Language, Content-Language, Content-Type (but only with a MIME type of its parsed value (ignoring parameters) of either application/x-www-form-urlencoded, multipart/form-data, or text/plain), are always available and don't need to be listed by this header.
    /// This header is required if the request has an Access-Control-Request-Headers header.
    /// Source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers
    /// </summary>
    ALLOWS_HEADERS = 'Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization';
    /// <summary>
    /// The Access-Control-Allow-Methods response header specifies the method or methods allowed when accessing the resource in response to a preflight request.
    /// Source: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods
    /// </summary>
    ALLOWS_METHODS = 'POST,GET,OPTIONS,PUT,DELETE';
    /// <summary>
    /// Indicates the number of seconds (60 by default) the information provided by
    /// the `Access-Control-Allow-Methods` and `Access-Control-Allow-Headers` headers can be cached.
    /// </summary>
    ACCESS_CONTROL_MAX_AGE = 60;
  end;

  TMVCCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict protected
    FAllowedOriginURL: string;
    FAllowedOriginURLs: TArray<String>;
    FAllowsCredentials: Boolean;
    FAllowsMethods: string;
    FExposeHeaders: string;
    FAllowsHeaders: string;
    FAccessControlMaxAge: string;
  protected
    function GetAllowedOriginURL(AContext: TWebContext): String; virtual;
    procedure FillCommonHeaders(AContext: TWebContext; const AAllowOrigin: String); virtual;
    procedure HandlePreflightRequest(AContext: TWebContext; var AHandled: Boolean); virtual;
    procedure HandleRequest(AContext: TWebContext; var AHandled: Boolean); virtual;

    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean
      ); virtual;

    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      ); virtual;

    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean); virtual;

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean
      ); virtual;

  public
    constructor Create(
      const AAllowedOriginURLs: string = TMVCCORSDefaults.ALLOWS_ORIGIN_URL;
      const AAllowsCredentials: Boolean = TMVCCORSDefaults.ALLOWS_CREDENTIALS;
      const AExposeHeaders: String = TMVCCORSDefaults.EXPOSE_HEADERS;
      const AAllowsHeaders: String = TMVCCORSDefaults.ALLOWS_HEADERS;
      const AAllowsMethods: string = TMVCCORSDefaults.ALLOWS_METHODS;
      const AAccessControlMaxAge: Integer = TMVCCORSDefaults.ACCESS_CONTROL_MAX_AGE
      ); virtual;
  end;

  TCORSMiddleware = TMVCCORSMiddleware;

implementation

uses
  System.SysUtils;

{ TMVCCORSMiddleware }

constructor TMVCCORSMiddleware.Create(
  const AAllowedOriginURLs: string;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: String;
  const AAllowsHeaders: String;
  const AAllowsMethods: string;
  const AAccessControlMaxAge: Integer
  );
begin
  inherited Create;
  FAllowedOriginURLs := AAllowedOriginURLs.Split([',']);
  FAllowsCredentials := AAllowsCredentials;
  FExposeHeaders := AExposeHeaders;
  FAllowsHeaders := AAllowsHeaders;
  FAllowsMethods := AAllowsMethods;
  FAccessControlMaxAge := IntToStr(AAccessControlMaxAge);
end;

procedure TMVCCORSMiddleware.FillCommonHeaders(AContext: TWebContext; const AAllowOrigin: String);
var
  lCustomHeaders: TStrings;
begin
  lCustomHeaders := AContext.Response.RawWebResponse.CustomHeaders;
  lCustomHeaders.Values['Access-Control-Allow-Origin'] := AAllowOrigin;
  lCustomHeaders.Values['Access-Control-Allow-Methods'] := FAllowsMethods;
  lCustomHeaders.Values['Access-Control-Allow-Headers'] := FAllowsHeaders;
  lCustomHeaders.Values['Access-Control-Max-Age'] := FAccessControlMaxAge;
  if FAllowsCredentials then
  begin
    // Omit Access-Control-Allow-Credentials if <> true
    // https://github.com/danieleteti/delphimvcframework/issues/679#issuecomment-1676535853
    lCustomHeaders.Values['Access-Control-Allow-Credentials'] := 'true';
  end;
end;

function TMVCCORSMiddleware.GetAllowedOriginURL(AContext: TWebContext): String;
var
  lRequestOrigin: string;
  lAllowed: String;
  I: Integer;
begin
  Result := '';
  lRequestOrigin := AContext.Request.Headers['Origin'];
  if lRequestOrigin <> '' then
  begin
    for I := Low(FAllowedOriginURLs) to High(FAllowedOriginURLs) do
    begin
      lAllowed := FAllowedOriginURLs[I].Trim;
      if SameText(lRequestOrigin, lAllowed) or (lAllowed = '*') then
      begin
        Exit(lAllowed);
      end;
    end;
  end;
end;

procedure TMVCCORSMiddleware.HandlePreflightRequest(AContext: TWebContext;
  var AHandled: Boolean);
var
  lAllowOrigin: String;
begin
  // https://fetch.spec.whatwg.org/#cors-preflight-request
  lAllowOrigin := GetAllowedOriginURL(AContext);
  AContext.Response.StatusCode := HTTP_STATUS.NoContent;
  if not lAllowOrigin.IsEmpty then
  begin
    FillCommonHeaders(AContext, lAllowOrigin);
  end;
  AHandled := True;
end;

procedure TMVCCORSMiddleware.HandleRequest(AContext: TWebContext;
  var AHandled: Boolean);
var
  lAllowOrigin: String;
  lCustomHeaders: TStrings;
begin
  // https://fetch.spec.whatwg.org/#http-responses
  lAllowOrigin := GetAllowedOriginURL(AContext);
  if not lAllowOrigin.IsEmpty then
  begin
    FillCommonHeaders(AContext, lAllowOrigin);
    lCustomHeaders := AContext.Response.RawWebResponse.CustomHeaders;
    lCustomHeaders.Values['Access-Control-Expose-Headers'] := FExposeHeaders; {only for not preflight requests}
  end;
  AHandled := False;
end;

procedure TMVCCORSMiddleware.OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCORSMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCORSMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCORSMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  if AContext.Request.HTTPMethod <> httpOPTIONS then
  begin
    //normal request, no preflight request
    HandleRequest(AContext, AHandled);
  end
  else
  begin
    //preflight
    HandlePreflightRequest(AContext, AHandled);
  end;
end;

end.
