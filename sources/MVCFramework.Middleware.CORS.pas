// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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
  end;

  TMVCCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAllowedOriginURL: string;
    FAllowsCredentials: string;
    FAllowsMethods: string;
    FExposeHeaders: string;
    FAllowsHeaders: string;
  protected
    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean
      );

    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      );

    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean
      );

  public
    constructor Create(
      const AAllowedOriginURL: string = TMVCCORSDefaults.ALLOWS_ORIGIN_URL;
      const AAllowsCredentials: Boolean = TMVCCORSDefaults.ALLOWS_CREDENTIALS;
      const AExposeHeaders: String = TMVCCORSDefaults.EXPOSE_HEADERS;
      const AAllowsHeaders: String = TMVCCORSDefaults.ALLOWS_HEADERS;
      const AAllowsMethods: string = TMVCCORSDefaults.ALLOWS_METHODS
      ); virtual;
  end;

  TCORSMiddleware = TMVCCORSMiddleware;

implementation

{ TMVCCORSMiddleware }

constructor TMVCCORSMiddleware.Create(
  const AAllowedOriginURL: string;
  const AAllowsCredentials: Boolean;
  const AExposeHeaders: String;
  const AAllowsHeaders: String;
  const AAllowsMethods: string
  );
begin
  inherited Create;
  FAllowedOriginURL := AAllowedOriginURL;
  FAllowsCredentials := IfThen(AAllowsCredentials, 'true', 'false');
  FExposeHeaders := AExposeHeaders;
  FAllowsHeaders := AAllowsHeaders;
  FAllowsMethods := AAllowsMethods;
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
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := FAllowedOriginURL;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Methods'] := FAllowsMethods;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := FAllowsHeaders;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Credentials'] := FAllowsCredentials;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Expose-Headers'] := FExposeHeaders;

  // allows preflight requests
  if (AContext.Request.HTTPMethod = httpOPTIONS) then
  begin
    AContext.Response.StatusCode := HTTP_STATUS.OK;
    AHandled := True;
  end;
end;

end.
