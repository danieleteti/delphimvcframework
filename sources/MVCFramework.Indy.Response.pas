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
// *************************************************************************** }

unit MVCFramework.Indy.Response;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils,
  Web.HTTPApp,
  IdCustomHTTPServer, IdContext, IdCookie,
  MVCFramework;

type
  TMVCIndyDirectResponse = class(TMVCWebResponse)
  private
    FResponseInfo: TIdHTTPResponseInfo;
    FContext: TIdContext;
    FCookies: TCookieCollection;
    FCustomHeaders: TStringList;
    FHeadersSent: Boolean;
  protected
    function GetCustomHeaders: TStrings; override;
    function GetReasonString: string; override;
    function GetStatusCode: Integer; override;
    function GetCookies: TCookieCollection; override;
    function GetContentType: string; override;
    function GetContentEncoding: string; override;
    function GetLocation: string; override;
    function GetContent: string; override;
    procedure SetReasonString(const AValue: string); override;
    procedure SetStatusCode(const AValue: Integer); override;
    procedure SetContentType(const AValue: string); override;
    procedure SetLocation(const AValue: string); override;
    procedure SetContent(const AValue: string); override;
    procedure SetContentEncoding(const Value: string); override;
    function GetContentStream: TStream; override;
    function GetContentLength: Int64; override;
    procedure SetDate(const AValue: TDateTime); override;
    function GetRawWebResponse: TWebResponse; override;
  public
    constructor Create(const AContext: TIdContext;
      const AResponseInfo: TIdHTTPResponseInfo);
    destructor Destroy; override;
    procedure Flush; override;
    procedure SetCustomHeader(const AName, AValue: string); override;
    procedure SetContentStream(const AStream: TStream; const AContentType: string); override;
    procedure InternalSetContentStream(const AStream: TStream; const AOwns: Boolean); override;
    function GetCustomHeader(const AName: string): string; override;
    procedure SendRedirect(const AUrl: string); override;
    procedure SendResponse; override;
  end;

implementation

uses
  System.DateUtils;

{ TMVCIndyDirectResponse }

constructor TMVCIndyDirectResponse.Create(const AContext: TIdContext;
  const AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited Create;
  FResponseInfo := AResponseInfo;
  FContext := AContext;
  FCookies := TCookieCollection.Create(TWebResponse(nil), TCookie);
  FCustomHeaders := TStringList.Create;
  FHeadersSent := False;
end;

destructor TMVCIndyDirectResponse.Destroy;
begin
  FCookies.Free;
  FCustomHeaders.Free;
  inherited;
end;

function TMVCIndyDirectResponse.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TMVCIndyDirectResponse.GetReasonString: string;
begin
  Result := FResponseInfo.ResponseText;
end;

function TMVCIndyDirectResponse.GetStatusCode: Integer;
begin
  Result := FResponseInfo.ResponseNo;
end;

function TMVCIndyDirectResponse.GetCookies: TCookieCollection;
begin
  Result := FCookies;
end;

function TMVCIndyDirectResponse.GetContentType: string;
begin
  Result := FResponseInfo.ContentType;
end;

function TMVCIndyDirectResponse.GetContentEncoding: string;
begin
  Result := FResponseInfo.ContentEncoding;
end;

function TMVCIndyDirectResponse.GetLocation: string;
begin
  Result := FCustomHeaders.Values['location'];
end;

function TMVCIndyDirectResponse.GetContent: string;
begin
  Result := FResponseInfo.ContentText;
end;

procedure TMVCIndyDirectResponse.SetReasonString(const AValue: string);
begin
  FResponseInfo.ResponseText := AValue;
end;

procedure TMVCIndyDirectResponse.SetStatusCode(const AValue: Integer);
begin
  FResponseInfo.ResponseNo := AValue;
end;

procedure TMVCIndyDirectResponse.SetContentType(const AValue: string);
begin
  FResponseInfo.ContentType := AValue;
end;

procedure TMVCIndyDirectResponse.SetLocation(const AValue: string);
begin
  FCustomHeaders.Values['location'] := AValue;
end;

procedure TMVCIndyDirectResponse.SetContent(const AValue: string);
begin
  FResponseInfo.ContentText := AValue;
end;

procedure TMVCIndyDirectResponse.SetContentEncoding(const Value: string);
begin
  FResponseInfo.ContentEncoding := Value;
end;

function TMVCIndyDirectResponse.GetContentStream: TStream;
begin
  Result := FResponseInfo.ContentStream;
end;

function TMVCIndyDirectResponse.GetContentLength: Int64;
begin
  Result := FResponseInfo.ContentLength;
end;

procedure TMVCIndyDirectResponse.SetDate(const AValue: TDateTime);
begin
  FResponseInfo.Date := AValue;
end;

function TMVCIndyDirectResponse.GetRawWebResponse: TWebResponse;
begin
  Result := nil; // No TWebResponse for direct Indy
end;

procedure TMVCIndyDirectResponse.Flush;
var
  I: Integer;
  lCookie: TCookie;
begin
  if FHeadersSent then Exit;
  FHeadersSent := True;

  // Sync custom headers to Indy response
  for I := 0 to FCustomHeaders.Count - 1 do
  begin
    FResponseInfo.CustomHeaders.Values[FCustomHeaders.Names[I]] :=
      Trim(FCustomHeaders.ValueFromIndex[I]);
  end;

  // Sync cookies to Indy response via Set-Cookie headers
  for I := 0 to FCookies.Count - 1 do
  begin
    lCookie := FCookies[I];
    with FResponseInfo.Cookies.Add do
    begin
      CookieName := lCookie.Name;
      Value := lCookie.Value;
      Path := lCookie.Path;
      Domain := lCookie.Domain;
      Expires := lCookie.Expires;
      Secure := lCookie.Secure;
      HttpOnly := lCookie.HttpOnly;
    end;
  end;

  // Indy sends the response automatically when the handler returns
end;

procedure TMVCIndyDirectResponse.SetCustomHeader(const AName, AValue: string);
begin
  FCustomHeaders.Values[AName] := AValue;
end;

procedure TMVCIndyDirectResponse.SetContentStream(const AStream: TStream;
  const AContentType: string);
begin
  FResponseInfo.ContentStream := AStream;
  FResponseInfo.FreeContentStream := True;
  ContentType := AContentType;
end;

procedure TMVCIndyDirectResponse.InternalSetContentStream(const AStream: TStream;
  const AOwns: Boolean);
begin
  FResponseInfo.ContentStream := AStream;
  FResponseInfo.FreeContentStream := AOwns;
end;

function TMVCIndyDirectResponse.GetCustomHeader(const AName: string): string;
begin
  Result := FCustomHeaders.Values[AName];
end;

procedure TMVCIndyDirectResponse.SendRedirect(const AUrl: string);
begin
  FResponseInfo.Redirect(AUrl);
end;

procedure TMVCIndyDirectResponse.SendResponse;
begin
  Flush;
end;

end.
