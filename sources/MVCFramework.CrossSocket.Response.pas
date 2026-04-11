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

unit MVCFramework.CrossSocket.Response;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils,
  Web.HTTPApp,
  Net.CrossHttpServer, Net.CrossHttpParams,
  MVCFramework;

type
  TMVCCrossSocketResponse = class(TMVCWebResponse)
  private
    FResponse: ICrossHttpResponse;
    FConnection: ICrossHttpConnection;
    FCookies: TCookieCollection;
    FCustomHeaders: TStringList;
    FContentStream: TStream;
    FOwnsContentStream: Boolean;
    FContent: string;
    FContentType: string;
    FContentEncoding: string;
    FStatusCode: Integer;
    FReasonString: string;
    FSent: Boolean;
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
    procedure InternalSetContentStream(const AStream: TStream; const AOwns: Boolean); override;
    function GetContentLength: Int64; override;
    procedure SetDate(const AValue: TDateTime); override;
    function GetCustomHeader(const AName: string): string; override;
    function GetRawWebResponse: TWebResponse; override;
  public
    constructor Create(const AConnection: ICrossHttpConnection;
      const AResponse: ICrossHttpResponse);
    destructor Destroy; override;
    procedure Flush; override;
    procedure SetCustomHeader(const AName, AValue: string); override;
    procedure SetContentStream(const AStream: TStream; const AContentType: string); override;
    procedure SendRedirect(const AUrl: string); override;
    procedure SendResponse; override;
  end;

implementation

uses
  System.DateUtils;

{ TMVCCrossSocketResponse }

constructor TMVCCrossSocketResponse.Create(const AConnection: ICrossHttpConnection;
  const AResponse: ICrossHttpResponse);
begin
  inherited Create;
  FResponse := AResponse;
  FConnection := AConnection;
  FCookies := TCookieCollection.Create(TWebResponse(nil), TCookie);
  FCustomHeaders := TStringList.Create;
  FContentStream := nil;
  FOwnsContentStream := False;
  FContent := '';
  FContentType := '';
  FContentEncoding := '';
  FStatusCode := 200;
  FReasonString := 'OK';
  FSent := False;
end;

destructor TMVCCrossSocketResponse.Destroy;
begin
  FCookies.Free;
  FCustomHeaders.Free;
  if FOwnsContentStream then
    FContentStream.Free;
  inherited;
end;

function TMVCCrossSocketResponse.GetCustomHeaders: TStrings;
begin
  Result := FCustomHeaders;
end;

function TMVCCrossSocketResponse.GetReasonString: string;
begin
  Result := FReasonString;
end;

function TMVCCrossSocketResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TMVCCrossSocketResponse.GetCookies: TCookieCollection;
begin
  Result := FCookies;
end;

function TMVCCrossSocketResponse.GetContentType: string;
begin
  Result := FContentType;
end;

function TMVCCrossSocketResponse.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TMVCCrossSocketResponse.GetLocation: string;
begin
  Result := FCustomHeaders.Values['Location'];
end;

function TMVCCrossSocketResponse.GetContent: string;
begin
  Result := FContent;
end;

procedure TMVCCrossSocketResponse.SetReasonString(const AValue: string);
begin
  FReasonString := AValue;
end;

procedure TMVCCrossSocketResponse.SetStatusCode(const AValue: Integer);
begin
  FStatusCode := AValue;
end;

procedure TMVCCrossSocketResponse.SetContentType(const AValue: string);
begin
  FContentType := AValue;
end;

procedure TMVCCrossSocketResponse.SetLocation(const AValue: string);
begin
  FCustomHeaders.Values['Location'] := AValue;
end;

procedure TMVCCrossSocketResponse.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TMVCCrossSocketResponse.SetContentEncoding(const Value: string);
begin
  FContentEncoding := Value;
end;

function TMVCCrossSocketResponse.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

procedure TMVCCrossSocketResponse.InternalSetContentStream(const AStream: TStream; const AOwns: Boolean);
begin
  if FOwnsContentStream and (FContentStream <> AStream) then
    FContentStream.Free;
  FContentStream := AStream;
  FOwnsContentStream := AOwns;
end;

function TMVCCrossSocketResponse.GetContentLength: Int64;
begin
  if Assigned(FContentStream) then
    Result := FContentStream.Size
  else
    Result := Length(TEncoding.UTF8.GetBytes(FContent));
end;

procedure TMVCCrossSocketResponse.SetDate(const AValue: TDateTime);
begin
  FCustomHeaders.Values['Date'] := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"', TTimeZone.Local.ToUniversalTime(AValue));
end;

function TMVCCrossSocketResponse.GetCustomHeader(const AName: string): string;
begin
  Result := FCustomHeaders.Values[AName];
end;

function TMVCCrossSocketResponse.GetRawWebResponse: TWebResponse;
begin
  Result := nil;
end;

procedure TMVCCrossSocketResponse.SetCustomHeader(const AName, AValue: string);
begin
  FCustomHeaders.Values[AName] := AValue;
end;

procedure TMVCCrossSocketResponse.SetContentStream(const AStream: TStream; const AContentType: string);
begin
  InternalSetContentStream(AStream, True);
  FContentType := AContentType;
end;

procedure TMVCCrossSocketResponse.SendRedirect(const AUrl: string);
begin
  FResponse.Redirect(AUrl);
  FSent := True;
end;

procedure TMVCCrossSocketResponse.SendResponse;
begin
  Flush;
end;

procedure TMVCCrossSocketResponse.Flush;
var
  I: Integer;
  lCookie: TCookie;
  lBodyBytes: TBytes;
begin
  if FSent then Exit;
  FSent := True;

  // Set status
  FResponse.StatusCode := FStatusCode;

  // Set content type
  if FContentType <> '' then
    FResponse.ContentType := FContentType;

  // Set custom headers
  for I := 0 to FCustomHeaders.Count - 1 do
    FResponse.Header[FCustomHeaders.Names[I]] := Trim(FCustomHeaders.ValueFromIndex[I]);

  // Set content encoding
  if FContentEncoding <> '' then
    FResponse.Header['Content-Encoding'] := FContentEncoding;

  // Set cookies
  for I := 0 to FCookies.Count - 1 do
  begin
    lCookie := FCookies[I];
    FResponse.Cookies.AddOrSet(
      lCookie.Name, lCookie.Value,
      Round((lCookie.Expires - Now) * SecsPerDay),
      lCookie.Path, lCookie.Domain,
      lCookie.HttpOnly, lCookie.Secure);
  end;

  // Send body
  if Assigned(FContentStream) and (FContentStream.Size > 0) then
  begin
    FContentStream.Position := 0;
    FResponse.Send(FContentStream);
  end
  else if FContent <> '' then
  begin
    FResponse.Send(FContent);
  end
  else
  begin
    FResponse.Send('');
  end;
end;

end.
