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

unit MVCFramework.WebBroker.Response;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  Web.HTTPApp,
  MVCFramework;

type
  TMVCWebBrokerResponse = class(TMVCWebResponse)
  private
    FWebResponse: TWebResponse;
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
    constructor Create(const AWebResponse: TWebResponse);
    procedure Flush; override;
    procedure SetCustomHeader(const AName, AValue: string); override;
    procedure SetContentStream(const AStream: TStream; const AContentType: string); override;
    procedure InternalSetContentStream(const AStream: TStream; const AOwns: Boolean); override;
    function GetCustomHeader(const AName: string): string; override;
    procedure SendRedirect(const AUrl: string); override;
    procedure SendResponse; override;
    property WebResponse: TWebResponse read FWebResponse;
  end;

implementation

{ TMVCWebBrokerResponse }

constructor TMVCWebBrokerResponse.Create(const AWebResponse: TWebResponse);
begin
  inherited Create;
  FWebResponse := AWebResponse;
end;

procedure TMVCWebBrokerResponse.Flush;
begin
  if not FWebResponse.Sent then
    FWebResponse.SendResponse;
end;

function TMVCWebBrokerResponse.GetContent: string;
begin
  Result := FWebResponse.Content;
end;

function TMVCWebBrokerResponse.GetContentEncoding: string;
begin
  Result := FWebResponse.ContentEncoding;
end;

function TMVCWebBrokerResponse.GetContentType: string;
begin
  Result := FWebResponse.ContentType;
end;

function TMVCWebBrokerResponse.GetCookies: TCookieCollection;
begin
  Result := FWebResponse.Cookies;
end;

function TMVCWebBrokerResponse.GetCustomHeaders: TStrings;
begin
  Result := FWebResponse.CustomHeaders;
end;

function TMVCWebBrokerResponse.GetLocation: string;
begin
  Result := CustomHeaders.Values['location'];
end;

function TMVCWebBrokerResponse.GetRawWebResponse: TWebResponse;
begin
  Result := FWebResponse;
end;

function TMVCWebBrokerResponse.GetReasonString: string;
begin
  Result := FWebResponse.ReasonString;
end;

function TMVCWebBrokerResponse.GetStatusCode: Integer;
begin
  Result := FWebResponse.StatusCode;
end;

procedure TMVCWebBrokerResponse.SetContent(const AValue: string);
begin
  FWebResponse.Content := AValue;
end;

procedure TMVCWebBrokerResponse.SetContentEncoding(const Value: string);
begin
  FWebResponse.ContentEncoding := Value;
end;

procedure TMVCWebBrokerResponse.SetContentStream(const AStream: TStream; const AContentType: string);
begin
  FWebResponse.ContentStream := AStream;
  ContentType := AContentType;
end;

procedure TMVCWebBrokerResponse.SetContentType(const AValue: string);
begin
  FWebResponse.ContentType := AValue;
end;

procedure TMVCWebBrokerResponse.SetCustomHeader(const AName, AValue: string);
begin
  FWebResponse.SetCustomHeader(AName, AValue);
end;

procedure TMVCWebBrokerResponse.SetLocation(const AValue: string);
begin
  CustomHeaders.Values['location'] := AValue;
end;

procedure TMVCWebBrokerResponse.SetReasonString(const AValue: string);
begin
  FWebResponse.ReasonString := AValue;
end;

procedure TMVCWebBrokerResponse.SetStatusCode(const AValue: Integer);
begin
  FWebResponse.StatusCode := AValue;
end;

function TMVCWebBrokerResponse.GetContentStream: TStream;
begin
  Result := FWebResponse.ContentStream;
end;

procedure TMVCWebBrokerResponse.InternalSetContentStream(const AStream: TStream; const AOwns: Boolean);
begin
  FWebResponse.ContentStream := AStream;
  FWebResponse.FreeContentStream := AOwns;
end;

function TMVCWebBrokerResponse.GetContentLength: Int64;
begin
  Result := FWebResponse.ContentLength;
end;

procedure TMVCWebBrokerResponse.SetDate(const AValue: TDateTime);
begin
  FWebResponse.Date := AValue;
end;

function TMVCWebBrokerResponse.GetCustomHeader(const AName: string): string;
begin
  Result := FWebResponse.GetCustomHeader(AName);
end;

procedure TMVCWebBrokerResponse.SendRedirect(const AUrl: string);
begin
  FWebResponse.SendRedirect(AUrl);
end;

procedure TMVCWebBrokerResponse.SendResponse;
begin
  FWebResponse.SendResponse;
end;

end.
