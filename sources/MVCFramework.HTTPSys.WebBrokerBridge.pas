// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// This unit uses parts of the mORMot project
// https://synopse.info/fossil/wiki?name=SQLite3+Framework
//
// THIS UNIT HAS BEEN INSPIRED BY:
// - IdHTTPWebBrokerBridge.pas (shipped with Delphi)
// - https://github.com/GsDelphi/SynWebBroker
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

unit MVCFramework.HTTPSys.WebBrokerBridge;

{.$DEFINE TRACE}

interface

uses
  System.Classes,
  Web.HTTPApp,
  System.SysUtils,
  WebReq,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Logger,
  SynCrtSock;

type
  EMVCHTTPSysWebBrokerBridgeException = class(EMVCException);

  EMVCHTTPSysInvalidIdxGetVariable = class(EMVCHTTPSysWebBrokerBridgeException)

  end;

  EMVCHTTPSysInvalidIdxSetVariable = class(EMVCHTTPSysWebBrokerBridgeException)

  end;

  EMVCHTTPSysInvalidStringVar = class(EMVCHTTPSysWebBrokerBridgeException)

  end;

  TMVCHTTPSysServer = class(THttpApiServer)
  protected
    procedure SetServerName(const aName: SockString); override;
  end;

  TMVCHTTPSysAppRequest = class(TWebRequest)
  private
    fRequest: THttpServerRequest;
    fHeaders: TMVCHeaders;
    fURL: String;
    fFullURL: String;
  protected
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetStringVariable(Index: Integer): string; override;
    function GetRemoteIP: string; override;
    function GetRawPathInfo: string; override;
    function GetRawContent: TBytes; override;
  public
    constructor Create(const ARequest: THttpServerRequest);
    destructor Destroy; override;
    function GetFieldByName(const Name: string): string; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): string; override;
    function TranslateURI(const URI: string): string; override;
    function WriteClient(var ABuffer; ACount: Integer): Integer; override;
    function WriteHeaders(StatusCode: Integer; const ReasonString, Headers: string): Boolean; override;
    function WriteString(const AString: string): Boolean; override;
  end;

  TMVCHTTPSysAppResponse = class(TWebResponse)
  private
    fResponse: THttpServerRequest;
    fStatusCode: Integer;
    fHeaders: TMVCHeaders;
    fReasonString: string;
    fSent: Boolean;
  protected
    function GetHeaders: SockString;
    function GetContent: string; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetStatusCode: Integer; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetLogMessage: string; override;
    function GetStringVariable(Index: Integer): string; override;
    procedure SetContent(const AValue: string); override;
    procedure SetContentStream(AValue: TStream); override;
    procedure SetStatusCode(AValue: Integer); override;
    procedure SetStringVariable(Index: Integer; const Value: string); override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
    procedure SetLogMessage(const Value: string); override;
  public
    procedure SendRedirect(const URI: string); override;
    procedure SendResponse; override;
    procedure SendStream(AStream: TStream); override;
    function Sent: Boolean; override;
    constructor Create( { ARequest: TWebRequest; } AResponse: THttpServerRequest);
    destructor Destroy; override;
  end;

  TMVCHTTPSysWebBrokerBridge = class(TObject)
  private
    fActive: Boolean;
    fHttpServer: TMVCHTTPSysServer;
    fPort: UInt16;
    fUseSSL: Boolean;
    fUseCompression: Boolean;
  protected
    procedure SetActive(const Value: Boolean);
    procedure SetDefaultPort(const Value: UInt16);
    function DoHandleRequest(ReqResp: THttpServerRequest): Cardinal;
    procedure SetUseSSL(const Value: Boolean);
    procedure SetUseCompression(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Active: Boolean read fActive write SetActive;
    property Port: UInt16 read fPort write SetDefaultPort;
    property UseSSL: Boolean read fUseSSL write SetUseSSL;
    property UseCompression: Boolean read fUseCompression write SetUseCompression;
  end;

implementation

uses
  Math, System.NetEncoding, IdGlobal, System.DateUtils, System.IOUtils, SynZip;

const
  RespIDX_Version = 0;
  RespIDX_ReasonString = 1;
  RespIDX_Server = 2;
  RespIDX_WWWAuthenticate = 3;
  RespIDX_Realm = 4;
  RespIDX_Allow = 5;
  RespIDX_Location = 6;
  RespIDX_ContentEncoding = 7;
  RespIDX_ContentType = 8;
  RespIDX_ContentVersion = 9;
  RespIDX_DerivedFrom = 10;
  RespIDX_Title = 11;
  RespIDX_ContentLength = 0;
  RespIDX_Date = 0;
  RespIDX_Expires = 1;
  RespIDX_LastModified = 2;

  ReqIDX_Method = 0;
  ReqIDX_ProtocolVersion = 1;
  ReqIDX_URL = 2;
  ReqIDX_Query = 3;
  ReqIDX_PathInfo = 4;
  ReqIDX_PathTranslated = 5;
  ReqIDX_CacheControl = 6;
  ReqIDX_Date = 7;
  ReqIDX_Accept = 8;
  ReqIDX_From = 9;
  ReqIDX_Host = 10;
  ReqIDX_IfModifiedSince = 11;
  ReqIDX_Referer = 12;
  ReqIDX_UserAgent = 13;
  ReqIDX_ContentEncoding = 14;
  ReqIDX_ContentType = 15;
  ReqIDX_ContentLength = 16;
  ReqIDX_ContentVersion = 17;
  ReqIDX_DerivedFrom = 18;
  ReqIDX_Expires = 19;
  ReqIDX_Title = 20;
  ReqIDX_RemoteAddr = 21;
  ReqIDX_RemoteHost = 22;
  ReqIDX_ScriptName = 23;
  ReqIDX_ServerPort = 24;
  ReqIDX_Content = 25;
  ReqIDX_Connection = 26;
  ReqIDX_Cookie = 27;
  ReqIDX_Authorization = 28;

constructor TMVCHTTPSysAppRequest.Create(const ARequest: THttpServerRequest);
begin
  fRequest := ARequest;
  inherited Create;
  fHeaders := TMVCHeaders.Create;
  ExtractFields([#13], [], String(fRequest.InHeaders), fHeaders);

  // if fRequest.PostDataSize > 0 then
  // begin
  // case fRequest.BodyType of
  // btBinary:
  // begin
  // SetLength(fBody, ARequest.ContentLength);
  // TBytesStream(fRequest.Body).Read(fBody, Length(fBody));
  // end;
  // else
  // SetLength(fBody, 0);
  // end;
  // end;




  // else
  // begin
  // if FRequestInfo.FormParams <> '' then
  // begin { do not localize }
  // // an input form that was submitted as "application/www-url-encoded"...
  // fContentStream := TStringStream.Create(FRequestInfo.FormParams);
  // end
  // else
  // begin
  // // anything else for now...
  // fContentStream := TStringStream.Create(FRequestInfo.UnparsedParams);
  // end;
  // FFreeContentStream := True;
  // end;

  // FThread := AThread;
  // FRequestInfo := ARequestInfo;
  // FResponseInfo := AResponseInfo;
  // inherited Create;
  // for i := 0 to ARequestInfo.Cookies.Count - 1 do
  // begin
  // CookieFields.Add(ARequestInfo.Cookies[i].ClientCookie);
  // end;
  // if Assigned(FRequestInfo.PostStream) then
  // begin
  // FContentStream := FRequestInfo.PostStream;
  // FFreeContentStream := False;
  // end
  // else
  // begin
  // if FRequestInfo.FormParams <> '' then
  // begin { do not localize }
  // // an input form that was submitted as "application/www-url-encoded"...
  // FContentStream := TStringStream.Create(FRequestInfo.FormParams);
  // end
  // else
  // begin
  // // anything else for now...
  // FContentStream := TStringStream.Create(FRequestInfo.UnparsedParams);
  // end;
  // FFreeContentStream := True;
  // end;
end;

destructor TMVCHTTPSysAppRequest.Destroy;
begin
  fHeaders.Free;
  inherited;
end;

function TMVCHTTPSysAppRequest.GetDateVariable(Index: Integer): TDateTime;
var
  lValue: string;
begin
  lValue := string(GetStringVariable(Index));
  if Length(lValue) > 0 then
  begin
    Result := ParseDate(lValue);
  end
  else
  begin
    Result := -1;
  end;
end;

function TMVCHTTPSysAppRequest.GetIntegerVariable(Index: Integer): Integer;
begin
  Result := StrToIntDef(string(GetStringVariable(Index)), -1)
end;

function TMVCHTTPSysAppRequest.GetRawPathInfo: string;
begin
  Result := GetStringVariable(ReqIDX_PathInfo);
end;

function TMVCHTTPSysAppRequest.GetRemoteIP: string;
begin
  Result := String(fRequest.RemoteIP);
end;

function TMVCHTTPSysAppRequest.GetRawContent: TBytes;
begin
  Result := TBytes(fRequest.InContent);
end;

function TMVCHTTPSysAppRequest.GetStringVariable(Index: Integer): string;
var
  lIdx: Integer;
begin
  case Index of
    ReqIDX_Method:
      Result := String(fRequest.Method);
    // ReqIDX_ProtocolVersion:
    // Result := fRequest.Version;
    ReqIDX_URL:
      begin
        if fURL = '' then
        begin
          fURL := URLDecode(String(fRequest.URL));
        end;
        Result := fURL;
      end;
    ReqIDX_Query:
      begin
        if fFullURL = '' then
        begin
          fFullURL := URLDecode(fRequest.FullURL);
        end;
        Result := fFullURL.Substring(fFullURL.IndexOf('?') + 1);
      end;
    ReqIDX_PathInfo:
      begin
        if fURL = '' then
        begin
          fURL := URLDecode(String(fRequest.URL));
        end;
        Result := fURL;
        lIdx := Result.IndexOf('?');
        if lIdx > -1 then
        begin
          Result := Result.Substring(0, lIdx);
        end;
      end;
    ReqIDX_PathTranslated:
      Result := String(fRequest.URL);
    ReqIDX_CacheControl:
      Result := fHeaders.Values['Cache-Control']; { do not localize }
    ReqIDX_Date:
      Result := fHeaders.Values['Date']; { do not localize }
    ReqIDX_Accept:
      Result := fHeaders.Values['Accept'];
    ReqIDX_From:
      Result := fHeaders.Values['From'];
    ReqIDX_Host:
      Result := fHeaders.Values['Host'];
    ReqIDX_IfModifiedSince:
      Result := fHeaders.Values['If-Modified-Since']; { do not localize }
    ReqIDX_Referer:
      Result := fHeaders.Values['Referrer'];
    ReqIDX_UserAgent:
      Result := fHeaders.Values['User-Agent'];
    ReqIDX_ContentEncoding:
      Result := fHeaders.Values['Content-Encoding'];
    ReqIDX_ContentType:
      Result := String(fRequest.InContentType);
    ReqIDX_ContentLength:
      Result := Length(fRequest.InContent).ToString;
    ReqIDX_ContentVersion:
      Result := fHeaders.Values['Content-Version']; { do not localize }
    ReqIDX_DerivedFrom:
      Result := fHeaders.Values['Derived-From']; { do not localize }
    ReqIDX_Expires:
      Result := fHeaders.Values['Expires']; { do not localize }
    ReqIDX_Title:
      Result := fHeaders.Values['Title']; { do not localize }
    ReqIDX_RemoteAddr:
      Result := String(fRequest.RemoteIP);
    // ReqIDX_RemoteHost:
    // Result := fRequest.Connection.PeerAddr;
    // ReqIDX_ScriptName:
    // Result := '';
    // ReqIDX_ServerPort:
    // Result := fRequest.Connection.PeerPort.ToString;
    ReqIDX_Connection:
      Result := fHeaders.Values['Connection']; { do not localize }
    // Result := fRequest.Header.Params['Connection']; { do not localize }
    ReqIDX_Cookie:
      Result := fHeaders.Values['Cookie']; { do not localize }
    // Result := fRequest.Header.Params['Cookie']; { do not localize }
    ReqIDX_Authorization:
      Result := fHeaders.Values['Authorization']; { do not localize }
    // Result := fRequest.Header.Params['Authorization']; { do not localize }
  else
    Result := '';
  end;
end;

function TMVCHTTPSysAppRequest.GetFieldByName(const Name: string): string;
begin
  Result := fHeaders.Values[Name];
end;

function TMVCHTTPSysAppRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('not implemented - ReadClient');
end;

function TMVCHTTPSysAppRequest.ReadString(Count: Integer): string;
begin
  raise Exception.Create('not implemented - ReadString');
end;

function TMVCHTTPSysAppRequest.TranslateURI(const URI: string): string;
begin
  Result := URI;
end;

function TMVCHTTPSysAppRequest.WriteHeaders(StatusCode: Integer; const ReasonString, Headers: string): Boolean;
begin
  raise Exception.Create('not implemented - WriteHeaders');
  // FResponseInfo.ResponseNo := StatusCode;
  // FResponseInfo.ResponseText := {$IFDEF WBB_ANSI}string(ReasonString){$ELSE}ReasonString{$ENDIF};
  // FResponseInfo.CustomHeaders.Add({$IFDEF WBB_ANSI}string(Headers){$ELSE}Headers{$ENDIF});
  // FResponseInfo.WriteHeader;
  // Result := True;
end;

function TMVCHTTPSysAppRequest.WriteString(const AString: string): Boolean;
begin
  raise Exception.Create('not implemented - WriteString');
end;

function TMVCHTTPSysAppRequest.WriteClient(var ABuffer; ACount: Integer): Integer;
begin
  raise Exception.Create('not implemented - WriteClient');
  // SetLength(LBuffer, ACount);
  // {$IFNDEF CLR}
  // Move(ABuffer, LBuffer[0], ACount);
  // {$ELSE}
  // // RLebeau: this can't be right?  It is interpretting the source as a
  // // null-terminated character string, which is likely not the case...
  // CopyTIdBytes(ToBytes(string(ABuffer)), 0, LBuffer, 0, ACount);
  // {$ENDIF}
  // FThread.Connection.IOHandler.Write(LBuffer);
  // Result := ACount;
end;

constructor TMVCHTTPSysAppResponse.Create(AResponse: THttpServerRequest);
begin
  inherited Create(nil);
  fResponse := AResponse;
  fHeaders := TMVCHeaders.Create;
  ContentType := BuildContentType(TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET);
  StatusCode := http_status.OK;
end;

destructor TMVCHTTPSysAppResponse.Destroy;
begin
  fHeaders.Free;
  inherited;
end;

function TMVCHTTPSysAppResponse.GetContent: string;
begin
  Result := String(fResponse.OutContent);
end;

function TMVCHTTPSysAppResponse.GetLogMessage: string;
begin
  raise Exception.Create('not implemented - GetLogMessage');
end;

function TMVCHTTPSysAppResponse.GetStatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TMVCHTTPSysAppResponse.GetDateVariable(Index: Integer): TDateTime;
  function ToGMT(ADateTime: TDateTime): TDateTime;
  begin
    Result := ADateTime;
    if Result <> -1 then
      Result := Result - OffsetFromUTC;
  end;

begin
  raise Exception.Create('not implemented');
  // case Index of
  // RespIDX_Date:
  // Result := ToGMT(ISOTimeStampToDateTime(fResponse.Header.Params['Date']));
  // RespIDX_Expires:
  // Result := ToGMT(ISOTimeStampToDateTime(fResponse.Header.Params['Expires']));
  // RespIDX_LastModified:
  // Result := ToGMT(ISOTimeStampToDateTime(fResponse.Header.Params['LastModified']));
  // else
  // raise ECrossSocketInvalidIdxGetVariable.Create(Format('Invalid Index for GetDateVariable: %d', [Index]));
  // end;
end;

procedure TMVCHTTPSysAppResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
// WebBroker apps are responsible for conversion to GMT
  function ToLocal(ADateTime: TDateTime): TDateTime;
  begin
    Result := ADateTime;
    if Result <> -1 then
      Result := Result + OffsetFromUTC;
  end;

begin
  // raise Exception.Create('not implemented');
  case Index of
    RespIDX_Date:
      fHeaders.Values['Date'] := DateTimeToISOTimeStamp(ToLocal(Value));
    // fResponse.Header.Params['Date'] := DateTimeToISOTimeStamp(ToLocal(Value));

    RespIDX_Expires:
      fHeaders.Values['Expires'] := DateTimeToISOTimeStamp(ToLocal(Value));
    RespIDX_LastModified:
      fHeaders.Values['LastModified'] := DateTimeToISOTimeStamp(ToLocal(Value));
  else
    raise EMVCHTTPSysInvalidIdxSetVariable.Create(Format('Invalid Index for SetDateVariable: %d', [Index]));
  end;
end;

function TMVCHTTPSysAppResponse.GetIntegerVariable(Index: Integer): Integer;
begin
  case Index of
    RespIDX_ContentLength:
      Result := fHeaders.Values['Content-Length'].ToInt64;
  else
    raise EMVCHTTPSysInvalidIdxGetVariable.Create(Format('Invalid Index for GetIntegerVariable: %d', [Index]));
  end;
end;

procedure TMVCHTTPSysAppResponse.SetIntegerVariable(Index, Value: Integer);
begin
  // raise Exception.Create('not implemented');
  case Index of
    RespIDX_ContentLength:
      fHeaders.Values['Content-Length'] := Value.ToString;
  else
    raise EMVCHTTPSysInvalidIdxSetVariable.Create(Format('Invalid Index for SetIntegerVariable: %d', [Index]));
  end;
end;

function TMVCHTTPSysAppResponse.GetStringVariable(Index: Integer): string;
begin
  case Index of
    // RespIDX_Version:
    // Result := fRequest.ProtocolVersion;
    RespIDX_ReasonString:
      Result := fReasonString;
    RespIDX_Server:
      Result := fHeaders.Values['Server'];
    RespIDX_WWWAuthenticate:
      raise Exception.Create('Not Implemented');
    // Result := fResponse.WWWAuthenticate.Text;
    RespIDX_Realm:
      raise Exception.Create('Not Implemented');
    // Result := fResponse.AuthRealm;
    RespIDX_Allow:
      Result := fHeaders.Values['Allow'];
    RespIDX_Location:
      Result := fHeaders.Values['Location'];
    RespIDX_ContentEncoding:
      Result := fHeaders.Values['Content-Encoding'];
    RespIDX_ContentType:
      Result := fHeaders.Values['Content-Type'];
    RespIDX_ContentVersion:
      Result := fHeaders.Values['Content-Version'];
    RespIDX_DerivedFrom:
      Result := fHeaders.Values['Derived-From'];
    RespIDX_Title:
      Result := fHeaders.Values['Title'];
  else
    raise EMVCHTTPSysInvalidIdxGetVariable.Create(Format('Invalid Index for GetStringVariable: %d', [Index]));
  end;
end;

procedure TMVCHTTPSysAppResponse.SetStringVariable(Index: Integer; const Value: string);
begin
  // raise Exception.Create('not implemented');
  case Index of
    RespIDX_ReasonString:
      fReasonString := Value;
    // fResponse.ReasonString := Value;
    RespIDX_Server:
      fHeaders.Values['Server'] := Value;
    // RespIDX_WWWAuthenticate:
    // raise Exception.Create('Not Implemented');
    // // fResponse.WWWAuthenticate.Text := LValue;
    // RespIDX_Realm:
    // raise Exception.Create('Not Implemented');
    // // fResponse.AuthRealm := LValue;
    RespIDX_Allow:
      fHeaders.Values['Allow'] := Value;
    RespIDX_Location:
      fHeaders.Values['Location'] := Value;
    RespIDX_ContentEncoding:
      fHeaders.Values['Content-Encoding'] := Value;
    RespIDX_ContentType:
      fHeaders.Values['Content-Type'] := Value;
    RespIDX_ContentVersion:
      fHeaders.Values['Content-Version'] := Value;
    RespIDX_DerivedFrom:
      fHeaders.Values['Derived-From'] := Value;
    RespIDX_Title:
      fHeaders.Values['Title'] := Value;
  else
    raise EMVCHTTPSysInvalidIdxSetVariable.Create(Format('Invalid Index for SetStringVariable: %d', [Index]));
  end;
end;

procedure TMVCHTTPSysAppResponse.SendRedirect(const URI: string);
begin
  if fSent then
    Exit;
  fSent := True;
  raise Exception.Create('not implemented');
  // fResponse.Redirect(URI);
end;

procedure TMVCHTTPSysAppResponse.SendResponse;
var
  lOutContent: SockString;
  lSize: Int64;
begin
  if fSent then
    Exit;
  fSent := True;
  if ContentStream <> nil then
  begin
    lSize := ContentStream.Size;
    ContentLength := ContentStream.Size;
    SetLength(lOutContent, lSize);
    ContentStream.Position := 0;
    ContentStream.Read(lOutContent[1], lSize);
    fResponse.OutContent := lOutContent;
  end;
  fResponse.OutContentType := SockString(ContentType);
  fResponse.OutCustomHeaders := GetHeaders;
end;

procedure TMVCHTTPSysAppResponse.SendStream(AStream: TStream);
begin
  SetContentStream(AStream);
  SendResponse;
end;

function TMVCHTTPSysAppResponse.Sent: Boolean;
begin
  Result := fSent;
end;

procedure TMVCHTTPSysAppResponse.SetContent(const AValue: string);
begin
  SetContentStream(TStringStream.Create(AValue));
end;

procedure TMVCHTTPSysAppResponse.SetLogMessage(const Value: string);
begin
  // logging not supported
end;

procedure TMVCHTTPSysAppResponse.SetStatusCode(AValue: Integer);
begin
  fStatusCode := AValue;
end;

procedure TMVCHTTPSysAppResponse.SetContentStream(AValue: TStream);
begin
  inherited SetContentStream(AValue);
end;

function TMVCHTTPSysAppResponse.GetHeaders: SockString;
var
  i: Integer;
  lBuilder: TStringBuilder;
begin
  lBuilder := TStringBuilder.Create(1024);
  try
    for i := 0 to fHeaders.Count - 1 do
    begin
      lBuilder.AppendLine(fHeaders.Names[i] + ':' + fHeaders.ValueFromIndex[i]);
    end;
    for i := 0 to CustomHeaders.Count - 1 do
    begin
      lBuilder.AppendLine(CustomHeaders.Names[i] + ':' + CustomHeaders.ValueFromIndex[i]);
    end;
    for i := 0 to Cookies.Count - 1 do
    begin
      lBuilder.AppendLine('Set-Cookie: ' + Cookies[i].HeaderValue);
    end;
    Result := SockString(lBuilder.ToString);
  finally
    lBuilder.Free;
  end;
end;

type
  TMVCHTTPSysWebBrokerBridgeRequestHandler = class(TWebRequestHandler)
  private
    class var FWebRequestHandler: TMVCHTTPSysWebBrokerBridgeRequestHandler;
  public
    constructor Create(AOwner: TComponent); override;
    class destructor Destroy;
    destructor Destroy; override;
    function Run(const ReqResp: THttpServerRequest): Cardinal;
  end;

function TMVCHTTPSysWebBrokerBridgeRequestHandler.Run(const ReqResp: THttpServerRequest): Cardinal;
var
  lRequest: TMVCHTTPSysAppRequest;
  lResponse: TMVCHTTPSysAppResponse;
begin
  Result := http_status.OK;
  try
    lRequest := TMVCHTTPSysAppRequest.Create(ReqResp);
    try
      lResponse := TMVCHTTPSysAppResponse.Create(ReqResp);
      try
        lResponse.FreeContentStream := True;
        try
          HandleRequest(lRequest, lResponse);
        finally
          Result := lResponse.StatusCode;
        end;
      finally
        FreeAndNil(lResponse);
      end;
    finally
      FreeAndNil(lRequest);
    end;
  except
    on E: Exception do
    begin
      LogE(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

constructor TMVCHTTPSysWebBrokerBridgeRequestHandler.Create(AOwner: TComponent);
begin
  inherited;
  System.Classes.ApplicationHandleException := HandleException;
end;

destructor TMVCHTTPSysWebBrokerBridgeRequestHandler.Destroy;
begin
  System.Classes.ApplicationHandleException := nil;
  inherited;
end;

class destructor TMVCHTTPSysWebBrokerBridgeRequestHandler.Destroy;
begin
  FreeAndNil(FWebRequestHandler);
end;

function MVCHTTPSysWebBrokerBridgeRequestHandler: TWebRequestHandler;
begin
  if not Assigned(TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler) then
    TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler := TMVCHTTPSysWebBrokerBridgeRequestHandler.Create(nil);
  Result := TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler;
end;

// procedure TCrossSocketWebBrokerBridge.InternalHandleRequest(const ARequest: ICrossHttpRequest;
// const AResponse: ICrossHttpResponse);
// begin
// if fWebModuleClass <> nil then
// begin
// RunWebModuleClass(ARequest, AResponse)
// end
// else
// begin
// TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler.Run(ARequest, AResponse);
// end;
// LogI(AResponse.Sent.ToString(TUseBoolStrs.True));
// end;

// procedure TCrossSocketWebBrokerBridge.CreateInternalRouter;
// begin
// fHttpServer.All('*',
// procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
// begin
// // TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler.Run(ARequest, AResponse);
// AResponse.Send('0123456789');
// end);
// end;

destructor TMVCHTTPSysWebBrokerBridge.Destroy;
begin
  Active := False;
  inherited;
end;

function TMVCHTTPSysWebBrokerBridge.DoHandleRequest(ReqResp: THttpServerRequest): Cardinal;
{$IF defined(TRACE)}
var
  lFile: TFileStream;
  lDirName, lReqFileName, lRespFileName: string;
const
  LINE_BREAK = sLineBreak + sLineBreak + sLineBreak;
{$ENDIF}
begin
{$IF defined(TRACE)}
  lDirName := TPath.Combine(AppPath, 'trace_' + ReqResp.ConnectionID.ToString.PadLeft(10));
  lReqFileName := TPath.Combine(lDirName, 'request.txt');
  lRespFileName := TPath.Combine(lDirName, 'response.txt');
  TDirectory.CreateDirectory(lDirName);
  if TFile.Exists(lReqFileName) then
    TFile.Delete(lReqFileName);
  lFile := TFile.Create(lReqFileName, fmCreate or fmOpenWrite);
  try
    lFile.Write(ReqResp.InHeaders[1], Length(ReqResp.InHeaders));
    lFile.Write(LINE_BREAK[1], Length(LINE_BREAK));
    lFile.Write(ReqResp.InContent[1], Length(ReqResp.InContent));
  finally
    lFile.Free;
  end;
{$ENDIF}
  Result := TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler.Run(ReqResp);
{$IF defined(TRACE)}
  if TFile.Exists(lRespFileName) then
    TFile.Delete(lRespFileName);
  lFile := TFile.Create(lRespFileName, fmCreate or fmOpenWrite);
  try
    lFile.Write(ReqResp.OutCustomHeaders[1], Length(ReqResp.OutCustomHeaders));
    lFile.Write(LINE_BREAK[1], Length(LINE_BREAK));
    lFile.Write(ReqResp.OutContent[1], Length(ReqResp.OutContent));
  finally
    lFile.Free;
  end;
{$ENDIF}
end;

procedure TMVCHTTPSysWebBrokerBridge.SetActive(const Value: Boolean);
begin
  if fActive = Value then
  begin
    Exit;
  end;
  if Value then
  begin
    fHttpServer := TMVCHTTPSysServer.Create(fUseSSL);
    fHttpServer.AddUrl(SockString(''), SockString(IntToStr(fPort)), fUseSSL, '+', True);
    fHttpServer.RegisterCompress(CompressDeflate);
    fHttpServer.OnRequest := DoHandleRequest;
    { TODO -odanielet -cGeneral : Try to find some adaptive and smart number here }
    fHttpServer.Clone(31); // will use a thread pool of 32 threads in total
    fActive := True;
  end
  else
  begin
    fHttpServer.Shutdown;
    FreeAndNil(fHttpServer);
    fActive := False;
  end;
end;

procedure TMVCHTTPSysWebBrokerBridge.SetDefaultPort(const Value: UInt16);
begin
  fPort := Value;
end;

procedure TMVCHTTPSysWebBrokerBridge.SetUseCompression(const Value: Boolean);
begin
  fUseCompression := Value;
end;

procedure TMVCHTTPSysWebBrokerBridge.SetUseSSL(const Value: Boolean);
begin
  fUseSSL := Value;
end;

constructor TMVCHTTPSysWebBrokerBridge.Create;
begin
  inherited Create;
  fUseSSL := False;
  fUseCompression := True;
end;

{ TMVCHTTPSysServer }

procedure TMVCHTTPSysServer.SetServerName(const aName: SockString);
begin
  inherited;
  fServerName := DMVCFRAMEWORK;
end;

initialization

WebReq.WebRequestHandlerProc := MVCHTTPSysWebBrokerBridgeRequestHandler;

finalization

FreeAndNil(TMVCHTTPSysWebBrokerBridgeRequestHandler.FWebRequestHandler);

end.
