// ***************************************************************************
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// ***************************************************************************

unit LoggerPro.WebhookAppender;

{ Webhook appender - POST each log item to a configured URL.

  This is the low-friction "send my logs somewhere via HTTP" appender.
  It is intentionally simple: one POST per log item, JSON or plain text
  body, optional API key, optional custom headers. Use it for generic
  webhook endpoints, Logstash-style collectors, in-house log gateways,
  Slack/Teams incoming-webhooks, n8n/Zapier triggers, etc.

  It is NOT meant to replace a real APM/observability SDK like ExeWatch.
  Features deliberately absent here (breadcrumbs, timing spans, user
  identity, periodic gauges, crash capture, ...) belong to dedicated
  platforms - use their native appender alongside this one if you need
  those capabilities.

  Cross-platform via System.Net.HttpClient. }

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.Generics.Collections,
  System.DateUtils;

type
  TLoggerProWebhookAppender = class;

  { Callback for customizing request data.
    Set aData to your custom stream; caller will free it. }
  TOnCreateWebhookData = reference to procedure(
    const Sender: TLoggerProWebhookAppender;
    const aLogItem: TLogItem;
    const aExtendedInfo: TLoggerProExtendedInfo;
    var aData: TStream;
    var aContentType: string);

  { Callback for handling network errors.
    Set aRetryCount to control retries (0 = no more retries). }
  TOnWebhookSendError = reference to procedure(
    const Sender: TLoggerProWebhookAppender;
    const aLogItem: TLogItem;
    const aException: Exception;
    var aRetryCount: Integer);

  TLoggerProWebhookAppender = class(TLoggerProAppenderBase)
  private
    FURL: string;
    FContentType: TWebhookContentType;
    FHTTPClient: THTTPClient;
    FCustomHeaders: TDictionary<string, string>;
    FExtendedInfo: TLoggerProExtendedInfo;
    FExtendedInfoData: array [low(TLogExtendedInfo) .. high(TLogExtendedInfo)] of string;
    FTimeoutSeconds: Integer;
    FMaxRetryCount: Integer;
    FAppendTagAndLevelToURL: Boolean;
    FAPIKey: string;
    FAPIKeyName: string;
    FAPIKeyLocation: TWebhookAPIKeyLocation;
    FOnCreateData: TOnCreateWebhookData;
    FOnSendError: TOnWebhookSendError;
    procedure LoadExtendedInfo;
    function LogItemToJSON(const aLogItem: TLogItem): string;
    function BuildURL(const aLogItem: TLogItem): string;
    procedure InternalWriteLog(const aURL: string; const aLogItem: TLogItem;
      const aData: TStream; const aContentTypeStr: string);
  protected
    function CreateData(const aLogItem: TLogItem; out aContentType: string): TStream; virtual;
  public
    const DEFAULT_TIMEOUT_SECONDS = 5;
    const DEFAULT_MAX_RETRY_COUNT = 3;
    const DEFAULT_EXTENDED_INFO = [TLogExtendedInfo.EIUserName, TLogExtendedInfo.EIComputerName,
      TLogExtendedInfo.EIProcessName, TLogExtendedInfo.EIProcessID];
    const DEFAULT_API_KEY_HEADER_NAME = 'X-API-Key';
    const DEFAULT_API_KEY_QUERY_PARAM = 'api_key';

    constructor Create(const aURL: string;
      aContentType: TWebhookContentType = TWebhookContentType.JSON;
      aTimeoutSeconds: Integer = DEFAULT_TIMEOUT_SECONDS;
      aExtendedInfo: TLoggerProExtendedInfo = DEFAULT_EXTENDED_INFO;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    destructor Destroy; override;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;

    { Add a custom header to be sent with each request }
    procedure AddHeader(const aName, aValue: string);

    /// <summary>Configure an API key to send on every POST. The key travels
    /// either as an HTTP header or as a URL query-string parameter.
    /// <para>
    /// <c>aValue</c> is the secret. <c>aName</c> is the header name (for
    /// Header) or the query-param name (for QueryString). When
    /// <c>aName = ''</c> the appropriate default is used
    /// (<c>X-API-Key</c> or <c>api_key</c>).
    /// </para>
    /// </summary>
    procedure SetAPIKey(const aValue: string;
      aLocation: TWebhookAPIKeyLocation = TWebhookAPIKeyLocation.Header;
      const aName: string = '');

    property URL: string read FURL write FURL;
    property ContentType: TWebhookContentType read FContentType write FContentType;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds;
    property MaxRetryCount: Integer read FMaxRetryCount write FMaxRetryCount;
    { If True, appends /tag/level to URL (REST style). Default False. }
    property AppendTagAndLevelToURL: Boolean read FAppendTagAndLevelToURL write FAppendTagAndLevelToURL;
    property ExtendedInfo: TLoggerProExtendedInfo read FExtendedInfo;
    { Read-only view of the API key configuration. Use SetAPIKey to modify. }
    property APIKey: string read FAPIKey;
    property APIKeyName: string read FAPIKeyName;
    property APIKeyLocation: TWebhookAPIKeyLocation read FAPIKeyLocation;

    { Callback to customize the data sent. If set, overrides default behavior. }
    property OnCreateData: TOnCreateWebhookData read FOnCreateData write FOnCreateData;
    { Callback for network errors. Use to implement custom retry logic. }
    property OnSendError: TOnWebhookSendError read FOnSendError write FOnSendError;
  end;

implementation

uses
  System.NetEncoding,
  System.IOUtils
{$IF Defined(MSWINDOWS)}
  , Winapi.Windows
{$ENDIF}
{$IF Defined(Android)}
  , Androidapi.Helpers
  , Androidapi.JNI.GraphicsContentViewText
  , Androidapi.JNI.JavaTypes
{$ENDIF}
  ;

{ TLoggerProWebhookAppender }

constructor TLoggerProWebhookAppender.Create(const aURL: string;
  aContentType: TWebhookContentType;
  aTimeoutSeconds: Integer;
  aExtendedInfo: TLoggerProExtendedInfo;
  aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  if aURL.Trim.IsEmpty then
    raise ELoggerPro.Create('URL cannot be empty');
  if aTimeoutSeconds < 1 then
    raise ELoggerPro.CreateFmt('TimeoutSeconds must be >= 1, got %d', [aTimeoutSeconds]);
  FURL := aURL;
  FContentType := aContentType;
  FTimeoutSeconds := aTimeoutSeconds;
  FExtendedInfo := aExtendedInfo;
  FMaxRetryCount := DEFAULT_MAX_RETRY_COUNT;
  FAppendTagAndLevelToURL := False;
  FCustomHeaders := TDictionary<string, string>.Create;
end;

destructor TLoggerProWebhookAppender.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

procedure TLoggerProWebhookAppender.LoadExtendedInfo;
{$IF Defined(MSWINDOWS)}
var
  lBufferSize: Cardinal;
  lBuffer: string;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  if TLogExtendedInfo.EIComputerName in FExtendedInfo then
  begin
    lBufferSize := MAX_COMPUTERNAME_LENGTH + 1;
    SetLength(lBuffer, lBufferSize);
    if GetComputerName(PChar(lBuffer), lBufferSize) then
      FExtendedInfoData[TLogExtendedInfo.EIComputerName] := Copy(lBuffer, 1, lBufferSize)
    else
      FExtendedInfoData[TLogExtendedInfo.EIComputerName] := 'unknown';
  end;

  if TLogExtendedInfo.EIUserName in FExtendedInfo then
  begin
    lBufferSize := 256;
    SetLength(lBuffer, lBufferSize);
    if GetUserName(PChar(lBuffer), lBufferSize) then
      FExtendedInfoData[TLogExtendedInfo.EIUserName] := Copy(lBuffer, 1, lBufferSize - 1)
    else
      FExtendedInfoData[TLogExtendedInfo.EIUserName] := 'unknown';
  end;

  if TLogExtendedInfo.EIProcessName in FExtendedInfo then
    FExtendedInfoData[TLogExtendedInfo.EIProcessName] := TPath.GetFileName(GetModuleName(HInstance));

  if TLogExtendedInfo.EIProcessID in FExtendedInfo then
    FExtendedInfoData[TLogExtendedInfo.EIProcessID] := IntToStr(GetCurrentProcessId);
{$ENDIF}

{$IF Defined(Android)}
  if TLogExtendedInfo.EIProcessName in FExtendedInfo then
    FExtendedInfoData[TLogExtendedInfo.EIProcessName] := TAndroidHelper.ApplicationTitle;
{$ENDIF}

{$IF Defined(POSIX) and not Defined(Android)}
  if TLogExtendedInfo.EIComputerName in FExtendedInfo then
  begin
    FExtendedInfoData[TLogExtendedInfo.EIComputerName] := GetEnvironmentVariable('HOSTNAME');
    if FExtendedInfoData[TLogExtendedInfo.EIComputerName].IsEmpty then
      FExtendedInfoData[TLogExtendedInfo.EIComputerName] := 'unknown';
  end;
{$ENDIF}
end;

procedure TLoggerProWebhookAppender.Setup;
begin
  inherited;
  LoadExtendedInfo;
  FHTTPClient := THTTPClient.Create;
  FHTTPClient.ConnectionTimeout := FTimeoutSeconds * 1000;
  FHTTPClient.ResponseTimeout := FTimeoutSeconds * 1000;
end;

procedure TLoggerProWebhookAppender.TearDown;
begin
  FHTTPClient.Free;
  FHTTPClient := nil;
  inherited;
end;

procedure TLoggerProWebhookAppender.AddHeader(const aName, aValue: string);
begin
  FCustomHeaders.AddOrSetValue(aName, aValue);
end;

procedure TLoggerProWebhookAppender.SetAPIKey(const aValue: string;
  aLocation: TWebhookAPIKeyLocation; const aName: string);
begin
  FAPIKey := aValue;
  FAPIKeyLocation := aLocation;
  if aName <> '' then
    FAPIKeyName := aName
  else
    case aLocation of
      TWebhookAPIKeyLocation.Header:      FAPIKeyName := DEFAULT_API_KEY_HEADER_NAME;
      TWebhookAPIKeyLocation.QueryString: FAPIKeyName := DEFAULT_API_KEY_QUERY_PARAM;
    end;
end;

function TLoggerProWebhookAppender.LogItemToJSON(const aLogItem: TLogItem): string;
var
  lJSON: TJSONObject;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair('timestamp', DateToISO8601(aLogItem.TimeStamp, False));
    lJSON.AddPair('level', aLogItem.LogTypeAsString);
    lJSON.AddPair('message', aLogItem.LogMessage);
    lJSON.AddPair('tag', aLogItem.LogTag);
    lJSON.AddPair('tid', TJSONNumber.Create(aLogItem.ThreadID));

    if TLogExtendedInfo.EIComputerName in FExtendedInfo then
      lJSON.AddPair('hostname', FExtendedInfoData[TLogExtendedInfo.EIComputerName]);
    if TLogExtendedInfo.EIUserName in FExtendedInfo then
      lJSON.AddPair('username', FExtendedInfoData[TLogExtendedInfo.EIUserName]);
    if TLogExtendedInfo.EIProcessName in FExtendedInfo then
      lJSON.AddPair('processname', FExtendedInfoData[TLogExtendedInfo.EIProcessName]);
    if TLogExtendedInfo.EIProcessID in FExtendedInfo then
      lJSON.AddPair('pid', FExtendedInfoData[TLogExtendedInfo.EIProcessID]);

    Result := lJSON.ToJSON;
  finally
    lJSON.Free;
  end;
end;

function TLoggerProWebhookAppender.BuildURL(const aLogItem: TLogItem): string;
begin
  Result := FURL;
  if FAppendTagAndLevelToURL then
    Result := Result + '/' + TNetEncoding.URL.Encode(aLogItem.LogTag.Trim) +
      '/' + TNetEncoding.URL.Encode(aLogItem.LogTypeAsString);
  // API key as query parameter: respect any existing '?' in the URL.
  if (FAPIKey <> '') and (FAPIKeyLocation = TWebhookAPIKeyLocation.QueryString) then
  begin
    if Pos('?', Result) > 0 then
      Result := Result + '&'
    else
      Result := Result + '?';
    Result := Result + TNetEncoding.URL.Encode(FAPIKeyName) + '=' +
      TNetEncoding.URL.Encode(FAPIKey);
  end;
end;

function TLoggerProWebhookAppender.CreateData(const aLogItem: TLogItem;
  out aContentType: string): TStream;
begin
  Result := nil;
  if Assigned(FOnCreateData) then
  begin
    FOnCreateData(Self, aLogItem, FExtendedInfo, Result, aContentType);
    Exit;
  end;
  case FContentType of
    TWebhookContentType.JSON:
      begin
        Result := TStringStream.Create(LogItemToJSON(aLogItem), TEncoding.UTF8);
        aContentType := 'application/json; charset=utf-8';
      end;
    TWebhookContentType.PlainText:
      begin
        Result := TStringStream.Create(FormatLog(aLogItem), TEncoding.UTF8);
        aContentType := 'text/plain; charset=utf-8';
      end;
  else
    Result := TStringStream.Create(FormatLog(aLogItem), TEncoding.UTF8);
    aContentType := 'text/plain; charset=utf-8';
  end;
end;

procedure TLoggerProWebhookAppender.InternalWriteLog(const aURL: string;
  const aLogItem: TLogItem; const aData: TStream; const aContentTypeStr: string);
var
  lHeaders: TNetHeaders;
  lHeaderPair: TPair<string, string>;
  lRetryCount: Integer;
  lResp: IHTTPResponse;
  lHeaderCount: Integer;
  I: Integer;
begin
  // Build headers array (Content-Type + custom + optional API key header)
  lHeaderCount := 1 + FCustomHeaders.Count;
  if (FAPIKey <> '') and (FAPIKeyLocation = TWebhookAPIKeyLocation.Header) then
    Inc(lHeaderCount);
  SetLength(lHeaders, lHeaderCount);
  lHeaders[0] := TNetHeader.Create('Content-Type', aContentTypeStr);
  I := 1;
  for lHeaderPair in FCustomHeaders do
  begin
    lHeaders[I] := TNetHeader.Create(lHeaderPair.Key, lHeaderPair.Value);
    Inc(I);
  end;
  if (FAPIKey <> '') and (FAPIKeyLocation = TWebhookAPIKeyLocation.Header) then
    lHeaders[I] := TNetHeader.Create(FAPIKeyName, FAPIKey);

  lRetryCount := 0;
  repeat
    try
      aData.Position := 0;
      lResp := FHTTPClient.Post(aURL, aData, nil, lHeaders);
      if not (lResp.StatusCode in [200, 201, 202, 204]) then
        raise ELoggerPro.CreateFmt('HTTP Error %d: %s', [lResp.StatusCode, lResp.StatusText]);
      Break;
    except
      on E: Exception do
      begin
        Inc(lRetryCount);
        if Assigned(FOnSendError) then
        begin
          FOnSendError(Self, aLogItem, E, lRetryCount);
          if lRetryCount = 0 then Break;
        end;
        if lRetryCount >= FMaxRetryCount then Break;
        Sleep(100 * lRetryCount);
      end;
    end;
  until False;
end;

procedure TLoggerProWebhookAppender.WriteLog(const aLogItem: TLogItem);
var
  lURL: string;
  lData: TStream;
  lContentType: string;
begin
  lURL := BuildURL(aLogItem);
  lData := CreateData(aLogItem, lContentType);
  try
    if Assigned(lData) then
      InternalWriteLog(lURL, aLogItem, lData, lContentType);
  finally
    lData.Free;
  end;
end;

end.
