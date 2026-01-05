// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
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
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit LoggerPro.HTTPAppender;

{ Cross-platform HTTP appender using System.Net.HttpClient }

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
  THTTPContentType = (JSON, PlainText);

  TLoggerProHTTPAppender = class;

  { Callback for customizing request data.
    Set aData to your custom stream; caller will free it. }
  TOnCreateHTTPData = reference to procedure(
    const Sender: TLoggerProHTTPAppender;
    const aLogItem: TLogItem;
    const aExtendedInfo: TLoggerProExtendedInfo;
    var aData: TStream;
    var aContentType: string);

  { Callback for handling network errors.
    Set aRetryCount to control retries (0 = no more retries). }
  TOnHTTPSendError = reference to procedure(
    const Sender: TLoggerProHTTPAppender;
    const aLogItem: TLogItem;
    const aException: Exception;
    var aRetryCount: Integer);

  { @abstract(HTTP POST appender for sending logs to REST endpoints)
    Cross-platform appender that sends log items via HTTP POST.
    Supports JSON and plain text content types.
    Use cases: Logstash, custom log collectors, webhooks, cloud logging services.
  }
  TLoggerProHTTPAppender = class(TLoggerProAppenderBase)
  private
    FURL: string;
    FContentType: THTTPContentType;
    FHTTPClient: THTTPClient;
    FCustomHeaders: TDictionary<string, string>;
    FExtendedInfo: TLoggerProExtendedInfo;
    FExtendedInfoData: array [low(TLogExtendedInfo) .. high(TLogExtendedInfo)] of string;
    FTimeoutSeconds: Integer;
    FMaxRetryCount: Integer;
    FAppendTagAndLevelToURL: Boolean;
    FOnCreateData: TOnCreateHTTPData;
    FOnSendError: TOnHTTPSendError;
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

    constructor Create(const aURL: string;
      aContentType: THTTPContentType = THTTPContentType.JSON;
      aTimeoutSeconds: Integer = DEFAULT_TIMEOUT_SECONDS;
      aExtendedInfo: TLoggerProExtendedInfo = DEFAULT_EXTENDED_INFO;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    destructor Destroy; override;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;

    { Add a custom header to be sent with each request }
    procedure AddHeader(const aName, aValue: string);

    property URL: string read FURL write FURL;
    property ContentType: THTTPContentType read FContentType write FContentType;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds;
    property MaxRetryCount: Integer read FMaxRetryCount write FMaxRetryCount;
    { If True, appends /tag/level to URL (REST style). Default False. }
    property AppendTagAndLevelToURL: Boolean read FAppendTagAndLevelToURL write FAppendTagAndLevelToURL;
    property ExtendedInfo: TLoggerProExtendedInfo read FExtendedInfo;

    { Callback to customize the data sent. If set, overrides default behavior. }
    property OnCreateData: TOnCreateHTTPData read FOnCreateData write FOnCreateData;
    { Callback for network errors. Use to implement custom retry logic. }
    property OnSendError: TOnHTTPSendError read FOnSendError write FOnSendError;
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

{ TLoggerProHTTPAppender }

constructor TLoggerProHTTPAppender.Create(const aURL: string;
  aContentType: THTTPContentType;
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

destructor TLoggerProHTTPAppender.Destroy;
begin
  FCustomHeaders.Free;
  inherited;
end;

procedure TLoggerProHTTPAppender.LoadExtendedInfo;
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
  begin
    FExtendedInfoData[TLogExtendedInfo.EIProcessName] := TPath.GetFileName(GetModuleName(HInstance));
  end;

  if TLogExtendedInfo.EIProcessID in FExtendedInfo then
  begin
    FExtendedInfoData[TLogExtendedInfo.EIProcessID] := IntToStr(GetCurrentProcessId);
  end;
{$ENDIF}

{$IF Defined(Android)}
  if TLogExtendedInfo.EIProcessName in FExtendedInfo then
  begin
    FExtendedInfoData[TLogExtendedInfo.EIProcessName] := TAndroidHelper.ApplicationTitle;
  end;
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

procedure TLoggerProHTTPAppender.Setup;
begin
  inherited;
  LoadExtendedInfo;
  FHTTPClient := THTTPClient.Create;
  FHTTPClient.ConnectionTimeout := FTimeoutSeconds * 1000;
  FHTTPClient.ResponseTimeout := FTimeoutSeconds * 1000;
end;

procedure TLoggerProHTTPAppender.TearDown;
begin
  FHTTPClient.Free;
  FHTTPClient := nil;
  inherited;
end;

procedure TLoggerProHTTPAppender.AddHeader(const aName, aValue: string);
begin
  FCustomHeaders.AddOrSetValue(aName, aValue);
end;

function TLoggerProHTTPAppender.LogItemToJSON(const aLogItem: TLogItem): string;
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

    // Add extended info
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

function TLoggerProHTTPAppender.BuildURL(const aLogItem: TLogItem): string;
begin
  Result := FURL;
  if FAppendTagAndLevelToURL then
  begin
    Result := Result + '/' + TNetEncoding.URL.Encode(aLogItem.LogTag.Trim) +
      '/' + TNetEncoding.URL.Encode(aLogItem.LogTypeAsString);
  end;
end;

function TLoggerProHTTPAppender.CreateData(const aLogItem: TLogItem;
  out aContentType: string): TStream;
begin
  Result := nil;

  // If custom handler is set, use it
  if Assigned(FOnCreateData) then
  begin
    FOnCreateData(Self, aLogItem, FExtendedInfo, Result, aContentType);
    Exit;
  end;

  // Default behavior
  case FContentType of
    THTTPContentType.JSON:
      begin
        Result := TStringStream.Create(LogItemToJSON(aLogItem), TEncoding.UTF8);
        aContentType := 'application/json; charset=utf-8';
      end;
    THTTPContentType.PlainText:
      begin
        Result := TStringStream.Create(FormatLog(aLogItem), TEncoding.UTF8);
        aContentType := 'text/plain; charset=utf-8';
      end;
  else
    Result := TStringStream.Create(FormatLog(aLogItem), TEncoding.UTF8);
    aContentType := 'text/plain; charset=utf-8';
  end;
end;

procedure TLoggerProHTTPAppender.InternalWriteLog(const aURL: string;
  const aLogItem: TLogItem; const aData: TStream; const aContentTypeStr: string);
var
  lHeaders: TNetHeaders;
  lHeaderPair: TPair<string, string>;
  lRetryCount: Integer;
  lResp: IHTTPResponse;
  I: Integer;
begin
  // Build headers array
  SetLength(lHeaders, FCustomHeaders.Count + 1);
  lHeaders[0] := TNetHeader.Create('Content-Type', aContentTypeStr);
  I := 1;
  for lHeaderPair in FCustomHeaders do
  begin
    lHeaders[I] := TNetHeader.Create(lHeaderPair.Key, lHeaderPair.Value);
    Inc(I);
  end;

  lRetryCount := 0;
  repeat
    try
      aData.Position := 0;
      lResp := FHTTPClient.Post(aURL, aData, nil, lHeaders);
      if not (lResp.StatusCode in [200, 201, 202, 204]) then
      begin
        raise ELoggerPro.CreateFmt('HTTP Error %d: %s', [lResp.StatusCode, lResp.StatusText]);
      end;
      Break; // Success, exit loop
    except
      on E: Exception do
      begin
        Inc(lRetryCount);

        // If custom error handler is set, let it decide
        if Assigned(FOnSendError) then
        begin
          FOnSendError(Self, aLogItem, E, lRetryCount);
          // If handler set lRetryCount to 0, stop retrying
          if lRetryCount = 0 then
            Break;
        end;

        // Check if we've exceeded max retries
        if lRetryCount >= FMaxRetryCount then
          Break;

        // Wait a bit before retrying
        Sleep(100 * lRetryCount);
      end;
    end;
  until False;
end;

procedure TLoggerProHTTPAppender.WriteLog(const aLogItem: TLogItem);
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
