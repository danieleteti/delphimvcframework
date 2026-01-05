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

unit LoggerPro.ElasticSearchAppender;

{ Log appender for ElasticSearch 6.4+ endpoints }

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.DateUtils;

type
  { Callback for handling network errors.
    Set aRetryCount to 0 to stop retrying. }
  TOnElasticSearchError = reference to procedure(
    const Sender: TObject;
    const aLogItem: TLogItem;
    const aException: Exception;
    var aRetryCount: Integer);

  { @abstract(Log appender for ElasticSearch 6.4+ endpoints)
    Sends log items to an ElasticSearch index via HTTP POST.
    Author: Daniele Teti and Salvatore Sparacino
  }
  TLoggerProElasticSearchAppender = class(TLoggerProAppenderBase)
  private
    FURL: string;
    FHTTPClient: THTTPClient;
    FTimeoutSeconds: Integer;
    FMaxRetryCount: Integer;
    FOnSendError: TOnElasticSearchError;
    function LogItemToJSON(const aLogItem: TLogItem): string;
    procedure InternalWriteLog(const aLogItem: TLogItem; const aJSON: string);
  public
    const DEFAULT_TIMEOUT_SECONDS = 5;
    const DEFAULT_MAX_RETRY_COUNT = 3;

    { Creates an ElasticSearch appender.
      @param aElasticSearchURL Full URL to ElasticSearch endpoint (e.g., 'http://localhost:9200/logs/_doc')
      @param aTimeoutSeconds Connection and response timeout
      @param aLogItemRenderer Optional custom renderer }
    constructor Create(const aElasticSearchURL: string;
      aTimeoutSeconds: Integer = DEFAULT_TIMEOUT_SECONDS;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce; overload;

    { Creates an ElasticSearch appender from host, port and index.
      @param aElasticHost ElasticSearch host (e.g., 'http://localhost')
      @param aElasticPort ElasticSearch port (e.g., 9200)
      @param aElasticIndex Index name (will be lowercased)
      @param aTimeoutSeconds Connection and response timeout
      @param aLogItemRenderer Optional custom renderer }
    constructor Create(const aElasticHost: string;
      aElasticPort: Integer;
      const aElasticIndex: string;
      aTimeoutSeconds: Integer = DEFAULT_TIMEOUT_SECONDS;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce; overload;

    destructor Destroy; override;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;

    property URL: string read FURL;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds;
    property MaxRetryCount: Integer read FMaxRetryCount write FMaxRetryCount;
    { Callback for network errors. Use to implement custom retry logic. }
    property OnSendError: TOnElasticSearchError read FOnSendError write FOnSendError;
  end;

implementation

{ TLoggerProElasticSearchAppender }

constructor TLoggerProElasticSearchAppender.Create(const aElasticSearchURL: string;
  aTimeoutSeconds: Integer;
  aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  if aElasticSearchURL.Trim.IsEmpty then
    raise ELoggerPro.Create('ElasticSearch URL cannot be empty');
  if aTimeoutSeconds < 1 then
    raise ELoggerPro.CreateFmt('TimeoutSeconds must be >= 1, got %d', [aTimeoutSeconds]);
  FURL := aElasticSearchURL;
  FTimeoutSeconds := aTimeoutSeconds;
  FMaxRetryCount := DEFAULT_MAX_RETRY_COUNT;
end;

constructor TLoggerProElasticSearchAppender.Create(const aElasticHost: string;
  aElasticPort: Integer;
  const aElasticIndex: string;
  aTimeoutSeconds: Integer;
  aLogItemRenderer: ILogItemRenderer);
begin
  Create(
    Format('%s:%d/%s/_doc', [aElasticHost, aElasticPort, aElasticIndex.ToLower]),
    aTimeoutSeconds,
    aLogItemRenderer);
end;

destructor TLoggerProElasticSearchAppender.Destroy;
begin
  inherited;
end;

procedure TLoggerProElasticSearchAppender.Setup;
begin
  inherited;
  FHTTPClient := THTTPClient.Create;
  FHTTPClient.ConnectionTimeout := FTimeoutSeconds * 1000;
  FHTTPClient.ResponseTimeout := FTimeoutSeconds * 1000;
end;

procedure TLoggerProElasticSearchAppender.TearDown;
begin
  FHTTPClient.Free;
  FHTTPClient := nil;
  inherited;
end;

procedure TLoggerProElasticSearchAppender.TryToRestart(var Restarted: Boolean);
begin
  inherited;
  // Recreate HTTP client
  if Assigned(FHTTPClient) then
    FHTTPClient.Free;
  FHTTPClient := THTTPClient.Create;
  FHTTPClient.ConnectionTimeout := FTimeoutSeconds * 1000;
  FHTTPClient.ResponseTimeout := FTimeoutSeconds * 1000;
  Restarted := True;
end;

function TLoggerProElasticSearchAppender.LogItemToJSON(const aLogItem: TLogItem): string;
var
  lJSON: TJSONObject;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair('log_tag', aLogItem.LogTag);
    lJSON.AddPair('log_level', aLogItem.LogTypeAsString);
    lJSON.AddPair('log_message', aLogItem.LogMessage);
    lJSON.AddPair('log_datetime', DateToISO8601(aLogItem.TimeStamp, False));
    lJSON.AddPair('timestamp', DateToISO8601(aLogItem.TimeStamp, False));
    lJSON.AddPair('thread_id', TJSONNumber.Create(aLogItem.ThreadID));
    Result := lJSON.ToJSON;
  finally
    lJSON.Free;
  end;
end;

procedure TLoggerProElasticSearchAppender.InternalWriteLog(const aLogItem: TLogItem;
  const aJSON: string);
var
  lHeaders: TNetHeaders;
  lRetryCount: Integer;
  lResp: IHTTPResponse;
  lStream: TStringStream;
begin
  SetLength(lHeaders, 1);
  lHeaders[0] := TNetHeader.Create('Content-Type', 'application/json; charset=utf-8');

  lRetryCount := 0;
  lStream := TStringStream.Create(aJSON, TEncoding.UTF8);
  try
    repeat
      try
        lStream.Position := 0;
        lResp := FHTTPClient.Post(FURL, lStream, nil, lHeaders);
        if not (lResp.StatusCode in [200, 201, 202]) then
        begin
          raise ELoggerPro.CreateFmt('ElasticSearch Error %d: %s',
            [lResp.StatusCode, lResp.StatusText]);
        end;
        Break; // Success
      except
        on E: Exception do
        begin
          Inc(lRetryCount);

          // If custom error handler is set, let it decide
          if Assigned(FOnSendError) then
          begin
            FOnSendError(Self, aLogItem, E, lRetryCount);
            if lRetryCount = 0 then
              Break;
          end;

          // Check if we've exceeded max retries
          if lRetryCount >= FMaxRetryCount then
            Break;

          // Wait before retrying
          Sleep(100 * lRetryCount);
        end;
      end;
    until False;
  finally
    lStream.Free;
  end;
end;

procedure TLoggerProElasticSearchAppender.WriteLog(const aLogItem: TLogItem);
var
  lJSON: string;
begin
  lJSON := LogItemToJSON(aLogItem);
  InternalWriteLog(aLogItem, lJSON);
end;

end.
