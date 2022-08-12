unit LoggerPro.RESTAppender;

interface

uses
  Classes,
  SysUtils,
  LoggerPro,
  System.Net.HttpClient;

type

  {
    Log appender for a REST endpoint
    Author: Daniele Teti (https://github.com/danieleteti/)
    Some ideas from NSQ appender from Stéphane "Fulgan" GROBETY (https://github.com/Fulgan/)
  }

  TOnCreateData = reference to procedure(const Sender: TObject; const LogItem: TLogItem; const ExtendedInfo: TLoggerProExtendedInfo;
    var Data: TStream);
  TOnNetSendError = reference to procedure(const Sender: TObject; const LogItem: TLogItem; const NetError: Exception;
    var RetryCount: Integer);

  TLoggerProRESTAppender = class(TLoggerProAppenderBase, ILogAppender)
  strict private
    FOnCreateData: TOnCreateData;
    FOnNetSendError: TOnNetSendError;
    fExtendedInfo: TLoggerProExtendedInfo;
    fContentType: string;
    fRESTUrl: string;
    {.$IFDEF MSWINDOWS}
    fExtendedInfoData: array [low(TLogExtendedInfo) .. high(TLogExtendedInfo)] of string;
    {.$ENDIF}
    procedure SetOnCreateData(const Value: TOnCreateData);
    procedure SetOnNetSendError(const Value: TOnNetSendError);
  strict protected
    procedure LoadExtendedInfo;
    function GetExtendedInfo: string;
  protected const
    { @abstract(Defines the default format string used by the @link(TLoggerProRESTAppender).)
      The positional parameters are the following:
      @orderedList(
      @item SetNumber 0
      @item TimeStamp
      @item ThreadID
      @item LogType
      @item LogMessage
      @item Extended Information
      @item LogTag
      )
    }
    DEFAULT_LOG_FORMAT = '%0:s [TID %1:10u][%2:-8s] %3:s {EI%4:s}[%5:s]';
    DEFAULT_EXTENDED_INFO = [TLogExtendedInfo.EIUserName, TLogExtendedInfo.EIComputerName, TLogExtendedInfo.EIProcessName,
      TLogExtendedInfo.EIProcessID, TLogExtendedInfo.EIDeviceID];
    DEFAULT_REST_URL = 'http://127.0.0.1:8080/api/logs';
    procedure InternalWriteLog(const aURI: string; const aLogItem: TLogItem; const aStream: TStream);
  public
    function GetRESTUrl: string;
    procedure SetRESTUrl(const Value: string);
    procedure WriteLog(const aLogItem: TLogItem); override;
    constructor Create(aRESTUrl: string = DEFAULT_REST_URL; aContentType: string = 'text/plain';
      aLogExtendedInfo: TLoggerProExtendedInfo = DEFAULT_EXTENDED_INFO; aLogFormat: string = DEFAULT_LOG_FORMAT); reintroduce;
    property RESTUrl: string read GetRESTUrl write SetRESTUrl;
    property OnCreateData: TOnCreateData read FOnCreateData write SetOnCreateData;
    property OnNetSendError: TOnNetSendError read FOnNetSendError write SetOnNetSendError;
    procedure TearDown; override;
    procedure Setup; override;
    function CreateData(const SrcLogItem: TLogItem): TStream; virtual;
    function FormatLog(const aLogItem: TLogItem): string; override;
  end;

implementation

uses
  System.NetEncoding,
  System.IOUtils,
  System.Net.URLClient
{$IF Defined(MSWINDOWS) }
    ,
  Winapi.Windows
{$ENDIF}
{$IF Defined(Android) }
    ,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.Helpers
{$ENDIF}
    ;

{$IFDEF MSWINDOWS }


function GetUserFromWindows: string;
var
  iLen: Cardinal;
begin
  iLen := 256;
  Result := StringOfChar(#0, iLen);
  GetUserName(PChar(Result), iLen);
  SetLength(Result, iLen - 1);
end;

function GetComputerNameFromWindows: string;
var
  iLen: Cardinal;
begin
  iLen := MAX_COMPUTERNAME_LENGTH + 1;
  Result := StringOfChar(#0, iLen);
  GetComputerName(PChar(Result), iLen);
  SetLength(Result, iLen);
end;

{$ENDIF}


constructor TLoggerProRESTAppender.Create(aRESTUrl: string = DEFAULT_REST_URL; aContentType: string = 'text/plain';
  aLogExtendedInfo: TLoggerProExtendedInfo = DEFAULT_EXTENDED_INFO; aLogFormat: string = DEFAULT_LOG_FORMAT);
begin
  inherited Create(aLogFormat);
  fRESTUrl := aRESTUrl;
  fExtendedInfo := aLogExtendedInfo;
  fContentType := aContentType;
  LoadExtendedInfo;
end;

function TLoggerProRESTAppender.CreateData(const SrcLogItem: TLogItem): TStream;
begin
  Result := nil;
  try
    if Assigned(FOnCreateData) then
    begin
      FOnCreateData(Self, SrcLogItem, fExtendedInfo, Result);
    end
    else
    begin
      Result := TStringStream.Create(FormatLog(SrcLogItem), TEncoding.UTF8);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TLoggerProRESTAppender.FormatLog(const aLogItem: TLogItem): string;
begin
  Result := Format(LogFormat, [datetimetostr(aLogItem.TimeStamp, FormatSettings), aLogItem.ThreadID, aLogItem.LogTypeAsString,
    aLogItem.LogMessage, GetExtendedInfo, aLogItem.LogTag]);
end;

function TLoggerProRESTAppender.GetExtendedInfo: string;
begin
  Result := '';

{$IFDEF MSWINDOWS}
  if TLogExtendedInfo.EIUserName in fExtendedInfo then
  begin
    Result := Result + ';UserName=' + fExtendedInfoData[TLogExtendedInfo.EIUserName];
  end;
  if TLogExtendedInfo.EIComputerName in fExtendedInfo then
  begin
    Result := Result + ';ComputerName=' + fExtendedInfoData[TLogExtendedInfo.EIComputerName];
  end;
  if TLogExtendedInfo.EIProcessName in fExtendedInfo then
  begin
    Result := Result + ';ProcessName=' + fExtendedInfoData[TLogExtendedInfo.EIProcessName];
  end;
  if TLogExtendedInfo.EIProcessID in fExtendedInfo then
  begin
    Result := Result + ';PID=' + fExtendedInfoData[TLogExtendedInfo.EIProcessID];
  end;
{$ENDIF}
{$IF Defined(Android)}
  if TLogExtendedInfo.EIProcessName in fExtendedInfo then
  begin
    Result := Result + ';ProcessName=' + fExtendedInfoData[TLogExtendedInfo.EIProcessName];
  end;
{$ENDIF}
  Result := '[' + Result.Substring(1) + ']';
end;

function TLoggerProRESTAppender.GetRESTUrl: string;
begin
  Result := fRESTUrl;
end;

procedure TLoggerProRESTAppender.LoadExtendedInfo;
begin
{$IF DEFINED(MSWINDOWS)}
  if TLogExtendedInfo.EIProcessID in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIProcessID] := IntToStr(GetCurrentProcessId);
  end;
  if TLogExtendedInfo.EIUserName in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIUserName] := GetUserFromWindows;
  end;
  if TLogExtendedInfo.EIComputerName in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIComputerName] := GetComputerNameFromWindows;
  end;
  if TLogExtendedInfo.EIProcessName in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIProcessName] := TPath.GetFileName(GetModuleName(HInstance));
  end;
  if TLogExtendedInfo.EIProcessID in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIProcessID] := IntToStr(GetCurrentProcessId);
  end;
{$ENDIF}
{$IF Defined(Android)}
  if TLogExtendedInfo.EIProcessName in fExtendedInfo then
  begin
    fExtendedInfoData[TLogExtendedInfo.EIProcessName] := TAndroidHelper.ApplicationTitle;
  end;
{$ENDIF}
end;

procedure TLoggerProRESTAppender.SetRESTUrl(const Value: string);
begin
  fRESTUrl := Value;
end;

procedure TLoggerProRESTAppender.SetOnCreateData(const Value: TOnCreateData);
begin
  FOnCreateData := Value;
end;

procedure TLoggerProRESTAppender.SetOnNetSendError(const Value: TOnNetSendError);
begin
  FOnNetSendError := Value;
end;

procedure TLoggerProRESTAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProRESTAppender.TearDown;
begin
  inherited;
end;

procedure TLoggerProRESTAppender.InternalWriteLog(const aURI: string; const aLogItem: TLogItem; const aStream: TStream);
var
  lHTTPCli: THTTPClient;
  lRetryCount: Integer;
  lResp: IHTTPResponse;
const
  MAX_RETRY_COUNT = 5;
begin
  lRetryCount := 0;
  lHTTPCli := THTTPClient.Create;
  try
    if Assigned(aStream) then
    begin
      repeat
        try
{$IF CompilerVersion >= 31}
          lHTTPCli.ConnectionTimeout := 1000;
          lHTTPCli.ResponseTimeout := 3000;
{$ENDIF}
          aStream.Seek(0, soFromBeginning);
          lResp := lHTTPCli.Post(aURI, aStream, nil, [TNetHeader.Create('content-type', fContentType)]);
          if not(lResp.StatusCode in [200, 201]) then
          begin
            raise ELoggerPro.Create(lResp.ContentAsString);
          end;
          Break;
        except
          on E: Exception do
          begin
            // if there is an event handler for net exception, call it
            if Assigned(FOnNetSendError) then
              OnNetSendError(Self, aLogItem, E, lRetryCount);
            Inc(lRetryCount);
            // if the handler has set FRetryCount to a positive value then retry the call
            if lRetryCount >= MAX_RETRY_COUNT then
              break;
          end;
        end;
      until False;
    end;
  finally
    FreeAndNil(lHTTPCli);
  end;
end;

procedure TLoggerProRESTAppender.WriteLog(const aLogItem: TLogItem);
var
  lURI: string;
  lData: TStream;
begin
  lURI := RESTUrl + '/' + TNetEncoding.URL.Encode(aLogItem.LogTag.Trim) + '/' + TNetEncoding.URL.Encode(aLogItem.LogTypeAsString);
  lData := CreateData(aLogItem);
  try
    if Assigned(lData) then
      InternalWriteLog(lURI, aLogItem, lData);
  finally
    lData.Free;
  end;
end;

end.
