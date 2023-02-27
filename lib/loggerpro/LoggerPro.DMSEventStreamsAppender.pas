unit LoggerPro.DMSEventStreamsAppender;

interface

uses
  System.Classes,
  MVCFramework.Commons, {this demo requires DMVCFramework}
  System.SysUtils,
  LoggerPro,
  System.Net.HttpClient,
  EventStreamsRPCProxy,
  JsonDataObjects;

type

  {
    Log appender for a DMSContainer EventStreams endpoint
    Author: Daniele Teti (https://github.com/danieleteti/)
  }

  TOnCreateJSONData = reference to procedure(const Sender: TObject; const LogItem: TLogItem;
    const ExtendedInfo: TLoggerProExtendedInfo;
    Data: TJSONObject);
  TOnNetSendError = reference to procedure(const Sender: TObject; const LogItem: TLogItem; const NetError: Exception;
    var RetryCount: Integer);

  {
    dmsatByTag:
    all messages with the same tag go in the same queue

    dmsatByType:
    all messages with the same type go in the same queue
    dmsatByTagThenType:
    messages are organized in one queue for each tag,
    then one queue for each type (es. myapp.mytag.error, myapp.mytag.warning etc)

    dmsatByTypeThenTag:
    messages are organized in one queue for each type,
    then one queue for each tag (es. myapp.error.mytag, myapp.warning.mytag etc)
  }
  TDMSQueueAggregationType = (dmsatByTag, dmsatByType, dmsatByTagThenType, dmsatByTypeThenTag);

  TLoggerProDMSContainerAppender = class(TLoggerProAppenderBase, ILogAppender)
  strict private
    fOnCreateJSONData: TOnCreateJSONData;
    fOnNetSendError: TOnNetSendError;
    fExtendedInfo: TLoggerProExtendedInfo;
    fEventStreamsProxy: TEventStreamsRPCProxy;
    fDMSContainerAPIKey: String;
    fExtendedInfoData: array [low(TLogExtendedInfo) .. high(TLogExtendedInfo)] of string;
    procedure SetOnCreateJSONData(const Value: TOnCreateJSONData);
    procedure SetOnNetSendError(const Value: TOnNetSendError);
  private
    fQueueNameBase: string;
    fLogItemAggregationType: TDMSQueueAggregationType;
  strict protected
    procedure LoadExtendedInfo;
    function GetExtendedInfo: TJSONObject;
  protected const
    DEFAULT_EXTENDED_INFO = [TLogExtendedInfo.EIUserName, TLogExtendedInfo.EIComputerName,
      TLogExtendedInfo.EIProcessName,
      TLogExtendedInfo.EIProcessID, TLogExtendedInfo.EIDeviceID];
    procedure InternalWriteLog(const aLogItem: TLogItem; const aJSONObject: TJSONObject);
  public
    procedure WriteLog(const aLogItem: TLogItem); override;
    constructor Create(aEventStreamsProxy: TEventStreamsRPCProxy;
      aDMSContainerAPIKey: String;
      aEventStreamsQueueNameBase: String = 'queues.logs.';
      aLogItemAggregationType: TDMSQueueAggregationType = dmsatByTag;
      aLogExtendedInfo: TLoggerProExtendedInfo = DEFAULT_EXTENDED_INFO); reintroduce;
    destructor Destroy; override;
    property OnCreateJSONData: TOnCreateJSONData read fOnCreateJSONData write SetOnCreateJSONData;
    property OnNetSendError: TOnNetSendError read fOnNetSendError write SetOnNetSendError;
    procedure TearDown; override;
    procedure Setup; override;
    function CreateData(const SrcLogItem: TLogItem): TJSONObject; virtual;
    function GetDefaultLog(const aLogItem: TLogItem): TJSONObject; virtual;
    class function GetModuleBaseName: String;
  end;

implementation

uses
{$IF Defined(MSWINDOWS) }
  Winapi.Windows,
{$ENDIF}
{$IF Defined(Android) }
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.Helpers,
{$ENDIF}
  System.NetEncoding,
  System.IOUtils,
  System.Net.URLClient;

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


constructor TLoggerProDMSContainerAppender.Create(
  aEventStreamsProxy: TEventStreamsRPCProxy;
  aDMSContainerAPIKey: String;
  aEventStreamsQueueNameBase: String;
  aLogItemAggregationType: TDMSQueueAggregationType;
  aLogExtendedInfo: TLoggerProExtendedInfo);
begin
  inherited Create;
  fEventStreamsProxy := aEventStreamsProxy;
  fQueueNameBase := aEventStreamsQueueNameBase;
  fLogItemAggregationType := aLogItemAggregationType;
  if not fQueueNameBase.EndsWith('.') then
    fQueueNameBase := fQueueNameBase + '.';
  fExtendedInfo := aLogExtendedInfo;
  fDMSContainerAPIKey := aDMSContainerAPIKey;
  LoadExtendedInfo;
end;

function TLoggerProDMSContainerAppender.CreateData(const SrcLogItem: TLogItem): TJSONObject;
begin
  Result := nil;
  try
    if Assigned(fOnCreateJSONData) then
    begin
      fOnCreateJSONData(Self, SrcLogItem, fExtendedInfo, Result);
    end
    else
    begin
      Result := GetDefaultLog(SrcLogItem);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

destructor TLoggerProDMSContainerAppender.Destroy;
begin
  fEventStreamsProxy.Free;
  inherited;
end;

function TLoggerProDMSContainerAppender.GetDefaultLog(const aLogItem: TLogItem): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.S['timestamp'] := datetimetostr(aLogItem.TimeStamp, FormatSettings);
    Result.L['tid'] := aLogItem.ThreadID;
    Result.S['type'] := aLogItem.LogTypeAsString;
    Result.S['text'] := aLogItem.LogMessage;
    Result.O['info'] := GetExtendedInfo;
    // Result.S['tag'] := aLogItem.LogTag;
  except
    Result.Free;
    raise;
  end;
end;

function TLoggerProDMSContainerAppender.GetExtendedInfo: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
{$IF Defined(MSWINDOWS)}
    if TLogExtendedInfo.EIUserName in fExtendedInfo then
    begin
      Result.S['username'] := fExtendedInfoData[TLogExtendedInfo.EIUserName];
    end;
    if TLogExtendedInfo.EIComputerName in fExtendedInfo then
    begin
      Result.S['computername'] := fExtendedInfoData[TLogExtendedInfo.EIComputerName];
    end;
    if TLogExtendedInfo.EIProcessName in fExtendedInfo then
    begin
      Result.S['processname'] := fExtendedInfoData[TLogExtendedInfo.EIProcessName];
    end;
    if TLogExtendedInfo.EIProcessID in fExtendedInfo then
    begin
      Result.S['pid'] := fExtendedInfoData[TLogExtendedInfo.EIProcessID];
    end;
{$ENDIF}
{$IF Defined(Android)}
    if TLogExtendedInfo.EIProcessName in fExtendedInfo then
    begin
      Result.S['processname'] := fExtendedInfoData[TLogExtendedInfo.EIProcessName];
    end;
{$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

class function TLoggerProDMSContainerAppender.GetModuleBaseName: String;
begin
{$IF Defined(MSWINDOWS)}
  Result := TPath.ChangeExtension(TPath.GetFileName(GetModuleName(HInstance)), '');
{$ENDIF}
{$IF Defined(Android)}
  Result := TAndroidHelper.ApplicationTitle;
{$ENDIF}
  if Result.IsEmpty then
  begin
    raise ELoggerPro.Create('Current platform not supported by ' + ClassName);
  end;
end;

procedure TLoggerProDMSContainerAppender.LoadExtendedInfo;
begin
{$IF Defined(MSWINDOWS)}
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

procedure TLoggerProDMSContainerAppender.SetOnCreateJSONData(const Value: TOnCreateJSONData);
begin
  fOnCreateJSONData := Value;
end;

procedure TLoggerProDMSContainerAppender.SetOnNetSendError(const Value: TOnNetSendError);
begin
  fOnNetSendError := Value;
end;

procedure TLoggerProDMSContainerAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProDMSContainerAppender.TearDown;
begin
  inherited;
end;

procedure TLoggerProDMSContainerAppender.InternalWriteLog(const aLogItem: TLogItem;
  const aJSONObject: TJSONObject);
var
  lRetryCount: Integer;
  lJSONResp: TJSONObject;
const
  MAX_RETRY_COUNT = 5;
begin
  lRetryCount := 0;
  repeat
    try
      lJSONResp := fEventStreamsProxy.EnqueueMessage(fDMSContainerAPIKey, fQueueNameBase + aLogItem.LogTag,
        aJSONObject);
      try

      finally
        lJSONResp.Free;
      end;
      Break;
    except
      on E: Exception do
      begin
        // if there is an event handler for net exception, call it
        if Assigned(fOnNetSendError) then
          OnNetSendError(Self, aLogItem, E, lRetryCount);
        Inc(lRetryCount);
        // if the handler has set FRetryCount to a positive value then retry the call
        if lRetryCount >= MAX_RETRY_COUNT then
          Break;
      end;
    end;
  until False;
  // finally
  // FreeAndNil(lHTTPCli);
  // end;
end;

procedure TLoggerProDMSContainerAppender.WriteLog(const aLogItem: TLogItem);
var
  lRetryCount: Integer;
  lJSONResp: TJSONObject;
  lQueueName: string;
const
  MAX_RETRY_COUNT = 5;
begin
  lRetryCount := 0;
  repeat
    try
      case fLogItemAggregationType of
        dmsatByTag:
          lQueueName := fQueueNameBase + aLogItem.LogTag;
        dmsatByType:
          lQueueName := fQueueNameBase + aLogItem.LogTypeAsString;
        dmsatByTagThenType:
          lQueueName := fQueueNameBase + aLogItem.LogTag + '.' + aLogItem.LogTypeAsString;
        dmsatByTypeThenTag:
          lQueueName := fQueueNameBase + aLogItem.LogTypeAsString + '.' + aLogItem.LogTag;
      else
        raise ELoggerPro.Create('Invalid Aggregation type');
      end;
      lJSONResp := fEventStreamsProxy.EnqueueMessage(fDMSContainerAPIKey,
        lQueueName,
        CreateData(aLogItem));
      try

      finally
        lJSONResp.Free;
      end;
      Break;
    except
      on E: Exception do
      begin
        // if there is an event handler for net exception, call it
        if Assigned(fOnNetSendError) then
          OnNetSendError(Self, aLogItem, E, lRetryCount);
        Inc(lRetryCount);
        // if the handler has set FRetryCount to a positive value then retry the call
        if lRetryCount >= MAX_RETRY_COUNT then
          Break;
      end;
    end;
  until False;
end;

end.
