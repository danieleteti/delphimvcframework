unit LoggerPro.NSQAppender;

interface

uses Classes, SysUtils, LoggerPro, System.Net.HttpClient;

type

{
  Author: Stéphane "Fulgan" GROBETY (https://github.com/Fulgan/)
  Log appender for NSQ (https://nsq.io) (https://github.com/nsqio/nsq)
  "NSQ is a realtime message processing system designed to operate at bitly's
  scale, handling billions of messages per day. It promotes distributed and
  decentralized topologies without single points of failure, enabling fault
  tolerance and high availability coupled with a reliable message delivery
  guarantee"

  For testing, you can navigate to the NSQ folder and type the following commands:

  This starts the NSQLookup service then starts a listener on the default
  endpoint(http:/127.0.0.1:4151)

  start nsqlookupd
  start nsqd --lookupd-tcp-address=127.0.0.1:4160


  This starts a consumer for the topic "test" that outputs the messages to the console:

  start nsq_tail --topic=test --lookupd-http-address=127.0.0.1:4161

  (optional) This starts a consumer for the ephemeral topic "test" that outputs the messages to the console:

  start nsq_tail --topic=test#ephemeral --lookupd-http-address=127.0.0.1:4161

  (optional) This starts a NSQAdmin web interface that can be reached on http://localhost:4171/

  start nsqadmin --lookupd-http-address=127.0.0.1:4161

  Note about consumers:
  - If there is no consumer to received messages for a channel, NSQ will
    save them to memory and disk unless the topic has been marked as Ephemeral.
    Use NSQAdmin to delete any extra channel created.
  - Ephemeral topics are not saved or cached and the topic will be deleted
    once the last consumer disconnects
  - Writing a consumer is more complex than writing a client. A list of available
    client libraries can be found at https://nsq.io/clients/client_libraries.html
}


  TOnCreateData = procedure(const sender : TObject; const LogItem: TLogItem; var Data: TStream);
  TOnNetSendError = procedure(const sender : TObject; const LogItem: TLogItem; const NetError: ENetHTTPClientException; var RetryCount: Integer);
  TLoggerProNSQAppenderBase = class(TLoggerProAppenderBase, ILogAppender)
  private
    FOnCreateData: TOnCreateData;
    FOnNetSendError: TOnNetSendError;
    procedure SetOnCreateData(const Value: TOnCreateData);
    procedure SetOnNetSendError(const Value: TOnNetSendError);
  protected
    FNSQUrl : string;
    FTopic: String;
    FUserName, FMachineName: string;
    FEphemeral: Boolean;
    FLastSignature: string;
    FLogFormat: string;
    FFormatSettings: TFormatSettings;
  public
    const DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-8s] %3:s [%4:s]';
    const DEFAULT_NSQ_URL = 'http://127.0.0.1:4151';

    function GetNSQUrl: string;
    procedure SetNSQUrl(const Value: string);
    function GetTopic: string;
    procedure SetTopic(const Value: string);
    procedure SetEphemeral(const Value: Boolean);
    /// <summary>TLoggerProNSQAppenderBase.Create
    /// </summary>
    /// <param name="aTopic"> (string) This is the "topic" of the channel. If left
    /// empty, the LogItem's tag will be used. </param>
    /// <param name="aEphemeral"> (Boolean) If true, the NSQ channel will be marked as
    /// Ephemeral: messages sent to this channel will neither be cached nor
    /// queued</param>
    /// <param name="aNSQUrl"> (string) URL of the NSQD service (usually, http://127.0.
    /// 0.1:4151)</param>
    /// <param name="aLogFormat"> (string) Log format to use if no custom log message
    /// creation event is defined </param>
    constructor Create(aTopic: string=''; aEphemeral: Boolean = False;
        aNSQUrl: string=DEFAULT_NSQ_URL;
        aLogFormat: string=DEFAULT_LOG_FORMAT);
        reintroduce;
    property NSQUrl: string read GetNSQUrl write SetNSQUrl;
    property Ephemeral: Boolean read FEphemeral write SetEphemeral;
    property OnCreateData: TOnCreateData read FOnCreateData write SetOnCreateData;
    property OnNetSendError: TOnNetSendError read FOnNetSendError write SetOnNetSendError;
    property Topic: string read GetTopic write SetTopic;
    procedure TearDown; override;
    procedure Setup; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    function CreateData(const SrcLogItem: TLogItem): TStream; virtual;
    function FormatLog(const aLogItem: TLogItem): string; virtual;
  end;

implementation

uses  System.NetEncoding;

constructor TLoggerProNSQAppenderBase.Create(aTopic: string=''; aEphemeral:
    Boolean = False; aNSQUrl: string=DEFAULT_NSQ_URL; aLogFormat:
    string=DEFAULT_LOG_FORMAT);
begin
  inherited Create();
  FEphemeral := aEphemeral;
  FNSQUrl := 'http://127.0.0.1:4151';
  FUserName := aNSQUrl;
  FLogFormat := aLogFormat;
  FTopic := aTopic;
end;

function TLoggerProNSQAppenderBase.CreateData(
  const SrcLogItem: TLogItem): TStream;
begin
  result := nil;
  try
    if assigned(FOnCreateData) then
    begin
      FOnCreateData(Self, SrcLogItem, Result);
    end
    else
    begin
      result := TStringStream.Create(FormatLog(SrcLogItem), TEncoding.UTF8);
    end;
  except
    on e: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TLoggerProNSQAppenderBase.FormatLog(
  const aLogItem: TLogItem): string;
begin
  result := Format(FLogFormat, [datetimetostr(aLogItem.TimeStamp, FFormatSettings), aLogItem.ThreadID,
      aLogItem.LogTypeAsString, aLogItem.LogMessage, aLogItem.LogTag])
end;

function TLoggerProNSQAppenderBase.GetNSQUrl: string;
begin
  result := FNSQUrl;
end;

function TLoggerProNSQAppenderBase.GetTopic: string;
begin
  result := FTopic;
end;

procedure TLoggerProNSQAppenderBase.SetEphemeral(const Value: Boolean);
begin
  FEphemeral := Value;
end;

procedure TLoggerProNSQAppenderBase.SetNSQUrl(const Value: string);
begin
  FNSQUrl := value;
end;

procedure TLoggerProNSQAppenderBase.SetOnCreateData(const Value: TOnCreateData);
begin
  FOnCreateData := Value;
end;

procedure TLoggerProNSQAppenderBase.SetOnNetSendError(
  const Value: TOnNetSendError);
begin
  FOnNetSendError := Value;
end;

procedure TLoggerProNSQAppenderBase.SetTopic(const Value: string);
begin
  FTopic := value;
end;

procedure TLoggerProNSQAppenderBase.Setup;
begin
  FFormatSettings := LoggerPro.GetDefaultFormatSettings;
  inherited;
end;

procedure TLoggerProNSQAppenderBase.TearDown;
begin
  inherited;
end;

procedure TLoggerProNSQAppenderBase.WriteLog(const aLogItem: TLogItem);
var
  FHTTPCli: THTTPClient;
  URI: string;
  Data: TStream;
  TopicName: string;
  FRetryCount: Integer;
begin
  FRetryCount := 0;
  FHTTPCli := THTTPClient.Create;
  try
    if Topic.trim.IsEmpty then
      TopicName := aLogItem.LogTag.Trim
    else
      TopicName := Topic.Trim;
    URI :=NSQUrl + '/pub?topic=' + TNetEncoding.URL.Encode(TopicName);
    if Ephemeral then
      URI := URI + '#ephemeral';
    Data := CreateData(aLogItem);
    if Assigned(Data) then
    begin
      repeat
        try
          // Set very short timeouts: this is a local call and we don't want to block the queue for too long.
{$IF CompilerVersion >= 31}
          FHTTPCli.ConnectionTimeout := 100;
          FHTTPCli.ResponseTimeout := 200;
{$ENDIF}
          Data.Seek(0, soFromBeginning);
          // ignore the respnse: as long as NSQD has accepted the POST, it will handle the result
          FHTTPCli.Post(URI, Data);
          break;
        except
          on e: ENetHTTPClientException do
          begin
            // if there is an event handler for net exception, call it
            if Assigned(FOnNetSendError) then
              OnNetSendError(self, aLogItem, e, FRetryCount);
            // if the handler has set FRetryCount to a positive value then retry the call
            if FRetryCount <= 0 then
              break;
          end;
        end;
      until false;
    end;
  finally
    FreeAndNil(FHTTPCli);
  end;
end;

end.
