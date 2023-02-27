unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.DMSEventStreamsAppender, LoggerPro.RedisAppender, Redis.Client, WinApi.Windows,
  EventStreamsRPCProxy, System.Net.URLClient;

const
  DMSCONTAINER_API_KEY =
    'eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJETVNDb250YWluZXIiLCJleHAiOj'+
    'Q4MTMxMTgzNTQsIm5iZiI6MTY1OTUxODA1NCwiaWF0IjoxNjU5NTE4MDU0LCJpc2FweWtleSI6Ij'+
    'EiLCJ1c2VyaWQiOiIxODAiLCJyb2xlcyI6ImV2ZW50X3dyaXRlcixldmVudF9yZWFkZXIiLCJ1c2V'+
    'ybmFtZSI6ImxvZ3Njb2xsZWN0b3IiLCJjb250ZXh0cyI6IiJ9.zCgh27qDgGPdpqg51KBlMk9Bnfh'+
    'FlkUWSmjaKANYsoMJxsR51xnnJtoNjNefAWwA37EnOtf69IZA5wjDml_deg';

var
  _Log: ILogWriter;
  _Events: TLoggerProEventsHandler;
  DMSProxy: TEventStreamsRPCProxy;

type
  TAllowSelfSignedCertificates = class
  public
    class procedure OnValidateCertificate(const Sender: TObject; const ARequest: TURLRequest;
      const Certificate: TCertificate; var Accepted: Boolean);
  end;

function Log: ILogWriter;
begin
  Result := _Log;
end;

{ TAllowSelfSignedCertificates }

class procedure TAllowSelfSignedCertificates.OnValidateCertificate(
  const Sender: TObject; const ARequest: TURLRequest;
  const Certificate: TCertificate; var Accepted: Boolean);
begin
  Accepted := True; // just for demo! Check your certificates in production!
end;

initialization

_Events := TLoggerProEventsHandler.Create;
_Events.OnAppenderError :=
    procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem; const Reason: TLogErrorReason;
    var Action: TLogErrorAction)
  begin
    Action := TLogErrorAction.SkipNewest;
    {$IF Defined(MSWINDOWS)}
    WinApi.Windows.Beep(800, 500);
    {$ENDIF}
  end;

DefaultLoggerProAppenderQueueSize := 10;

//DMSProxy := TEventStreamsRPCProxy.Create('https://localhost/eventstreamsrpc');
DMSProxy := TEventStreamsRPCProxy.Create('http://dms.marr.it:8080/eventstreamsrpc');
DMSProxy.RPCExecutor.SetOnValidateServerCertificate(TAllowSelfSignedCertificates.OnValidateCertificate);

_Log := BuildLogWriter([TLoggerProDMSContainerAppender.Create(DMSProxy, DMSCONTAINER_API_KEY,
  'logs.' + TLoggerProDMSContainerAppender.GetModuleBaseName,
     dmsatByTag         {es logs.dmscontaineresappendersample.tag1}
  // dmsatByType        {es logs.dmscontaineresappendersample.debug}
  // dmsatByTagThenType {es logs.dmscontaineresappendersample.tag1.debug}
  // dmsatByTypeThenTag {es logs.dmscontaineresappendersample.debug.tag1}
  )], _Events);

finalization

_Log := nil;
_Events.Free;

end.
