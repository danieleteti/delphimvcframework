unit SSEControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TSSEController = class(TMVCController)
  public
    [MVCPath('/stocks')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('text/event-stream')]
    procedure Index;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionName: string); override;
  end;

implementation

uses
  MVCFramework.Logger, System.SysUtils, StorageU;

procedure TSSEController.Index;
var
  lLastEventID: Integer;
  lCurrentEventID: Integer;
  lMessage: string;
begin
  // wait a little bit
  Sleep(1000 + Random(2000));

  // retrieve the last id received by the client reading the request header.
  lLastEventID := StrToIntDef(Context.Request.Headers['Last-Event-ID'], 0);

  // get the next message to send based on the last id already received by the client
  lMessage := GetNextDataToSend(lLastEventID, lCurrentEventID);

  // setting up the correct SSE headers
  ContentType := 'text/event-stream';
  Context.Response.SetCustomHeader('Cache-Control', 'no-cache');
  Context.Response.SetCustomHeader('Connection', 'keep-alive');

  // render the response using SSE compliant data format

  // current event id (the client will resend this number at the next request)
  ResponseStream.Append('id: ' + IntToStr(lCurrentEventID) + #13);

  // The browser attempts to reconnect to the source roughly 3 seconds after
  // each connection is closed. You can change that timeout by including a line
  // beginning with "retry:", followed by the number of milliseconds to wait
  // before trying to reconnect.
  ResponseStream.Append('retry: 100'#13);


  ResponseStream.Append('event: stockupdate'#13);

  // actual message
  ResponseStream.Append('data: ' + lMessage + #13#13);

  // render all the stuff
  RenderResponseStream;
end;

procedure TSSEController.OnAfterAction(Context: TWebContext;
  const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TSSEController.OnBeforeAction(Context: TWebContext;
  const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
