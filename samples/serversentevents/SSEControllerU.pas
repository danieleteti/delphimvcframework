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
  lLastEventID := StrToIntDef(Context.Request.Headers[TMVCConstants.SSE_LAST_EVENT_ID], 0);

  // get the next message to send based on the last id already received by the client
  lMessage := GetNextDataToSend(lLastEventID, lCurrentEventID);

  RenderSSE(lCurrentEventID.ToString, lMessage, 'stockupdate');
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
