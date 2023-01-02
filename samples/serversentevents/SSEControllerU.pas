unit SSEControllerU;


interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.SSEController;

type
  [MVCPath('/stocks')]
  TMySSEController = class(TMVCSSEController)
  protected
    function GetServerSentEvents(const LastEventID: String): TMVCSSEMessages; override;
  end;

implementation

uses
  MVCFramework.Logger, System.SysUtils, StorageU, System.DateUtils;

{ TMySSEController }

function TMySSEController.GetServerSentEvents(const LastEventID: String): TMVCSSEMessages;
var
  lCurrentEventID: Integer;
begin
  Sleep(1000);
  var lSSEMessage: TSSEMessage;
  lSSEMessage.Event := 'stockupdate';
  lSSEMessage.Data := GetNextDataToSend(StrToIntDef(LastEventID, 0), lCurrentEventID);
  lSSEMessage.Id := lCurrentEventID.ToString;
  lSSEMessage.Retry := 300;
  Result := [
    lSSEMessage
  ];
end;

end.

