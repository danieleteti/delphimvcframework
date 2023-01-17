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
  lSSEMessage: TSSEMessage;
begin
  Sleep(1000);
  lSSEMessage.Event := 'stockupdate';
  lSSEMessage.Data := GetNextDataToSend(StrToIntDef(LastEventID, 0), lCurrentEventID);
  lSSEMessage.Id := lCurrentEventID.ToString;
  Result := [
    lSSEMessage
  ];
end;

end.

