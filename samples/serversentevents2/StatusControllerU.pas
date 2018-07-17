unit StatusControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, SysConstantsU;

type

  [MVCPath(BASEURL + '/notifications')]
  TStatusController = class(TMVCController)
  protected

  public
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure ChangeStatus;

    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetLastStatus;

    [MVCPath('/messages')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCurrentStatus;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, JsonDataObjects,
  EntitiesU, StatusesServiceU, System.Diagnostics,
  MVCFramework.Serializer.Defaults;

procedure TStatusController.ChangeStatus;
var
  lChangeStatus: TNotificationEntity;
  lStatusService: TStatusService;
begin
  lChangeStatus := Context.Request.BodyAs<TNotificationEntity>;
  lStatusService := TStatusService.Create;
  try
    lStatusService.PersistStatus(lChangeStatus);
  finally
    lStatusService.free;
  end;
  Render(201, 'Status Changed OK');
end;

procedure TStatusController.GetCurrentStatus;
var
  lLastEventID: Integer;
  lMessage: string;
  lStatusService: TStatusService;
  lCurrentStatus: TCurrentStatusEntity;
  lCurrStatusID: Integer;
  lStopWatch: TStopwatch;
  lLastPersistedStatus: TFullStatusEntity;
begin
  // retrieve the last id received by the client reading the request header.
  lLastEventID := StrToIntDef(Context.Request.Headers['Last-Event-ID'], 0);
  lStopWatch := TStopwatch.StartNew;
  lStatusService := TStatusService.Create;
  try
    lCurrentStatus := lStatusService.GetCurrentStatus;
    while true do
    begin
      lCurrStatusID := lCurrentStatus.GetStatus;
      if (lCurrStatusID <> lLastEventID) or (IsShuttingDown) then
      begin
        Break;
      end
      else
      begin
        if lStopWatch.Elapsed.Seconds >= 10 then
        begin
          // lCurrStatusID := -1;
          // just to check if the client is alive, the response is written to the socket
          Break;
        end
        else
        begin
          Sleep(500);
        end;
      end;
    end;

    if IsShuttingDown then
    begin
      ContentType := TMVCMediaType.APPLICATION_JSON;
      Render(HTTP_STATUS.Gone, 'Server is shutting down');
      Exit;
    end;

    if lCurrStatusID = -1 then
    begin
      lMessage := '{"id":-1}';
    end
    else
    begin
      lLastPersistedStatus := lStatusService.GetLastPersistedStatus;
      try
        lMessage := GetDefaultSerializer.SerializeObject(lLastPersistedStatus);
      finally
        lLastPersistedStatus.Free;
      end;
    end;
  finally
    lStatusService.Free;
  end;
  RenderSSE(lCurrStatusID.ToString, lMessage, 'statusupdate');
end;

procedure TStatusController.GetLastStatus;
var
  lStatusService: TStatusService;
  lFullStatus: TFullStatusEntity;
begin
  lStatusService := TStatusService.Create;
  try
    lFullStatus := lStatusService.GetLastPersistedStatus;
  finally
    lStatusService.free;
  end;
  Render(lFullStatus, True);
end;

end.
