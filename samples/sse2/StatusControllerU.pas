unit StatusControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.SSEController,
  MVCFramework.SSE, SysConstantsU;

type
  /// <summary>
  /// REST controller for writing notifications (POST).
  /// </summary>
  [MVCPath(BASEURL + '/notifications')]
  TStatusController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure ChangeStatus;

    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetLastStatus;
  end;

  /// <summary>
  /// SSE controller that checks the database for new notifications
  /// using the OnInterval callback. This replaces the old long-polling
  /// approach with a proper SSE stream.
  /// </summary>
  [MVCPath(BASEURL + '/notifications/stream')]
  TStatusSSEController = class(TMVCSSEController)
  protected
    procedure OnClientConnected(const AConnection: TSSEConnection); override;
    procedure OnClientDisconnected(const AConnection: TSSEConnection); override;
    /// <summary>
    /// Called every Interval ms. Checks TCurrentStatusEntity for changes
    /// and sends an SSE event when the status ID changes.
    /// Demonstrates adaptive polling: faster when changes are detected,
    /// slower when idle.
    /// </summary>
    procedure OnInterval(const AConnection: TSSEConnection;
      var ANextIntervalMS: Integer); override;
    function Interval: Integer; override;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger,
  EntitiesU, StatusesServiceU,
  MVCFramework.Serializer.Defaults;

{ TStatusController }

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
    lStatusService.Free;
  end;
  Render(201, 'Status Changed OK');
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
    lStatusService.Free;
  end;
  Render(lFullStatus, True);
end;

{ TStatusSSEController }

procedure TStatusSSEController.OnClientConnected(const AConnection: TSSEConnection);
var
  lLastEventId: Integer;
  lStatusService: TStatusService;
  lCurrentStatus: TCurrentStatusEntity;
begin
  LogI('SSE client connected: %s', [AConnection.ClientId], 'sse');

  // Initialize with the current status ID so OnInterval can detect changes.
  // If the client sends Last-Event-ID, use that; otherwise read from DB.
  lLastEventId := StrToIntDef(AConnection.LastEventId, -1);
  if lLastEventId < 0 then
  begin
    lStatusService := TStatusService.Create;
    try
      lCurrentStatus := lStatusService.GetCurrentStatus;
      lLastEventId := lCurrentStatus.GetStatus;
    finally
      lStatusService.Free;
    end;
  end;
  // Store last known ID as CustomData (pointer-sized integer)
  AConnection.CustomData := TObject(NativeInt(lLastEventId));
end;

procedure TStatusSSEController.OnClientDisconnected(const AConnection: TSSEConnection);
begin
  LogI('SSE client disconnected: %s', [AConnection.ClientId], 'sse');
end;

function TStatusSSEController.Interval: Integer;
begin
  // Check for changes every 500ms
  Result := 500;
end;

procedure TStatusSSEController.OnInterval(const AConnection: TSSEConnection;
  var ANextIntervalMS: Integer);
var
  lLastKnownId: NativeInt;
  lCurrentId: Integer;
  lStatusService: TStatusService;
  lCurrentStatus: TCurrentStatusEntity;
  lPersistedStatus: TFullStatusEntity;
  lMessage: string;
begin
  lLastKnownId := NativeInt(AConnection.CustomData);

  lStatusService := TStatusService.Create;
  try
    lCurrentStatus := lStatusService.GetCurrentStatus;
    lCurrentId := lCurrentStatus.GetStatus;

    if lCurrentId <> lLastKnownId then
    begin
      // Something changed! Send the new status
      lPersistedStatus := lStatusService.GetLastPersistedStatus;
      try
        lMessage := GetDefaultSerializer.SerializeObject(lPersistedStatus);
      finally
        lPersistedStatus.Free;
      end;

      AConnection.Send(
        TSSEMessage.Create('statusupdate', lMessage, lCurrentId.ToString));

      // Update last known ID
      AConnection.CustomData := TObject(NativeInt(lCurrentId));

      // Change detected: check again quickly in case of burst updates
      ANextIntervalMS := 200;
    end
    else
    begin
      // No change: back off to save resources
      ANextIntervalMS := 1000;
    end;
  finally
    lStatusService.Free;
  end;
end;

end.
