unit LogsCollectorControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type

  [MVCPath('/api')]
  TLogsCollectorController = class(TMVCController)
  public
    [MVCPath('/logs/($logtag)/($logtype)')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.TEXT_PLAIN)]
    procedure Logs(const logtag, logtype: string);

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

  public
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  LoggerProConfig,
  LoggerPro;

procedure TLogsCollectorController.Logs(const logtag, logtype: string);
var
  lLogType: TLogType;
begin
  lLogType := LoggerPro.StringToLogType(logtype);
  Log.Log(lLogType, Context.Request.Body, logtag);
end;

procedure TLogsCollectorController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TLogsCollectorController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
