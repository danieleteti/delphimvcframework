unit ServiceU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Vcl.Dialogs,
  MVCFramework.HTTPSys.WebBrokerBridge;

type
  TArticlesServiceHTTPSys = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    fServer: TMVCHTTPSysWebBrokerBridge;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  ArticlesServiceHTTPSys: TArticlesServiceHTTPSys;

implementation

uses
  Web.WebReq,
  WebModuleUnit1, MVCFramework.Commons, FireDAC.Comp.Client, System.IOUtils;

{$R *.DFM}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ArticlesServiceHTTPSys.Controller(CtrlCode);
end;

function TArticlesServiceHTTPSys.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TArticlesServiceHTTPSys.ServiceCreate(Sender: TObject);
begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
end;

procedure TArticlesServiceHTTPSys.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    ServiceThread.ProcessRequests(True);
    Sleep(1000);
  end;
end;

procedure TArticlesServiceHTTPSys.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FDManager.ConnectionDefFileName := TPath.Combine(AppPath, 'FDConnectionDefs.ini');
  FDManager.LoadConnectionDefFile;
  fServer := TMVCHTTPSysWebBrokerBridge.Create;
  fServer.Port := 8080;
  fServer.Active := True;
end;

procedure TArticlesServiceHTTPSys.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  fServer.Free;
end;

end.
