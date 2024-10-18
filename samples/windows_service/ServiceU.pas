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
  IdHTTPWebBrokerBridge;

type
  TArticlesService = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    fServer: TIdHTTPWebBrokerBridge;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  ArticlesService: TArticlesService;

implementation

uses
  Web.WebReq,
  WebModuleUnit1, MVCFramework.Commons;

{$R *.DFM}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ArticlesService.Controller(CtrlCode);
end;

function TArticlesService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TArticlesService.ServiceCreate(Sender: TObject);
begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
end;

procedure TArticlesService.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    ServiceThread.ProcessRequests(True);
    Sleep(1000);
  end;
end;

procedure TArticlesService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  fServer := TIdHTTPWebBrokerBridge.Create(nil);
  fServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
  fServer.DefaultPort := 8080;
  fServer.Active := True;
end;

procedure TArticlesService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  fServer.Free;
end;

end.
