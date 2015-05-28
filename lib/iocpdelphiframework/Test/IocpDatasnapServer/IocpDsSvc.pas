unit IocpDsSvc;

interface

uses
  Winapi.Windows, Vcl.SvcMgr;

type
  TSvcEEPMServerX = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  SvcEEPMServerX: TSvcEEPMServerX;

implementation

uses
  IocpDsServer;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SvcEEPMServerX.Controller(CtrlCode);
end;

function TSvcEEPMServerX.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSvcEEPMServerX.ServiceCreate(Sender: TObject);
begin
  DisplayName := Name;
end;

procedure TSvcEEPMServerX.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TDataSnapServer.StartServer(8080);
end;

procedure TSvcEEPMServerX.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  TDataSnapServer.StopServer;
end;

end.
