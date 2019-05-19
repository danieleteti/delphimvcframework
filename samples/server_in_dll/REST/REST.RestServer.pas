unit REST.RestServer;

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge;

type
  TDMVCRestServer = class
  private
    fServer: TIdHTTPWebBrokerBridge;
  public
    constructor Create(const aPort: integer); overload;
    procedure Activate;
    destructor Destroy; override;
  end;

implementation

uses
  REST.WebModule;

procedure TDMVCRestServer.Activate;
begin
  fServer.Active := true;

  if fServer.Active then
  begin
    LogD('Server active on port=' + IntToStr(fServer.DefaultPort));
  end;
end;

constructor TDMVCRestServer.Create(const aPort: integer);
begin
  inherited Create;
  fServer := TIdHTTPWebBrokerBridge.Create();
  if WebRequestHandler <> nil then
  begin
    WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
  end;
  fServer.DefaultPort := aPort;
end;

destructor TDMVCRestServer.Destroy;
begin
  EnterInShutdownState;
  try
    fServer.Active := false;
    FreeAndNil(fServer);
    LogD('Server not active');
  except
    on E: Exception do
    begin
      LogE('[TDMVCRestServer.Destroy] ' + E.Message);
    end;
  end;
  ReleaseGlobalLogger;
  inherited Destroy;
end;

end.
