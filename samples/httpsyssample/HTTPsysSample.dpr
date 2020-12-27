program HTTPsysSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,  
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.HTTPSys.WebBrokerBridge,
  MyControllerU in 'MyControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TMVCHTTPSysWebBrokerBridge;
begin
  Writeln('** ' + DMVCFRAMEWORK + ' Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TMVCHTTPSysWebBrokerBridge.Create;
  try
    LServer.Port := APort;
    LServer.RootPath := 'root';
    LServer.UseSSL := False;
    LServer.UseCompression := True;
    LServer.Active := True;
    Write('Hit return to shutdown the server');
    ReadLn;
    Write('Bye bye...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  IsMultiThread := true;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 0;
    RunServer(6666);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
