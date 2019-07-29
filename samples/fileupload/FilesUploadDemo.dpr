program FilesUploadDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF }
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  FileUploadControllerU in 'FileUploadControllerU.pas',
  MVCFramework.View.Renderers.TemplatePro in '..\serversideviewcustom\MVCFramework.View.Renderers.TemplatePro.pas',
  TemplateProU in '..\serversideviewcustom\lib\TemplateProU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press RETURN to stop the server');

{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', 'http://localhost:3000', nil, nil, SW_SHOW);

{$ENDIF}
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
