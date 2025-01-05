program FilesUploadDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF }
  IdHTTPWebBrokerBridge,
  MVCFramework.Logger,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  FileUploadControllerU in 'FileUploadControllerU.pas', System.IOUtils;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LogI('Press RETURN to stop the server');

{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', 'http://localhost:3000', nil, nil, SW_SHOW);

{$ENDIF}
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  TDirectory.CreateDirectory(TFileUploadController.UPLOAD_FOLDER);
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(3000);
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end

end.
