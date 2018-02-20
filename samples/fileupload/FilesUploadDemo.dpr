program FilesUploadDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,

  {$IFDEF MSWINDOWS}

  Winapi.Windows,
  Winapi.ShellAPI,

  {$ENDIF}

  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,

  {$IFNDEF LINUX}

  ReqMulti, // if compiler doesn't find this unit, update with the last update

  {$ENDIF}

  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  FileUploadControllerU in 'FileUploadControllerU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin

  {$IFDEF LINUX}

  raise Exception.Create('This DEMO doesn''t work on linux due a bug in Delphi 10.2 Tokyo');

  {$ENDIF}

  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press RETURN to stop the server');

    {$IFDEF MSWINDOWS}

    ShellExecute(0, 'open', 'http://localhost:3000/fileupload.html', nil, nil, SW_SHOW);

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
