program SSLSample;
{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  IdSSLOpenSSL,
  Web.WebReq,
  Web.HTTPApp,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  MyControllerU in 'MyControllerU.pas',
  MyObjectsU in 'MyObjectsU.pas';

{$R *.res}


type
  TGetSSLPassword = class
    procedure OnGetSSLPassword(var APassword: String);
  end;

procedure TGetSSLPassword.OnGetSSLPassword(var APassword: String);
begin
  APassword := '';
end;

procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
  LGetSSLPassword: TGetSSLPassword;
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  Writeln(Format('Starting DMVCFramework HTTPS Server or port %d', [APort]));
  LGetSSLPassword := nil;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LGetSSLPassword := TGetSSLPassword.Create;
    LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(LServer);
    LIOHandleSSL.SSLOptions.CertFile := '..\..\cacert.pem';
    LIOHandleSSL.SSLOptions.RootCertFile := '';
    LIOHandleSSL.SSLOptions.KeyFile := '..\..\privkey.pem';
    LIOHandleSSL.OnGetPassword := LGetSSLPassword.OnGetSSLPassword;
    LServer.IOHandler := LIOHandleSSL;
    LServer.DefaultPort := APort;

    LServer.Active := True;
    Writeln('Press ESC to stop the server');

    LHandle := GetStdHandle(STD_INPUT_HANDLE);
    while True do
    begin
      Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
      if (LInputRecord.EventType = KEY_EVENT) and
        LInputRecord.Event.KeyEvent.bKeyDown and
        (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
        break;
    end;
  finally
    LServer.Free;
    LGetSSLPassword.Free;
  end;
end;

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
