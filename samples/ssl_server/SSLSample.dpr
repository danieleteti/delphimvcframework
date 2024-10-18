program SSLSample;
{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  IdSSLOpenSSL,
  System.IOUtils,
  Web.WebReq,
  Web.HTTPApp,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  MyControllerU in 'MyControllerU.pas',
  MyObjectsU in 'MyObjectsU.pas', MVCFramework.Commons;

{$R *.res}


type
  TSSLEventHandlers = class
    procedure OnGetSSLPassword(var APassword: {$IF CompilerVersion < 27}AnsiString{$ELSE}string{$ENDIF});
    procedure OnQuerySSLPort(APort: Word; var VUseSSL: boolean);
  end;

procedure TSSLEventHandlers.OnGetSSLPassword(var APassword: {$IF CompilerVersion < 27}AnsiString{$ELSE}string{$ENDIF});
begin
  APassword := '';
end;

procedure TSSLEventHandlers.OnQuerySSLPort(APort: Word; var VUseSSL: boolean);
begin
  VUseSSL := true;
end;

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LGetSSLPassword: TSSLEventHandlers;
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
begin
  Writeln(Format('Starting DMVCFramework HTTPS Server or port %d', [APort]));
  LGetSSLPassword := nil;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LGetSSLPassword := TSSLEventHandlers.Create;
    LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(LServer);
    LIOHandleSSL.SSLOptions.SSLVersions := [
      TIdSSLVersion.sslvSSLv23,
      TIdSSLVersion.sslvSSLv3,
      TIdSSLVersion.sslvTLSv1,
      TIdSSLVersion.sslvTLSv1_1,
      TIdSSLVersion.sslvTLSv1_2
      ];
    LIOHandleSSL.SSLOptions.Mode := sslmServer;
    LIOHandleSSL.SSLOptions.CertFile := 'cacert.pem';
    LIOHandleSSL.SSLOptions.RootCertFile := '';
    LIOHandleSSL.SSLOptions.KeyFile := 'privkey.pem';
    LIOHandleSSL.OnGetPassword := LGetSSLPassword.OnGetSSLPassword;
    LServer.IOHandler := LIOHandleSSL;
    LServer.DefaultPort := APort;
{$IF CompilerVersion >= 33}
    LServer.OnQuerySSLPort := LGetSSLPassword.OnQuerySSLPort;
{$ENDIF}
    LServer.Active := true;
    Writeln('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
    LGetSSLPassword.Free;
  end;
end;

const
  OPENSSL_LIBS: array of string = ['libeay32.dll', 'ssleay32.dll'];

procedure CheckOPENSSLLibs;
var
  lOpenSSLLib: string;
begin
  // Just a check for
  for lOpenSSLLib in OPENSSL_LIBS do
  begin
    write('Checking ', lOpenSSLLib, '...');
    if not TFile.Exists(lOpenSSLLib) then
      raise Exception.CreateFmt('Required OPENSSL library not found in the exe folder: %s' + sLineBreak +
        'Download INDY compatible OpenSSL Libraries from http://indy.fulgan.com/SSL/', [lOpenSSLLib]);
    Writeln('OK');
  end;
end;

begin
  CheckOPENSSLLibs;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(4433 { standard https port } );
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
