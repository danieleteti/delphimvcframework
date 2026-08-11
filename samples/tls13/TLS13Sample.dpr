program TLS13Sample;

// HTTPS on Indy Direct + TaurusTLS (OpenSSL 1.1.1+ / 3.x), pinned to TLS 1.3.
//
//   TLS13Sample.exe            serve https://localhost:8443 until Ctrl+C
//   TLS13Sample.exe selftest   connect to itself, print the negotiated
//                              protocol and cipher, exit 1 if it is not TLS 1.3
//
// TLS belongs to IMVCServer: UseHTTPS + CertFile/KeyFile + the HTTPSConfigurator
// that installs the TLS stack. The caller never touches the IOHandler of the
// Indy component directly.
//
// See README.md for certificates and the OpenSSL DLLs.

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  IdHTTP,
  TaurusTLS,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI,
  MVCFramework.Server.Intf,
  MVCFramework.Server.Indy,
  MVCFramework.Server.Factory,
  MVCFramework.Server.HTTPS.TaurusTLS;

const
  PORT = 8443; // not 443 on purpose: Indy negotiates TLS only on 443 unless
               // told otherwise, and UseHTTPS forces it on every port

// TaurusTLSIndyConfigurator builds the IOHandler out of the certificate
// properties set on IMVCServer. Wrapping it is how you reach the TaurusTLS
// knobs it does not expose - here the protocol floor, which defaults to 1.2.
// A TLS 1.2 client is refused outright; there is no downgrade.
function TLS13OnlyConfigurator: TMVCHTTPSConfigurator;
var
  lBuildIOHandler: TMVCHTTPSConfigurator;
begin
  lBuildIOHandler := TaurusTLSIndyConfigurator();
  Result :=
    procedure(AServer: IMVCServer)
    begin
      lBuildIOHandler(AServer);
      ((AServer as TMVCIndyServer).HTTPServer.IOHandler as TTaurusTLSServerIOHandler)
        .SSLOptions.MinTLSVersion := TLSv1_3;
    end;
end;

procedure ConfigureRoutes(AEngine: TMVCEngine);
begin
  AEngine.Root.AsApi.MapGet('/hello',
    function: IMVCResponse
    begin
      Result := Ok('Hello over TLS 1.3');
    end);
end;

function NewHTTPSServer(AEngine: TMVCEngine): IMVCServer;
var
  lCertDir: string;
begin
  lCertDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'certificates');
  Result := TMVCServerFactory.CreateIndyDirect(AEngine);
  Result.HTTPSConfigurator := TLS13OnlyConfigurator();
  Result.UseHTTPS := True;
  Result.CertFile := TPath.Combine(lCertDir, 'localhost.crt');
  Result.KeyFile := TPath.Combine(lCertDir, 'localhost.key');
  if not(TFile.Exists(Result.CertFile) and TFile.Exists(Result.KeyFile)) then
    raise Exception.Create('localhost.crt / localhost.key not found in ' +
      lCertDir + ' - see README.md');
end;

procedure RunServer;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
begin
  lEngine := TMVCEngine.Create;
  try
    ConfigureRoutes(lEngine);
    lServer := NewHTTPSServer(lEngine);
    WriteLn(Format('Listening on https://localhost:%d/hello - Ctrl+C to stop', [PORT]));
    lServer.RunAndWait(PORT);
    lServer := nil;
  finally
    lEngine.Free;
  end;
end;

// Serves itself over TLS and reports what was actually negotiated. Reading
// SSLSocket after Get is safe because the connection is kept alive.
function SelfTest: Boolean;
var
  lEngine: TMVCEngine;
  lServer: IMVCServer;
  lHTTP: TIdHTTP;
  lSSL: TTaurusTLSIOHandlerSocket;
begin
  lEngine := TMVCEngine.Create;
  try
    ConfigureRoutes(lEngine);
    lServer := NewHTTPSServer(lEngine);
    lServer.Listen(PORT);
    try
      lHTTP := TIdHTTP.Create(nil);
      try
        lSSL := TTaurusTLSIOHandlerSocket.Create(lHTTP);
        lSSL.SSLOptions.Mode := sslmClient;
        lSSL.SSLOptions.VerifyMode := []; // self-signed development certificate
        lHTTP.IOHandler := lSSL;
        WriteLn('response : ', lHTTP.Get(Format('https://localhost:%d/hello', [PORT])));
        WriteLn('protocol : ', lSSL.SSLSocket.SSLProtocolVersionStr);
        WriteLn('cipher   : ', lSSL.SSLSocket.Cipher.Name);
        Result := lSSL.SSLSocket.SSLProtocolVersion = TLSv1_3;
      finally
        lHTTP.Free;
      end;
    finally
      lServer.Stop;
      lServer := nil;
    end;
  finally
    lEngine.Free;
  end;
end;

begin
  IsMultiThread := True;
  try
    if (ParamCount > 0) and SameText(ParamStr(1), 'selftest') then
    begin
      if SelfTest then
        WriteLn('PASS: TLS 1.3 negotiated')
      else
      begin
        WriteLn('FAIL: TLS 1.3 not negotiated');
        ExitCode := 1;
      end;
    end
    else
      RunServer;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
