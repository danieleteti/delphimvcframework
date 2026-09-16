program ValidationShowcaseServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  ValidationControllerU in 'ValidationControllerU.pas',
  ValidationModelsU in 'ValidationModelsU.pas',
  WebModuleU in 'WebModuleU.pas' {ValidationWebModule: TWebModule};

{$R *.res}

const
  SERVER_PORT = 8080;

procedure RunServer;
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  WriteLn('===============================================================================');
  WriteLn('  DMVCFramework - VALIDATION SHOWCASE SERVER');
  WriteLn('===============================================================================');
  WriteLn('');
  WriteLn('This sample demonstrates the new VALIDATION SYSTEM features:');
  WriteLn('');
  WriteLn('  - Automatic validation on [MVCFromBody] deserialization');
  WriteLn('  - Nested object validation (Address inside Customer)');
  WriteLn('  - Collection validation (Items inside Order)');
  WriteLn('  - Format validators (CreditCard, IBAN, Email, etc.)');
  WriteLn('  - OnValidate method for cross-field validation');
  WriteLn('  - HTTP 422 Unprocessable Entity response on validation failure');
  WriteLn('');
  WriteLn('===============================================================================');
  WriteLn('');

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := SERVER_PORT;
    LServer.KeepAlive := True;
    LServer.MaxConnections := 100;
    LServer.ListenQueue := 200;

    LServer.Active := True;

    WriteLn(Format('Server started on http://localhost:%d', [SERVER_PORT]));
    WriteLn('');
    WriteLn('Available endpoints:');
    WriteLn('  POST /api/users/register    - User registration (basic validators)');
    WriteLn('  POST /api/customers         - Customer creation (nested validation)');
    WriteLn('  POST /api/orders            - Order creation (collection validation)');
    WriteLn('  POST /api/payments          - Payment processing (format validators)');
    WriteLn('  POST /api/events            - Event booking (OnValidate cross-field validation)');
    WriteLn('  POST /api/validate/user     - Manual validation example');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop the server...');
    WriteLn('');

    WaitForTerminationSignal;

    WriteLn('');
    WriteLn('Shutting down...');
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
