program SOAPREST;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  frmServerU in 'frmServerU.pas' {Form1},
  wmSOAPRESTU in 'wmSOAPRESTU.pas' {wmSOAPREST: TWebModule},
  BOCustomersU in 'businessobjects\BOCustomersU.pas',
  WSHelperCustomersU in 'webservices\helpers\WSHelperCustomersU.pas',
  RESTControllerCustomerU in 'webservices\rest\RESTControllerCustomerU.pas',
  SOAPCustomerImplU in 'webservices\soap\SOAPCustomerImplU.pas',
  SOAPCustomerIntfU in 'webservices\soap\SOAPCustomerIntfU.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
