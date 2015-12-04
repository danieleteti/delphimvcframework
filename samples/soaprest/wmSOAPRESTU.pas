unit wmSOAPRESTU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, Soap.InvokeRegistry,
  Soap.WSDLIntf, System.TypInfo, Soap.WebServExp, Soap.WSDLBind, Xml.XMLSchema,
  Soap.WSDLPub, Soap.SOAPPasInv, Soap.SOAPHTTPPasInv, Soap.SOAPHTTPDisp,
  Soap.WebBrokerSOAP, MVCFramework;

type
  TwmSOAPREST = class(TWebModule)
    HTTPSoapDispatcher: THTTPSoapDispatcher;
    HTTPSoapPascalInvoker: THTTPSoapPascalInvoker;
    WSDLHTMLPublish: TWSDLHTMLPublish;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
    procedure wmSOAPRESTSoapActionAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TwmSOAPREST;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses
  RESTControllerCustomerU;

procedure TwmSOAPREST.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // WSDLHTMLPublish1.ServiceInfo(Sender, Request, Response, Handled);
  Response.Content := '<html><heading/><body>SOAP and REST</body></html>' +
    '<p>This example uses the allow_unhandled_action option to allow standard Delphi SOAP webservice functionality.</p>'
    + '<p>Visit "<a href="/soap">/soap</a>" for WSDL</p>' +
    '<p>Visit "<a href="/customers">/customer</a>" for Customer REST</p>';
end;

procedure TwmSOAPREST.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(self);
  FMVCEngine.Config['document_root'] := 'www';
  FMVCEngine.Config['allow_unhandled_action'] := 'true';
  FMVCEngine.AddController(TControllerCustomer);
end;

procedure TwmSOAPREST.WebModuleDestroy(Sender: TObject);
begin
  if Assigned(FMVCEngine) then
    FreeAndNil(FMVCEngine);
end;

procedure TwmSOAPREST.wmSOAPRESTSoapActionAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  WSDLHTMLPublish.ServiceInfo(Sender, Request, Response, Handled);
end;

end.
