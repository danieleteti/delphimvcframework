unit wmSOAPRESTU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, Soap.InvokeRegistry,
  Soap.WSDLIntf, System.TypInfo, Soap.WebServExp, Soap.WSDLBind, Xml.XMLSchema,
  Soap.WSDLPub, Soap.SOAPPasInv, Soap.SOAPHTTPPasInv, Soap.SOAPHTTPDisp,
  Soap.WebBrokerSOAP, MVCFramework, MVCFramework.Commons;

type
  TwmSOAPREST = class(TWebModule)
    HTTPSoapDispatcher: THTTPSoapDispatcher;
    HTTPSoapPascalInvoker: THTTPSoapPascalInvoker;
    WSDLHTMLPublish: TWSDLHTMLPublish;
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
  MVCFramework.Middleware.StaticFiles,
  RESTControllerCustomerU;

procedure TwmSOAPREST.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.AllowUnhandledAction] := 'true';
    end);
  FMVCEngine.AddController(TControllerCustomer);
  FMVCEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    'www', { DocumentRoot }
    'index.html' { IndexDocument - Before it was named fallbackresource },
    False
    ));
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
