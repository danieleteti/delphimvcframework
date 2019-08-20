program ServerWithControllersRegister;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MainFormU in 'MainFormU.pas' {Form1},
  WebModuleMainU in 'WebModuleMainU.pas' {WebModule1: TWebModule},
  MyController1U in 'MyController1U.pas',
  MyController2U in 'MyController2U.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
