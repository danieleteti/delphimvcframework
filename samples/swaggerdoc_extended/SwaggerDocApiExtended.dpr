program SwaggerDocApiExtended;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MainFormU in 'MainFormU.pas' {MainForm},
  WebModuleMainU in 'WebModuleMainU.pas' {WebModule1: TWebModule},
  PeopleControllerU in 'PeopleControllerU.pas',
  MVCFramework.Middleware.Swagger in '..\..\sources\MVCFramework.Middleware.Swagger.pas',
  MVCFramework.Swagger.Commons in '..\..\sources\MVCFramework.Swagger.Commons.pas',
  AuthHandler in 'AuthHandler.pas',
  BaseControllerU in 'BaseControllerU.pas',
  EntitiesU in 'EntitiesU.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
