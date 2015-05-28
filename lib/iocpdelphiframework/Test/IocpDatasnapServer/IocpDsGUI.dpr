program IocpDsGUI;

{$R *.dres}

uses
  Vcl.Forms,
  Web.WebReq,
  IocpDsServer in 'IocpDsServer.pas',
  IocpDsForm in 'IocpDsForm.pas' {fmEsGUI},
  IocpDsServerMethods in 'IocpDsServerMethods.pas' {ServerMethodsDs: TDSServerModule},
  IocpDsWebModule in 'IocpDsWebModule.pas' {WebModuleEs: TWebModule};

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmEsGUI, fmEsGUI);
  Application.Run;
end.
