program dmvcwindowsservice;

uses
  Vcl.SvcMgr,
  ServiceU in 'ServiceU.pas' {ArticlesService: TService},
  WebModuleUnit1 in '..\articles_crud_server\WebModuleUnit1.pas' {WebModule1: TWebModule},
  MainDM in '..\articles_crud_server\MainDM.pas' {dmMain: TDataModule};

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TArticlesService, ArticlesService);
  Application.Run;
end.
