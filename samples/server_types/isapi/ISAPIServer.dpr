library ISAPIServer;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  Winapi.Windows,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas',
  WebModuleU in '..\webbroker_standalone\WebModuleU.pas' {SampleWebModule: TWebModule};

{$R *.res}

function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;
begin
  ReleaseGlobalLogger;
  Result := Web.Win.ISAPIThreadPool.TerminateExtension(dwFlags);
end;

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
