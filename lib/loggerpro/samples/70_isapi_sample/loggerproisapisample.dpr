library loggerproisapisample;

uses
  LoggerPro.GlobalLogger,
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  Winapi.Windows,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule};

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
