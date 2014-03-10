library isapiapp;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  WebModuleU in '..\WebModules\WebModuleU.pas' {WebModule1: TWebModule},
  BusinessObjectsU in '..\BO\BusinessObjectsU.pas',
  RoutingSampleControllerU in '..\Controllers\RoutingSampleControllerU.pas';

{$R *.res}

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
