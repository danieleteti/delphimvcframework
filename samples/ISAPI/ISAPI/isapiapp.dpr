library isapiapp;





uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  MainDataModuleUnit in '..\..\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule},
  MainWebModuleUnit in '..\..\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  WineCellarAppControllerU in '..\..\winecellarserver\WineCellarAppControllerU.pas',
  WinesBO in '..\..\winecellarserver\WinesBO.pas';

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
