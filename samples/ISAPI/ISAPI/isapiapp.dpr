library isapiapp;





uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  MainDataModuleUnit in '..\..\WineCellarSample\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule},
  MainWebModuleUnit in '..\..\WineCellarSample\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  WinesBO in '..\..\WineCellarSample\winecellarserver\WinesBO.pas',
  WineCellarAppControllerU in '..\..\WineCellarSample\winecellarserver\WineCellarAppControllerU.pas';

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
