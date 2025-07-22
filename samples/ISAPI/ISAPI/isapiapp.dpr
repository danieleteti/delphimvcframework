library isapiapp;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  Winapi.Windows,
  WinesBO in '..\..\wine_cellar_sample\winecellarserver\WinesBO.pas',
  WineCellarAppControllerU in '..\..\wine_cellar_sample\winecellarserver\WineCellarAppControllerU.pas',
  MainWebModuleUnit in '..\..\wine_cellar_sample\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  MainDataModuleUnit in '..\..\wine_cellar_sample\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule};

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
