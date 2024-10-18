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
  MainDataModuleUnit in '..\..\WineCellarSample\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule},
  MainWebModuleUnit in '..\..\WineCellarSample\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  WinesBO in '..\..\WineCellarSample\winecellarserver\WinesBO.pas',
  WineCellarAppControllerU in '..\..\WineCellarSample\winecellarserver\WineCellarAppControllerU.pas',
  Winapi.Windows;

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
  dotEnvConfigure(
    function: IMVCDotEnv
    begin
      Result := NewDotEnv
               .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                                   //if available, by default, loads default environment (.env)
               .UseProfile('test') //if available loads the test environment (.env.test)
               .UseProfile('prod') //if available loads the prod environment (.env.prod)
               .UseLogger(procedure(LogItem: String)
                          begin
                            LogW('dotEnv: ' + LogItem);
                          end)
               .Build();             //uses the executable folder to look for .env* files
    end);
  Application.Run;
end.
