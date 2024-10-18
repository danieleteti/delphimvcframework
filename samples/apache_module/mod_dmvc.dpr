library mod_dmvc;

uses
  System.Threading,
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  Web.HTTPDMethods,
  Winapi.Windows,
  System.Classes,
  MainDataModuleUnit in '..\wine_cellar_sample\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule},
  MainWebModuleUnit in '..\wine_cellar_sample\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  WineCellarAppControllerU in '..\wine_cellar_sample\winecellarserver\WineCellarAppControllerU.pas',
  WinesBO in '..\wine_cellar_sample\winecellarserver\WinesBO.pas';

{$R *.res}
// httpd.conf entries:
//
(*
  LoadModule dmvc_module modules/mod_dmvc.dll

  <Location /xyz>
  SetHandler mod_dmvc-handler
  </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
// 1. The TApacheModuleData variable name is changed
// 2. The project is renamed.
// 3. The output directory is not the apache/modules directory
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;

exports
  GModuleData name 'dmvc_module';

{
  Navigate to http://localhost/winecellar/
}

begin
  CoInitFlags := COINIT_MULTITHREADED;
  dotEnvConfigure(
    function : IMVCDotEnv
    begin
      Result := NewDotEnv
                  .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                  .UseLogger(procedure(LogItem: String)
                             begin
                               LogW('dotEnv: ' + LogItem);
                             end)
                  .Build();           //uses the executable folder to look for .env* files
    end);
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;

end.
