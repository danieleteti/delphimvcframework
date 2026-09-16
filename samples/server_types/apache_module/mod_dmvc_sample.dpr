library mod_dmvc_sample;

uses
  System.Threading,
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  Web.HTTPDMethods,
  Winapi.Windows,
  System.Classes,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas',
  WebModuleU in '..\webbroker_standalone\WebModuleU.pas' {SampleWebModule: TWebModule};

{$R *.res}

// httpd.conf entries:
//
(*
  LoadModule dmvc_sample_module modules/mod_dmvc_sample.dll

  <Location /api>
    SetHandler mod_dmvc_sample-handler
  </Location>
*)

var
  GModuleData: TApacheModuleData;

exports
  GModuleData name 'dmvc_sample_module';

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
