unit MainWebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.DotEnv
  ;

type
  Twm = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

uses
  WineCellarAppControllerU,
  MVCFramework.Middleware.StaticFiles,
  System.IOUtils;

{$R *.dfm}

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Conf: TMVCConfig)
    begin
//      Conf.Value[tmvcconfigkey.PathPrefix] := '/dmvc';
    end);
  MVCEngine.AddController(TWineCellarApp);
  if not IsLibrary then
  begin
    MVCEngine.AddMiddleware(TMVCStaticFilesMiddleware.Create('/app', { StaticFilesPath }
      TPath.Combine(AppPath, '..\..\www'), { DocumentRoot }
      'index.html' { IndexDocument - Before it was named fallbackresource }
      ));
  end;
end;

end.
