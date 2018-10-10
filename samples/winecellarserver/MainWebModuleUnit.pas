unit MainWebModuleUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVCEngine: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

uses
  WineCellarAppControllerU,
  MVCFramework.Commons,
  System.IOUtils;

{$R *.dfm}

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine.AddController(TWineCellarApp);
  MVCEngine.Config[TMVCConfigKey.DocumentRoot] := TPath.Combine(AppPath, '..\..\www');
  MVCEngine.Config[TMVCConfigKey.IndexDocument] := 'index.html';
  MVCEngine.Config[TMVCConfigKey.FallbackResource] := 'index.html';
end;

end.
