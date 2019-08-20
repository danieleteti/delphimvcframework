unit REST.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.Compression;

type
  TMainWebModule = class(TWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMainWebModule;

implementation

uses
  System.IOUtils,
  MVCFramework.Commons,
  REST.MainController,
  MVCFramework.Middleware.Authentication;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}


procedure TMainWebModule.DataModuleCreate(Sender: TObject);
begin
  fEngine := TMVCEngine.Create(self).AddController(TMainController);
end;

procedure TMainWebModule.DataModuleDestroy(Sender: TObject);
begin
  fEngine.Free;
end;

end.
