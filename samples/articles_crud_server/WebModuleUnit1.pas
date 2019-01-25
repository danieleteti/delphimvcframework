unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, mvcframework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

uses Controllers.Articles, MVCFramework.Middleware.CORS, MVCFramework.Middleware.Compression;

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(self);
  FEngine.AddController(TArticlesController);
  FEngine.AddMiddleware(TCORSMiddleware.Create);
  FEngine.AddMiddleware(TMVCCompressionMiddleware.Create(256));

end;

end.
