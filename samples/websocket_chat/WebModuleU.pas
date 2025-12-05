unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles;

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self);

  // Serve static HTML chat client from www folder
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/chat', { URL path }
    '.\www',  { Physical path }
    'index.html' { Default document }
  ));

  // Redirect root to /chat
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/',
    '.\www',
    'index.html'
  ));
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
