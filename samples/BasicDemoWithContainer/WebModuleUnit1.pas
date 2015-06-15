unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses App1MainControllerU;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self);
  MVC.Config['view_path'] := '..\Debug\HTML5Application';
  MVC.Config['document_root'] := 'HTML5Application\public_html';
  MVC.AddController(TApp1MainController);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  MVC.free;
end;

end.
