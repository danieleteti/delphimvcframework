unit WebModule01U;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomWebModuleU, MVCFramework,
  App1MainControllerU;

type
  TWebModule01 = class(TCustomWebModule)
  protected
    procedure DoConfigureEngine(const aMVCEngine: TMVCEngine); override;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}
{ TWebModule2 }

procedure TWebModule01.DoConfigureEngine(const aMVCEngine: TMVCEngine);
begin
  inherited;
  aMVCEngine.AddController(TApp1MainController);
end;

end.
