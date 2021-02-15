unit WebModule03U;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomWebModuleU, MVCFramework;

type
  TWebModule03 = class(TCustomWebModule)
  protected
    procedure DoConfigureEngine(const aMVCEngine: TMVCEngine); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses App1MainControllerU;
{ TWebModule03 }

procedure TWebModule03.DoConfigureEngine(const aMVCEngine: TMVCEngine);
begin
  inherited;
  aMVCEngine.AddController(TApp1MainController);
end;

end.
