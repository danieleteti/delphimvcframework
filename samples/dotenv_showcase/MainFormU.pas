unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  MVCFramework.DotEnv;

type
  TMainForm = class(TForm)
    btnSimple: TButton;
    mmVars: TMemo;
    btnTestEnv: TButton;
    btnProdEnv: TButton;
    Shape1: TShape;
    btnSingleEnv: TButton;
    procedure btnSimpleClick(Sender: TObject);
    procedure btnTestEnvClick(Sender: TObject);
    procedure btnProdEnvClick(Sender: TObject);
    procedure btnSingleEnvClick(Sender: TObject);
  private
    procedure UpdateUI(dotEnv: IMVCDotEnv);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnProdEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .WithStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('prod')
    .Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnSimpleClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv.WithStrategy(TMVCDotEnvPriority.EnvThenFile).Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnSingleEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .WithStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('prod')
    .Build('env1');
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnTestEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .WithStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('test')
    .Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.UpdateUI(dotEnv: IMVCDotEnv);
begin
  Caption := 'dotEnv ShowCase :: MODE = ' + dotEnv.Env('mode');
end;

end.
