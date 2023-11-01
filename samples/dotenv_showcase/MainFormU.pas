unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  MVCFramework.DotEnv, Vcl.ComCtrls, System.IOUtils;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btnSingleEnv: TButton;
    btnRequireKeys: TButton;
    btnRequireKeys2: TButton;
    btnProdEnv: TButton;
    btnTestEnv: TButton;
    btnSimple: TButton;
    mmVars: TMemo;
    memDst: TMemo;
    Splitter1: TSplitter;
    memSrc: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnSkipDefaultFile: TButton;
    procedure btnSimpleClick(Sender: TObject);
    procedure btnTestEnvClick(Sender: TObject);
    procedure btnProdEnvClick(Sender: TObject);
    procedure btnSingleEnvClick(Sender: TObject);
    procedure btnRequireKeysClick(Sender: TObject);
    procedure btnRequireKeys2Click(Sender: TObject);
    procedure memSrcChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSkipDefaultFileClick(Sender: TObject);
  private
    procedure UpdatePlayGround;
    procedure UpdateUI(dotEnv: IMVCDotEnv);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnSkipDefaultFileClick(Sender: TObject);
begin
  { with this configuration, only .env.prod is loaded, because .env, which is the default, is skipped }
  var dotEnv := NewDotEnv
    .SkipDefaultEnv
    .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
    .UseProfile('prod')
    .Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnProdEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('prod')
    .Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnRequireKeys2Click(Sender: TObject);
begin
  var dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.EnvThenFile).Build();
  dotEnv.RequireKeys(['mode','dbuser','blablabla','dbhostname','unknown']);
end;

procedure TMainForm.btnRequireKeysClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.EnvThenFile).Build();
  dotEnv.RequireKeys(['mode','dbuser','dbpassword','dbhostname']);
  ShowMessage('Required Keys FOUND!');
end;

procedure TMainForm.btnSimpleClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.EnvThenFile).Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnSingleEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('prod')
    .Build('env1');
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.btnTestEnvClick(Sender: TObject);
begin
  var dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('test')
    .Build();
  mmVars.Clear;
  mmVars.Lines.AddStrings(dotEnv.ToArray);
  UpdateUI(dotEnv);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if DebugHook<>0 then
  begin
    ShowMessage('Please, run this sample without debugging!' + sLineBreak +
      'It can raise exceptions many times while using the playground');
  end;

  UpdatePlayGround;
end;

procedure TMainForm.memSrcChange(Sender: TObject);
begin
  UpdatePlayGround;
end;

procedure TMainForm.UpdatePlayGround;
begin
  var lFileName := TPath.Combine(TPath.GetHomePath, '.env.playground');
  memSrc.Lines.SaveToFile(lFileName);
  try
    var dotEnv := NewDotEnv
      .UseStrategy(TMVCDotEnvPriority.OnlyFile)
      .UseProfile('playground')
      .Build(TPath.GetHomePath);
    memDst.Clear;
    memDst.Lines.AddStrings(dotEnv.ToArray);
    memDst.Color := clWindow;
  except
    on E: Exception do
    begin
      memDst.Lines.Text := E.Message;
      memDst.Color := clRed;
    end;
  end;
end;

procedure TMainForm.UpdateUI(dotEnv: IMVCDotEnv);
begin
  Caption := 'dotEnv ShowCase :: MODE = ' + dotEnv.Env('mode');
end;

end.
