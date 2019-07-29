unit MainFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    btnParse: TButton;
    Panel2: TPanel;
    mmSQL: TMemo;
    Splitter1: TSplitter;
    lbRQL: TListBox;
    Panel3: TPanel;
    edtExpression: TEdit;
    btnAdd: TButton;
    rgBackend: TRadioGroup;
    Memo1: TMemo;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbRQLClick(Sender: TObject);
  private
    procedure SaveHistory;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils,
  MVCFramework.RQL.Parser,
  System.TypInfo,
  MVCFramework.RQL.AST2FirebirdSQL,
  MVCFramework.RQL.AST2InterbaseSQL,
  MVCFramework.RQL.AST2SQLite,
  MVCFramework.RQL.AST2PostgreSQL,
  MVCFramework.RQL.AST2MSSQL,
  MVCFramework.RQL.AST2MySQL;

{$R *.dfm}


procedure TMainForm.FormCreate(Sender: TObject);
var
  lComp: string;
begin
  if TFile.Exists('rqlhistory.txt') then
  begin
    lbRQL.Items.LoadFromFile('rqlhistory.txt');
  end;

  rgBackend.Items.Clear;
  for lComp in TRQLCompilerRegistry.Instance.RegisteredCompilers do
  begin
    rgBackend.Items.AddObject(lComp, TRQLCompilerRegistry.Instance.GetCompiler(lComp).Create(nil));
  end;
  rgBackend.ItemIndex := 0;
end;

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  if not Trim(edtExpression.Text).IsEmpty then
  begin
    lbRQL.Items.Insert(0, edtExpression.Text);
    SaveHistory;
  end;
end;

procedure TMainForm.btnParseClick(Sender: TObject);
var
  lParser: TRQL2SQL;
  lSQL: string;
begin
  lParser := TRQL2SQL.Create;
  try
    try
      lParser.Execute(edtExpression.Text, lSQL, TRQLCompiler(rgBackend.Items.Objects[rgBackend.ItemIndex]));
      mmSQL.Lines.Text := lSQL;
    except
      on E: Exception do
      begin
        Memo1.Lines.Text := E.Message;
      end;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  SaveHistory;
  for i := 0 to rgBackend.Items.count - 1 do
  begin
    rgBackend.Items.Objects[i].Free;
  end;
end;

procedure TMainForm.lbRQLClick(Sender: TObject);
begin
  edtExpression.Text := lbRQL.Items[lbRQL.ItemIndex];
end;

procedure TMainForm.SaveHistory;
begin
  lbRQL.Items.SaveToFile('rqlhistory.txt');
end;

end.
