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
  MVCFramework.RQL.Parser;

{$R *.dfm}


procedure TMainForm.FormCreate(Sender: TObject);
begin
  if TFile.Exists('rqlhistory.txt') then
  begin
    lbRQL.Items.LoadFromFile('rqlhistory.txt');
  end;
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
    lParser.Execute(edtExpression.Text, lSQL, []);
    mmSQL.Lines.Text := lSQL;
  finally
    lParser.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveHistory;
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
