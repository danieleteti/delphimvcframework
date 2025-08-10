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
    BtnParse: TButton;
    Panel2: TPanel;
    MmSQL: TMemo;
    Splitter1: TSplitter;
    LbRQL: TListBox;
    Panel3: TPanel;
    EdtExpression: TEdit;
    BtnAdd: TButton;
    RgBackend: TRadioGroup;
    Memo1: TMemo;
    Splitter2: TSplitter;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnParseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LbRQLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  MVCFramework.RQL.AST2MySQL, System.Generics.Collections;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  LComp: string;
begin
  if TFile.Exists('rqlhistory.txt') then
  begin
    LbRQL.Items.LoadFromFile('rqlhistory.txt');
  end;

  RgBackend.Items.Clear;
  for LComp in TRQLCompilerRegistry.Instance.RegisteredCompilers do
  begin
    RgBackend.Items.AddObject(LComp, TRQLCompilerRegistry.Instance.GetCompiler(LComp).Create(nil));
  end;
  RgBackend.ItemIndex := 0;
end;

procedure TMainForm.BtnAddClick(Sender: TObject);
begin
  if not Trim(EdtExpression.Text).IsEmpty then
  begin
    LbRQL.Items.Insert(0, EdtExpression.Text);
    SaveHistory;
  end;
end;

procedure TMainForm.BtnParseClick(Sender: TObject);
var
  LParser: TRQL2SQL;
  LSQL: string;
begin
  LParser := TRQL2SQL.Create;
  try
    Memo1.Clear;
    try
      LParser.Execute(EdtExpression.Text, LSQL, TRQLCompiler(RgBackend.Items.Objects[RgBackend.ItemIndex]));
      MmSQL.Lines.Text := LSQL;
    except
      on E: Exception do
      begin
        Memo1.Lines.Text := E.Message;
      end;
    end;
  finally
    LParser.Free;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  LParser: TRQL2SQL;
  LSQL: string;
  LRQLItem: String;
  LRQLFileContent: TList<String>;
  LSQLFileContent: TList<String>;
begin
  Memo1.Lines.Clear;
  LRQLFileContent := TList<String>.Create;
  try
    LSQLFileContent := TList<String>.Create;
    try
      LParser := TRQL2SQL.Create;
      try
        try
          for LRQLItem in LbRQL.Items do
          begin
            try
              LParser.Execute(LRQLItem, LSQL, TRQLCompiler(RgBackend.Items.Objects[RgBackend.ItemIndex]));
            except
              on E: Exception do
              begin
                LSQL := 'ERROR:' + E.Message;
              end;
            end;
            LRQLFileContent.Add(LRQLItem);
            LSQLFileContent.Add(LSQL);
          end;
          TFile.WriteAllLines(
            '..\..\..\unittests\general\Several\RQLFixtures\RQL_' + TRQLCompiler(RgBackend.Items.Objects[RgBackend.ItemIndex]).ClassName + '.fixture',
            LRQLFileContent.ToArray
            );
          TFile.WriteAllLines(
            '..\..\..\unittests\general\Several\RQLFixtures\SQL_' + TRQLCompiler(RgBackend.Items.Objects[RgBackend.ItemIndex]).ClassName + '.fixture',
            LSQLFileContent.ToArray
            );
        except
          on E: Exception do
          begin
            Memo1.Lines.Text := E.Message;
          end;
        end;
      finally
        LParser.Free;
      end;
    finally
      LSQLFileContent.Free;
    end;
  finally
    LRQLFileContent.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: Integer;
begin
  SaveHistory;
  for I := 0 to RgBackend.Items.Count - 1 do
  begin
    RgBackend.Items.Objects[I].Free;
  end;
end;

procedure TMainForm.LbRQLClick(Sender: TObject);
begin
  EdtExpression.Text := LbRQL.Items[LbRQL.ItemIndex];
end;

procedure TMainForm.SaveHistory;
begin
  LbRQL.Items.SaveToFile('rqlhistory.txt');
end;

end.
