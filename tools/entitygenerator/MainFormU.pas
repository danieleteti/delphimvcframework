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
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.FBDef,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.ValEdit;

type
  TMainForm = class(TForm)
    FDConnection1: TFDConnection;
    Panel1: TPanel;
    Panel2: TPanel;
    qry: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Splitter1: TSplitter;
    mmConnectionParams: TMemo;
    Label2: TLabel;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    cboConnectionDefs: TComboBox;
    Panel3: TPanel;
    Panel4: TPanel;
    btnGenEntities: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    btnGetTables: TButton;
    veTablesMapping: TValueListEditor;
    mmOutput: TMemo;
    Panel5: TPanel;
    btnSaveCode: TButton;
    FileSaveDialog1: TFileSaveDialog;
    procedure btnGenEntitiesClick(Sender: TObject);
    procedure btnGetTablesClick(Sender: TObject);
    procedure btnSaveCodeClick(Sender: TObject);
    procedure cboConnectionDefsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Buff: TStringStream;
    FHistoryFileName: string;
    function GetClassName(const aTableName: string): string;
    procedure EmitUnit;
    procedure EmitUnitEnd;
    procedure EmitProperty(F: TField);
    procedure EmitField(F: TField);
    procedure EmitClass(const aTableName, aClassName: string);
    procedure EmitClassEnd;
    function GetDelphiType(FT: TFieldType): string;
    function GetProperCase(const Value: string): string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  {Spring.SystemUtils,} System.IOUtils;

{$R *.dfm}

procedure TMainForm.btnGenEntitiesClick(Sender: TObject);
var
  I: Integer;
  lTableName: string;
  lClassName: string;
  F: Integer;
begin
  Buff.Clear;
  EmitUnit;
  for I := 1 to veTablesMapping.RowCount - 1 do
  begin
    lTableName := veTablesMapping.Cells[0, I];
    lClassName := veTablesMapping.Cells[1, I];
    EmitClass(lTableName, lClassName);
    qry.Open('select * from ' + lTableName + ' where 1=0');

    Buff.WriteString('private' + sLineBreak);
    for F := 0 to qry.Fields.Count - 1 do
    begin
      EmitField(qry.Fields[F]);
    end;

    Buff.WriteString('public' + sLineBreak);
    for F := 0 to qry.Fields.Count - 1 do
    begin
      EmitProperty(qry.Fields[F]);
    end;

    EmitClassEnd;
  end;
  EmitUnitEnd;
  mmOutput.Lines.Text := Buff.DataString;

  // mmOutput.Lines.SaveToFile(
  // mmConnectionParams.Lines.SaveToFile(FHistoryFileName);
end;

procedure TMainForm.btnGetTablesClick(Sender: TObject);
var
  lTables: TStringList;
  lTable: string;
  lClassName: string;
begin
  FDConnection1.Close;
  FDConnection1.Params.Assign(mmConnectionParams.Lines);
  FDConnection1.Connected := True;

  lTables := TStringList.Create;
  try
    FDConnection1.GetTableNames('', '', '', lTables);
    veTablesMapping.Row := 1;
    for lTable in lTables do
    begin
      lClassName := GetClassName(lTable);
      veTablesMapping.InsertRow(lTable, lClassName, True);
    end;
  finally
    lTables.Free;
  end;

end;

procedure TMainForm.btnSaveCodeClick(Sender: TObject);
begin
  FileSaveDialog1.FileName := 'EntitiesU.pas';
  if FileSaveDialog1.Execute then
  begin
    mmOutput.Lines.SaveToFile(FileSaveDialog1.FileName);
  end;
end;

procedure TMainForm.cboConnectionDefsChange(Sender: TObject);
begin
  FDManager.GetConnectionDefParams(cboConnectionDefs.Text, mmConnectionParams.Lines);
end;

procedure TMainForm.EmitClass(const aTableName, aClassName: string);
begin
  Buff.WriteString('[MVCNameCase(ncLowerCase)]' + sLineBreak);
  Buff.WriteString(Format('[Table(''%s'')]', [aTableName]) + sLineBreak);
  if trim(aClassName) = '' then
    raise Exception.Create('Invalid class name');
  Buff.WriteString(aClassName + ' = class' + sLineBreak);
end;

procedure TMainForm.EmitClassEnd;
begin
  Buff.WriteString('end;' + sLineBreak + sLineBreak);
end;

procedure TMainForm.EmitField(F: TField);
begin
  Buff.WriteString(Format('  [TableField(''%s'')]', [F.FieldName]) + sLineBreak + '  f' + GetProperCase(F.FieldName) + ': ' +
    GetDelphiType(F.DataType) + ';' + sLineBreak);
end;

procedure TMainForm.EmitProperty(F: TField);
begin
  // Buff.WriteString(Format('  [TableField(''%s'')]', [F.FieldName]) + sLineBreak + '  property ' + GetProperCase(F.FieldName) + ': ' +
  // GetDelphiType(F.DataType) + ';' + sLineBreak);
  Buff.WriteString('  property ' + GetProperCase(F.FieldName) + ': ' + GetDelphiType(F.DataType));
  Buff.WriteString(' read f' + GetProperCase(F.FieldName) + ' write f' + GetProperCase(F.FieldName));
  Buff.WriteString(';' + sLineBreak);
end;

procedure TMainForm.EmitUnit;
begin
  Buff.WriteString('unit Entities;' + sLineBreak);
  Buff.WriteString('' + sLineBreak);
  Buff.WriteString('interface' + sLineBreak);
  Buff.WriteString('' + sLineBreak);
  Buff.WriteString('uses' + sLineBreak);
  Buff.WriteString('  MVCFramework.Serializer.Commons,' + sLineBreak);
  Buff.WriteString('  MVCFramework.ActiveRecord,' + sLineBreak);
  Buff.WriteString('  System.Classes;' + sLineBreak);
  Buff.WriteString('' + sLineBreak);
  Buff.WriteString('type' + sLineBreak);
  Buff.WriteString('' + sLineBreak);
end;

procedure TMainForm.EmitUnitEnd;
begin
  Buff.WriteString(sLineBreak + 'end.');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Buff.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Buff := TStringStream.Create;
  FHistoryFileName := TPath.GetDocumentsPath + PathDelim + 'eg.history';
  try
    if TFile.Exists(FHistoryFileName) then
      mmConnectionParams.Lines.LoadFromFile(FHistoryFileName)
    else
      mmConnectionParams.Lines.Assign(FDConnection1.Params);
  except

  end;

  FDManager.LoadConnectionDefFile;
  FDManager.GetConnectionNames(cboConnectionDefs.Items);
end;

function TMainForm.GetClassName(const aTableName: string): string;
var
  lTableName: string;
  lNextLetter: Integer;
  lNextLetterChar: string;
begin
  lTableName := aTableName.ToLower;
  Result := 'T' + lTableName.Substring(0, 1).ToUpper + lTableName.Substring(1).ToLower;

  while Result.IndexOf('_') > -1 do
  begin
    lNextLetter := Result.IndexOf('_') + 1;
    lNextLetterChar := UpperCase(Result.Chars[lNextLetter]);
    Result := Result.Remove(Result.IndexOf('_') + 1, 1);
    Result := Result.Insert(Result.IndexOf('_') + 1, lNextLetterChar);
    Result := Result.Remove(Result.IndexOf('_'), 1);
  end;
end;

function TMainForm.GetDelphiType(FT: TFieldType): string;
begin
  case FT of
    ftString:
      Result := 'String';
    ftSmallint, ftInteger, ftWord, ftLongWord, ftShortint:
      Result := 'Integer';
    ftByte:
      Result := 'Byte';
    ftLargeint:
      Result := 'Int64';
    ftBoolean:
      Result := 'Boolean';
    ftFloat, ftSingle, ftExtended:
      Result := 'Double';
    ftCurrency, ftBCD:
      Result := 'Currency';
    ftDate:
      Result := 'TDate';
    ftTime:
      Result := 'TTime';
    ftDateTime:
      Result := 'TDateTime';
    ftTimeStamp:
      Result := 'TDateTime {timestamp}';
    ftAutoInc:
      Result := 'Integer; {autoincrement}';
    ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftWideMemo, ftStream:
      Result := 'TStream';
    ftFixedChar:
      Result := 'String; {fixedchar}';
    ftWideString:
      Result := 'String';
  else
    Result := '<UNSUPPORTED TYPE: ' + IntToStr(Ord(FT)) + '>';
  end;
end;

function TMainForm.GetProperCase(const Value: string): string;
var
  Pieces: TArray<string>;
  s: string;
begin
  if Value.Length <= 2 then
    exit(Value.ToUpper);

  Result := '';
  Pieces := Value.ToLower.Split(['_']);
  for s in Pieces do
  begin
    if s = 'id' then
      Result := Result + 'ID'
    else
      Result := Result + UpperCase(s.Chars[0]) + s.Substring(1);
  end;
end;

end.
