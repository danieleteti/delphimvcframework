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
  Vcl.ValEdit,
  FireDAC.Phys.MySQLDef,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.PGDef,
  FireDAC.Phys.PG,
  FireDAC.Phys.IBDef,
  FireDAC.Phys.IB,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, Vcl.DBGrids;

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
    btnGetTables: TButton;
    mmOutput: TMemo;
    Panel5: TPanel;
    btnSaveCode: TButton;
    FileSaveDialog1: TFileSaveDialog;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDPhysFBDriverLink2: TFDPhysFBDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysMySQLDriverLink2: TFDPhysMySQLDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    dsTablesMapping: TFDMemTable;
    dsTablesMappingTABLE_NAME: TStringField;
    dsTablesMappingCLASS_NAME: TStringField;
    DBGrid1: TDBGrid;
    dsrcTablesMapping: TDataSource;
    Panel6: TPanel;
    GroupBox1: TGroupBox;
    lstSchema: TListBox;
    lstCatalog: TListBox;
    btnRefreshCatalog: TButton;
    Label1: TLabel;
    chGenerateMapping: TCheckBox;
    procedure btnGenEntitiesClick(Sender: TObject);
    procedure btnGetTablesClick(Sender: TObject);
    procedure btnSaveCodeClick(Sender: TObject);
    procedure cboConnectionDefsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lstCatalogDblClick(Sender: TObject);
    procedure btnRefreshCatalogClick(Sender: TObject);
    procedure mmConnectionParamsChange(Sender: TObject);
    procedure lstSchemaDblClick(Sender: TObject);
  private
    fCatalog: string;
    fSchema: string;
    fIntfBuff, fImplBuff, fInitializationBuff: TStringStream;
    FHistoryFileName: string;
    lTypesName: TArray<string>;
    procedure EmitHeaderComments;
    function GetClassName(const aTableName: string): string;
    procedure EmitUnit;
    procedure EmitUnitEnd;
    procedure EmitProperty(F: TField);
    procedure EmitField(F: TField; const IsPK: Boolean);
    procedure EmitClass(const aTableName, aClassName: string);
    procedure EmitClassEnd;
    function GetDelphiType(FT: TFieldType): string;
    function GetFieldName(const Value: string): string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  {Spring.SystemUtils,} System.IOUtils, System.TypInfo;

{$R *.dfm}


const
  INDENT = '  ';

procedure TMainForm.btnGenEntitiesClick(Sender: TObject);
var
  I: Integer;
  lTableName: string;
  lClassName: string;
  F: Integer;
  lFieldsName: TArray<string>;
  lKeyFields: TStringList;
begin
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitializationBuff.Clear;
  lKeyFields := TStringList.Create;
  try
    EmitHeaderComments;
    EmitUnit;
    dsTablesMapping.First;
    I := 0;
    while not dsTablesMapping.Eof do
    begin
      lTableName := dsTablesMappingTABLE_NAME.AsString;
      lClassName := dsTablesMappingCLASS_NAME.AsString;
      EmitClass(lTableName, lClassName);
      qry.Open('select * from ' + lTableName + ' where 1=0');
      FDConnection1.GetKeyFieldNames(fCatalog, fSchema, lTableName, '', lKeyFields);
      lFieldsName := [];
      lTypesName := [];
      fIntfBuff.WriteString(INDENT + 'private' + sLineBreak);
      for F := 0 to qry.Fields.Count - 1 do
      begin
        EmitField(qry.Fields[F], lKeyFields.IndexOf(qry.Fields[F].FieldName) > -1);

        if GetDelphiType(qry.Fields[F].DataType) = 'TStream' then
        begin
          lFieldsName := lFieldsName + [GetFieldName(qry.Fields[F].FieldName)];
          lTypesName := lTypesName + ['TMemoryStream'];
        end;

      end;

      fIntfBuff.WriteString(INDENT + 'public' + sLineBreak);
      fIntfBuff.WriteString(INDENT + '  constructor Create; override;' + sLineBreak);

      fImplBuff.WriteString('constructor ' + lClassName + '.Create;' + sLineBreak);
      fImplBuff.WriteString('begin' + sLineBreak);
      fImplBuff.WriteString('  inherited Create;' + sLineBreak);
      for F := low(lFieldsName) to high(lFieldsName) do
      begin
        fImplBuff.WriteString('  ' + lFieldsName[F] + ' := ' + lTypesName[F] + '.Create;' + sLineBreak);
      end;
      fImplBuff.WriteString('end;' + sLineBreak + sLineBreak);

      fIntfBuff.WriteString(INDENT + '  destructor Destroy; override;' + sLineBreak);
      fImplBuff.WriteString('destructor ' + lClassName + '.Destroy;' + sLineBreak);
      fImplBuff.WriteString('begin' + sLineBreak);
      for F := low(lFieldsName) to high(lFieldsName) do
      begin
        fImplBuff.WriteString('  ' + lFieldsName[F] + '.Free;' + sLineBreak);
      end;
      fImplBuff.WriteString('  inherited;' + sLineBreak);
      fImplBuff.WriteString('end;' + sLineBreak + sLineBreak);

      for F := 0 to qry.Fields.Count - 1 do
      begin
        EmitProperty(qry.Fields[F]);
      end;

      EmitClassEnd;
      dsTablesMapping.Next;
    end;
    EmitUnitEnd;
    mmOutput.Lines.Text := fIntfBuff.DataString + fImplBuff.DataString + fInitializationBuff.DataString;

  finally
    lKeyFields.Free;
  end;
  // mmOutput.Lines.SaveToFile(
  // mmConnectionParams.Lines.SaveToFile(FHistoryFileName);
end;

procedure TMainForm.btnGetTablesClick(Sender: TObject);
var
  lTables: TStringList;
  lTable: string;
  lClassName: string;
begin
  FDConnection1.Connected := True;
  lTables := TStringList.Create;
  try
    fCatalog := '';
    if lstCatalog.ItemIndex > -1 then
    begin
      fCatalog := lstCatalog.Items[lstCatalog.ItemIndex];
    end;
    fSchema := '';
    if lstSchema.ItemIndex > -1 then
    begin
      fSchema := lstSchema.Items[lstSchema.ItemIndex];
    end;
    FDConnection1.GetTableNames(fCatalog, fSchema, '', lTables);


    // FDConnection1.GetTableNames('', 'public', '', lTables);
    // FDConnection1.GetTableNames('', '', '', lTables);
    // if lTables.Count = 0 then
    // FDConnection1.GetTableNames('', 'dbo', '', lTables);

    dsTablesMapping.EmptyDataSet;
    for lTable in lTables do
    begin
      lClassName := GetClassName(lTable);
      dsTablesMapping.AppendRecord([lTable, lClassName]);
    end;
    dsTablesMapping.First;
  finally
    lTables.Free;
  end;

end;

procedure TMainForm.btnRefreshCatalogClick(Sender: TObject);
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Text := mmConnectionParams.Text;
  FDConnection1.Open;
  lstCatalog.Items.Clear;
  FDConnection1.GetCatalogNames('', lstCatalog.Items);
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
  FDConnection1.Close;
  FDManager.GetConnectionDefParams(cboConnectionDefs.Text, mmConnectionParams.Lines);
  lstCatalog.Items.Clear;
  lstSchema.Items.Clear;
  FDConnection1.Params.Clear;
  FDConnection1.Params.Text := mmConnectionParams.Text;
end;

procedure TMainForm.EmitClass(const aTableName, aClassName: string);
begin
  fIntfBuff.WriteString(INDENT + '[MVCNameCase(ncLowerCase)]' + sLineBreak);
  fIntfBuff.WriteString(INDENT + Format('[MVCTable(''%s'')]', [aTableName]) + sLineBreak);
  if trim(aClassName) = '' then
    raise Exception.Create('Invalid class name');
  fIntfBuff.WriteString(INDENT + aClassName + ' = class(TMVCActiveRecord)' + sLineBreak);
  if chGenerateMapping.Checked then
    fInitializationBuff.WriteString(INDENT + Format('ActiveRecordMappingRegistry.AddEntity(''%s'',%s);',
      [aTableName.ToLower, aClassName]) + sLineBreak);
end;

procedure TMainForm.EmitClassEnd;
begin
  fIntfBuff.WriteString(INDENT + 'end;' + sLineBreak + sLineBreak);
end;

procedure TMainForm.EmitField(F: TField; const IsPK: Boolean);
var
  lAttrib, lField: String;
begin
  if IsPK then
  begin
    lAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey, foAutoGenerated])]', [F.FieldName]);
  end
  else
  begin
    lAttrib := Format('[MVCTableField(''%s'')]', [F.FieldName]);
  end;
  lField := GetFieldName(F.FieldName) + ': ' + GetDelphiType(F.DataType) + ';' + sLineBreak;

  if GetDelphiType(F.DataType).ToUpper.Contains('UNSUPPORTED TYPE') then
  begin
    lAttrib := '//' + lAttrib;
    lField := '//' + lField;
  end
  else
  begin
    lField := '  ' + lField;
    lAttrib := '  ' + lAttrib;
  end;
  fIntfBuff.WriteString(INDENT + lAttrib + sLineBreak + INDENT + lField);
end;

procedure TMainForm.EmitHeaderComments;
begin
  fIntfBuff.WriteString('// *************************************************************************** }' +
    sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// Delphi MVC Framework' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// https://github.com/danieleteti/delphimvcframework' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// ***************************************************************************' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// Licensed under the Apache License, Version 2.0 (the "License");' + sLineBreak);
  fIntfBuff.WriteString('// you may not use this file except in compliance with the License.' + sLineBreak);
  fIntfBuff.WriteString('// You may obtain a copy of the License at' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// http://www.apache.org/licenses/LICENSE-2.0' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// Unless required by applicable law or agreed to in writing, software' + sLineBreak);
  fIntfBuff.WriteString('// distributed under the License is distributed on an "AS IS" BASIS,' + sLineBreak);
  fIntfBuff.WriteString('// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.' + sLineBreak);
  fIntfBuff.WriteString('// See the License for the specific language governing permissions and' + sLineBreak);
  fIntfBuff.WriteString('// limitations under the License.' + sLineBreak);
  fIntfBuff.WriteString('//' + sLineBreak);
  fIntfBuff.WriteString('// ***************************************************************************' + sLineBreak);
  fIntfBuff.WriteString(sLineBreak);
end;

procedure TMainForm.EmitProperty(F: TField);
var
  lProp: String;
begin
  lProp := 'property ' + GetFieldName(F.FieldName).Substring(1) { remove f } + ': ' + GetDelphiType(F.DataType) +
    ' read ' + GetFieldName(F.FieldName) + ' write ' + GetFieldName(F.FieldName) + ';' + sLineBreak;

  if GetDelphiType(F.DataType).ToUpper.Contains('UNSUPPORTED TYPE') then
  begin
    lProp := '  //' + lProp
  end
  else
  begin
    lProp := '  ' + lProp;
  end;
  fIntfBuff.WriteString(INDENT + lProp)
end;

procedure TMainForm.EmitUnit;
begin
  fIntfBuff.WriteString('unit EntitiesU;' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);
  fIntfBuff.WriteString('interface' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);
  fIntfBuff.WriteString('uses' + sLineBreak);
  fIntfBuff.WriteString('  MVCFramework.Serializer.Commons,' + sLineBreak);
  fIntfBuff.WriteString('  MVCFramework.ActiveRecord,' + sLineBreak);
  fIntfBuff.WriteString('  System.Classes;' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);
  fIntfBuff.WriteString('type' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);

  fImplBuff.WriteString('implementation' + sLineBreak + sLineBreak);

  fInitializationBuff.WriteString('initialization' + sLineBreak + sLineBreak);
end;

procedure TMainForm.EmitUnitEnd;
begin
  fInitializationBuff.WriteString(sLineBreak + 'end.');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fIntfBuff.Free;
  fImplBuff.Free;
  fInitializationBuff.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fIntfBuff := TStringStream.Create;
  fImplBuff := TStringStream.Create;
  fInitializationBuff := TStringStream.Create;
  FHistoryFileName := TPath.Combine(TPath.GetDocumentsPath, TPath.GetFileNameWithoutExtension(ParamStr(0)) +
    '.history');
  try
    if TFile.Exists(FHistoryFileName) then
    begin
      mmConnectionParams.Lines.LoadFromFile(FHistoryFileName)
    end;
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
  lTableName := aTableName.ToLower.DeQuotedString('"').Replace(' ', '_', [rfReplaceAll]);
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
    ftFloat, TFieldType.ftSingle, TFieldType.ftExtended:
      Result := 'Double';
    ftCurrency, ftBCD, ftFMTBcd:
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
      Result := 'Integer {autoincrement}';
    ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftWideMemo, ftStream:
      Result := 'TStream';
    ftFixedChar:
      Result := 'String {fixedchar}';
    ftWideString:
      Result := 'String';
    ftGuid:
      Result := 'TGuid';
  else
    Result := '<UNSUPPORTED TYPE: ' + GetEnumName(TypeInfo(TFieldType), Ord(FT)) + '>';
  end;
end;

function TMainForm.GetFieldName(const Value: string): string;
var
  Pieces: TArray<string>;
  s: string;
begin
  if Value.Length <= 2 then
    exit('f' + Value.ToUpper);

  Result := '';
  Pieces := Value.ToLower.Split(['_'], TStringSplitOptions.ExcludeEmpty);
  for s in Pieces do
  begin
    if s = 'id' then
      Result := Result + 'ID'
    else
      Result := Result + UpperCase(s.Chars[0]) + s.Substring(1);
  end;
  Result := 'f' + Result;
end;

procedure TMainForm.lstCatalogDblClick(Sender: TObject);
begin
  lstSchema.Items.Clear;
  FDConnection1.GetSchemaNames(lstCatalog.Items[lstCatalog.ItemIndex], '', lstSchema.Items);
end;

procedure TMainForm.lstSchemaDblClick(Sender: TObject);
begin
  btnGetTablesClick(Self);
end;

procedure TMainForm.mmConnectionParamsChange(Sender: TObject);
begin
  FDConnection1.Close;
  lstSchema.Clear;
  lstCatalog.Clear;
end;

end.
