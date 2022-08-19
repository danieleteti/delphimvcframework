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
  FireDAC.Phys.SQLite, Vcl.DBGrids, FireDAC.Phys.SQLiteWrapper.Stat, Vcl.Buttons,
  JsonDataObjects, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.StdActns,
  Vcl.ExtActns;

type
  TSelectionType = (stAll, stNone, stInverse);

  TMainForm = class(TForm)
    FDConnection: TFDConnection;
    qry: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
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
    dsrcTablesMapping: TDataSource;
    dsTablesMappingGENERATE: TBooleanField;
    pcMain: TPageControl;
    tsConnectionDefinition: TTabSheet;
    tsTablesMapping: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    mmConnectionParams: TMemo;
    Panel6: TPanel;
    GroupBox1: TGroupBox;
    lstSchema: TListBox;
    lstCatalog: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    btnGenEntities: TButton;
    chkGenerateMapping: TCheckBox;
    rgNameCase: TRadioGroup;
    rgFieldNameFormatting: TRadioGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGrid1: TDBGrid;
    Panel7: TPanel;
    btnUZ: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel8: TPanel;
    btnPrev: TButton;
    btnNext: TButton;
    ProjectFileOpenDialog: TFileOpenDialog;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    actLoadProject: TAction;
    actSaveProject: TAction;
    actSaveProjectAs: TAction;
    File1: TMenuItem;
    LoadProject1: TMenuItem;
    SaveProject1: TMenuItem;
    Saveprojectas1: TMenuItem;
    FileExit1: TFileExit;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    cboConnectionDefs: TComboBox;
    Panel9: TPanel;
    btnRefreshCatalog: TButton;
    TabNextTab1: TNextTab;
    TabPreviousTab1: TPreviousTab;
    tsGeneratedCode: TTabSheet;
    Panel5: TPanel;
    btnSaveCode: TButton;
    mmOutput: TMemo;
    actSaveGeneratedCode: TAction;
    actGenerateCode: TAction;
    actRefreshCatalog: TAction;
    Panel10: TPanel;
    btnGetTables: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    actRefreshTableList: TAction;
    Label3: TLabel;
    Panel11: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    FileSaveDialogProject: TFileSaveDialog;
    actNewProject: TAction;
    NewProject1: TMenuItem;
    procedure cboConnectionDefsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mmConnectionParamsChange(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSlice1Click(Sender: TObject);
    procedure btnUZClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure actLoadProjectExecute(Sender: TObject);
    procedure actSaveGeneratedCodeExecute(Sender: TObject);
    procedure actGenerateCodeExecute(Sender: TObject);
    procedure lstCatalogClick(Sender: TObject);
    procedure actRefreshCatalogExecute(Sender: TObject);
    procedure TabNextTab1AfterTabChange(Sender: TObject);
    procedure actRefreshTableListExecute(Sender: TObject);
    procedure TabNextTab1Update(Sender: TObject);
    procedure actSaveProjectExecute(Sender: TObject);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actSaveGeneratedCodeUpdate(Sender: TObject);
  private
    fConfig: TJSONObject;
    fCatalog: string;
    fProjectFileName: string;
    fSchema: string;
    fIntfBuff, fImplBuff, fInitializationBuff: TStringStream;
    FHistoryFileName: string;
    lTypesName: TArray<string>;
    fBookmark: TArray<Byte>;
    procedure ResetUI;
    procedure LoadProjectFromFile;
    procedure SaveProject;
    function SelectTables(const FromLetter: AnsiChar; const ToLetter: AnsiChar): Integer;
    procedure EmitHeaderComments;
    function GetClassName(const aTableName: string): string;
    function GetUniqueFieldNames(const Fields: TFields; const FormatAsPascalCase: Boolean): TArray<String>;
    procedure EmitUnit;
    procedure EmitUnitEnd;
    procedure EmitProperty(const FieldName: String; const FieldDataType: TFieldType);
    procedure EmitField(const DatabaseFieldName: String; const UniqueFieldName: String; const FieldDataType: TFieldType;
      const IsPK: Boolean);
    procedure EmitClass(const aTableName, aClassName, aNameCase: string);
    procedure EmitClassEnd;
    function GetDelphiType(FT: TFieldType): string;
    function GetFieldName(const Value: string): string;
    procedure DoSelection(const SelectionType: TSelectionType);
    procedure SetProjectFileName(const Value: String);
    function GetProjectFileExists: Boolean;
  public
    property ProjectFileName: String read fProjectFileName write SetProjectFileName;
    property ProjectFileExists: Boolean read GetProjectFileExists;
  end;

var
  MainForm: TMainForm;

const
  LOG_TAG = 'generator';
  CONFIG_FILE = 'lastConfig.json';

implementation

uses
  System.IOUtils,
  System.TypInfo,
  System.DateUtils,
  LoggerPro.GlobalLogger,
  System.Generics.Collections,
  MVCFramework.Commons, UtilsU;

{$R *.dfm}

const
  INDENT = '  ';

procedure TMainForm.actGenerateCodeExecute(Sender: TObject);
var
  I: Integer;
  lTableName: string;
  lClassName: string;
  F: Integer;
  lFieldNamesToInitialize: TArray<string>;
  lKeyFields: TStringList;
  lUniqueFieldNames: TArray<String>;
begin
//  SaveProject;
  Log.Info('Starting entities generation', LOG_TAG);
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitializationBuff.Clear;
  lKeyFields := TStringList.Create;
  try
    EmitHeaderComments;
    EmitUnit;
    dsTablesMapping.First;
    while not dsTablesMapping.Eof do
    begin
      if not dsTablesMappingGENERATE.Value then
      begin
        Log.Info('Skipping table %s', [dsTablesMappingTABLE_NAME.AsString], LOG_TAG);
        dsTablesMapping.Next;
        Continue;
      end;
      lTableName := dsTablesMappingTABLE_NAME.AsString;
      Log.Info('Generating entity %s for table %s', [dsTablesMappingCLASS_NAME.AsString,
        dsTablesMappingTABLE_NAME.AsString], LOG_TAG);
      lClassName := dsTablesMappingCLASS_NAME.AsString;
      EmitClass(lTableName, lClassName, rgNameCase.Items[rgNameCase.ItemIndex]);
      lKeyFields.Clear;
      qry.Close;
      qry.SQL.Text := 'select * from ' + lTableName + ' where 1=0';
      qry.Open;
      try
        FDConnection.GetKeyFieldNames(fCatalog, fSchema, lTableName, '', lKeyFields);
      except
      end;
      lFieldNamesToInitialize := [];
      lTypesName := [];
      fIntfBuff.WriteString(INDENT + 'private' + sLineBreak);
      lUniqueFieldNames := GetUniqueFieldNames(qry.Fields, rgFieldNameFormatting.ItemIndex = 1);
      for F := 0 to qry.Fields.Count - 1 do
      begin

        var
        lReq := qry.Fields[F].Required;
        if lReq then
        begin
          lReq := lReq;
        end;
        EmitField(qry.Fields[F].FieldName, lUniqueFieldNames[F], qry.Fields[F].DataType,
          lKeyFields.IndexOf(qry.Fields[F].FieldName) > -1);

        if GetDelphiType(qry.Fields[F].DataType) = 'TStream' then
        begin
          lFieldNamesToInitialize := lFieldNamesToInitialize + [GetFieldName(lUniqueFieldNames[F])];
          lTypesName := lTypesName + ['TMemoryStream'];
        end;

      end;

      fIntfBuff.WriteString(INDENT + 'public' + sLineBreak);
      fIntfBuff.WriteString(INDENT + '  constructor Create; override;' + sLineBreak);

      fImplBuff.WriteString('constructor ' + lClassName + '.Create;' + sLineBreak);
      fImplBuff.WriteString('begin' + sLineBreak);
      fImplBuff.WriteString('  inherited Create;' + sLineBreak);
      for F := low(lFieldNamesToInitialize) to high(lFieldNamesToInitialize) do
      begin
        fImplBuff.WriteString('  ' + lFieldNamesToInitialize[F] + ' := ' + lTypesName[F] + '.Create;' + sLineBreak);
      end;
      fImplBuff.WriteString('end;' + sLineBreak + sLineBreak);

      fIntfBuff.WriteString(INDENT + '  destructor Destroy; override;' + sLineBreak);
      fImplBuff.WriteString('destructor ' + lClassName + '.Destroy;' + sLineBreak);
      fImplBuff.WriteString('begin' + sLineBreak);
      for F := low(lFieldNamesToInitialize) to high(lFieldNamesToInitialize) do
      begin
        fImplBuff.WriteString('  ' + lFieldNamesToInitialize[F] + '.Free;' + sLineBreak);
      end;
      fImplBuff.WriteString('  inherited;' + sLineBreak);
      fImplBuff.WriteString('end;' + sLineBreak + sLineBreak);

      for F := 0 to qry.Fields.Count - 1 do
      begin
        EmitProperty(lUniqueFieldNames[F], qry.Fields[F].DataType);
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
  ShowMessage('Generation Completed');
  TabNextTab1.Execute;
end;

procedure TMainForm.actLoadProjectExecute(Sender: TObject);
begin
  ProjectFileOpenDialog.DefaultExtension := 'entgen';

  if ProjectFileOpenDialog.Execute then
  begin
    ProjectFileName := ProjectFileOpenDialog.FileName;
    LoadProjectFromFile;
  end;
end;

procedure TMainForm.actNewProjectExecute(Sender: TObject);
begin
  ProjectFileName := DEFAULT_PROJECT_NAME;
  LoadProjectFromFile;
end;

procedure TMainForm.actRefreshCatalogExecute(Sender: TObject);
begin
  FDConnection.Params.Clear;
  FDConnection.Params.Text := mmConnectionParams.Text;
  try
    FDConnection.Open;
    lstCatalog.Items.Clear;
    FDConnection.GetCatalogNames('', lstCatalog.Items);
  except
    on E: Exception do
    begin
      Application.ShowException(E);
    end;
  end;
end;

procedure TMainForm.actRefreshTableListExecute(Sender: TObject);
var
  lTables: TStringList;
  lTable: string;
  lClassName: string;
begin
  FDConnection.Connected := True;
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
    FDConnection.GetTableNames(fCatalog, fSchema, '', lTables);


    // FDConnection1.GetTableNames('', 'public', '', lTables);
    // FDConnection1.GetTableNames('', '', '', lTables);
    // if lTables.Count = 0 then
    // FDConnection1.GetTableNames('', 'dbo', '', lTables);

    dsTablesMapping.EmptyDataSet;
    for lTable in lTables do
    begin
      lClassName := GetClassName(lTable);
      dsTablesMapping.AppendRecord([True, lTable, lClassName]);
    end;
    dsTablesMapping.First;
  finally
    lTables.Free;
  end;
  TabSheet1.Caption := 'Tables (' + dsTablesMapping.RecordCount.ToString + ')';
end;

procedure TMainForm.actSaveGeneratedCodeExecute(Sender: TObject);
begin
  FileSaveDialog1.FileName := 'EntitiesU.pas';
  if FileSaveDialog1.Execute then
  begin
    mmOutput.Lines.SaveToFile(FileSaveDialog1.FileName);
  end;
end;

procedure TMainForm.actSaveGeneratedCodeUpdate(Sender: TObject);
begin
  actSaveGeneratedCode.Enabled := mmOutput.Lines.Count > 0;
end;

procedure TMainForm.actSaveProjectAsExecute(Sender: TObject);
begin
  if FileSaveDialogProject.Execute then
  begin
    ProjectFileName := FileSaveDialogProject.FileName;
    SaveProject;
  end;
end;

procedure TMainForm.actSaveProjectExecute(Sender: TObject);
begin
  if not ProjectFileExists then
    actSaveProjectAs.Execute
  else
    SaveProject;
end;

procedure TMainForm.btnSlice1Click(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('R', 'T').ToString + ' new tables');
end;

procedure TMainForm.btnUZClick(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('U', 'Z').ToString + ' new tables');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('L', 'Q').ToString + ' new tables');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('E', 'K').ToString + ' new tables');
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('C', 'D').ToString + ' new tables');
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  ShowMessage('Select ' + SelectTables('A', 'B').ToString + ' new tables');
end;

procedure TMainForm.cboConnectionDefsChange(Sender: TObject);
begin
  FDConnection.Close;
  FDManager.GetConnectionDefParams(cboConnectionDefs.Text, mmConnectionParams.Lines);
  lstCatalog.Items.Clear;
  lstSchema.Items.Clear;
  FDConnection.Params.Clear;
  FDConnection.Params.Text := mmConnectionParams.Text;
  actRefreshCatalog.Execute;
end;

procedure TMainForm.DBGrid1CellClick(Column: TColumn);
begin
  if Column.FieldName = 'GENERATE' then
  begin
    if not(dsTablesMapping.State = dsEdit) then
    begin
      dsTablesMapping.Edit;
    end;
    dsTablesMappingGENERATE.Value := not dsTablesMappingGENERATE.Value;
    dsTablesMapping.Post;
  end;
end;

procedure TMainForm.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
const
  IsChecked: array [Boolean] of Integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var
  DrawState: Integer;
  DrawRect: TRect;
begin
  if (Column.Field.FieldName = 'GENERATE') then
  begin
    DrawRect := Rect;
    InflateRect(DrawRect, -1, -1);
    DrawState := IsChecked[Column.Field.AsBoolean];
    DBGrid1.Canvas.FillRect(Rect);
    DrawFrameControl(DBGrid1.Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
  end
  else
  begin
    DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TMainForm.DoSelection(const SelectionType: TSelectionType);
begin
  dsTablesMapping.DisableControls;
  try
    fBookmark := dsTablesMapping.GetBookmark;
    dsTablesMapping.First;
    while not dsTablesMapping.Eof do
    begin
      dsTablesMapping.Edit;
      case SelectionType of
        stAll:
          dsTablesMappingGENERATE.Value := True;
        stNone:
          dsTablesMappingGENERATE.Value := False;
        stInverse:
          dsTablesMappingGENERATE.Value := not dsTablesMappingGENERATE.Value;
      end;
      dsTablesMapping.Post;
      dsTablesMapping.Next;
    end;
    dsTablesMapping.Bookmark := fBookmark;
  finally
    dsTablesMapping.EnableControls;
  end;
end;

procedure TMainForm.EmitClass(const aTableName, aClassName, aNameCase: string);
begin
  fIntfBuff.WriteString(INDENT + '[MVCNameCase(nc' + aNameCase + ')]' + sLineBreak);
  fIntfBuff.WriteString(INDENT + Format('[MVCTable(''%s'')]', [aTableName]) + sLineBreak);
  if trim(aClassName) = '' then
    raise Exception.Create('Invalid class name');
  fIntfBuff.WriteString(INDENT + aClassName + ' = class(TMVCActiveRecord)' + sLineBreak);
  if chkGenerateMapping.Checked then
    fInitializationBuff.WriteString(Format('ActiveRecordMappingRegistry.AddEntity(''%s'', %s);',
      [aTableName.ToLower, aClassName]) + sLineBreak);
end;

procedure TMainForm.EmitClassEnd;
begin
  fIntfBuff.WriteString(INDENT + 'end;' + sLineBreak + sLineBreak);
end;

procedure TMainForm.EmitField(const DatabaseFieldName: String; const UniqueFieldName: String;
  const FieldDataType: TFieldType; const IsPK: Boolean);
var
  lAttrib, lField: string;
begin
  if IsPK then
  begin
    lAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey, foAutoGenerated])]', [DatabaseFieldName]);
  end
  else
  begin
    lAttrib := Format('[MVCTableField(''%s'')]', [DatabaseFieldName]);
  end;
  lField := GetFieldName(UniqueFieldName) + ': ' + GetDelphiType(FieldDataType) + ';' + sLineBreak;

  if GetDelphiType(FieldDataType).ToUpper.Contains('UNSUPPORTED TYPE') then
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
  fIntfBuff.WriteString('// Copyright (c) 2010-' + YearOf(Date).ToString + ' Daniele Teti and the DMVCFramework Team' +
    sLineBreak);
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

procedure TMainForm.EmitProperty(const FieldName: String; const FieldDataType: TFieldType);
var
  lProp: string;
begin
  // if GetFieldName(F.FieldName).Substring(1).ToLower <> F.FieldName then
  // begin
  // lProp := Format('[MVCNameAs(''%s'')]', [F.FieldName]) + sLineBreak + INDENT + INDENT;
  // end;
  lProp := lProp + 'property ' + GetFieldName(FieldName).Substring(1) { remove f } + ': ' + GetDelphiType(FieldDataType)
    + ' read ' + GetFieldName(FieldName) + ' write ' + GetFieldName(FieldName) + ';' + sLineBreak;

  if GetDelphiType(FieldDataType).ToUpper.Contains('UNSUPPORTED TYPE') then
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
  pcMain.ActivePageIndex := 0;
  fConfig := TJSONObject.Create;
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

  ProjectFileName := DEFAULT_PROJECT_NAME;

  // LoadProjectFromFile;
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
    ftString, ftMemo, ftFmtMemo, ftWideMemo:
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
    ftBlob, { ftMemo, } ftGraphic, { ftFmtMemo, ftWideMemo, } ftStream:
      Result := 'TStream';
    ftFixedChar:
      Result := 'String {fixedchar}';
    ftWideString:
      Result := 'String';
    ftGuid:
      Result := 'TGuid';
    ftDBaseOle:
      Result := 'String {ftDBaseOle}';
  else
    Result := '<UNSUPPORTED TYPE: ' + GetEnumName(TypeInfo(TFieldType), Ord(FT)) + '>';
  end;
end;

function TMainForm.GetFieldName(const Value: string): string;
var
  Pieces: TArray<string>;
  Piece: string;
begin
  if Value.Length <= 2 then
  begin
    Exit('f' + Value.ToUpper);
  end;
  Result := 'f' + Value;
end;

function TMainForm.GetProjectFileExists: Boolean;
begin
  Result := TFile.Exists(ProjectFileName);
end;

function TMainForm.GetUniqueFieldNames(const Fields: TFields; const FormatAsPascalCase: Boolean): TArray<String>;
var
  I: Integer;
  lList: TStringList;
  lF: string;
  lFTemp: string;
  lCount: Integer;
begin
  SetLength(Result, Fields.Count);
  lList := TStringList.Create;
  try
    lList.Sorted := True;
    for I := 0 to Fields.Count - 1 do
    begin
      lCount := 0;
      if FormatAsPascalCase then
      begin
        lF := CamelCase(Fields[I].FieldName, True);
      end
      else
      begin
        lF := Fields[I].FieldName;
      end;
      if lList.IndexOf(lF) > -1 then
      begin
        lF := Fields[I].FieldName;
      end;
      lFTemp := lF;

      if IsReservedKeyword(lFTemp) then
      begin
        lFTemp := '_' + lFTemp;
      end;

      while (lList.IndexOf(lFTemp) > -1) do
      begin
        Inc(lCount);
        lFTemp := lF + '__' + IntToStr(lCount);
      end;
      lF := lFTemp;
      lList.Add(lF);
      Result[I] := lF;
    end;
  finally
    lList.Free;
  end;
end;

procedure TMainForm.LoadProjectFromFile;
var
  I, J: Integer;
  lJObj: TJSONObject;
begin
  ResetUI;

  if not TFile.Exists(fProjectFileName) then
  begin
    Exit;
  end;

  fConfig.LoadFromFile(fProjectFileName);
  if cboConnectionDefs.Items.IndexOf(fConfig.S[cboConnectionDefs.Name]) = -1 then
    Exit;

  cboConnectionDefs.ItemIndex := cboConnectionDefs.Items.IndexOf(fConfig.S[cboConnectionDefs.Name]);
  cboConnectionDefsChange(self);
  actRefreshCatalog.Execute;
  lstCatalog.Update;
  if fConfig.IndexOf(lstCatalog.Name) > -1 then
  begin
    lstCatalog.ItemIndex := lstCatalog.Items.IndexOf(fConfig.S[lstCatalog.Name]);
    lstCatalogClick(self);
    lstSchema.ItemIndex := lstSchema.Items.IndexOf(fConfig.S[lstSchema.Name]);
  end;
  actRefreshTableList.Execute;

  rgNameCase.ItemIndex := fConfig.I[rgNameCase.Name];
  rgFieldNameFormatting.ItemIndex := fConfig.I[rgFieldNameFormatting.Name];
  chkGenerateMapping.Checked := fConfig.B[chkGenerateMapping.Name];

  dsTablesMapping.First;
  I := 0;
  while not dsTablesMapping.Eof do
  begin
    lJObj := fConfig.A['tables'].Items[I].ObjectValue;
    if lJObj.S['TABLE_NAME'] = dsTablesMapping.FieldByName('TABLE_NAME').AsString then
    begin
      dsTablesMapping.Edit;
      try
        for J := 0 to lJObj.Count - 1 do
        begin
          if Assigned(dsTablesMapping.FindField(lJObj.Names[J])) then
          begin
            dsTablesMapping.Fields[J].AsString := lJObj.S[lJObj.Names[J]];
          end;
        end;
        Inc(I);
      finally
        dsTablesMapping.Post;
      end;
    end;
    dsTablesMapping.Next;
  end;
  dsTablesMapping.First;
end;

procedure TMainForm.lstCatalogClick(Sender: TObject);
begin
  lstSchema.Items.Clear;
  FDConnection.GetSchemaNames(lstCatalog.Items[lstCatalog.ItemIndex], '', lstSchema.Items);

end;

procedure TMainForm.mmConnectionParamsChange(Sender: TObject);
begin
  FDConnection.Close;
  lstSchema.Clear;
  lstCatalog.Clear;
end;

procedure TMainForm.ResetUI;
begin
  cboConnectionDefs.ItemIndex := -1;
  mmConnectionParams.Clear;
  lstCatalog.Clear;
  lstSchema.Clear;
  rgNameCase.ItemIndex := 0;
  rgFieldNameFormatting.ItemIndex := 0;
  chkGenerateMapping.Checked := False;
  dsTablesMapping.EmptyDataSet;
  mmOutput.Clear;
end;

procedure TMainForm.SaveProject;
var
  lJObj: TJSONObject;
  lField: TField;
begin
  fConfig.S[cboConnectionDefs.Name] := cboConnectionDefs.Items[cboConnectionDefs.ItemIndex];
  if lstCatalog.ItemIndex > -1 then
    fConfig.S[lstCatalog.Name] := lstCatalog.Items[lstCatalog.ItemIndex]
  else
    fConfig.Remove(lstCatalog.Name);

  if lstSchema.ItemIndex > -1 then
    fConfig.S[lstSchema.Name] := lstSchema.Items[lstSchema.ItemIndex]
  else
    fConfig.Remove(lstSchema.Name);

  fConfig.I[rgNameCase.Name] := rgNameCase.ItemIndex;
  fConfig.I[rgFieldNameFormatting.Name] := rgFieldNameFormatting.ItemIndex;
  fConfig.B[chkGenerateMapping.Name] := chkGenerateMapping.Checked;

  fConfig.Remove('tables');
  dsTablesMapping.First;
  while not dsTablesMapping.Eof do
  begin
    lJObj := fConfig.A['tables'].AddObject;
    for lField in dsTablesMapping.Fields do
    begin
      lJObj.S[lField.FieldName] := lField.AsString;
    end;
    dsTablesMapping.Next;
  end;
  dsTablesMapping.First;
  fConfig.SaveToFile(fProjectFileName, False);
end;

function TMainForm.SelectTables(const FromLetter, ToLetter: AnsiChar): Integer;
var
  lFirstChar: AnsiChar;
  lLetters: set of AnsiChar;
  I: Integer;
  lSelectedTables: Integer;
begin
  lLetters := [];
  for I := Ord(FromLetter) to Ord(ToLetter) do
  begin
    lLetters := lLetters + [Chr(I)];
  end;

  lSelectedTables := 0;
  dsTablesMapping.First;
  while not dsTablesMapping.Eof do
  begin
    lFirstChar := AnsiChar(dsTablesMappingTABLE_NAME.AsString.ToUpper.Chars[0]);
    if lFirstChar in lLetters then
    begin
      dsTablesMapping.Edit;
      dsTablesMappingGENERATE.Value := True;
      dsTablesMapping.Post;
      Inc(lSelectedTables);
    end;
    dsTablesMapping.Next;
  end;
  dsTablesMapping.First;
  Result := lSelectedTables;
end;

procedure TMainForm.SetProjectFileName(const Value: String);
begin
  fProjectFileName := TPath.ChangeExtension(Value, '.entgen');
  Caption := 'DMVCFramework Entities Generator :: ' + fProjectFileName;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  DoSelection(stAll);
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  DoSelection(stNone);
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  DoSelection(stInverse);
end;

procedure TMainForm.TabNextTab1AfterTabChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsTablesMapping then
  begin
    actRefreshTableList.Execute;
  end;
  if pcMain.ActivePage = tsGeneratedCode then
  begin
    actGenerateCode.Execute;
  end;

end;

procedure TMainForm.TabNextTab1Update(Sender: TObject);
begin
  if pcMain.ActivePage = tsConnectionDefinition then
  begin
    TabNextTab1.Enabled := (cboConnectionDefs.ItemIndex > -1) and  (
      (lstCatalog.Items.Count = 0)
      or
      ((lstCatalog.ItemIndex > -1) and (lstSchema.ItemIndex > -1))
      );
  end;
end;

end.
