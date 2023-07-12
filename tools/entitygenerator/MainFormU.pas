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
  Vcl.ExtActns, System.ImageList, Vcl.ImgList,
  LoggerPro.FileAppender,
  LoggerPro.VCLListBoxAppender,
  LoggerPro;

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
    TabNextTab1: TNextTab;
    TabPreviousTab1: TPreviousTab;
    actSaveGeneratedCode: TAction;
    actGenerateCode: TAction;
    Panel10: TPanel;
    btnGetTables: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    actRefreshTableList: TAction;
    Label3: TLabel;
    FileSaveDialogProject: TFileSaveDialog;
    actNewProject: TAction;
    NewProject1: TMenuItem;
    ImageListMainMenu: TImageList;
    ImageListButtons: TImageList;
    qryMeta: TFDMetaInfoQuery;
    Entities1: TMenuItem;
    RefreshCatalog1: TMenuItem;
    RefreshTableList1: TMenuItem;
    GenerateCode1: TMenuItem;
    SaveGeneratedCode1: TMenuItem;
    Panel12: TPanel;
    lbLog: TListBox;
    Splitter1: TSplitter;
    EditTableNameFilter: TEdit;
    Label4: TLabel;
    Panel5: TPanel;
    Label6: TLabel;
    btnSaveAs: TSpeedButton;
    EditOutputFileName: TEdit;
    Button6: TButton;
    gbOptions: TGroupBox;
    chkClassAsAbstract: TCheckBox;
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
    procedure TabNextTab1AfterTabChange(Sender: TObject);
    procedure actRefreshTableListExecute(Sender: TObject);
    procedure TabNextTab1Update(Sender: TObject);
    procedure actSaveProjectExecute(Sender: TObject);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actGenerateCodeUpdate(Sender: TObject);
    procedure actRefreshTableListUpdate(Sender: TObject);
    procedure EditTableNameFilterChange(Sender: TObject);
  private
    fConfig: TJSONObject;
    fCatalog: string;
    fProjectFileName: string;
    fSchema: string;
    fIntfBuff, fImplBuff, fInitializationBuff: TStringStream;
    FHistoryFileName: string;
    lTypesName: TArray<string>;
    fBookmark: TArray<Byte>;
    Log: ILogWriter;
    procedure OpenMetaDS(const Catalog, Schema, TableName: String);
    function GetCurrentColumnAttribute: TFDDataAttributes;
    procedure ResetUI;
    procedure LoadProjectFromFile;
    procedure SaveProject;
    function SelectTables(const FromLetter: AnsiChar; const ToLetter: AnsiChar): Integer;
    procedure EmitHeaderComments;
    function GetClassName(const aTableName: string): string;
    function GetOutputFileName(out OutputFileName: String): Boolean;
//    function GetUniqueFieldNames(const Fields: TFields; const FormatAsPascalCase: Boolean): TArray<String>;
    function GetUniqueFieldNames(const MetaDS: TFDMetaInfoQuery; const FormatAsPascalCase: Boolean): TArray<String>;
    procedure EmitUnit(const UnitName: String);
    procedure EmitUnitEnd;
    procedure EmitProperty(const FieldName: String; const ColumnAttribs: TFDDataAttributes; const FieldDataType: TFDDataType; const IsPK: Boolean);
    procedure EmitField(const DatabaseFieldName: String; const UniqueFieldName: String;
      const FieldDataType: TFDDataType; const ColumnAttribs: TFDDataAttributes; const IsPK: Boolean);
    procedure EmitClass(const aTableName, aClassName, aNameCase: string; const IsAbstract: Boolean);
    procedure EmitClassEnd;
    function GetDelphiType(const FireDACType: TFDDataType; const ColumnAttribs: TFDDataAttributes; const ForceNullable: Boolean = False): string;
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

  META_F_TABLE_NAME = 'TABLE_NAME';
  META_F_COLUMN_NAME = 'COLUMN_NAME';
  META_F_COLUMN_DATATYPE = 'COLUMN_DATATYPE';
  META_F_COLUMN_TYPENAME = 'COLUMN_TYPENAME';
  META_F_COLUMN_ATTRIBUTES = 'COLUMN_ATTRIBUTES';
  META_F_COLUMN_PRECISION = 'COLUMN_PRECISION';
  META_F_COLUMN_SCALE = 'COLUMN_SCALE';
  META_F_COLUMN_LENGTH = 'COLUMN_LENGTH';

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
  lFieldDataType: TFDDataType;
  lFieldName: string;
  lColAttrib: TFDDataAttributes;
  lOutputFileName: string;
  lUnitName: string;
  lGeneratedEntities: Integer;
  lIsAbstract: Boolean;
begin
//https://docwiki.embarcadero.com/RADStudio/Sydney/en/Metadata_Structure_(FireDAC)
//https://docwiki.embarcadero.com/Libraries/Sydney/en/FireDAC.Stan.Intf.TFDDataAttribute
  if not GetOutputFileName(lOutputFileName) then Exit;
//  SaveProject;
  Log.Info('Starting entities generation', LOG_TAG);
  lUnitName := TPath.GetFileNameWithoutExtension(lOutputFileName);
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitializationBuff.Clear;
  lKeyFields := TStringList.Create;
  try
    EmitHeaderComments;
    EmitUnit(lUnitName);
    dsTablesMapping.First;
    lGeneratedEntities := 0;
    while not dsTablesMapping.Eof do
    begin
      if not dsTablesMappingGENERATE.Value then
      begin
        Log.Info('Skipping table %s', [dsTablesMappingTABLE_NAME.AsString], LOG_TAG);
        dsTablesMapping.Next;
        Continue;
      end;
      Inc(lGeneratedEntities);
      lTableName := dsTablesMappingTABLE_NAME.AsString;
      Log.Info('Generating entity [%s] for table [%s]', [
        dsTablesMappingCLASS_NAME.AsString,
        dsTablesMappingTABLE_NAME.AsString
      ], LOG_TAG);
      lClassName := dsTablesMappingCLASS_NAME.AsString;
      lIsAbstract := chkClassAsAbstract.Checked;
      if lIsAbstract then
      begin
        lClassName := lClassName.Chars[0] + 'Custom' + lClassName.Substring(1);
      end;

      EmitClass(lTableName, lClassName, rgNameCase.Items[rgNameCase.ItemIndex], lIsAbstract);
      lKeyFields.Clear;
      FDConnection.GetKeyFieldNames(fCatalog, fSchema, lTableName, '', lKeyFields);

      OpenMetaDS(fCatalog, fSchema, lTableName);

      lFieldNamesToInitialize := [];
      lTypesName := [];
      fIntfBuff.WriteString(INDENT + 'private' + sLineBreak);
      lUniqueFieldNames := GetUniqueFieldNames(qryMeta, rgFieldNameFormatting.ItemIndex = 1);

      I := 0;
      qryMeta.First;
      while not qryMeta.Eof do
      begin
        lColAttrib := GetCurrentColumnAttribute;
        lFieldDataType := TFDDataType(qryMeta.FieldByName(META_F_COLUMN_DATATYPE).AsInteger);
        lFieldName := qryMeta.FieldByName(META_F_COLUMN_NAME).AsString;
        EmitField(
          lFieldName,
          lUniqueFieldNames[I],
          lFieldDataType,
          lColAttrib,
          lKeyFields.IndexOf(qryMeta.FieldByName(META_F_COLUMN_NAME).AsString) > -1);

        if GetDelphiType(lFieldDataType, lColAttrib) = 'TStream' then
        begin
          lFieldNamesToInitialize := lFieldNamesToInitialize + [GetFieldName(lUniqueFieldNames[I])];
          lTypesName := lTypesName + ['TMemoryStream'];
        end;
        Inc(I);
        qryMeta.Next;
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

      qryMeta.First;
      I := 0;
      while not qryMeta.Eof do
      begin
        lFieldDataType := TFDDataType(qryMeta.FieldByName(META_F_COLUMN_DATATYPE).AsInteger);
        lColAttrib := GetCurrentColumnAttribute;
        EmitProperty(
          lUniqueFieldNames[I],
          lColAttrib,
          lFieldDataType,
          lKeyFields.IndexOf(qryMeta.FieldByName(META_F_COLUMN_NAME).AsString) > -1);
        Inc(I);
        qryMeta.Next;
      end;

      EmitClassEnd;
      dsTablesMapping.Next;
    end;
    EmitUnitEnd;

//    mmOutput.Lines.Text := fIntfBuff.DataString + fImplBuff.DataString + fInitializationBuff.DataString;

    TFile.WriteAllText(lOutputFileName,
      fIntfBuff.DataString + fImplBuff.DataString + fInitializationBuff.DataString);

  finally
    lKeyFields.Free;
  end;
  // mmOutput.Lines.SaveToFile(
  // mmConnectionParams.Lines.SaveToFile(FHistoryFileName);
  //ShowMessage('Generation Completed');
//  TabNextTab1.Execute;
  Log.Info('Generated %d entities', [lGeneratedEntities],  LOG_TAG);
end;

procedure TMainForm.actGenerateCodeUpdate(Sender: TObject);
begin
  actGenerateCode.Enabled := not String(EditOutputFileName.Text).IsEmpty;
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

procedure TMainForm.actRefreshTableListExecute(Sender: TObject);
var
  lTables: TStringList;
  lSelectedTables: TStringList;
  lTable: string;
  lClassName: string;
begin
  dsTablesMapping.DisableControls;
  try
    lSelectedTables := TStringList.Create;
    try
      if dsTablesMapping.RecordCount > 0 then
      begin
        dsTablesMapping.First;
        while not dsTablesMapping.Eof do
        begin
          if dsTablesMappingGENERATE.Value then
          begin
            lSelectedTables.Add(dsTablesMappingTABLE_NAME.AsString);
          end;
          dsTablesMapping.Next;
        end;
        lSelectedTables.Sorted := True;
      end;

      FDConnection.Connected := True;
      lTables := TStringList.Create;
      try
        fCatalog := FDConnection.Params.Database;
        fSchema := '';
        if lstSchema.ItemIndex > 0 then //at index 0 there is "<all>"
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
          dsTablesMapping.AppendRecord([(lSelectedTables.IndexOf(lTable) > -1), lTable, lClassName]);
        end;
        dsTablesMapping.First;
      finally
        lTables.Free;
      end;
    finally
      lSelectedTables.Free;
    end;
    TabSheet1.Caption := 'Tables (' + dsTablesMapping.RecordCount.ToString + ')';
  finally
    dsTablesMapping.EnableControls;
  end;
end;

procedure TMainForm.actRefreshTableListUpdate(Sender: TObject);
begin
  actRefreshTableList.Enabled := cboConnectionDefs.ItemIndex >= 0;
end;

procedure TMainForm.actSaveGeneratedCodeExecute(Sender: TObject);
begin
  FileSaveDialog1.FileName := 'EntitiesU.pas';
  if FileSaveDialog1.Execute then
  begin
    EditOutputFileName.Text := FileSaveDialog1.FileName;
    fConfig.S[EditOutputFileName.Name] := EditOutputFileName.Text;
  end;
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
  Log.Info('Selecting ConnectionDef: ' + cboConnectionDefs.Text, LOG_TAG);
  FDConnection.Close;
  FDManager.GetConnectionDefParams(cboConnectionDefs.Text, mmConnectionParams.Lines);
  lstSchema.Items.Clear;
  FDConnection.Params.Clear;
  FDConnection.Params.Text := mmConnectionParams.Text;
  FDConnection.Open;
  lstSchema.Items.Clear;
  FDConnection.GetSchemaNames(FDConnection.Params.Database, '', lstSchema.Items);
  lstSchema.Items.Insert(0, '<all>');
  lstSchema.ItemIndex := 0;

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

procedure TMainForm.EditTableNameFilterChange(Sender: TObject);
begin
  if EditTableNameFilter.Text <> '' then
  begin
    dsTablesMapping.Filter := 'upper(TABLE_NAME) like ''' + String(EditTableNameFilter.Text).ToUpper + '%'' ';
    dsTablesMapping.Filtered := True;
  end
  else
  begin
    dsTablesMapping.Filtered := False;
  end;
  TabSheet1.Caption := 'Tables (' + dsTablesMapping.RecordCount.ToString + ')';
end;

procedure TMainForm.EmitClass(const aTableName, aClassName, aNameCase: string; const IsAbstract: Boolean);
var
  lAbstract: string;
begin
  fIntfBuff.WriteString(INDENT + '[MVCNameCase(nc' + aNameCase + ')]' + sLineBreak);
  fIntfBuff.WriteString(INDENT + Format('[MVCTable(''%s'')]', [aTableName]) + sLineBreak);
  if trim(aClassName) = '' then
    raise Exception.Create('Invalid class name');
  lAbstract := '';
  if IsAbstract then
    lAbstract := ' abstract';
  fIntfBuff.WriteString(INDENT + aClassName + ' = class' + lAbstract + '(TMVCActiveRecord)' + sLineBreak);
  if chkGenerateMapping.Checked then
    fInitializationBuff.WriteString(Format('ActiveRecordMappingRegistry.AddEntity(''%s'', %s);',
      [aTableName.ToLower, aClassName]) + sLineBreak);
end;

procedure TMainForm.EmitClassEnd;
begin
  fIntfBuff.WriteString(INDENT + 'end;' + sLineBreak + sLineBreak);
end;

procedure TMainForm.EmitField(const DatabaseFieldName: String; const UniqueFieldName: String;
  const FieldDataType: TFDDataType; const ColumnAttribs: TFDDataAttributes; const IsPK: Boolean);
var
  lRTTIAttrib, lField: string;
  lColType: string;
begin
  if IsPK then
  begin
    if caAutoInc in ColumnAttribs then
      lRTTIAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey, foAutoGenerated])]', [DatabaseFieldName])
    else
      lRTTIAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey])]', [DatabaseFieldName])
  end
  else
  begin
    lColType := qryMeta.FieldByName(META_F_COLUMN_TYPENAME).AsString.ToLower;
    if lColType.Contains('json') or lColType.Contains('xml') then
      lRTTIAttrib := Format('[MVCTableField(''%s'', [], ''%s'')]', [DatabaseFieldName, lColType])
    else
      lRTTIAttrib := Format('[MVCTableField(''%s'')]', [DatabaseFieldName])
  end;

  if IsPK and (caAutoInc in ColumnAttribs) then
  begin
    lField := GetFieldName(UniqueFieldName) + ': ' + GetDelphiType(FieldDataType, ColumnAttribs, True) + ';' + sLineBreak;
  end
  else
    lField := GetFieldName(UniqueFieldName) + ': ' + GetDelphiType(FieldDataType, ColumnAttribs) + ';' + sLineBreak;


  if GetDelphiType(FieldDataType, ColumnAttribs).ToUpper.Contains('UNSUPPORTED TYPE') then
  begin
    lRTTIAttrib := '//' + lRTTIAttrib;
    lField := '//' + lField;
  end
  else
  begin
    lField := '  ' + lField;
    lRTTIAttrib := '  ' + lRTTIAttrib;
  end;
  fIntfBuff.WriteString(INDENT + lRTTIAttrib + sLineBreak + INDENT + lField);
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

procedure TMainForm.EmitProperty(
  const FieldName: String; const ColumnAttribs: TFDDataAttributes;
  const FieldDataType: TFDDataType; const IsPK: Boolean);
var
  lProp: string;
begin
  if IsPK then
  begin
    lProp := lProp + 'property ' + GetFieldName(FieldName).Substring(1) { remove f } + ': ' +
      GetDelphiType(FieldDataType, ColumnAttribs, [caAllowNull,caAutoInc] * ColumnAttribs <> [])
      + ' read ' + GetFieldName(FieldName) + ' write ' + GetFieldName(FieldName) + ';' + sLineBreak;
  end
  else
  begin
    lProp := lProp + 'property ' + GetFieldName(FieldName).Substring(1) { remove f } + ': ' +
      GetDelphiType(FieldDataType, ColumnAttribs)
      + ' read ' + GetFieldName(FieldName) + ' write ' + GetFieldName(FieldName) + ';' + sLineBreak;
  end;

  if GetDelphiType(FieldDataType, ColumnAttribs).ToUpper.Contains('UNSUPPORTED TYPE') then
  begin
    lProp := '  //' + lProp
  end
  else
  begin
    lProp := '  ' + lProp;
  end;
  fIntfBuff.WriteString(INDENT + lProp)
end;

procedure TMainForm.EmitUnit(const UnitName: String);
begin
  fIntfBuff.WriteString('unit ' + UnitName + ';' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);
  fIntfBuff.WriteString('interface' + sLineBreak);
  fIntfBuff.WriteString('' + sLineBreak);
  fIntfBuff.WriteString('uses' + sLineBreak);
  fIntfBuff.WriteString('  MVCFramework.Serializer.Commons,' + sLineBreak);
  fIntfBuff.WriteString('  MVCFramework.Nullables,' + sLineBreak);
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
var
  UILogFormat: String;
begin
  UILogFormat := '%0:s [%2:-10s] %3:s';
  Log := BuildLogWriter([
    TLoggerProFileAppender.Create,
    TVCLListBoxAppender.Create(lbLog, 2000, UILogFormat)
    ]);
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
  lTableName := lTableName.ToLower.DeQuotedString('"').Replace('.', '__', [rfReplaceAll]);
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

function TMainForm.GetCurrentColumnAttribute: TFDDataAttributes;
var
  i: Integer;
begin
{
TFDDataAttribute = (caSearchable, caAllowNull, caFixedLen,
caBlobData, caReadOnly, caAutoInc, caROWID, caDefault,
caRowVersion, caInternal, caCalculated, caVolatile, caUnnamed,
caVirtual, caBase, caExpr);
}
  i := qryMeta.FieldByName('COLUMN_ATTRIBUTES').AsInteger;
  Result := TFDDataAttributes(Pointer(@i)^);
end;

function TMainForm.GetDelphiType(const FireDACType: TFDDataType; const ColumnAttribs: TFDDataAttributes; const ForceNullable: Boolean): string;
begin
  case FireDACType of
    dtWideString, dtWideMemo:
      Result := 'String';
    dtAnsiString, dtMemo:
      Result := 'String';
    dtByte:
      Result := 'Byte';
    dtInt16:
      Result := 'Int16';
    dtUInt16:
      Result := 'UInt16';
    dtInt32:
      Result := 'Int32';
    dtUInt32:
      Result := 'UInt32';
    dtInt64:
      Result := 'Int64';
    dtUInt64:
      Result := 'UInt64';
    dtBoolean:
      Result := 'Boolean';
    dtDouble,  dtExtended:
      Result := 'Double';
    dtSingle:
      Result := 'Single';
    dtCurrency, dtBCD, dtFmtBCD:
      Result := 'Currency';
    dtDate:
      Result := 'TDate';
    dtTime:
      Result := 'TTime';
    dtDateTime:
      Result := 'TDateTime';
    dtTimeIntervalFull:
      Result := 'TDateTime {dtTimeIntervalFull}';
    dtDateTimeStamp:
      Result := 'TDateTime {dtDateTimeStamp}';

//    dtAutoInc:
//      Result := 'Integer {autoincrement}';
    dtBlob: //, { ftMemo, } dtGraphic, { ftFmtMemo, ftWideMemo, } dtStream:
      Result := 'TStream';
//    dtFixedChar:
//      Result := 'String {fixedchar}';
//    ftWideString:
//      Result := 'String';
    dtXML:
      Result := 'String {XML}';
    dtGuid:
      Result := 'TGuid';
//    dtDBaseOle:
//      Result := 'String {ftDBaseOle}';
  else
    Result := '<UNSUPPORTED TYPE: ' + GetEnumName(TypeInfo(TFDDataType), Ord(FireDACType)) + '>';
  end;

  if ForceNullable or ((Result <> 'TStream') and  (caAllowNull in ColumnAttribs)) then
  begin
    Result := 'Nullable' + Result;
  end;

end;

function TMainForm.GetFieldName(const Value: string): string;
begin
  if Value.Length <= 2 then
  begin
    Exit('f' + Value.ToUpper);
  end;
  Result := 'f' + Value;
end;

function TMainForm.GetOutputFileName(out OutputFileName: String): Boolean;
var
  lFName: String;
begin
  lFName := EditOutputFileName.Text;
  if lFName.IsEmpty then
  begin
    FileSaveDialog1.FileName := 'EntitiesU.pas';
    if FileSaveDialog1.Execute then
    begin
      EditOutputFileName.Text := FileSaveDialog1.FileName;
      EditOutputFileName.Update;
      lFName := EditOutputFileName.Text;
    end;
  end;
  OutputFileName := lFName;
  Result := True;
end;

function TMainForm.GetProjectFileExists: Boolean;
begin
  Result := TFile.Exists(ProjectFileName);
end;

function TMainForm.GetUniqueFieldNames(const MetaDS: TFDMetaInfoQuery;
  const FormatAsPascalCase: Boolean): TArray<String>;
var
  I: Integer;
  lList: TStringList;
  lF: string;
  lFTemp: string;
  lCount: Integer;
  lFieldName: String;
begin
  MetaDS.First;
  SetLength(Result, MetaDS.RecordCount);
  lList := TStringList.Create;
  try
    lList.Sorted := True;
    I := 0;
    while not MetaDS.Eof do
    begin
      lFieldName := MetaDS.FieldByName(META_F_COLUMN_NAME).AsString;
      lCount := 0;
      if FormatAsPascalCase then
      begin
        lF := CamelCase(lFieldName, True);
      end
      else
      begin
        lF := lFieldName;
      end;
      if lList.IndexOf(lF) > -1 then
      begin
        lF := lFieldName;
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
      Inc(I);
      MetaDS.Next;
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

//  if fConfig.IndexOf(lstCatalog.Name) > -1 then
//  begin
//    lstCatalog.ItemIndex := lstCatalog.Items.IndexOf(fConfig.S[lstCatalog.Name]);
//    lstCatalogClick(self);
//    lstSchema.ItemIndex := lstSchema.Items.IndexOf(fConfig.S[lstSchema.Name]);
//  end;
  lstSchema.ItemIndex := lstSchema.Items.IndexOf(fConfig.S[lstSchema.Name]);
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

  EditOutputFileName.Text := fConfig.S[EditOutputFileName.Name];
end;

procedure TMainForm.mmConnectionParamsChange(Sender: TObject);
begin
  FDConnection.Close;
  lstSchema.Clear;
end;

procedure TMainForm.OpenMetaDS(const Catalog, Schema, TableName: String);
begin
  qryMeta.Close;
  qryMeta.MetaInfoKind := mkTableFields;
  qryMeta.ObjectName := TableName;
  qryMeta.SchemaName := Schema;
  qryMeta.CatalogName := Catalog;
  qryMeta.Open;
  qryMeta.FetchAll;
  qryMeta.First;
end;

procedure TMainForm.ResetUI;
begin
  cboConnectionDefs.ItemIndex := -1;
  mmConnectionParams.Clear;
  lstSchema.Clear;
  rgNameCase.ItemIndex := 0;
  rgFieldNameFormatting.ItemIndex := 0;
  chkGenerateMapping.Checked := False;
  dsTablesMapping.EmptyDataSet;
end;

procedure TMainForm.SaveProject;
var
  lJObj: TJSONObject;
  lField: TField;
begin
  fConfig.S[cboConnectionDefs.Name] := cboConnectionDefs.Items[cboConnectionDefs.ItemIndex];
  if lstSchema.ItemIndex > -1 then
    fConfig.S[lstSchema.Name] := lstSchema.Items[lstSchema.ItemIndex]
  else
    fConfig.Remove(lstSchema.Name);

  fConfig.I[rgNameCase.Name] := rgNameCase.ItemIndex;
  fConfig.I[rgFieldNameFormatting.Name] := rgFieldNameFormatting.ItemIndex;
  fConfig.B[chkGenerateMapping.Name] := chkGenerateMapping.Checked;
  fConfig.S[EditOutputFileName.Name] := EditOutputFileName.Text;

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
  Caption := Format('DMVCFramework Entities Generator :: [%0:s] - DMVCFramework-%1:s', [fProjectFileName, DMVCFRAMEWORK_VERSION]);
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
end;

procedure TMainForm.TabNextTab1Update(Sender: TObject);
begin
  if pcMain.ActivePage = tsConnectionDefinition then
  begin
    TabNextTab1.Enabled := (cboConnectionDefs.ItemIndex > -1);
  end
  else
  begin
    TabNextTab1.Enabled := False;
  end;
end;

end.
