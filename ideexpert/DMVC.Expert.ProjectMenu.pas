// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit DMVC.Expert.ProjectMenu;

// "DMVCFramework" submenu in the Project Manager local menu of a DMVC project:
// new REST controller (optionally through a service), new service, database
// connection for ActiveRecord, new web controller + view, new Minimal API route
// group, new TemplatePro view. Delphi 12 and later only (the packages of the older
// versions do not contain this unit).

interface

procedure RegisterProjectMenu;
procedure UnregisterProjectMenu;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Winapi.Windows,
  System.Math,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  ToolsAPI,
  DMVC.Expert.ProjectItems;

const
  // Verbs double as TMenuItem names: identifiers only
  MENU_VERB = 'DMVCFrameworkProjectMenu';

type
  TItemAction = reference to procedure(const AProject: IOTAProject);

  TDMVCProjectMenuItem = class(TNotifierObject, IOTALocalMenu, IOTAProjectManagerMenu)
  private
    fCaption, fName, fParent, fVerb: string;
    fPosition, fHelpContext: Integer;
    fChecked, fEnabled, fMultiSelectable: Boolean;
    fAction: TItemAction;
  public
    constructor Create(const ACaption, AVerb, AParent: string; APosition: Integer;
      const AAction: TItemAction);
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); overload;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;
  end;

  TDMVCProjectMenuNotifier = class(TNotifierObject, IOTAProjectMenuItemCreatorNotifier)
  public
    procedure AddMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
  end;

  // Name (+ URL segment, + model class) or view path, up to two options, OK/Cancel
  TItemDialog = class(TForm)
  private
    fViewMode, fWithModel, fWithResource, fResourceEdited, fModelEdited, fSettingDefaults: Boolean;
    edtName, edtResource, edtModel: TEdit;
    chkOption, chkOption2: TCheckBox;
    procedure NameChange(Sender: TObject);
    procedure ResourceChange(Sender: TObject);
    procedure ModelChange(Sender: TObject);
    procedure DialogCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor CreateDialog(const ATitle, AHint, AOptionCaption: string;
      AOptionDefault, AViewMode, AWithModel, AWithResource: Boolean; const AOption2Caption: string);
  end;

var
  GNotifierIndex: Integer = -1;

{ helpers }

function ProjectFolder(const AProject: IOTAProject): string;
begin
  Result := ExtractFilePath(AProject.FileName);
end;

function ProjectName(const AProject: IOTAProject): string;
begin
  Result := ChangeFileExt(ExtractFileName(AProject.FileName), '');
end;

// '' when the project is not a DMVC application
function DMVCProjectSource(const AProject: IOTAProject): string;
var
  lDpr: string;
begin
  Result := '';
  lDpr := ChangeFileExt(AProject.FileName, '.dpr');
  if TFile.Exists(lDpr) then
    Result := TFile.ReadAllText(lDpr);
  if not Result.Contains('MVCFramework') then
    Result := '';
end;

function SourceEditorOf(const AModule: IOTAModule): IOTASourceEditor;
var
  I: Integer;
begin
  for I := 0 to AModule.GetModuleFileCount - 1 do
    if Supports(AModule.GetModuleFileEditor(I), IOTASourceEditor, Result) then
      Exit;
  Result := nil;
end;

// The editor buffer is the source of truth while the file is open
function BufferText(const AEditor: IOTASourceEditor): string;
const
  CHUNK = 16384;
var
  lReader: IOTAEditReader;
  lBytes: TBytes;
  lPos, lRead: Integer;
begin
  lReader := AEditor.CreateReader;
  lPos := 0;
  repeat
    SetLength(lBytes, lPos + CHUNK);
    lRead := lReader.GetText(lPos, PAnsiChar(@lBytes[lPos]), CHUNK);
    Inc(lPos, lRead);
  until lRead < CHUNK;
  SetLength(lBytes, lPos);
  Result := TEncoding.UTF8.GetString(lBytes);
end;

function CurrentText(const AFileName: string): string;
var
  lModule: IOTAModule;
  lEditor: IOTASourceEditor;
begin
  lModule := (BorlandIDEServices as IOTAModuleServices).FindModule(AFileName);
  if Assigned(lModule) then
  begin
    lEditor := SourceEditorOf(lModule);
    if Assigned(lEditor) then
      Exit(BufferText(lEditor));
  end;
  Result := TFile.ReadAllText(AFileName);
end;

// Opens AFileName in the editor and applies the edits APlan computes on its buffer
function EditInIDE(const AFileName: string;
  const APlan: TFunc<string, TDMVCCodeEdits>): Boolean;
var
  lModule: IOTAModule;
  lEditor: IOTASourceEditor;
  lText: string;
  lEdits: TDMVCCodeEdits;
  lWriter: IOTAEditWriter;
  I, J: Integer;
  lEdit: TDMVCCodeEdit;
begin
  lModule := (BorlandIDEServices as IOTAModuleServices).OpenModule(AFileName);
  lEditor := SourceEditorOf(lModule);
  if not Assigned(lEditor) then
    Exit(False);
  lText := BufferText(lEditor);
  lEdits := APlan(lText);
  if Length(lEdits) = 0 then
    Exit(False);
  for I := 1 to High(lEdits) do // ascending: the writer only moves forward
    for J := I downto 1 do
      if lEdits[J].Offset < lEdits[J - 1].Offset then
      begin
        lEdit := lEdits[J];
        lEdits[J] := lEdits[J - 1];
        lEdits[J - 1] := lEdit;
      end;
  lWriter := lEditor.CreateUndoableWriter;
  for lEdit in lEdits do
  begin
    // the buffer is UTF-8: offsets are bytes
    lWriter.CopyTo(TEncoding.UTF8.GetByteCount(lText.Substring(0, lEdit.Offset)));
    lWriter.Insert(PAnsiChar(UTF8String(lEdit.Text)));
  end;
  lWriter := nil;
  lEditor.Show;
  Result := True;
end;

procedure OpenFile(const AFileName: string);
begin
  (BorlandIDEServices as IOTAActionServices).OpenFile(AFileName);
end;

function AddNewUnit(const AProject: IOTAProject; const AUnit: TDMVCNewUnit): string;
begin
  Result := TPath.Combine(ProjectFolder(AProject), AUnit.FileName);
  if TFile.Exists(Result) then
    raise Exception.CreateFmt('%s already exists', [Result]);
  TFile.WriteAllText(Result, AUnit.Source, TEncoding.UTF8);
  AProject.AddFile(Result, True);
  OpenFile(Result);
end;

function FindViewsFolder(const AProject: IOTAProject): string;
var
  lCandidate: string;
begin
  for lCandidate in ['bin\templates', 'templates'] do
  begin
    Result := TPath.Combine(ProjectFolder(AProject), lCandidate);
    if TDirectory.Exists(Result) then
      Exit;
  end;
  Result := '';
end;

function ViewsFolder(const AProject: IOTAProject): string;
begin
  Result := FindViewsFolder(AProject);
  if Result = '' then
    raise Exception.Create('Views folder not found (bin\templates or templates next to the project)');
end;

function WriteView(const AProject: IOTAProject; const AViewName, ATitle: string;
  AFragment: Boolean): string;
begin
  Result := TPath.Combine(ViewsFolder(AProject), AViewName.Replace('/', PathDelim) + '.html');
  if TFile.Exists(Result) then
    raise Exception.CreateFmt('%s already exists', [Result]);
  ForceDirectories(ExtractFilePath(Result));
  TFile.WriteAllText(Result, NewViewSource(AViewName, ATitle, AFragment), TEncoding.UTF8);
end;

// Tries every .pas of the project; the first one that accepts the edits wins
function WireIn(const AProject: IOTAProject; const AMarker: string;
  const APlan: TFunc<string, TDMVCCodeEdits>): string;
var
  I: Integer;
  lFile: string;
begin
  for I := 0 to AProject.GetModuleCount - 1 do
  begin
    lFile := AProject.GetModule(I).FileName;
    if not SameText(ExtractFileExt(lFile), '.pas') or not TFile.Exists(lFile) then
      Continue;
    if CurrentText(lFile).Contains(AMarker) and (Length(APlan(CurrentText(lFile))) > 0) and
      EditInIDE(lFile, APlan) then
      Exit(lFile);
  end;
  Result := '';
end;

// The .dpr or any unit, as the IDE has them now, sets up the OpenAPI document
function ProjectHasOpenAPI(const AProject: IOTAProject; AMinimalAPI: Boolean): Boolean;
var
  I: Integer;
  lFile: string;
begin
  if HasOpenAPI(DMVCProjectSource(AProject), AMinimalAPI) then
    Exit(True);
  for I := 0 to AProject.GetModuleCount - 1 do
  begin
    lFile := AProject.GetModule(I).FileName;
    if SameText(ExtractFileExt(lFile), '.pas') and TFile.Exists(lFile) and
      HasOpenAPI(CurrentText(lFile), AMinimalAPI) then
      Exit(True);
  end;
  Result := False;
end;

procedure ExplainManualStep(const AWhat, AUnitName, ALine: string);
begin
  MessageDlg(Format('%s was created, but no place to register it was found.' + sLineBreak + sLineBreak +
    'Add %s to the uses clause and this line where the others are:' + sLineBreak + sLineBreak + '  %s',
    [AWhat, AUnitName, ALine]), mtInformation, [mbOK], 0);
end;

{ actions }

function Ask(const ATitle, AHint, AOptionCaption: string; AOptionDefault, AViewMode, AWithModel,
  AWithResource: Boolean; const AOption2Caption: string;
  out AName, AResource, AModel: string; out AOption, AOption2: Boolean): Boolean; overload;
var
  lDialog: TItemDialog;
begin
  lDialog := TItemDialog.CreateDialog(ATitle, AHint, AOptionCaption, AOptionDefault, AViewMode, AWithModel,
    AWithResource, AOption2Caption);
  try
    Result := lDialog.ShowModal = mrOk;
    AName := Trim(lDialog.edtName.Text);
    AResource := Trim(lDialog.edtResource.Text);
    AModel := Trim(lDialog.edtModel.Text);
    AOption := lDialog.chkOption.Checked;
    AOption2 := lDialog.chkOption2.Checked;
  finally
    lDialog.Free;
  end;
end;

function Ask(const ATitle, AHint, AOptionCaption: string; AOptionDefault, AViewMode, AWithModel: Boolean;
  out AName, AResource, AModel: string; out AOption: Boolean): Boolean; overload;
var
  lDummy: Boolean;
begin
  Result := Ask(ATitle, AHint, AOptionCaption, AOptionDefault, AViewMode, AWithModel, True, '',
    AName, AResource, AModel, AOption, lDummy);
end;

procedure RegisterController(const AProject: IOTAProject; const AUnit: TDMVCNewUnit);
begin
  if WireIn(AProject, '.AddController(',
    function(S: string): TDMVCCodeEdits
    begin
      PlanControllerRegistration(S, AUnit.UnitName, AUnit.TypeName, Result);
    end) = '' then
    ExplainManualStep(AUnit.TypeName, AUnit.UnitName, 'AEngine.AddController(' + AUnit.TypeName + ');');
end;

// The file of the project that adds a TMVCActiveRecordMiddleware, or ''
function ActiveRecordHost(const AProject: IOTAProject): string;
var
  I: Integer;
  lFile: string;
begin
  for I := 0 to AProject.GetModuleCount - 1 do
  begin
    lFile := AProject.GetModule(I).FileName;
    if SameText(ExtractFileExt(lFile), '.pas') and TFile.Exists(lFile) and
      CurrentText(lFile).Contains('TMVCActiveRecordMiddleware') then
      Exit(lFile);
  end;
  Result := '';
end;

// The model class the new controller or service works on: the one the project already
// declares, wherever it is, or a new Entities.<Model>U (an ActiveRecord entity when the
// project has an ActiveRecord connection). AUnitName is the unit to use; AIsActiveRecord
// tells whether a service can implement its CRUD with ActiveRecord.
procedure EnsureModel(const AProject: IOTAProject; const AName, AModelClass, ATableName: string;
  out AUnitName: string; out AIsActiveRecord: Boolean);
var
  I: Integer;
  lFile: string;
  lModel: TDMVCNewUnit;
begin
  for I := 0 to AProject.GetModuleCount - 1 do
  begin
    lFile := AProject.GetModule(I).FileName;
    if SameText(ExtractFileExt(lFile), '.pas') and TFile.Exists(lFile) and
      DeclaresClass(CurrentText(lFile), AModelClass, AIsActiveRecord) then
    begin
      AUnitName := ChangeFileExt(ExtractFileName(lFile), '');
      Exit;
    end;
  end;
  AIsActiveRecord := ActiveRecordHost(AProject) <> '';
  lModel := NewModel(AName, AModelClass, ATableName, AIsActiveRecord);
  AddNewUnit(AProject, lModel);
  AUnitName := lModel.UnitName;
end;

procedure RegisterService(const AProject: IOTAProject; const AUnit: TDMVCNewUnit; const AInterfaceName: string);
begin
  if WireIn(AProject, 'RegisterServices',
    function(S: string): TDMVCCodeEdits
    begin
      PlanServiceRegistration(S, AUnit.UnitName, AUnit.TypeName, AInterfaceName, Result);
    end) = '' then
    ExplainManualStep(AUnit.TypeName, AUnit.UnitName,
      ServiceRegistrationLine('Container', AUnit.TypeName, AInterfaceName));
end;

procedure NewRestControllerAction(const AProject: IOTAProject);
var
  lName, lResource, lModel, lModelUnit: string;
  lCrud, lWithService, lModelIsAR: Boolean;
  lUnit, lService: TDMVCNewUnit;
begin
  if not Ask('New REST controller', 'Creates Controllers.<Name>U.pas under /api/<URL segment>, ' +
    'with the model class the request bodies are bound to (reused when the project declares it, otherwise ' +
    'created in Entities.<Model>U), and registers the controller.', 'CRUD actions',
    True, False, True, True, 'Through a service (Services.<Name>U, injected)',
    lName, lResource, lModel, lCrud, lWithService) then
    Exit;
  // the model, then the service: each unit uses the one before
  EnsureModel(AProject, lName, lModel, LowerCase(lName), lModelUnit, lModelIsAR);
  if lWithService then
  begin
    lService := NewService(lName, lModel, lModelUnit, True, lModelIsAR);
    AddNewUnit(AProject, lService);
    RegisterService(AProject, lService, ServiceInterfaceName(lName));
  end;
  lUnit := NewRestController(lName, lResource, lModel, lModelUnit, lCrud, ProjectHasOpenAPI(AProject, False),
    lWithService);
  AddNewUnit(AProject, lUnit);
  RegisterController(AProject, lUnit);
end;

// A list to pick one item from, OK/Cancel
function AskChoice(const ATitle, AHint, ALabel: string; const AChoices: TArray<string>;
  out AIndex: Integer): Boolean;
var
  lForm: TForm;
  lCombo: TComboBox;
  lPPI, lTop: Integer;
  lChoice: string;

  function S(AValue: Integer): Integer;
  begin
    Result := MulDiv(AValue, lPPI, 96);
  end;

  procedure AddLabel(const ACaption: string; AHeight: Integer; AWrap: Boolean);
  var
    lLabel: TLabel;
  begin
    lLabel := TLabel.Create(lForm);
    lLabel.Parent := lForm;
    lLabel.Caption := ACaption;
    lLabel.WordWrap := AWrap;
    lLabel.AutoSize := False; // AutoSize does not measure before the form has a handle
    lLabel.SetBounds(S(16), lTop, S(368), S(AHeight));
    lTop := lLabel.Top + lLabel.Height + S(4);
  end;

  procedure AddButton(const ACaption: string; ALeft: Integer; AResult: TModalResult; ADefault: Boolean);
  var
    lButton: TButton;
  begin
    lButton := TButton.Create(lForm);
    lButton.Parent := lForm;
    lButton.Caption := ACaption;
    lButton.SetBounds(S(ALeft), lTop, S(88), S(28));
    lButton.ModalResult := AResult;
    lButton.Default := ADefault;
    lButton.Cancel := not ADefault;
  end;

begin
  lForm := TForm.CreateNew(nil);
  try
    lPPI := Screen.MonitorFromPoint(Mouse.CursorPos).PixelsPerInch;
    lForm.Caption := ATitle;
    lForm.BorderStyle := bsDialog;
    lForm.Position := poScreenCenter;
    lForm.Font.Height := -MulDiv(9, lPPI, 72);
    lTop := S(16);
    AddLabel(AHint, 54, True);
    Inc(lTop, S(8));
    AddLabel(ALabel, 16, False);
    lCombo := TComboBox.Create(lForm);
    lCombo.Parent := lForm;
    lCombo.Style := csDropDownList;
    lCombo.SetBounds(S(16), lTop, S(368), S(24));
    for lChoice in AChoices do
      lCombo.Items.Add(lChoice);
    lCombo.ItemIndex := 0;
    lTop := lCombo.Top + lCombo.Height + S(16);
    AddButton('OK', 200, mrOk, True);
    AddButton('Cancel', 296, mrCancel, False);
    lForm.ClientWidth := S(400);
    lForm.ClientHeight := lTop + S(28) + S(16);
    lForm.ActiveControl := lCombo;
    Result := lForm.ShowModal = mrOk;
    AIndex := lCombo.ItemIndex;
  finally
    lForm.Free;
  end;
end;

procedure AddDatabaseAction(const AProject: IOTAProject);
var
  lHost, lEnv: string;
  lCaptions: TArray<string>;
  lDatabase: TDMVCDatabase;
  lIndex: Integer;
  lUnit: TDMVCNewUnit;
begin
  lHost := ActiveRecordHost(AProject);
  if lHost <> '' then
  begin
    MessageDlg('This project already has an ActiveRecord connection (' + ExtractFileName(lHost) + ').' +
      sLineBreak + sLineBreak + 'To use another database, change its connection definition and the two ' +
      'units ActiveRecord needs for it: the FireDAC driver (FireDAC.Phys.*) and the SQL generator ' +
      '(MVCFramework.SQLGenerators.*).', mtInformation, [mbOK], 0);
    Exit;
  end;
  lCaptions := [];
  for lDatabase in Databases do
    lCaptions := lCaptions + [lDatabase.Caption];
  if not AskChoice('Add database connection', 'Creates FDConnectionConfigU.pas with the FireDAC driver and ' +
    'the ActiveRecord SQL generator of the database, adds the db.* keys to bin\.env and the ActiveRecord ' +
    'middleware to the engine.', 'Database', lCaptions, lIndex) or (lIndex < 0) then
    Exit;
  lDatabase := Databases[lIndex];
  lUnit := NewDatabaseConfig(lDatabase, 'MainDB');
  AddNewUnit(AProject, lUnit);
  if WireIn(AProject, '.AddController(',
    function(S: string): TDMVCCodeEdits
    begin
      PlanDatabaseRegistration(S, lUnit.UnitName, Result);
    end) = '' then
    ExplainManualStep('FDConnectionConfigU', lUnit.UnitName + ' and MVCFramework.Middleware.ActiveRecord',
      DatabaseRegistrationLines('AEngine'));
  // not a unit: no IDE buffer to go through
  lEnv := TPath.Combine(ProjectFolder(AProject), 'bin\.env');
  if TFile.Exists(lEnv) and not TFile.ReadAllText(lEnv, TEncoding.UTF8).Contains('db.database=') then
    TFile.WriteAllText(lEnv, TFile.ReadAllText(lEnv, TEncoding.UTF8) + DatabaseEnvLines(lDatabase),
      TEncoding.UTF8);
end;

procedure NewServiceAction(const AProject: IOTAProject);
var
  lName, lDummyResource, lModel, lModelUnit: string;
  lCrud, lDummy, lModelIsAR: Boolean;
  lUnit: TDMVCNewUnit;
begin
  if not Ask('New service', 'Creates Services.<Name>U.pas with I<Name>Service and T<Name>Service, ' +
    'and registers it in RegisterServices. Inject it with [MVCInject]. The model class is reused when the ' +
    'project declares it, otherwise created in Entities.<Model>U.', 'CRUD methods over the model class',
    True, False, True, False, '', lName, lDummyResource, lModel, lCrud, lDummy) then
    Exit;
  lModelUnit := '';
  lModelIsAR := False;
  if lCrud then
    EnsureModel(AProject, lName, lModel, LowerCase(lName), lModelUnit, lModelIsAR);
  lUnit := NewService(lName, lModel, lModelUnit, lCrud, lModelIsAR);
  AddNewUnit(AProject, lUnit);
  RegisterService(AProject, lUnit, ServiceInterfaceName(lName));
end;

procedure NewWebControllerAction(const AProject: IOTAProject);
var
  lName, lResource, lDummyModel: string;
  lDummy: Boolean;
  lUnit: TDMVCNewUnit;
begin
  if not Ask('New web controller', 'Creates Controllers.<Name>U.pas under /web/<URL segment>, ' +
    'its page <URL segment>/index.html in the views folder, and registers the controller.', '', False, False,
    False, lName, lResource, lDummyModel, lDummy) then
    Exit;
  lUnit := NewWebController(lName, lResource, ProjectName(AProject));
  OpenFile(WriteView(AProject, lResource + '/index', lName, False));
  AddNewUnit(AProject, lUnit);
  RegisterController(AProject, lUnit);
end;

procedure NewRoutesAction(const AProject: IOTAProject);
var
  lName, lResource, lModel, lCallFmt: string;
  lCrud: Boolean;
  lUnit: TDMVCNewUnit;
begin
  if not Ask('New Minimal API route group', 'Creates <Name>RoutesU.pas with Map<Name>Routes and the ' +
    'model class the request bodies are bound to, mounted on /api/<URL segment> in ConfigureRoutes.',
    'CRUD routes', True, False, True, lName, lResource, lModel, lCrud) then
    Exit;
  lUnit := NewRoutesUnit(lName, lResource, lModel, lCrud, ProjectHasOpenAPI(AProject, True), lCallFmt);
  AddNewUnit(AProject, lUnit);
  if WireIn(AProject, 'ConfigureRoutes',
    function(S: string): TDMVCCodeEdits
    begin
      PlanRoutesRegistration(S, lUnit.UnitName, lCallFmt, Result);
    end) = '' then
    ExplainManualStep(lUnit.TypeName, lUnit.UnitName, Format(lCallFmt, ['ARoot']) + ';');
end;

procedure NewViewAction(const AProject: IOTAProject);
var
  lName, lDummy, lDummyModel: string;
  lFragment: Boolean;
begin
  if not Ask('New TemplatePro view', 'Path in the views folder, without extension ' +
    '(e.g. orders/index). A page extends baselayout.html; a fragment has no layout.',
    'HTMX fragment (no layout)', False, True, False, lName, lDummy, lDummyModel, lFragment) then
    Exit;
  OpenFile(WriteView(AProject, lName, lName.Substring(lName.LastIndexOf('/') + 1), lFragment));
end;

{ TDMVCProjectMenuItem }

constructor TDMVCProjectMenuItem.Create(const ACaption, AVerb, AParent: string;
  APosition: Integer; const AAction: TItemAction);
begin
  inherited Create;
  fCaption := ACaption;
  fVerb := AVerb;
  fName := AVerb;
  fParent := AParent;
  fPosition := APosition;
  fAction := AAction;
  fEnabled := True;
end;

procedure TDMVCProjectMenuItem.Execute(const MenuContextList: IInterfaceList);
var
  lContext: IOTAProjectMenuContext;
begin
  if not Assigned(fAction) or (MenuContextList.Count = 0) or
    not Supports(MenuContextList[0], IOTAProjectMenuContext, lContext) then
    Exit;
  try
    fAction(lContext.Project);
  except
    // an exception must not reach the IDE
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

function TDMVCProjectMenuItem.GetCaption: string;
begin
  Result := fCaption;
end;

function TDMVCProjectMenuItem.GetChecked: Boolean;
begin
  Result := fChecked;
end;

function TDMVCProjectMenuItem.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TDMVCProjectMenuItem.GetHelpContext: Integer;
begin
  Result := fHelpContext;
end;

function TDMVCProjectMenuItem.GetIsMultiSelectable: Boolean;
begin
  Result := fMultiSelectable;
end;

function TDMVCProjectMenuItem.GetName: string;
begin
  Result := fName;
end;

function TDMVCProjectMenuItem.GetParent: string;
begin
  Result := fParent;
end;

function TDMVCProjectMenuItem.GetPosition: Integer;
begin
  Result := fPosition;
end;

function TDMVCProjectMenuItem.GetVerb: string;
begin
  Result := fVerb;
end;

function TDMVCProjectMenuItem.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

function TDMVCProjectMenuItem.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

procedure TDMVCProjectMenuItem.SetCaption(const Value: string);
begin
  fCaption := Value;
end;

procedure TDMVCProjectMenuItem.SetChecked(Value: Boolean);
begin
  fChecked := Value;
end;

procedure TDMVCProjectMenuItem.SetEnabled(Value: Boolean);
begin
  fEnabled := Value;
end;

procedure TDMVCProjectMenuItem.SetHelpContext(Value: Integer);
begin
  fHelpContext := Value;
end;

procedure TDMVCProjectMenuItem.SetIsMultiSelectable(Value: Boolean);
begin
  fMultiSelectable := Value;
end;

procedure TDMVCProjectMenuItem.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TDMVCProjectMenuItem.SetParent(const Value: string);
begin
  fParent := Value;
end;

procedure TDMVCProjectMenuItem.SetPosition(Value: Integer);
begin
  fPosition := Value;
end;

procedure TDMVCProjectMenuItem.SetVerb(const Value: string);
begin
  fVerb := Value;
end;

{ TDMVCProjectMenuNotifier }

procedure TDMVCProjectMenuNotifier.AddMenu(const Project: IOTAProject; const IdentList: TStrings;
  const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
var
  lDpr: string;
  lMinimal, lViews: Boolean;
begin
  try
    if IsMultiSelect or not Assigned(Project) or (IdentList.IndexOf(sProjectContainer) < 0) then
      Exit;
    lDpr := DMVCProjectSource(Project);
    if lDpr = '' then
      Exit;
    lMinimal := IsMinimalAPIProject(lDpr);
    lViews := FindViewsFolder(Project) <> '';
  except
    Exit; // a menu that cannot be built is not worth an IDE error
  end;
  // only what can be wired into this kind of project
  ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('DMVCFramework', MENU_VERB, '',
    pmmpUserAdd, nil));
  if not lMinimal then
    ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('New REST Controller...',
      MENU_VERB + 'RestController', MENU_VERB, pmmpUserAdd + 10, NewRestControllerAction));
  // controllers and Minimal API handlers alike take services
  ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('New Service...',
    MENU_VERB + 'Service', MENU_VERB, pmmpUserAdd + 15, NewServiceAction));
  if not lMinimal then
    ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('Add Database Connection...',
      MENU_VERB + 'Database', MENU_VERB, pmmpUserAdd + 17, AddDatabaseAction));
  if not lMinimal and lViews then
    ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('New Web Controller and View...',
      MENU_VERB + 'WebController', MENU_VERB, pmmpUserAdd + 20, NewWebControllerAction));
  if lMinimal then
    ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('New Minimal API Route Group...',
      MENU_VERB + 'Routes', MENU_VERB, pmmpUserAdd + 30, NewRoutesAction));
  if lViews then
    ProjectManagerMenuList.Add(TDMVCProjectMenuItem.Create('New TemplatePro View...',
      MENU_VERB + 'View', MENU_VERB, pmmpUserAdd + 40, NewViewAction));
end;

{ TItemDialog }

constructor TItemDialog.CreateDialog(const ATitle, AHint, AOptionCaption: string;
  AOptionDefault, AViewMode, AWithModel, AWithResource: Boolean; const AOption2Caption: string);
var
  lPPI, lTop: Integer;

  function S(AValue: Integer): Integer;
  begin
    Result := MulDiv(AValue, lPPI, 96);
  end;

  function AddLabel(const ACaption: string; AWidth: Integer; AWrap: Boolean): TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := Self;
    Result.Caption := ACaption;
    Result.WordWrap := AWrap;
    // AutoSize does not measure before the form has a handle: fixed heights
    Result.AutoSize := False;
    Result.SetBounds(S(16), lTop, S(AWidth), S(IfThen(AWrap, 54, 16)));
    lTop := Result.Top + Result.Height + S(4);
  end;

  function AddEdit: TEdit;
  begin
    Result := TEdit.Create(Self);
    Result.Parent := Self;
    Result.SetBounds(S(16), lTop, S(368), S(24));
    lTop := Result.Top + Result.Height + S(12);
  end;

  function AddButton(const ACaption: string; ALeft: Integer; AResult: TModalResult): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Parent := Self;
    Result.Caption := ACaption;
    Result.SetBounds(S(ALeft), lTop, S(88), S(28));
    Result.ModalResult := AResult;
  end;

begin
  inherited CreateNew(nil);
  lPPI := Screen.MonitorFromPoint(Mouse.CursorPos).PixelsPerInch;
  fViewMode := AViewMode;
  fWithModel := AWithModel;
  fWithResource := AWithResource and not AViewMode;
  Caption := ATitle;
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Font.Height := -MulDiv(9, lPPI, 72);
  lTop := S(16);
  AddLabel(AHint, 368, True);
  Inc(lTop, S(8));
  if AViewMode then
    AddLabel('View path', 368, False)
  else
    AddLabel('Name (e.g. Orders)', 368, False);
  edtName := AddEdit;
  edtName.OnChange := NameChange;
  edtResource := TEdit.Create(Self);
  if fWithResource then
  begin
    AddLabel('URL segment', 368, False);
    edtResource.Parent := Self;
    edtResource.SetBounds(S(16), lTop, S(368), S(24));
    lTop := edtResource.Top + edtResource.Height + S(12);
    edtResource.OnChange := ResourceChange;
  end;
  edtModel := TEdit.Create(Self);
  if AWithModel then
  begin
    AddLabel('Model class', 368, False);
    edtModel.Parent := Self;
    edtModel.SetBounds(S(16), lTop, S(368), S(24));
    lTop := edtModel.Top + edtModel.Height + S(12);
    edtModel.OnChange := ModelChange;
  end;
  chkOption := TCheckBox.Create(Self);
  chkOption.Checked := AOptionDefault;
  if AOptionCaption <> '' then
  begin
    chkOption.Parent := Self;
    chkOption.Caption := AOptionCaption;
    chkOption.SetBounds(S(16), lTop, S(368), S(20));
    lTop := chkOption.Top + chkOption.Height + S(12);
  end;
  chkOption2 := TCheckBox.Create(Self);
  if AOption2Caption <> '' then
  begin
    chkOption2.Parent := Self;
    chkOption2.Caption := AOption2Caption;
    chkOption2.SetBounds(S(16), lTop, S(368), S(20));
    lTop := chkOption2.Top + chkOption2.Height + S(12);
  end;
  Inc(lTop, S(4));
  AddButton('OK', 200, mrOk).Default := True;
  AddButton('Cancel', 296, mrCancel).Cancel := True;
  ClientWidth := S(400);
  ClientHeight := lTop + S(28) + S(16);
  OnCloseQuery := DialogCloseQuery;
  ActiveControl := edtName;
end;

procedure TItemDialog.NameChange(Sender: TObject);
begin
  if fViewMode then
    Exit;
  fSettingDefaults := True;
  try
    if not fResourceEdited then
      edtResource.Text := LowerCase(Trim(edtName.Text));
    if fWithModel and not fModelEdited then
      if Trim(edtName.Text) = '' then
        edtModel.Text := ''
      else
        edtModel.Text := DefaultModelClass(Trim(edtName.Text));
  finally
    fSettingDefaults := False;
  end;
end;

procedure TItemDialog.ResourceChange(Sender: TObject);
begin
  if not fSettingDefaults then
    fResourceEdited := True;
end;

procedure TItemDialog.ModelChange(Sender: TObject);
begin
  if not fSettingDefaults then
    fModelEdited := True;
end;

procedure TItemDialog.DialogCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  lProblem: string;
begin
  if ModalResult <> mrOk then
    Exit;
  lProblem := '';
  if fViewMode then
  begin
    if not IsValidPathName(Trim(edtName.Text)) then
      lProblem := 'The view path takes letters, digits, "-", "_" and "/" (e.g. orders/index).';
  end
  else if not IsValidItemName(Trim(edtName.Text)) then
    lProblem := 'The name must be a Delphi identifier (e.g. Orders).'
  else if fWithResource and not IsValidPathName(Trim(edtResource.Text)) then
    lProblem := 'The URL segment takes letters, digits, "-", "_" and "/" (e.g. orders).'
  else if fWithModel and not IsValidItemName(Trim(edtModel.Text)) then
    lProblem := 'The model class must be a Delphi identifier (e.g. TOrder).'
  else if fWithModel and (SameText(Trim(edtModel.Text), 'TObject') or
    SameText(Trim(edtModel.Text), 'T' + Trim(edtName.Text) + 'Controller') or
    SameText(Trim(edtModel.Text), 'T' + Trim(edtName.Text) + 'Service') or
    SameText(Trim(edtModel.Text), ServiceInterfaceName(Trim(edtName.Text)))) then
    // the new unit would declare it and hide the one it needs
    lProblem := 'The model class needs a name of its own (e.g. TOrder).';
  CanClose := lProblem = '';
  if not CanClose then
    MessageDlg(lProblem, mtWarning, [mbOK], 0);
end;

{ registration }

procedure RegisterProjectMenu;
var
  lManager: IOTAProjectManager;
begin
  if (GNotifierIndex < 0) and Supports(BorlandIDEServices, IOTAProjectManager, lManager) then
    GNotifierIndex := lManager.AddMenuItemCreatorNotifier(TDMVCProjectMenuNotifier.Create);
end;

procedure UnregisterProjectMenu;
var
  lManager: IOTAProjectManager;
begin
  if (GNotifierIndex >= 0) and Supports(BorlandIDEServices, IOTAProjectManager, lManager) then
    lManager.RemoveMenuItemCreatorNotifier(GNotifierIndex);
  GNotifierIndex := -1;
end;

initialization

finalization
  UnregisterProjectMenu;

end.
