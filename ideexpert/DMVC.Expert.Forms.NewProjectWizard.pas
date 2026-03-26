// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************

unit DMVC.Expert.Forms.NewProjectWizard;

{$I ..\sources\dmvcframework.inc}

interface

uses
  WinAPI.Windows,
  WinAPI.Messages,
  WinAPI.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.Imaging.pngimage,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  System.Actions,
  Vcl.ActnList,
  Vcl.AppEvnts,
  JsonDataObjects;

type
  TfrmDMVCNewProject = class(TForm)
    Shape1: TShape;
    Image1: TImage;
    lblFrameworkVersion: TLabel;
    lblBook: TLabel;
    lblCopyRight: TLabel;
    Shape2: TShape;
    Image2: TImage;
    lblPATREON: TLabel;
    lblPageTitle: TLabel;
    lblPageHint: TLabel;
    pcWizard: TPageControl;
    tsAppType: TTabSheet;
    tsServer: TTabSheet;
    tsFeatures: TTabSheet;
    tsOptions: TTabSheet;
    // tsAppType controls
    rgApplicationType: TRadioGroup;
    lblAppTypeDescription: TLabel;
    // tsServer controls
    rgServerProtocol: TRadioGroup;
    lblServerPort: TLabel;
    edtServerPort: TEdit;
    btnTestPort: TButton;
    lblProtocolDescription: TLabel;
    lblProjectName: TLabel;
    edtProjectName: TEdit;
    lblProjectFolder: TLabel;
    edtProjectFolder: TEdit;
    btnBrowseFolder: TButton;
    // tsFeatures controls
    gbControllerUnitOptions: TGroupBox;
    chkCreateIndexMethod: TCheckBox;
    chkCreateActionFiltersMethods: TCheckBox;
    chkCreateCRUDMethods: TCheckBox;
    chkProfileActions: TCheckBox;
    gbAdditionalFeatures: TGroupBox;
    lblSSV: TLabel;
    lblSessionType: TLabel;
    lblJSONRPCClassName: TLabel;
    cbSSV: TComboBox;
    cbSessionType: TComboBox;
    chkJSONRPC: TCheckBox;
    EdtJSONRPCClassName: TEdit;
    chkWebSocketServer: TCheckBox;
    chkServicesContainer: TCheckBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Bevel1: TBevel;
    Label5: TLabel;
    chkCompression: TCheckBox;
    chkAnalyticsMiddleware: TCheckBox;
    chkStaticFiles: TCheckBox;
    chkCORS: TCheckBox;
    chkTrace: TCheckBox;
    chkETAG: TCheckBox;
    chkRateLimit: TCheckBox;
    chkJWT: TCheckBox;
    chkActiveRecord: TCheckBox;
    EdtFDConnDefFileName: TEdit;
    EdtConnDefName: TEdit;
    // tsOptions controls
    lblNameCase: TLabel;
    lblClassName: TLabel;
    lblWbModule: TLabel;
    cbNameCase: TComboBox;
    edtControllerClassName: TEdit;
    edtWebModuleName: TEdit;
    chkMSHeap: TCheckBox;
    chkSqids: TCheckBox;
    chkHtmx: TCheckBox;
    chkCustomConfigDotEnv: TCheckBox;
    chkCreateSubfolder: TCheckBox;
    lblSummary: TLabel;
    // Buttons
    btnBack: TButton;
    btnNext: TButton;
    btnFinish: TButton;
    btnCancel: TButton;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure lblBookClick(Sender: TObject);
    procedure lblBookMouseEnter(Sender: TObject);
    procedure lblBookMouseLeave(Sender: TObject);
    procedure lblFrameworkVersionClick(Sender: TObject);
    procedure lblFrameworkVersionMouseEnter(Sender: TObject);
    procedure lblFrameworkVersionMouseLeave(Sender: TObject);
    procedure lblPATREONClick(Sender: TObject);
    procedure lblPATREONMouseEnter(Sender: TObject);
    procedure lblPATREONMouseLeave(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure btnFinishClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBrowseFolderClick(Sender: TObject);
    procedure rgApplicationTypeClick(Sender: TObject);
    procedure rgServerProtocolClick(Sender: TObject);
    procedure btnTestPortClick(Sender: TObject);
    procedure chkCreateSubfolderClick(Sender: TObject);
  private
    fModel: TJsonObject;
    fCurrentPage: Integer;
    fIsCustomPreset: Boolean;
    fPresetCaption: string;
    procedure UpdateSummary;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
    function GetServerPort: Integer;
    function GetCreateJSONRPCInterface: boolean;
    function GetJSONRPCClassName: String;
    function GetBaseFolder: string;
    function GetProjectName: string;
    function GetProjectFolder: string;
    procedure UpdateProjectNameHint;
    procedure NavigateToPage(APageIndex: Integer);
    function GetPageCount: Integer;
    function MapPageIndex(AVisualIndex: Integer): Integer;
    function ValidateCurrentPage: Boolean;
  public
    property ControllerClassName: string read GetControllerClassName;
    property WebModuleClassName: string read GetWebModuleClassName;
    property ProjectName: string read GetProjectName;
    property ProjectFolder: string read GetProjectFolder;
    procedure SetCustomMode(AIsCustom: Boolean);
    procedure SetPresetCaption(const ACaption: string);
    procedure InitWizardPages;
    function GetConfigModel: TJSONObject;
  end;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.StrUtils,
  System.IOUtils,
  Vcl.FileCtrl,
  DMVC.Expert.Commons,
  System.TypInfo,
  IdTCPServer,
  IdGlobal;

{$R *.dfm}

const
  PAGE_APP_TYPE = 0;
  PAGE_SERVER   = 1;
  PAGE_FEATURES = 2;
  PAGE_OPTIONS  = 3;

  PAGE_TITLES: array[0..3] of string = (
    'Application Type',
    'Server && Project',
    'Features',
    'Project Options'
  );

  PAGE_HINTS: array[0..3] of string = (
    'Select the type of application to be created',
    'Configure the server protocol, port, and project location',
    'Select controller options, middleware, and features',
    'Configure naming, class names, and additional options'
  );

function IsValidDelphiIdentifier(const AName: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;
  if AName.IsEmpty then
    Exit;
  C := AName[1];
  if not CharInSet(C, ['A'..'Z', 'a'..'z', '_']) then
    Exit;
  for I := 2 to Length(AName) do
  begin
    C := AName[I];
    if not CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Exit;
  end;
  Result := True;
end;

{ Navigation }

function TfrmDMVCNewProject.GetPageCount: Integer;
begin
  if fIsCustomPreset then
    Result := 4
  else
    Result := 3;
end;

function TfrmDMVCNewProject.MapPageIndex(AVisualIndex: Integer): Integer;
begin
  if fIsCustomPreset then
    Result := AVisualIndex
  else
  begin
    case AVisualIndex of
      0: Result := PAGE_APP_TYPE;
      1: Result := PAGE_SERVER;
    else
      Result := PAGE_OPTIONS;
    end;
  end;
end;

procedure TfrmDMVCNewProject.NavigateToPage(APageIndex: Integer);
var
  LActualPage: Integer;
  LIsLast, LIsFirst: Boolean;
  LPages: array[0..3] of TTabSheet;
begin
  LPages[0] := tsAppType;
  LPages[1] := tsServer;
  LPages[2] := tsFeatures;
  LPages[3] := tsOptions;

  fCurrentPage := APageIndex;
  LActualPage := MapPageIndex(APageIndex);
  LIsFirst := (APageIndex = 0);
  LIsLast := (APageIndex = GetPageCount - 1);

  pcWizard.ActivePage := LPages[LActualPage];

  lblPageTitle.Caption := PAGE_TITLES[LActualPage];
  lblPageHint.Caption := PAGE_HINTS[LActualPage];

  if fPresetCaption <> '' then
    Caption := Format('DMVCFramework :: %s (%d of %d)',
      [fPresetCaption, APageIndex + 1, GetPageCount])
  else
    Caption := Format('DMVCFramework :: New Project Wizard (%d of %d)',
      [APageIndex + 1, GetPageCount]);

  // Update summary on the last page
  if LIsLast then
    UpdateSummary;

  btnBack.Enabled := not LIsFirst;
  btnNext.Enabled := not LIsLast;
  btnFinish.Enabled := LIsLast;
  if LIsLast then
    btnFinish.Default := True
  else
    btnNext.Default := True;
end;

function TfrmDMVCNewProject.ValidateCurrentPage: Boolean;
var
  LActualPage: Integer;
  lProjectName: string;
begin
  Result := True;
  LActualPage := MapPageIndex(fCurrentPage);

  case LActualPage of
    PAGE_SERVER:
    begin
      // Validate port
      var lPort: Integer;
      var lPortText := Trim(edtServerPort.Text);
      if lPortText.IsEmpty or not TryStrToInt(lPortText, lPort) or (lPort < 1) or (lPort > 65534) then
      begin
        ShowMessage('Please enter a valid port number (1-65534).');
        edtServerPort.SetFocus;
        Result := False;
        Exit;
      end;

      // Validate project name
      lProjectName := Trim(edtProjectName.Text);
      if lProjectName.IsEmpty then
        lProjectName := 'MyDMVCProject';
      if not IsValidDelphiIdentifier(lProjectName) then
      begin
        ShowMessage('Project name must be a valid Delphi identifier:' + sLineBreak +
          '- Start with a letter or underscore' + sLineBreak +
          '- Contain only letters, digits, and underscores' + sLineBreak +
          '- No spaces or special characters');
        edtProjectName.SetFocus;
        Result := False;
      end;
    end;
  end;
end;

procedure TfrmDMVCNewProject.btnBackClick(Sender: TObject);
begin
  if fCurrentPage > 0 then
    NavigateToPage(fCurrentPage - 1);
end;

procedure TfrmDMVCNewProject.btnNextClick(Sender: TObject);
begin
  if ValidateCurrentPage then
    if fCurrentPage < GetPageCount - 1 then
      NavigateToPage(fCurrentPage + 1);
end;

procedure TfrmDMVCNewProject.SetCustomMode(AIsCustom: Boolean);
begin
  fIsCustomPreset := AIsCustom;
end;

procedure TfrmDMVCNewProject.InitWizardPages;
begin
  NavigateToPage(0);
end;

{ Form events }

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
var
  lDefaultProjectsFolder: string;
begin
  edtControllerClassName.TextHint := TDefaultValues.sDefaultControllerName;
  edtWebModuleName.TextHint := TDefaultValues.sDefaultWebModuleName;
  edtServerPort.TextHint := TDefaultValues.sDefaultServerPort;
  lblFrameworkVersion.Caption := 'dmvcframework-' + DMVCFRAMEWORK_VERSION;
  chkJSONRPC.Checked := False;
  EdtJSONRPCClassName.TextHint := 'TMyJSONRPCService';
  chkWebSocketServer.Checked := False;
  lblCopyRight.Caption := TMVCConstants.COPYRIGHT;
  fModel := TJsonObject.Create;
  fIsCustomPreset := False;

  lDefaultProjectsFolder := TPath.Combine(
    TPath.GetDocumentsPath,
    'Embarcadero\Studio\Projects');
  if not TDirectory.Exists(lDefaultProjectsFolder) then
    lDefaultProjectsFolder := TPath.GetHomePath;
  edtProjectFolder.Text := lDefaultProjectsFolder;
  edtProjectFolder.TextHint := lDefaultProjectsFolder;

  UpdateProjectNameHint;

  {$IF not Defined(FASTCGI)}
  rgServerProtocol.Items.Delete(rgServerProtocol.Items.Count-1);
  rgServerProtocol.ItemIndex := 0;
  {$ENDIF}
end;

procedure TfrmDMVCNewProject.FormDestroy(Sender: TObject);
begin
  fModel.Free;
end;

procedure TfrmDMVCNewProject.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
  procedure SyncServerPort(const aPort: String);
  begin
    if String(edtServerPort.Text).IsEmpty or (edtServerPort.Text = edtServerPort.TextHint) then
    begin
      edtServerPort.Text := aPort;
      edtServerPort.TextHint := aPort;
    end;
  end;
begin
  EdtFDConnDefFileName.Enabled := chkActiveRecord.Checked;
  EdtConnDefName.Enabled := chkActiveRecord.Checked;
  EdtJSONRPCClassName.Enabled := chkJSONRPC.Checked;
  chkProfileActions.Enabled := chkCreateIndexMethod.Checked or chkCreateCRUDMethods.Checked;
  if not chkProfileActions.Enabled then
    chkProfileActions.Checked := False;
  case rgServerProtocol.ItemIndex of
    0: begin
         lblServerPort.Caption := 'HTTP Server Port';
         SyncServerPort('8080');
       end;
    1: begin
         lblServerPort.Caption := 'HTTPS Server Port';
         SyncServerPort('443');
       end;
    2: begin
         lblServerPort.Caption := 'FastCGI Server Port';
         SyncServerPort('9000');
       end;
  end;
end;

procedure TfrmDMVCNewProject.btnFinishClick(Sender: TObject);
var
  lHints: TArray<String>;
  lProjectFolder: string;
  lProjectName: string;
begin
  lProjectName := Trim(edtProjectName.Text);
  if lProjectName.IsEmpty then
    lProjectName := 'MyDMVCProject';

  if not IsValidDelphiIdentifier(lProjectName) then
  begin
    ShowMessage('Project name must be a valid Delphi identifier:' + sLineBreak +
      '- Start with a letter or underscore' + sLineBreak +
      '- Contain only letters, digits, and underscores' + sLineBreak +
      '- No spaces or special characters');
    ModalResult := mrNone;
    edtProjectName.SetFocus;
    Exit;
  end;

  lProjectFolder := GetProjectFolder;
  if TDirectory.Exists(lProjectFolder) then
  begin
    if (Length(TDirectory.GetFiles(lProjectFolder)) > 0) or
       (Length(TDirectory.GetDirectories(lProjectFolder)) > 0) then
    begin
      if MessageDlg('The project folder already exists and is not empty. Continue?',
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
  end;

  lHints := [];
  if chkActiveRecord.Checked then
    lHints := lHints + ['- Include required FireDAC units in your project'];
  if rgServerProtocol.ItemIndex = 1 then
    lHints := lHints + ['- Install TaurusTLS from GetIT or directly from github (https://github.com/TurboPack/indy_extras)'];
  if Length(lHints) > 0 then
    ShowMessage('Remember to:' + sLineBreak + String.Join(sLineBreak, lHints));
end;

{ Property getters }

function TfrmDMVCNewProject.GetControllerClassName: string;
begin
  if Trim(edtControllerClassName.Text) = '' then
    Result := TDefaultValues.sDefaultControllerName
  else
    Result := Trim(edtControllerClassName.Text);
end;

function TfrmDMVCNewProject.GetWebModuleClassName: string;
begin
  if Trim(edtWebModuleName.Text) = '' then
    Result := TDefaultValues.sDefaultWebModuleName
  else
    Result := Trim(edtWebModuleName.Text);
end;

function TfrmDMVCNewProject.GetServerPort: Integer;
var
  lServerPort: Integer;
begin
  Result := StrToInt(TDefaultValues.sDefaultServerPort);
  if (Trim(edtServerPort.Text) <> '') and TryStrToInt(edtServerPort.Text, lServerPort) then
    if (lServerPort > 0) and (lServerPort < 65535) then
      Result := lServerPort;
end;

function TfrmDMVCNewProject.GetCreateJSONRPCInterface: boolean;
begin
  Result := chkJSONRPC.Checked;
end;

function TfrmDMVCNewProject.GetJSONRPCClassName: String;
begin
  if GetCreateJSONRPCInterface then
  begin
    Result := EdtJSONRPCClassName.Text;
    if Result.IsEmpty then
      Result := EdtJSONRPCClassName.TextHint;
  end
  else
    Result := '';
end;

function TfrmDMVCNewProject.GetBaseFolder: string;
begin
  Result := Trim(edtProjectFolder.Text);
  if Result.IsEmpty then
  begin
    Result := TPath.Combine(TPath.GetDocumentsPath, 'Embarcadero\Studio\Projects');
    if not TDirectory.Exists(Result) then
      Result := TPath.GetHomePath;
  end;
end;

function TfrmDMVCNewProject.GetProjectName: string;
var
  LBaseFolder: string;
  LCounter: Integer;
begin
  Result := Trim(edtProjectName.Text);
  if Result.IsEmpty then
  begin
    LBaseFolder := GetBaseFolder;
    LCounter := 1;
    repeat
      Result := 'DMVCFrameworkProject' + LCounter.ToString;
      Inc(LCounter);
    until not TDirectory.Exists(TPath.Combine(LBaseFolder, Result));
  end;
end;

function TfrmDMVCNewProject.GetProjectFolder: string;
begin
  if chkCreateSubfolder.Checked then
    Result := TPath.Combine(GetBaseFolder, GetProjectName)
  else
    Result := GetBaseFolder;
end;

procedure TfrmDMVCNewProject.UpdateProjectNameHint;
var
  LBaseFolder: string;
  LCounter: Integer;
  LSuggestedName: string;
begin
  LBaseFolder := GetBaseFolder;
  LCounter := 1;
  repeat
    LSuggestedName := 'DMVCFrameworkProject' + LCounter.ToString;
    Inc(LCounter);
  until not TDirectory.Exists(TPath.Combine(LBaseFolder, LSuggestedName));
  edtProjectName.Text := LSuggestedName;
  edtProjectName.TextHint := LSuggestedName;
end;

procedure TfrmDMVCNewProject.chkCreateSubfolderClick(Sender: TObject);
begin
  if chkCreateSubfolder.Checked then
    lblProjectFolder.Caption := 'Base Folder (project created as subfolder)'
  else
    lblProjectFolder.Caption := 'Project Folder';
end;

procedure TfrmDMVCNewProject.btnBrowseFolderClick(Sender: TObject);
var
  LDir: string;
begin
  LDir := edtProjectFolder.Text;
  if SelectDirectory('Select Project Folder', '', LDir) then
  begin
    edtProjectFolder.Text := LDir;
    UpdateProjectNameHint;
  end;
end;

{ Link handlers }

procedure TfrmDMVCNewProject.Image1Click(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://github.com/danieleteti/delphimvcframework'), nil, nil, SW_SHOW);
end;

procedure TfrmDMVCNewProject.lblBookClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://leanpub.com/delphimvcframework'), nil, nil, SW_SHOW);
end;

procedure TfrmDMVCNewProject.lblBookMouseEnter(Sender: TObject);
begin
  lblBook.Font.Color := clHighlight;
  lblBook.Font.Style := lblBook.Font.Style + [fsUnderline];
end;

procedure TfrmDMVCNewProject.lblBookMouseLeave(Sender: TObject);
begin
  lblBook.Font.Color := Font.Color;
  lblBook.Font.Style := lblBook.Font.Style - [fsUnderline];
end;

procedure TfrmDMVCNewProject.lblFrameworkVersionClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://github.com/danieleteti/delphimvcframework/releases/latest'), nil, nil, SW_SHOW);
end;

procedure TfrmDMVCNewProject.lblFrameworkVersionMouseEnter(Sender: TObject);
begin
  lblFrameworkVersion.Font.Color := clHighlight;
  lblFrameworkVersion.Font.Style := lblFrameworkVersion.Font.Style + [fsUnderline];
end;

procedure TfrmDMVCNewProject.lblFrameworkVersionMouseLeave(Sender: TObject);
begin
  lblFrameworkVersion.Font.Color := Font.Color;
  lblFrameworkVersion.Font.Style := lblFrameworkVersion.Font.Style - [fsUnderline];
end;

procedure TfrmDMVCNewProject.lblPATREONClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://www.patreon.com/delphimvcframework'), nil, nil, SW_SHOW);
end;

procedure TfrmDMVCNewProject.lblPATREONMouseEnter(Sender: TObject);
begin
  lblPATREON.Font.Color := clHighlight;
  lblPATREON.Font.Style := lblPATREON.Font.Style + [fsUnderline];
end;

procedure TfrmDMVCNewProject.lblPATREONMouseLeave(Sender: TObject);
begin
  lblPATREON.Font.Color := Font.Color;
  lblPATREON.Font.Style := lblPATREON.Font.Style - [fsUnderline];
end;

procedure TfrmDMVCNewProject.rgApplicationTypeClick(Sender: TObject);
const
  APP_TYPE_DESCRIPTIONS: array[0..1] of string = (
    'Runs in a terminal/console window. Ideal for development, debugging, ' +
    'and Docker/container deployments. Works on both Windows and Linux.',
    'Runs as a background Windows service managed by the Service Control Manager (SCM). ' +
    'Starts automatically with Windows, runs without user login. Ideal for production servers.'
  );
begin
  if (rgApplicationType.ItemIndex >= 0) and (rgApplicationType.ItemIndex <= High(APP_TYPE_DESCRIPTIONS)) then
    lblAppTypeDescription.Caption := APP_TYPE_DESCRIPTIONS[rgApplicationType.ItemIndex];
end;

procedure TfrmDMVCNewProject.rgServerProtocolClick(Sender: TObject);
const
  PROTOCOL_DESCRIPTIONS: array[0..2] of string = (
    'Standard HTTP server. Best choice for development and reverse-proxy deployments (nginx, Apache, IIS).',
    'HTTPS with TLS encryption. Requires TaurusTLS library. Use for direct client-facing deployments without a reverse proxy.',
    'FastCGI protocol for integration with web servers (nginx, Apache, IIS). The web server handles HTTP/HTTPS and forwards requests.'
  );
begin
  if (rgServerProtocol.ItemIndex >= 0) and (rgServerProtocol.ItemIndex <= High(PROTOCOL_DESCRIPTIONS)) then
    lblProtocolDescription.Caption := PROTOCOL_DESCRIPTIONS[rgServerProtocol.ItemIndex];
end;

procedure TfrmDMVCNewProject.btnTestPortClick(Sender: TObject);
var
  LPort: Integer;
  LTCPServer: TIdTCPServer;
begin
  if not TryStrToInt(Trim(edtServerPort.Text), LPort) or (LPort < 1) or (LPort > 65534) then
  begin
    ShowMessage('Please enter a valid port number (1-65534).');
    Exit;
  end;

  LTCPServer := TIdTCPServer.Create(nil);
  try
    LTCPServer.DefaultPort := LPort;
    try
      LTCPServer.Active := True;
      LTCPServer.Active := False;
      ShowMessage(Format('Port %d is available.', [LPort]));
    except
      on E: Exception do
        ShowMessage(Format('Port %d is NOT available: %s', [LPort, E.Message]));
    end;
  finally
    LTCPServer.Free;
  end;
end;

procedure TfrmDMVCNewProject.SetPresetCaption(const ACaption: string);
begin
  fPresetCaption := ACaption;
end;

procedure TfrmDMVCNewProject.UpdateSummary;
var
  LSummary: TStringList;
  LMiddlewares: TStringList;
begin
  LSummary := TStringList.Create;
  try
    LMiddlewares := TStringList.Create;
    try
      LSummary.Add('--- Project Summary ---');
      LSummary.Add('');

      // Application type
      if rgApplicationType.ItemIndex = 0 then
        LSummary.Add('Application: Console (' + rgServerProtocol.Items[rgServerProtocol.ItemIndex] + ')')
      else
        LSummary.Add('Application: Windows Service');

      LSummary.Add('Port: ' + edtServerPort.Text);
      LSummary.Add('Controller: ' + GetControllerClassName);
      LSummary.Add('WebModule: ' + GetWebModuleClassName);

      // Middlewares
      if chkCompression.Checked then LMiddlewares.Add('Compression');
      if chkCORS.Checked then LMiddlewares.Add('CORS');
      if chkStaticFiles.Checked then LMiddlewares.Add('Static Files');
      if chkJWT.Checked then LMiddlewares.Add('JWT');
      if chkActiveRecord.Checked then LMiddlewares.Add('ActiveRecord');
      if chkETAG.Checked then LMiddlewares.Add('ETag');
      if chkRateLimit.Checked then LMiddlewares.Add('Rate Limit');
      if chkAnalyticsMiddleware.Checked then LMiddlewares.Add('Analytics');
      if chkTrace.Checked then LMiddlewares.Add('Trace');
      if LMiddlewares.Count > 0 then
        LSummary.Add('Middleware: ' + LMiddlewares.DelimitedText)
      else
        LSummary.Add('Middleware: (none)');

      // Features
      if cbSSV.ItemIndex > 0 then
        LSummary.Add('Views: ' + cbSSV.Items[cbSSV.ItemIndex]);
      if cbSessionType.ItemIndex > 0 then
        LSummary.Add('Session: ' + cbSessionType.Items[cbSessionType.ItemIndex]);
      if chkWebSocketServer.Checked then
        LSummary.Add('WebSocket: Yes');
      if chkJSONRPC.Checked then
        LSummary.Add('JSON-RPC: Yes');

      lblSummary.Caption := LSummary.Text;
    finally
      LMiddlewares.Free;
    end;
  finally
    LSummary.Free;
  end;
end;

{ Config model }

function TfrmDMVCNewProject.GetConfigModel: TJSONObject;
begin
  fModel.Clear;
  fModel.S[TConfigKey.program_name] := GetProjectName;
  fModel.S[TConfigKey.program_default_server_port] := GetServerPort.ToString;
  fModel.B[TConfigKey.program_msheap] := chkMSHeap.Checked;
  fModel.B[TConfigKey.program_sqids] := chkSqids.Checked;
  fModel.B[TConfigKey.program_dotenv] := chkCustomConfigDotEnv.Checked;
  fModel.B[TConfigKey.program_htmx] := chkHtmx.Checked;
  fModel.B[TConfigKey.program_ssv_templatepro] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'templatepro');
  fModel.B[TConfigKey.program_ssv_webstencils] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'webstencils');
  fModel.B[TConfigKey.program_ssv_mustache] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'mustache');
  if fModel.B[TConfigKey.program_ssv_templatepro] or
     fModel.B[TConfigKey.program_ssv_webstencils] or
     fModel.B[TConfigKey.program_ssv_mustache] then
    fModel.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML'
  else
    fModel.S[TConfigKey.default_media_type] := 'TMVCConstants.DEFAULT_CONTENT_TYPE';
  fModel.B[TConfigKey.program_service_container_generate] := chkServicesContainer.Checked;
  fModel.S[TConfigKey.program_service_container_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_classname] := GetControllerClassName;
  fModel.B[TConfigKey.controller_index_methods_generate] := chkCreateIndexMethod.Checked;
  fModel.B[TConfigKey.controller_action_filters_generate] := chkCreateActionFiltersMethods.Checked;
  fModel.B[TConfigKey.controller_crud_methods_generate] := chkCreateCRUDMethods.Checked;
  fModel.B[TConfigKey.controller_actions_profiling_generate] := chkProfileActions.Checked;
  fModel.B[TConfigKey.entity_generate] := fModel.B[TConfigKey.controller_crud_methods_generate] or fModel.B[TConfigKey.program_service_container_generate];
  fModel.S[TConfigKey.entity_classname] := 'TPerson';
  fModel.S[TConfigKey.entity_unit_name] := 'TBA';
  fModel.B[TConfigKey.jsonrpc_generate] := GetCreateJSONRPCInterface;
  fModel.S[TConfigKey.jsonrpc_classname] := GetJSONRPCClassName;
  fModel.S[TConfigKey.jsonrpc_unit_name] := 'TBA';
  fModel.S[TConfigKey.serializer_name_case] := GetEnumName(TypeInfo(TMVCNameCase), cbNameCase.ItemIndex + 1);
  fModel.S[TConfigKey.websocket_unit_name] := 'WebSocketServerU';
  fModel.B[TConfigKey.websocket_generate] := chkWebSocketServer.Checked;

  case rgServerProtocol.ItemIndex of
    0: fModel.S['program.server.protocol'] := 'http';
    1: fModel.S['program.server.protocol'] := 'https';
    2: fModel.S['program.server.protocol'] := 'fastcgi';
  end;

  case rgApplicationType.ItemIndex of
    0: begin // Console (Win/Linux)
         case rgServerProtocol.ItemIndex of
           0: fModel.S[TConfigKey.program_type] := TProgramTypes.HTTP_CONSOLE;
           1: fModel.S[TConfigKey.program_type] := TProgramTypes.HTTPS_CONSOLE;
           2: fModel.S[TConfigKey.program_type] := TProgramTypes.FASTCGI_CONSOLE;
         end;
       end;
    1: fModel.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  else
    raise Exception.Create('Invalid Application Type');
  end;

  fModel.S[TConfigKey.mustache_helpers_unit_name] := 'TBA';
  fModel.S[TConfigKey.templatepro_helpers_unit_name] := 'TBA';
  fModel.S[TConfigKey.webstencils_helpers_unit_name] := 'TBA';

  fModel.S[TConfigKey.webmodule_unit_name] := 'TBA';
  fModel.S[TConfigKey.webmodule_classname] := GetWebModuleClassName;
  fModel.S[TConfigKey.webmodule_classname_short] := GetWebModuleClassName.Substring(1);
  fModel.B[TConfigKey.webmodule_middleware_analytics] := chkAnalyticsMiddleware.Checked;
  fModel.B[TConfigKey.webmodule_middleware_staticfiles] := chkStaticFiles.Checked;
  fModel.B[TConfigKey.webmodule_middleware_trace] := chkTrace.Checked;
  fModel.B[TConfigKey.webmodule_middleware_compression] := chkCompression.Checked;
  fModel.B[TConfigKey.webmodule_middleware_etag] := chkETAG.Checked;
  fModel.B[TConfigKey.webmodule_middleware_cors] := chkCORS.Checked;
  fModel.B[TConfigKey.webmodule_middleware_ratelimit] := chkRateLimit.Checked;
  fModel.B[TConfigKey.webmodule_middleware_jwt] := chkJWT.Checked;
  fModel.B[TConfigKey.webmodule_middleware_activerecord] := chkActiveRecord.Checked;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] := EdtConnDefName.Text;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_filename] := EdtFDConnDefFileName.Text;
  fModel.S[TConfigKey.con_def_filename] := ExtractFileName(StringReplace(EdtFDConnDefFileName.Text, '$(AppPath)', '', [rfIgnoreCase]));

  fModel.B[TConfigKey.webmodule_middleware_session_memory] := cbSessionType.ItemIndex = 1;
  fModel.B[TConfigKey.webmodule_middleware_session_file] := cbSessionType.ItemIndex = 2;
  fModel.B[TConfigKey.webmodule_middleware_session_database] := cbSessionType.ItemIndex = 3;
  fModel.I[TConfigKey.webmodule_middleware_session_timeout] := 0;

  Result := fModel;
end;

end.
