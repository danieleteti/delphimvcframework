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
// This IDE expert is based off of the one included with the DUnitX }
// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti. }
//
// The DUnitX project is run by Vincent Parrett and can be found at: }
//
// https://github.com/VSoftTechnologies/DUnitX }
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
  System.Actions,
  Vcl.ActnList,
  Vcl.AppEvnts,
  JsonDataObjects;

type
  TfrmDMVCNewProject = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtWebModuleName: TEdit;
    lblWbModule: TLabel;
    edtServerPort: TEdit;
    lblServerPort: TLabel;
    Image1: TImage;
    lblFrameworkVersion: TLabel;
    gbControllerUnitOptions: TGroupBox;
    lblClassName: TLabel;
    Label1: TLabel;
    chkCreateIndexMethod: TCheckBox;
    edtControllerClassName: TEdit;
    chkCreateActionFiltersMethods: TCheckBox;
    chkCreateCRUDMethods: TCheckBox;
    Shape1: TShape;
    GroupBox1: TGroupBox;
    chkAnalyticsMiddleware: TCheckBox;
    chkCompression: TCheckBox;
    chkStaticFiles: TCheckBox;
    chkTrace: TCheckBox;
    chkCORS: TCheckBox;
    chkETAG: TCheckBox;
    lblBook: TLabel;
    chkActiveRecord: TCheckBox;
    EdtFDConnDefFileName: TEdit;
    Label4: TLabel;
    Bevel1: TBevel;
    Label5: TLabel;
    EdtConnDefName: TEdit;
    ApplicationEvents: TApplicationEvents;
    lblCopyRight: TLabel;
    chkProfileActions: TCheckBox;
    lblPATREON: TLabel;
    Image2: TImage;
    Shape2: TShape;
    rgServerProtocol: TRadioGroup;
    rgApplicationType: TRadioGroup;
    chkRateLimit: TCheckBox;
    chkJWT: TCheckBox;
    lblProjectName: TLabel;
    lblProjectFolder: TLabel;
    edtProjectName: TEdit;
    edtProjectFolder: TEdit;
    btnBrowseFolder: TButton;
    gbNameCase: TGroupBox;
    cbNameCase: TComboBox;
    Label2: TLabel;
    Label6: TLabel;
    cbSSV: TComboBox;
    Label7: TLabel;
    cbSessionType: TComboBox;
    chkJSONRPC: TCheckBox;
    Label3: TLabel;
    EdtJSONRPCClassName: TEdit;
    chkWebSocketServer: TCheckBox;
    chkServicesContainer: TCheckBox;
    chkSqids: TCheckBox;
    chkMSHeap: TCheckBox;
    chkCustomConfigDotEnv: TCheckBox;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure lblBookMouseEnter(Sender: TObject);
    procedure lblBookMouseLeave(Sender: TObject);
    procedure lblBookClick(Sender: TObject);
    procedure lblFrameworkVersionMouseEnter(Sender: TObject);
    procedure lblFrameworkVersionMouseLeave(Sender: TObject);
    procedure lblFrameworkVersionClick(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblPATREONClick(Sender: TObject);
    procedure lblPATREONMouseEnter(Sender: TObject);
    procedure lblPATREONMouseLeave(Sender: TObject);
    procedure rgSSVClick(Sender: TObject);
    procedure btnBrowseFolderClick(Sender: TObject);
  private
    { Private declarations }
    fModel: TJsonObject;
    function GetCreateIndexMethod: boolean;
    function GetCreateControllerUnit: boolean;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
    function GetCreateActionFiltersMethods: boolean;
    function GetServerPort: Integer;
    function GetCreateCRUDMethods: boolean;
    function GetCreateJSONRPCInterface: boolean;
    function GetJSONRPCClassName: String;
    function GetBaseFolder: string;
    function GetProjectName: string;
    function GetProjectFolder: string;
    procedure UpdateProjectNameHint;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property ControllerClassName: string read GetControllerClassName;
    property CreateControllerUnit: boolean read GetCreateControllerUnit;
    property JSONRPCClassName: String read GetJSONRPCClassName;
    property CreateIndexMethod: boolean read GetCreateIndexMethod;
    property CreateCRUDMethods: boolean read GetCreateCRUDMethods;
    property CreateActionFiltersMethods: boolean
      read GetCreateActionFiltersMethods;
    property WebModuleClassName: string read GetWebModuleClassName;
    property ProjectName: string read GetProjectName;
    property ProjectFolder: string read GetProjectFolder;
    function GetConfigModel: TJSONObject;
  end;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.StrUtils,
  System.IOUtils,
  Vcl.FileCtrl,
  DMVC.Expert.Commons, System.TypInfo;

{$R *.dfm}

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
  begin
    chkProfileActions.Checked := False;
  end;
  case rgServerProtocol.ItemIndex of
    0: begin //http
         lblServerPort.Caption := 'HTTP Server Port';
         SyncServerPort('8080');
       end;
    1: begin //https
         lblServerPort.Caption := 'HTTPS Server Port';
         SyncServerPort('443');
       end;
    2: begin //fastcgi
         lblServerPort.Caption := 'FastCGI Server Port';
         SyncServerPort('9000');
       end;
  end;
end;

function IsValidDelphiIdentifier(const AName: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;
  if AName.IsEmpty then
    Exit;

  // First character must be a letter or underscore
  C := AName[1];
  if not CharInSet(C, ['A'..'Z', 'a'..'z', '_']) then
    Exit;

  // Remaining characters must be letters, digits, or underscores
  for I := 2 to Length(AName) do
  begin
    C := AName[I];
    if not CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Exit;
  end;

  Result := True;
end;

procedure TfrmDMVCNewProject.btnOKClick(Sender: TObject);
var
  lHints: TArray<String>;
  lProjectFolder: string;
  lProjectName: string;
begin
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
    ModalResult := mrNone;
    edtProjectName.SetFocus;
    Exit;
  end;

  // Check if project folder already exists and has files
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
  begin
    lHints := lHints + ['- Include required FireDAC units in your project'];
  end;
  if rgServerProtocol.ItemIndex = 1 then
  begin
    lHints := lHints + ['- Install TaurusTLS from GetIT or directly from github (https://github.com/TurboPack/indy_extras)'];
  end;
  if Length(lHints) > 0 then
  begin
    ShowMessage('Remember to:' + sLineBreak + String.Join(sLineBreak, lHints));
  end;
end;

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
var
  lDefaultProjectsFolder: string;
begin
  edtControllerClassName.TextHint := TDefaultValues.sDefaultControllerName;
  edtWebModuleName.TextHint := TDefaultValues.sDefaultWebModuleName;
  edtServerPort.TextHint := TDefaultValues.sDefaultServerPort;
  lblFrameworkVersion.Caption := 'dmvcframework-' + DMVCFRAMEWORK_VERSION;
  chkJSONRPC.Checked := False;
  chkWebSocketServer.Checked := False;
  lblCopyRight.Caption := TMVCConstants.COPYRIGHT;
  fModel := TJsonObject.Create;

  // Set default project folder to Embarcadero\Studio\Projects folder (or home folder if not exists)
  lDefaultProjectsFolder := TPath.Combine(
    TPath.GetDocumentsPath,
    'Embarcadero\Studio\Projects');
  if not TDirectory.Exists(lDefaultProjectsFolder) then
    lDefaultProjectsFolder := TPath.GetHomePath;
  edtProjectFolder.Text := lDefaultProjectsFolder;
  edtProjectFolder.TextHint := lDefaultProjectsFolder;

  // Set default project name hint (will show next available DMVCFrameworkProjectN)
  UpdateProjectNameHint;

  {$IF not Defined(FASTCGI)}
  // FastCGI is only available from Delphi 13 Florence onwards
  rgServerProtocol.Items.Delete(rgServerProtocol.Items.Count-1);
  rgServerProtocol.ItemIndex := 0;
  {$ENDIF}
end;

procedure TfrmDMVCNewProject.FormDestroy(Sender: TObject);
begin
  fModel.Free;
end;

function TfrmDMVCNewProject.GetCreateIndexMethod: boolean;
begin
  Result := chkCreateIndexMethod.Checked;
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
    begin
      Result := EdtJSONRPCClassName.TextHint;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

function TfrmDMVCNewProject.GetServerPort: Integer;
var
  lServerPort: Integer;
begin
  Result := StrToInt(TDefaultValues.sDefaultServerPort);
  if (Trim(edtServerPort.Text) <> '') and TryStrToInt(edtServerPort.Text,
    lServerPort) then
  begin
    if (lServerPort > 0) and (lServerPort < 65535) then
      Result := lServerPort;
  end;
end;

function TfrmDMVCNewProject.GetWebModuleClassName: string;
begin
  if Trim(edtWebModuleName.Text) = '' then
  begin
    Result := TDefaultValues.sDefaultWebModuleName
  end
  else
  begin
    Result := Trim(edtWebModuleName.Text);
  end;
end;

procedure TfrmDMVCNewProject.Image1Click(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://github.com/danieleteti/delphimvcframework'),
    nil, nil, SW_SHOW);
end;

procedure TfrmDMVCNewProject.lblPATREONClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://www.patreon.com/delphimvcframework'),
    nil, nil, SW_SHOW);
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

procedure TfrmDMVCNewProject.rgSSVClick(Sender: TObject);
begin
{$if not Defined(WEBSTENCILS)}
  if SameText(rgSSV.Items[rgSSV.ItemIndex], 'webstencils') then
  begin
    ShowMessage('This Delphi version doesn''t support WebStencils, so DelphiMVCFramework cannot use it.' +
      sLineBreak + 'Consider to use TemplatePro.');
    rgSSV.ItemIndex := 1;
  end;
{$endif}
end;

procedure TfrmDMVCNewProject.lblBookClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://leanpub.com/delphimvcframework'),
    nil, nil, SW_SHOW);
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
    PChar('https://github.com/danieleteti/delphimvcframework/releases/latest'),
    nil, nil, SW_SHOW);
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

function TfrmDMVCNewProject.GetCreateActionFiltersMethods: boolean;
begin
  Result := chkCreateActionFiltersMethods.Checked;
end;

function TfrmDMVCNewProject.GetCreateControllerUnit: boolean;
begin
  Result := True;
end;

function TfrmDMVCNewProject.GetCreateCRUDMethods: boolean;
begin
  Result := chkCreateCRUDMethods.Checked;
end;

function TfrmDMVCNewProject.GetConfigModel: TJSONObject;
begin
  fModel.Clear;
  fModel.S[TConfigKey.program_name] := GetProjectName;
  fModel.S[TConfigKey.program_default_server_port] := GetServerPort.ToString;
  fModel.B[TConfigKey.program_msheap] := chkMSHeap.Checked;
  fModel.B[TConfigKey.program_sqids] := chkSqids.Checked;
  fModel.B[TConfigKey.program_dotenv] := chkCustomConfigDotEnv.Checked;
  fModel.B[TConfigKey.program_ssv_templatepro] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'templatepro');
  fModel.B[TConfigKey.program_ssv_webstencils] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'webstencils');
  fModel.B[TConfigKey.program_ssv_mustache] := SameText(cbSSV.Items[cbSSV.ItemIndex], 'mustache');
  // Set default media type based on SSV selection
  if fModel.B[TConfigKey.program_ssv_templatepro] or
     fModel.B[TConfigKey.program_ssv_webstencils] or
     fModel.B[TConfigKey.program_ssv_mustache] then
    fModel.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML'
  else
    fModel.S[TConfigKey.default_media_type] := 'TMVCConstants.DEFAULT_CONTENT_TYPE';
  fModel.B[TConfigKey.program_service_container_generate] := chkServicesContainer.Checked;
  fModel.S[TConfigKey.program_service_container_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_classname] :=  GetControllerClassName;
  fModel.B[TConfigKey.controller_index_methods_generate] :=  chkCreateIndexMethod.Checked;
  fModel.B[TConfigKey.controller_action_filters_generate] :=  chkCreateActionFiltersMethods.Checked;
  fModel.B[TConfigKey.controller_crud_methods_generate] :=  chkCreateCRUDMethods.Checked;
  fModel.B[TConfigKey.controller_actions_profiling_generate] :=  chkProfileActions.Checked;
  fModel.B[TConfigKey.entity_generate] := fModel.B[TConfigKey.controller_crud_methods_generate] or fModel.B[TConfigKey.program_service_container_generate];
  fModel.S[TConfigKey.entity_classname] :=  'TPerson';
  fModel.S[TConfigKey.entity_unit_name] := 'TBA';
  fModel.B[TConfigKey.jsonrpc_generate] :=  GetCreateJSONRPCInterface;
  fModel.S[TConfigKey.jsonrpc_classname] :=  GetJSONRPCClassName;
  fModel.S[TConfigKey.jsonrpc_unit_name] := 'TBA';
  fModel.S[TConfigKey.serializer_name_case] := GetEnumName(TypeInfo(TMVCNameCase), cbNameCase.ItemIndex + 1);
  fModel.S[TConfigKey.websocket_unit_name] := 'WebSocketServerU';
  fModel.B[TConfigKey.websocket_generate] := chkWebSocketServer.Checked;

  // Determine program type based on Server Protocol + Application Type
  // Store server protocol for all types
  case rgServerProtocol.ItemIndex of
    0: fModel.S['program.server.protocol'] := 'http';
    1: fModel.S['program.server.protocol'] := 'https';
    2: fModel.S['program.server.protocol'] := 'fastcgi';
  end;

  // Determine application type
  case rgApplicationType.ItemIndex of
    0: begin // Console
         case rgServerProtocol.ItemIndex of
           0: fModel.S[TConfigKey.program_type] := TProgramTypes.HTTP_CONSOLE;
           1: fModel.S[TConfigKey.program_type] := TProgramTypes.HTTPS_CONSOLE;
           2: fModel.S[TConfigKey.program_type] := TProgramTypes.FASTCGI_CONSOLE;
         end;
       end;
    1: begin // Windows Service
         fModel.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
       end;
    2: begin // Linux Daemon (future)
         fModel.S[TConfigKey.program_type] := 'linux.daemon';
       end;
    else
      raise Exception.Create('Invalid Application Type');
  end;

  // Helper unit names (defaults, will be updated after units are created)
  fModel.S[TConfigKey.mustache_helpers_unit_name] := 'TBA';
  fModel.S[TConfigKey.templatepro_helpers_unit_name] := 'TBA';
  fModel.S[TConfigKey.webstencils_helpers_unit_name] := 'TBA';

  //webmodule
  fModel.S[TConfigKey.webmodule_unit_name] := 'TBA';
  fModel.S[TConfigKey.webmodule_classname] :=  GetWebModuleClassName;
  fModel.S[TConfigKey.webmodule_classname_short] := GetWebModuleClassName.Substring(1); // TMyWebModule -> MyWebModule
  fModel.B[TConfigKey.webmodule_middleware_analytics] :=  chkAnalyticsMiddleware.Checked;
  fModel.B[TConfigKey.webmodule_middleware_staticfiles] :=  chkStaticFiles.Checked;
  fModel.B[TConfigKey.webmodule_middleware_trace] :=  chkTrace.Checked;
  fModel.B[TConfigKey.webmodule_middleware_compression] :=  chkCompression.Checked;
  fModel.B[TConfigKey.webmodule_middleware_etag] :=  chkETAG.Checked;
  fModel.B[TConfigKey.webmodule_middleware_cors] :=  chkCORS.Checked;
  fModel.B[TConfigKey.webmodule_middleware_ratelimit] :=  chkRateLimit.Checked;
  fModel.B[TConfigKey.webmodule_middleware_jwt] :=  chkJWT.Checked;
  fModel.B[TConfigKey.webmodule_middleware_activerecord] :=  chkActiveRecord.Checked;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] :=  EdtConnDefName.Text;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_filename] :=  EdtFDConnDefFileName.Text;
  // Extract just the filename for file generation (e.g., "$(AppPath)FDConnectionDefs.ini" -> "FDConnectionDefs.ini")
  fModel.S[TConfigKey.con_def_filename] := ExtractFileName(StringReplace(EdtFDConnDefFileName.Text, '$(AppPath)', '', [rfIgnoreCase]));

  // Session middleware
  fModel.B[TConfigKey.webmodule_middleware_session_memory] := cbSessionType.ItemIndex = 1;
  fModel.B[TConfigKey.webmodule_middleware_session_file] := cbSessionType.ItemIndex = 2;
  fModel.B[TConfigKey.webmodule_middleware_session_database] := cbSessionType.ItemIndex = 3;
  fModel.I[TConfigKey.webmodule_middleware_session_timeout] := 0; // Default timeout

  //webmodule - end
  Result := fModel;
end;

function TfrmDMVCNewProject.GetControllerClassName: string;
begin
  if Trim(edtControllerClassName.Text) = '' then
  begin
    Result := TDefaultValues.sDefaultControllerName
  end
  else
  begin
    Result := Trim(edtControllerClassName.Text);
  end;
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

  // Set both the Text and TextHint to the suggested name
  edtProjectName.Text := LSuggestedName;
  edtProjectName.TextHint := LSuggestedName;
end;

function TfrmDMVCNewProject.GetBaseFolder: string;
begin
  Result := Trim(edtProjectFolder.Text);
  if Result.IsEmpty then
  begin
    Result := TPath.Combine(TPath.GetDocumentsPath, 'Embarcadero\Studio\Projects');
    // Fallback to user's home directory if Embarcadero folder doesn't exist
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
    // Generate automatic project name: DMVCFrameworkProject1, DMVCFrameworkProject2, etc.
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
  // Append project name as subfolder to base folder
  Result := TPath.Combine(GetBaseFolder, GetProjectName);
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

end.
