// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

{$I dmvcframework.inc}

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
    chkAddToProjectGroup: TCheckBox;
    edtWebModuleName: TEdit;
    lblWbModule: TLabel;
    edtServerPort: TEdit;
    Label2: TLabel;
    Image1: TImage;
    lblFrameworkVersion: TLabel;
    Panel2: TPanel;
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
    GroupBoxJSONRPC: TGroupBox;
    Label3: TLabel;
    EdtJSONRPCClassName: TEdit;
    chkJSONRPC: TCheckBox;
    Label4: TLabel;
    Bevel1: TBevel;
    Label5: TLabel;
    EdtConnDefName: TEdit;
    ApplicationEvents: TApplicationEvents;
    lblCopyRight: TLabel;
    chkMSHeap: TCheckBox;
    chkCustomConfigDotEnv: TCheckBox;
    chkProfileActions: TCheckBox;
    lblPATREON: TLabel;
    chkServicesContainer: TCheckBox;
    chkSqids: TCheckBox;
    rgNameCase: TRadioGroup;
    rgSSV: TRadioGroup;
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
  private
    { Private declarations }
    fModel: TJsonObject;
    function GetAddToProjectGroup: boolean;
    function GetCreateIndexMethod: boolean;
    function GetCreateControllerUnit: boolean;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
    function GetCreateActionFiltersMethods: boolean;
    function GetServerPort: Integer;
    function GetCreateCRUDMethods: boolean;
    function GetCreateJSONRPCInterface: boolean;
    function GetJSONRPCClassName: String;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property ControllerClassName: string read GetControllerClassName;
    property CreateControllerUnit: boolean read GetCreateControllerUnit;
    property JSONRPCClassName: String read GetJSONRPCClassName;
    property AddToProjectGroup: boolean read GetAddToProjectGroup;
    property CreateIndexMethod: boolean read GetCreateIndexMethod;
    property CreateCRUDMethods: boolean read GetCreateCRUDMethods;
    property CreateActionFiltersMethods: boolean
      read GetCreateActionFiltersMethods;
    property WebModuleClassName: string read GetWebModuleClassName;
    function GetConfigModel: TJSONObject;
  end;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.StrUtils,
  DMVC.Expert.Commons, System.TypInfo;

{$R *.dfm}

procedure TfrmDMVCNewProject.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  EdtFDConnDefFileName.Enabled := chkActiveRecord.Checked;
  EdtConnDefName.Enabled := chkActiveRecord.Checked;
  EdtJSONRPCClassName.Enabled := chkJSONRPC.Checked;
  chkProfileActions.Enabled := chkCreateIndexMethod.Checked or chkCreateCRUDMethods.Checked;
  if not chkProfileActions.Enabled then
  begin
    chkProfileActions.Checked := False;
  end;
end;

procedure TfrmDMVCNewProject.btnOKClick(Sender: TObject);
begin
  if chkActiveRecord.Checked then
  begin
    ShowMessage('Remember to include required FireDAC units in your project');
  end;
end;

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
begin
  edtControllerClassName.TextHint := TDefaultValues.sDefaultControllerName;
  edtWebModuleName.TextHint := TDefaultValues.sDefaultWebModuleName;
  edtServerPort.TextHint := TDefaultValues.sDefaultServerPort;
  lblFrameworkVersion.Caption := 'dmvcframework-' + DMVCFRAMEWORK_VERSION;
  chkJSONRPC.Checked := False;
  lblCopyRight.Caption := TMVCConstants.COPYRIGHT;
  fModel := TJsonObject.Create;
end;

procedure TfrmDMVCNewProject.FormDestroy(Sender: TObject);
begin
  fModel.Free;
end;

function TfrmDMVCNewProject.GetAddToProjectGroup: boolean;
begin
  Result := chkAddToProjectGroup.Checked;
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
    ShowMessage('This Delphi version doesn''t support WebStencils, so DMVCFramework cannot use it.' +
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
  fModel.S[TConfigKey.program_name] :=  'TBA';
  fModel.S[TConfigKey.program_default_server_port] := GetServerPort.ToString;
  fModel.B[TConfigKey.program_msheap] := chkMSHeap.Checked;
  fModel.B[TConfigKey.program_sqids] := chkSqids.Checked;
  fModel.B[TConfigKey.program_dotenv] := chkCustomConfigDotEnv.Checked;
  fModel.B[TConfigKey.program_ssv_templatepro] := SameText(rgSSV.Items[rgSSV.ItemIndex], 'templatepro');
  fModel.B[TConfigKey.program_ssv_webstencils] := SameText(rgSSV.Items[rgSSV.ItemIndex], 'webstencils');
  fModel.B[TConfigKey.program_ssv_mustache] := SameText(rgSSV.Items[rgSSV.ItemIndex], 'mustache');
  fModel.B[TConfigKey.program_service_container_generate] := chkServicesContainer.Checked;
  fModel.S[TConfigKey.program_service_container_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_classname] :=  GetControllerClassName;
  fModel.B[TConfigKey.controller_index_methods_generate] :=  chkCreateIndexMethod.Checked;
  fModel.B[TConfigKey.controller_action_filters_generate] :=  chkCreateActionFiltersMethods.Checked;
  fModel.B[TConfigKey.controller_crud_methods_generate] :=  chkCreateCRUDMethods.Checked;
  fModel.B[TConfigKey.controller_actions_profiling_generate] :=  chkProfileActions.Checked;
  fModel.B[TConfigKey.entity_generate] :=  fModel.B[TConfigKey.controller_crud_methods_generate];
  fModel.S[TConfigKey.entity_classname] :=  'TPerson';
  fModel.B[TConfigKey.jsonrpc_generate] :=  GetCreateJSONRPCInterface;
  fModel.S[TConfigKey.jsonrpc_classname] :=  GetJSONRPCClassName;
  fModel.S[TConfigKey.jsonrpc_unit_name] := 'TBA';
  fModel.S[TConfigKey.serializer_name_case] := GetEnumName(TypeInfo(TMVCNameCase), rgNameCase.ItemIndex + 1);
  //webmodule

  fModel.S[TConfigKey.webmodule_classname] :=  GetWebModuleClassName;
  fModel.B[TConfigKey.webmodule_middleware_analytics] :=  chkAnalyticsMiddleware.Checked;
  fModel.B[TConfigKey.webmodule_middleware_staticfiles] :=  chkStaticFiles.Checked;
  fModel.B[TConfigKey.webmodule_middleware_trace] :=  chkTrace.Checked;
  fModel.B[TConfigKey.webmodule_middleware_compression] :=  chkCompression.Checked;
  fModel.B[TConfigKey.webmodule_middleware_etag] :=  chkETAG.Checked;
  fModel.B[TConfigKey.webmodule_middleware_cors] :=  chkCORS.Checked;
  fModel.B[TConfigKey.webmodule_middleware_activerecord] :=  chkActiveRecord.Checked;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] :=  EdtConnDefName.Text;
  fModel.S[TConfigKey.webmodule_middleware_activerecord_con_def_filename] :=  EdtFDConnDefFileName.Text;

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

end.
