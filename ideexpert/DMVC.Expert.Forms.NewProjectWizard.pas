// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// project.  Original source by Robert Love.  Adapted by Nick Hodges. }
//
// The DUnitX project is run by Vincent Parrett and can be found at: }
//
// https://github.com/VSoftTechnologies/DUnitX }
// ***************************************************************************

unit DMVC.Expert.Forms.NewProjectWizard;

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
  Vcl.AppEvnts;

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
    edtClassName: TEdit;
    chkCreateActionFiltersMethods: TCheckBox;
    chkCreateCRUDMethods: TCheckBox;
    chkCreateControllerUnit: TCheckBox;
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
    procedure chkCreateControllerUnitClick(Sender: TObject);
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
  private
    { Private declarations }
    function GetAddToProjectGroup: boolean;
    function GetCreateIndexMethod: boolean;
    function GetCreateControllerUnit: boolean;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
    function GetCreateActionFiltersMethods: boolean;
    function GetServerPort: Integer;
    function GetCreateCRUDMethods: boolean;
    function GetMiddlewares: TArray<String>;
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
    property Middlewares: TArray<String> read GetMiddlewares;
    property CreateActionFiltersMethods: boolean
      read GetCreateActionFiltersMethods;
    property WebModuleClassName: string read GetWebModuleClassName;
    property ServerPort: Integer read GetServerPort;
  end;

var
  frmDMVCNewProject: TfrmDMVCNewProject;

implementation

uses
  DMVC.Expert.CodeGen.Templates,
  MVCFramework.Commons,
  System.StrUtils;

{$R *.dfm}

procedure TfrmDMVCNewProject.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  EdtFDConnDefFileName.Enabled := chkActiveRecord.Checked;
  EdtConnDefName.Enabled := chkActiveRecord.Checked;
  EdtJSONRPCClassName.Enabled := chkJSONRPC.Checked;
end;

procedure TfrmDMVCNewProject.btnOKClick(Sender: TObject);
begin
  if chkActiveRecord.Checked then
  begin
    ShowMessage('Remember to include required FireDAC units in your project');
  end;
end;

procedure TfrmDMVCNewProject.chkCreateControllerUnitClick(Sender: TObject);
begin
  chkCreateIndexMethod.Enabled := chkCreateControllerUnit.Checked;
  chkCreateActionFiltersMethods.Enabled := chkCreateControllerUnit.Checked;
  chkCreateCRUDMethods.Enabled := chkCreateControllerUnit.Checked;
  edtClassName.Enabled := chkCreateControllerUnit.Checked;
end;

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := sDefaultControllerName;
  edtWebModuleName.TextHint := sDefaultWebModuleName;
  edtServerPort.TextHint := sDefaultServerPort;
  lblFrameworkVersion.Caption := 'dmvcframework-' + DMVCFRAMEWORK_VERSION;
  chkJSONRPC.Checked := False;
  lblCopyRight.Caption := TMVCConstants.COPYRIGHT;
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

function TfrmDMVCNewProject.GetMiddlewares: TArray<String>;
const
  M_ANALYTICS = 'FMVC.AddMiddleware(TMVCAnalyticsMiddleware.Create(GetAnalyticsDefaultLogger));';
  M_STATICFILES = 'FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(''/static'', TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), ''www'')));';
  M_TRACE = 'FMVC.AddMiddleware(TMVCTraceMiddleware.Create);';
  M_COMPRESSION = 'FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);';
  M_ETAG = 'FMVC.AddMiddleware(TMVCETagMiddleware.Create);';
  M_CORS = 'FMVC.AddMiddleware(TMVCCORSMiddleware.Create);';
  M_ACTIVERECORD = 'FMVC.AddMiddleware(TMVCActiveRecordMiddleware.Create(' + sLineBreak + 
  '    dotEnv.Env(''firedac.connection_definition_name'', ''%s''), ' + sLineBreak +
  '    dotEnv.Env(''firedac.connection_definitions_filename'', ''%s'')' + sLineBreak +
  '  ));';

  function GetText(const Edit: TCustomEdit): String;
  begin
    if Edit.Text = '' then
    begin
      Result := Edit.TextHint;
    end
    else
    begin
      Result := Edit.Text;
    end;
  end;
begin
  Result := [];
  Result := Result + ['', '// Analytics middleware generates a csv log, useful to do traffic analysis'];
  Result := Result + [ifthen(not chkAnalyticsMiddleware.Checked, '//') + M_ANALYTICS];
  Result := Result + ['', '// The folder mapped as documentroot for TMVCStaticFilesMiddleware must exists!'];
  Result := Result + [ifthen(not chkStaticFiles.Checked, '//') + M_STATICFILES];
  Result := Result + ['', '// Trace middlewares produces a much detailed log for debug purposes'];
  Result := Result + [ifthen(not chkTrace.Checked, '//') + M_TRACE];
  Result := Result + ['', '// CORS middleware handles... well, CORS'];
  Result := Result + [ifthen(not chkCORS.Checked, '//') + M_CORS];
  Result := Result + ['', '// Simplifies TMVCActiveRecord connection definition'];
  Result := Result + [
    ifthen(not chkActiveRecord.Checked, '{') + sLineBreak +
    '  ' + Format(M_ACTIVERECORD, [GetText(EdtConnDefName), GetText(EdtFDConnDefFileName)]) + sLineBreak +
    ifthen(not chkActiveRecord.Checked, '  }') + sLineBreak
    ];
  Result := Result + ['', '// Compression middleware must be the last in the chain, just before the ETag, if present.'];
  Result := Result + [ifthen(not chkCompression.Checked, '//') + M_COMPRESSION];
  Result := Result + ['', '// ETag middleware must be the latest in the chain'];
  Result := Result + [ifthen(not chkETAG.Checked, '//') + M_ETAG];
end;

function TfrmDMVCNewProject.GetServerPort: Integer;
var
  lServerPort: Integer;
begin
  Result := StrToInt(sDefaultServerPort);
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
    Result := sDefaultWebModuleName
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
  Result := chkCreateControllerUnit.Checked;
end;

function TfrmDMVCNewProject.GetCreateCRUDMethods: boolean;
begin
  Result := chkCreateCRUDMethods.Checked;
end;

function TfrmDMVCNewProject.GetControllerClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
  begin
    Result := sDefaultControllerName
  end
  else
  begin
    Result := Trim(edtClassName.Text);
  end;
end;

end.
