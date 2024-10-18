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

unit DMVC.Expert.Forms.NewUnitWizard;

interface

uses
  WinAPI.Windows,
  WinAPI.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  JsonDataObjects;

type
  TfrmDMVCNewUnit = class(TForm)
    GroupBox1: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblClassName: TLabel;
    edtClassName: TEdit;
    chkCreateIndexMethod: TCheckBox;
    chkCreateActionFiltersMethods: TCheckBox;
    chkCreateCRUDMethods: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fModel: TJSONObject;
    function GetCreateIndexMethod: boolean;
    function GetControllerClassName: string;
    function GetCreateActionFiltersMethods: boolean;
    function GetCreateCRUDMethods: boolean;
    function GetAddAnalyticsMiddleware: boolean;
    { Private declarations }
  public
    { Public declarations }
    property ControllerClassName: string read GetControllerClassName;
    property CreateIndexMethod: boolean read GetCreateIndexMethod;
    property CreateCRUDMethods: boolean read GetCreateCRUDMethods;
    property CreateActionFiltersMethods: boolean read GetCreateActionFiltersMethods;
    property AddAnalyticsMiddleware: boolean read GetAddAnalyticsMiddleware;
    function GetConfigModel: TJSONObject;
  end;

implementation

uses
  DMVC.Expert.Commons;

{$R *.dfm}

procedure TfrmDMVCNewUnit.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := TDefaultValues.sDefaultControllerName;
  fModel := TJsonObject.Create;
end;

function TfrmDMVCNewUnit.GetCreateActionFiltersMethods: boolean;
begin
  Result := chkCreateActionFiltersMethods.Checked;
end;

function TfrmDMVCNewUnit.GetCreateCRUDMethods: boolean;
begin
  Result := chkCreateCRUDMethods.Checked;
end;

function TfrmDMVCNewUnit.GetCreateIndexMethod: boolean;
begin
  Result := chkCreateIndexMethod.Checked;
end;

procedure TfrmDMVCNewUnit.FormDestroy(Sender: TObject);
begin
  fModel.Free;
end;

function TfrmDMVCNewUnit.GetAddAnalyticsMiddleware: boolean;
begin
  Result := False;
end;

function TfrmDMVCNewUnit.GetConfigModel: TJSONObject;
begin
  fModel.S[TConfigKey.program_name] :=  'TBA';
  fModel.S[TConfigKey.controller_unit_name] := 'TBA';
  fModel.S[TConfigKey.controller_classname] :=  GetControllerClassName;
  fModel.B[TConfigKey.controller_index_methods_generate] :=  chkCreateIndexMethod.Checked;
  fModel.B[TConfigKey.controller_action_filters_generate] :=  chkCreateActionFiltersMethods.Checked;
  fModel.B[TConfigKey.controller_crud_methods_generate] :=  chkCreateCRUDMethods.Checked;
  fModel.B[TConfigKey.entity_generate] :=  fModel.B[TConfigKey.controller_crud_methods_generate];
  fModel.S[TConfigKey.entity_classname] :=  'TPerson';

  Result := fModel;
end;

function TfrmDMVCNewUnit.GetControllerClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
  begin
    Result := TDefaultValues.sDefaultControllerName
  end
  else
  begin
    Result := Trim(edtClassName.Text);
  end;
end;

end.
