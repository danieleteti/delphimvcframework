// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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

unit DMVC.Expert.NewUnitWizardEx;

interface

uses
  ToolsApi,
  VCL.Graphics,
  PlatformAPI;

type
  TDMVCNewUnitWizard = class
  public
    class procedure RegisterDMVCNewUnitWizard(const APersonality: string);
  end;

implementation

{$I ..\sources\dmvcframework.inc}

uses
  DMVC.Expert.Forms.NewUnitWizard,
  DMVC.Expert.CodeGen.NewControllerUnit,
  VCL.Controls,
  VCL.Forms,
  WinApi.Windows,
  ExpertsRepository, JsonDataObjects;

resourcestring
  sNewDMVCUnitCaption = 'DelphiMVCFramework Controller';
  sNewDMVCProjectHint = 'Create New DelphiMVCFramework Controller Unit';

class procedure TDMVCNewUnitWizard.RegisterDMVCNewUnitWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality, sNewDMVCProjectHint,
    sNewDMVCUnitCaption, 'DMVC.Wizard.NewUnitWizard', // do not localize
    'DelphiMVCFramework', 'DelphiMVCFramework Team - https://github.com/danieleteti/delphimvcframework', // do not localize
    procedure
    var
      WizardForm: TfrmDMVCNewUnit;
      ModuleServices: IOTAModuleServices;
      Project: IOTAProject;
      ControllerUnit: IOTAModule;
      lJSON: TJSONObject;
    begin
      WizardForm := TfrmDMVCNewUnit.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          lJSON := WizardForm.GetConfigModel;
          ModuleServices := (BorlandIDEServices as IOTAModuleServices);
          Project := GetActiveProject;
          ControllerUnit := ModuleServices.CreateModule(
            TNewControllerUnitEx.Create(lJSON, APersonality));
          if Project <> nil then
          begin
            Project.AddFile(ControllerUnit.FileName, true);
          end;
        end;
      finally
        WizardForm.Free;
      end;
    end,
    function: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF}
    begin
      Result := LoadIcon(HInstance, 'DMVCNewUnitIcon');
    end, TArray<string>.Create(cWin32Platform, cWin64Platform
    {$IF Defined(TOKYOORBETTER)}
    , cLinux64Platform
    {$ENDIF}
    ), nil));
end;

end.
