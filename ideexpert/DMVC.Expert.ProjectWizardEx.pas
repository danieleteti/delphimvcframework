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

unit DMVC.Expert.ProjectWizardEx;

interface

uses
  ToolsApi,
  VCL.Graphics,
  PlatformAPI;

type
  TDMVCNewProjectWizard = class
  public
    class procedure RegisterDMVCProjectWizard(const APersonality: string);
  end;

implementation

{$I ..\sources\dmvcframework.inc}

uses
  DccStrs,
  System.IOUtils,
  VCL.Controls,
  VCL.Forms,
  WinApi.Windows,
  System.SysUtils,
  DMVC.Expert.Forms.NewProjectWizard,
  ExpertsRepository,
  JsonDataObjects,
  DMVC.Expert.Commons,
  DMVC.Expert.ProjectGenerator;

resourcestring
  sNewDMVCProjectCaption = 'DelphiMVCFramework Project';
  sNewDMVCProjectHint = 'Create New DelphiMVCFramework Project with Controller';

class procedure TDMVCNewProjectWizard.RegisterDMVCProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality, sNewDMVCProjectHint, sNewDMVCProjectCaption,
    'DMVC.Wizard.NewProjectWizard', // do not localize
    'DelphiMVCFramework', 'DelphiMVCFramework Team - https://github.com/danieleteti/delphimvcframework', // do not localize
    procedure
    var
      WizardForm: TfrmDMVCNewProject;
      lJSON: TJSONObject;
      lProjectFolder: string;
      lProjectName: string;
      lProjectPath: string;
      lProject: IOTAProject;
      lConfig: IOTABuildConfiguration;
    begin
      WizardForm := TfrmDMVCNewProject.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          // Get project info from wizard
          lProjectFolder := WizardForm.ProjectFolder;
          lProjectName := WizardForm.ProjectName;
          lJSON := WizardForm.GetConfigModel;

          // Generate all project files to disk
          // This approach avoids timing issues - all unit names are known upfront
          TDMVCProjectGenerator.Generate(lProjectFolder, lProjectName, lJSON);

          // Open the generated project in the IDE
          lProjectPath := TPath.Combine(lProjectFolder, lProjectName + '.dpr');
          (BorlandIDEServices as IOTAActionServices).OpenFile(lProjectPath);

          // Configure the project
          lProject := GetActiveProject;
          if lProject <> nil then
          begin
            lConfig := (lProject.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
            lConfig.SetValue(sUnitSearchPath, '$(DMVC)');
            lConfig.SetValue(sFramework, 'FMX');
            // Set output directories
            // EXE goes to .\bin for easy deployment
            // DCU goes to standard .\$(Platform)\$(Config) folder
            lConfig.SetValue(sExeOutput, '.\bin');
            lConfig.SetValue(sDcuOutput, '.\$(Platform)\$(Config)');
          end;
        end;
      finally
        WizardForm.Free;
      end;
    end,
    function: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF}
    begin
      Result := LoadIcon(HInstance, 'DMVCNewProjectIcon');
    end, TArray<string>.Create(cWin32Platform, cWin64Platform
    {$IF Defined(TOKYOORBETTER)}
    , cLinux64Platform
    {$ENDIF}
    ), nil));
end;

end.
