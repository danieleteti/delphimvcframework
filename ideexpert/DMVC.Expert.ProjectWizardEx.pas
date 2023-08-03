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
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges.
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
  private
    class function GetUnitName(aFilename: string): string;
  public
    class procedure RegisterDMVCProjectWizard(const APersonality: string);
  end;

implementation

{$I dmvcframework.inc}

uses
  DccStrs,
  System.IOUtils,
  VCL.Controls,
  VCL.Forms,
  WinApi.Windows,
  System.SysUtils,
  DMVC.Expert.Forms.NewProjectWizard,
  DMVC.Expert.CodeGen.NewDMVCProject,
  DMVC.Expert.CodeGen.NewControllerUnit,
  DMVC.Expert.CodeGen.NewWebModuleUnit,
  ExpertsRepository;

resourcestring
  sNewDMVCProjectCaption = 'DelphiMVCFramework Project';
  sNewDMVCProjectHint = 'Create New DelphiMVCFramework Project with Controller';

  { TDUnitXNewProjectWizard }

class function TDMVCNewProjectWizard.GetUnitName(aFilename: string): string;
begin
  Result := TPath.GetFileNameWithoutExtension(aFilename);
end;

class procedure TDMVCNewProjectWizard.RegisterDMVCProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality, sNewDMVCProjectHint, sNewDMVCProjectCaption,
    'DMVC.Wizard.NewProjectWizard', // do not localize
    'DMVCFramework', 'DMVCFramework Team - https://github.com/danieleteti/delphimvcframework', // do not localize
    procedure
    var
      WizardForm: TfrmDMVCNewProject;
      ModuleServices: IOTAModuleServices;
      Project: IOTAProject;
      Config: IOTABuildConfiguration;
      ControllerUnit: IOTAModule;
      JSONRPCUnit: IOTAModule;
      WebModuleUnit: IOTAModule;
      ControllerCreator: IOTACreator;
      JSONRPCUnitCreator: IOTACreator;
      WebModuleCreator: IOTAModuleCreator;
      lProjectSourceCreator: IOTACreator;
      lJSONRPCUnitName: string;
    begin
      WizardForm := TfrmDMVCNewProject.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          if not WizardForm.AddToProjectGroup then
          begin
            (BorlandIDEServices as IOTAModuleServices).CloseAll;
          end;
          ModuleServices := (BorlandIDEServices as IOTAModuleServices);

          // Create Project Source
          lProjectSourceCreator := TDMVCProjectFile.Create(APersonality);
          TDMVCProjectFile(lProjectSourceCreator).DefaultPort := WizardForm.ServerPort;
          ModuleServices.CreateModule(lProjectSourceCreator);
          Project := GetActiveProject;

          Config := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
          Config.SetValue(sUnitSearchPath, '$(DMVC)');
          Config.SetValue(sFramework, 'VCL');

          // Create Controller Unit
          if WizardForm.CreateControllerUnit then
          begin
            ControllerCreator := TNewControllerUnitEx.Create(
              WizardForm.CreateIndexMethod,
              WizardForm.CreateCRUDMethods,
              WizardForm.CreateActionFiltersMethods,
              WizardForm.ControllerClassName,
              APersonality);
            ControllerUnit := ModuleServices.CreateModule(ControllerCreator);
            if Project <> nil then
            begin
              Project.AddFile(ControllerUnit.FileName, True);
            end;
          end;

          lJSONRPCUnitName := '';
          // Create JSONRPC Unit
          if not WizardForm.JSONRPCClassName.IsEmpty then
          begin
            JSONRPCUnitCreator := TNewJSONRPCUnitEx.Create(
              WizardForm.JSONRPCClassName,
              APersonality);
            JSONRPCUnit := ModuleServices.CreateModule(JSONRPCUnitCreator);
            lJSONRPCUnitName := GetUnitName(JSONRPCUnit.FileName);
            if Project <> nil then
            begin
              Project.AddFile(JSONRPCUnit.FileName, True);
            end;
          end;


          // Create Webmodule Unit
          WebModuleCreator := TNewWebModuleUnitEx.Create(
            WizardForm.WebModuleClassName,
            WizardForm.ControllerClassName,
            GetUnitName(ControllerUnit.FileName),
            WizardForm.Middlewares,
            WizardForm.JSONRPCClassName,
            lJSONRPCUnitName,
            APersonality);
          WebModuleUnit := ModuleServices.CreateModule(WebModuleCreator);
          if Project <> nil then
          begin
            Project.AddFile(WebModuleUnit.FileName, True);
          end;
        end;
      finally
        WizardForm.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(HInstance, 'DMVCNewProjectIcon');
    end, TArray<string>.Create(cWin32Platform, cWin64Platform
    {$IF Defined(TOKYOORBETTER)}
    , cLinux64Platform
    {$ENDIF}
    ), nil));
end;

end.
