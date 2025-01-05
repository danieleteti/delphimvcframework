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
  private
    class function GetUnitName(aFilename: string): string;
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
  DMVC.Expert.CodeGen.NewDMVCProject,
  DMVC.Expert.CodeGen.NewControllerUnit,
  DMVC.Expert.CodeGen.NewWebModuleUnit,
  ExpertsRepository,
  JsonDataObjects,
  DMVC.Expert.Commons, DMVC.Expert.CodeGen.SourceFile,
  DMVC.Expert.Commands.Templates;

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
      ServicesUnit: IOTAModule;
      WebModuleUnit: IOTAModule;
      MustacheHelperUnit: IOTAModule;
      TemplateProHelperUnit: IOTAModule;
      ControllerCreator: IOTACreator;
      EntityCreator: IOTACreator;
      JSONRPCUnitCreator: IOTACreator;
      ServicesUnitCreator: IOTACreator;
      HelpersUnitCreator: IOTACreator;
      WebModuleCreator: IOTAModuleCreator;
      lProjectSourceCreator: IOTACreator;
      lJSONRPCUnitName: string;
      lServicesUnitName: string;
      lJSON: TJSONObject;
      lMustacheHelpersUnitName: string;
      lTemplateProHelpersUnitName: string;
      lEntityUnitName: string;
      EntityUnit: IOTAModule;
    lWebStencilsHelpersUnitName: string;
    WebStencilsHelperUnit: IOTAModule;
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
          lJSON := WizardForm.GetConfigModel;

          // Create Project Source
          lProjectSourceCreator := TDMVCProjectFile.Create(APersonality, lJSON);
          ModuleServices.CreateModule(lProjectSourceCreator);
          Project := GetActiveProject;

          Config := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
          Config.SetValue(sUnitSearchPath, '$(DMVC)');
          Config.SetValue(sFramework, 'FMX');


          lEntityUnitName := '';
          // Create ENTITY Unit
          if lJSON.B[TConfigKey.controller_crud_methods_generate] then
          begin
            EntityCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillEntitiesTemplates,
              TConfigKey.entity_unit_name,
              APersonality);
            EntityUnit := ModuleServices.CreateModule(EntityCreator);
            ChangeIOTAModuleFileNamePrefix(EntityUnit, 'Entity.' + lJSON.S[TConfigKey.entity_classname].Substring(1));
            lEntityUnitName := GetUnitName(EntityUnit.FileName);
            lJSON.S[TConfigKey.entity_unit_name] := lEntityUnitName;
            if Project <> nil then
            begin
              Project.AddFile(EntityUnit.FileName, True);
            end;
          end;

          lServicesUnitName := '';
          // Create Services Unit
          if lJSON.B[TConfigKey.program_service_container_generate] then
          begin
            ServicesUnitCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillServicesTemplates,
              TConfigKey.program_service_container_unit_name,
              APersonality);
            ServicesUnit := ModuleServices.CreateModule(ServicesUnitCreator);
            ChangeIOTAModuleFileNamePrefix(ServicesUnit, 'Services');
            lServicesUnitName := GetUnitName(ServicesUnit.FileName);
            lJSON.S[TConfigKey.program_service_container_unit_name] := lServicesUnitName;
            if Project <> nil then
            begin
              Project.AddFile(ServicesUnit.FileName, True);
            end;
          end;

          // Create Controller Unit
          if WizardForm.CreateControllerUnit then
          begin
            ControllerCreator := TNewControllerUnitEx.Create(lJSON, APersonality);
            ControllerUnit := ModuleServices.CreateModule(ControllerCreator);
            ChangeIOTAModuleFileNamePrefix(ControllerUnit, 'Controllers.' + lJSON.S[TConfigKey.controller_classname].SubString(1));
            lJSON.S[TConfigKey.controller_unit_name] := TPath.GetFileNameWithoutExtension(ControllerUnit.FileName);
            if Project <> nil then
            begin
              Project.AddFile(ControllerUnit.FileName, True);
            end;
          end;


          lJSONRPCUnitName := '';
          // Create JSONRPC Unit
          if lJSON.B[TConfigKey.jsonrpc_generate] then
          begin
            JSONRPCUnitCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillJSONRPCTemplates,
              TConfigKey.jsonrpc_unit_name,
              APersonality);
            JSONRPCUnit := ModuleServices.CreateModule(JSONRPCUnitCreator);
            ChangeIOTAModuleFileNamePrefix(JSONRPCUnit, 'JSONRPC.' + lJSON.S[TConfigKey.jsonrpc_classname].Substring(1));
            lJSONRPCUnitName := GetUnitName(JSONRPCUnit.FileName);
            lJSON.S[TConfigKey.jsonrpc_unit_name] := lJSONRPCUnitName;
            if Project <> nil then
            begin
              Project.AddFile(JSONRPCUnit.FileName, True);
            end;
          end;

          {********** SERVER SIDE VIEWS TEMPLATE ENGINE CONFIGURATION **************}

          lMustacheHelpersUnitName := '';
          // Create Mustache Helpers Unit
          if lJSON.B[TConfigKey.program_ssv_mustache] then
          begin
            HelpersUnitCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillMustacheTemplates,
              TConfigKey.mustache_helpers_unit_name,
              APersonality);
            MustacheHelperUnit := ModuleServices.CreateModule(HelpersUnitCreator);
            ChangeIOTAModuleFileNamePrefix(MustacheHelperUnit, 'MustacheHelpers');
            lMustacheHelpersUnitName := GetUnitName(MustacheHelperUnit.FileName);
            lJSON.S[TConfigKey.mustache_helpers_unit_name] := lMustacheHelpersUnitName;
            if Project <> nil then
            begin
              Project.AddFile(MustacheHelperUnit.FileName, True);
            end;
          end;

          lTemplateProHelpersUnitName := '';
          // Create TemplatePro Helpers Unit
          if lJSON.B[TConfigKey.program_ssv_templatepro] then
          begin
            HelpersUnitCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillTemplateProTemplates,
              TConfigKey.templatepro_helpers_unit_name,
              APersonality);
            TemplateProHelperUnit := ModuleServices.CreateModule(HelpersUnitCreator);
            ChangeIOTAModuleFileNamePrefix(TemplateProHelperUnit, 'TemplateProHelpers');
            lTemplateProHelpersUnitName := GetUnitName(TemplateProHelperUnit.FileName);
            lJSON.S[TConfigKey.templatepro_helpers_unit_name] := lTemplateProHelpersUnitName;
            if Project <> nil then
            begin
              Project.AddFile(TemplateProHelperUnit.FileName, True);
            end;
          end;

          lWebStencilsHelpersUnitName := '';
          // Create WebStencils Helpers Unit
          if lJSON.B[TConfigKey.program_ssv_webstencils] then
          begin
            HelpersUnitCreator := TNewGenericUnitFromTemplate.Create(
              lJSON,
              FillWebStencilsTemplates,
              TConfigKey.webstencils_helpers_unit_name,
              APersonality);
            WebStencilsHelperUnit := ModuleServices.CreateModule(HelpersUnitCreator);
            ChangeIOTAModuleFileNamePrefix(WebStencilsHelperUnit, 'WebStencilsHelpers');
            lWebStencilsHelpersUnitName := GetUnitName(WebStencilsHelperUnit.FileName);
            lJSON.S[TConfigKey.webstencils_helpers_unit_name] := lWebStencilsHelpersUnitName;
            if Project <> nil then
            begin
              Project.AddFile(WebStencilsHelperUnit.FileName, True);
            end;
          end;

          {******** END - SERVER SIDE VIEWS TEMPLATE ENGINE CONFIGURATION ************}

          // Create Webmodule Unit
          WebModuleCreator := TNewWebModuleUnitEx.Create(
            lJSON,
            APersonality);
          WebModuleUnit := ModuleServices.CreateModule(WebModuleCreator);
          ChangeIOTAModuleFileNamePrefix(WebModuleUnit, lJSON.S[TConfigKey.webmodule_classname].SubString(1));
          lJSON.S[TConfigKey.webmodule_unit_name] := TPath.GetFileNameWithoutExtension(WebModuleUnit.FileName);
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
