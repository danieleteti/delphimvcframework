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
  DMVC.Expert.Presets,
  ExpertsRepository,
  JsonDataObjects,
  DMVC.Expert.Commons,
  DMVC.Expert.ProjectGenerator;

procedure ExecuteWizardForPreset(APreset: TDMVCProjectPreset);
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
    // Configure wizard mode and setup pages
    WizardForm.SetCustomMode(APreset = ppCustom);
    WizardForm.SetPresetCaption(PRESET_INFOS[APreset].Caption);
    ApplyPresetToForm(APreset, WizardForm);
    WizardForm.InitWizardPages;

    if WizardForm.ShowModal = mrOk then
    begin
      lProjectFolder := WizardForm.ProjectFolder;
      lProjectName := WizardForm.ProjectName;
      lJSON := WizardForm.GetConfigModel;

      TDMVCProjectGenerator.Generate(lProjectFolder, lProjectName, lJSON);

      lProjectPath := TPath.Combine(lProjectFolder, lProjectName + '.dpr');
      (BorlandIDEServices as IOTAActionServices).OpenFile(lProjectPath);

      lProject := GetActiveProject;
      if lProject <> nil then
      begin
        lConfig := (lProject.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
        lConfig.SetValue(sUnitSearchPath, '$(DMVC)');
        lConfig.SetValue(sFramework, 'FMX');
        lConfig.SetValue(sExeOutput, '.\bin');
        lConfig.SetValue(sDcuOutput, '.\$(Platform)\$(Config)');
      end;
    end;
  finally
    WizardForm.Free;
  end;
end;

// Creates a TProc that captures the preset by value (avoids closure-in-loop issue)
function MakeWizardProc(APreset: TDMVCProjectPreset): TProc;
begin
  Result :=
    procedure
    begin
      ExecuteWizardForPreset(APreset);
    end;
end;

// Creates an icon loader function that captures the icon resource name by value
function MakeIconFunc(const AIconResource: string): TFunc<{$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF}>;
var
  LIconRes: string;
begin
  LIconRes := AIconResource;
  Result :=
    function: {$IFDEF WIN32}Cardinal{$ELSE}UInt64{$ENDIF}
    begin
      Result := LoadIcon(HInstance, PChar(LIconRes));
    end;
end;

class procedure TDMVCNewProjectWizard.RegisterDMVCProjectWizard(const APersonality: string);
var
  LPreset: TDMVCProjectPreset;
begin
  // Register in reverse order: IDE "Default" sort uses LIFO (last registered = first displayed)
  for LPreset := High(TDMVCProjectPreset) downto Low(TDMVCProjectPreset) do
  begin
    RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(
      APersonality,
      PRESET_INFOS[LPreset].Hint,
      PRESET_INFOS[LPreset].Caption,
      'DMVC.Wizard.NewProject.' + PRESET_INFOS[LPreset].IDSuffix, // unique ID per preset
      'DelphiMVCFramework', // page in Object Repository
      'DelphiMVCFramework Team - https://github.com/danieleteti/delphimvcframework',
      MakeWizardProc(LPreset),
      MakeIconFunc(PRESET_INFOS[LPreset].IconResource),
      TArray<string>.Create(cWin32Platform, cWin64Platform
      {$IF Defined(TOKYOORBETTER)}
      , cLinux64Platform
      {$ENDIF}
      ), nil));
  end;
end;

end.
