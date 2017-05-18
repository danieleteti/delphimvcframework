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

uses
  DccStrs,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms,
  WinApi.Windows,
  System.SysUtils,
  DMVC.Expert.Forms.NewProjectWizard,
  DMVC.Expert.CodeGen.NewDMVCProject,
  DMVC.Expert.CodeGen.NewControllerUnit,
  DMVC.Expert.CodeGen.NewWebModuleUnit,
  ExpertsRepository;

resourcestring
  sNewDMVCProjectCaption = 'DMVC Project';
  sNewDMVCProjectHint = 'Create New DMVC Project';

{ TDUnitXNewProjectWizard }

class function TDMVCNewProjectWizard.GetUnitName(aFilename: string): string;
begin
  Result := TPath.GetFileNameWithoutExtension(aFilename);
end;

class procedure TDMVCNewProjectWizard.RegisterDMVCProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
                        sNewDMVCProjectHint, sNewDMVCProjectCaption, 'DMVC.Wizard.NewProjectWizard',  // do not localize
                        'DMVC', 'DMVC Team - https://github.com/danieleteti/delphimvcframework', // do not localize
    procedure
    var
      WizardForm        : TfrmDMVCNewProject;
      ModuleServices    : IOTAModuleServices;
      Project           : IOTAProject;
      Config            : IOTABuildConfiguration;
      ControllerUnit    : IOTAModule;
      WebModuleUnit     : IOTAModule;
      ControllerCreator : IOTACreator;
      WebModuleCreator  : IOTAModuleCreator;
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
          ModuleServices.CreateModule(TDMVCProjectFile.Create(APersonality));
          Project :=  GetActiveProject;

          Config := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
          Config.SetValue(sUnitSearchPath,'$(DMVC)');
          Config.SetValue(sFramework, 'VCL');

          // Create Controller Unit
          if WizardForm.CreateControllerUnit then
          begin
             ControllerCreator := TNewControllerUnitEx.Create(WizardForm.CreateIndexMethod, WizardForm.ControllerClassName, aPersonality);
             ControllerUnit := ModuleServices.CreateModule(ControllerCreator);
             if Project <> nil then
             begin
               Project.AddFile(ControllerUnit.FileName, True);
             end;
          end;

          // Create Webmodule Unit
          WebModuleCreator := TNewWebModuleUnitEx.Create(WizardForm.WebModuleClassName, WizardForm.ControllerClassName, GetUnitName(ControllerUnit.FileName),  APersonality);
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
              Result := LoadIcon(HInstance,'DMVCNewProjectIcon');
            end,
            TArray<string>.Create(cWin32Platform, cWin64Platform),
            nil));
end;

end.
