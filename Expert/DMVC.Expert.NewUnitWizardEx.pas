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

uses
  DMVC.Expert.Forms.NewUnitWizard,
  DMVC.Expert.CodeGen.NewControllerUnit,
  Controls,
  Forms,
  Windows,
  ExpertsRepository;

resourcestring
 sNewDMVCUnitCaption = 'DMVC Unit';
 sNewDMVCProjectHint = 'Create New DMVC Unit';


class procedure TDMVCNewUnitWizard.RegisterDMVCNewUnitWizard(const aPersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(aPersonality,
                        sNewDMVCProjectHint, sNewDMVCUnitCaption, 'DMVC.Wizard.NewUnitWizard',  // do not localize
                        'DMVC', 'DMVC Team - https://github.com/danieleteti/delphimvcframework', // do not localize
    procedure
    var
      WizardForm     : TfrmDMVCNewUnit;
      ModuleServices : IOTAModuleServices;
      Project        : IOTAProject;
      ControllerUnit : IOTAModule;
    begin
      WizardForm := TfrmDMVCNewUnit.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          ModuleServices := (BorlandIDEServices as IOTAModuleServices);
          Project :=  GetActiveProject;
          ControllerUnit := ModuleServices.CreateModule(
                           TNewControllerUnitEx.Create(WizardForm.CreateIndexMethod,
                                                       WizardForm.ControllerClassName,
                                                       aPersonality));
          if Project <> nil then
          begin
            Project.AddFile(ControllerUnit.FileName,true);
          end;
        end;
      finally
        WizardForm.Free;
      end;
    end,
            function: Cardinal
            begin
              Result := LoadIcon(HInstance,'DMVCNewUnitIcon');
            end,
            TArray<string>.Create(cWin32Platform, cWin64Platform),
            nil));
 end;

end.
