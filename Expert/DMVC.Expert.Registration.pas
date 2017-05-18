unit DMVC.Expert.Registration;

interface

//Note: "Register" method name is case senstive.
procedure Register;

implementation

uses
  ToolsApi,
  Vcl.Dialogs,
  DMVC.Expert.ProjectWizardEx,
  DMVC.Expert.NewUnitWizardEx;

procedure Register;
begin
  TDMVCNewProjectWizard.RegisterDMVCProjectWizard(sDelphiPersonality);
  TDMVCNewUnitWizard.RegisterDMVCNewUnitWizard(sDelphiPersonality);
end;

end.
