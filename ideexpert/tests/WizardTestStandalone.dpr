program WizardTestStandalone;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {frmMain},
  DMVC.Expert.Forms.NewProjectWizard in '..\DMVC.Expert.Forms.NewProjectWizard.pas' {frmDMVCNewProject},
  DMVC.Expert.ProjectGenerator in '..\DMVC.Expert.ProjectGenerator.pas',
  DMVC.Expert.Commons in '..\DMVC.Expert.Commons.pas';

{$R *.res}
{$R ..\DMVC.Expert.Templates.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
