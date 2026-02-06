unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    btnOpenWizard: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    procedure btnOpenWizardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  DMVC.Expert.Forms.NewProjectWizard,
  DMVC.Expert.ProjectGenerator,
  DMVC.Expert.Commons,
  JsonDataObjects,
  System.IOUtils;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('DMVC Framework Wizard - Standalone Test');
  Memo1.Lines.Add('========================================');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Click "Open Wizard" to test the project wizard.');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('The generated project will be created in the folder you specify.');
  Memo1.Lines.Add('Check the log for details.');
end;

procedure TfrmMain.btnOpenWizardClick(Sender: TObject);
var
  LWizardForm: TfrmDMVCNewProject;
  LProjectFolder: string;
  LProjectName: string;
begin
  LWizardForm := TfrmDMVCNewProject.Create(nil);
  try
    if LWizardForm.ShowModal = mrOk then
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('========================================');
      Memo1.Lines.Add('Wizard completed successfully!');
      Memo1.Lines.Add('');

      // Get project info from the wizard's model
      LProjectFolder := LWizardForm.ProjectFolder;
      LProjectName := LWizardForm.ProjectName;

      Memo1.Lines.Add('Project Name: ' + LProjectName);
      Memo1.Lines.Add('Project Folder: ' + LProjectFolder);
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Generating project files...');
      Application.ProcessMessages;

      try
        // Call the generator (it uses the model from the wizard form)
        TDMVCProjectGenerator.Generate(
          LProjectFolder,
          LProjectName,
          LWizardForm.GetConfigModel
        );

        Memo1.Lines.Add('');
        Memo1.Lines.Add('SUCCESS! Project generated successfully!');
        Memo1.Lines.Add('');
        Memo1.Lines.Add('Location: ' + TPath.Combine(LProjectFolder, LProjectName));
        Memo1.Lines.Add('');
        Memo1.Lines.Add('You can now open the .dpr file in Delphi.');
        Memo1.Lines.Add('');

        // Show log file location
        Memo1.Lines.Add('Check log file for details:');
        Memo1.Lines.Add(TPath.Combine(TPath.GetHomePath, 'dmvc_wizard.log'));

        ShowMessage('Project generated successfully!'#13#10#13#10 +
                    'Location: ' + TPath.Combine(LProjectFolder, LProjectName));
      except
        on E: Exception do
        begin
          Memo1.Lines.Add('');
          Memo1.Lines.Add('ERROR: ' + E.ClassName + ': ' + E.Message);
          Memo1.Lines.Add('');
          Memo1.Lines.Add('Check log file: ' + TPath.Combine(TPath.GetHomePath, 'dmvc_wizard.log'));
          ShowMessage('Error generating project:'#13#10#13#10 + E.Message);
        end;
      end;
    end
    else
    begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Wizard cancelled by user.');
    end;
  finally
    LWizardForm.Free;
  end;
end;

end.
