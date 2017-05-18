unit DMVC.Expert.Forms.NewProjectWizard;

interface

uses
  WinAPI.Windows,
  WinAPI.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls;

type
  TfrmDMVCNewProject = class(TForm)
    gbControllerUnitOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkCreateIndexMethod: TCheckBox;
    chkCreateControllerUnit: TCheckBox;
    chkAddToProjectGroup: TCheckBox;
    edtClassName: TEdit;
    lblClassName: TLabel;
    edtWebModuleName: TEdit;
    Label1: TLabel;
    lblWbModule: TLabel;
    procedure chkCreateControllerUnitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetAddToProjectGroup: boolean;
    function GetCreateIndexMethod: boolean;
    function GetCreateControllerUnit: boolean;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property ControllerClassName: string read GetControllerClassName;
    property CreateControllerUnit: Boolean read GetCreateControllerUnit;
    property AddToProjectGroup: Boolean read GetAddToProjectGroup;
    property CreateIndexMethod: Boolean read GetCreateIndexMethod;
    property WebModuleClassName: string read GetWebModuleClassName;
  end;

var
  frmDMVCNewProject: TfrmDMVCNewProject;

implementation

uses
  DMVC.Expert.CodeGen.Templates;

{$R *.dfm}

procedure TfrmDMVCNewProject.chkCreateControllerUnitClick(Sender: TObject);
begin
  gbControllerUnitOptions.Enabled := chkCreateIndexMethod.Checked;
  chkCreateIndexMethod.Enabled := chkCreateControllerUnit.Checked;
  edtClassName.Enabled := chkCreateControllerUnit.Checked;
end;

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := sDefaultControllerName;
  edtWebModuleName.TextHint := sDefaultWebModuleName;
end;

function TfrmDMVCNewProject.GetAddToProjectGroup: boolean;
begin
  Result := chkAddToProjectGroup.Checked;
end;

function TfrmDMVCNewProject.GetCreateIndexMethod: boolean;
begin
  Result := chkCreateIndexMethod.Checked;
end;

function TfrmDMVCNewProject.GetWebModuleClassName: string;
begin
  if Trim(edtWebModuleName.Text) = '' then
  begin
    Result := sDefaultWebModuleName
  end else
  begin
    Result := Trim(edtWebModuleName.Text);
  end;
end;

function TfrmDMVCNewProject.GetCreateControllerUnit: boolean;
begin
  Result := chkCreateControllerUnit.Checked;
end;

function TfrmDMVCNewProject.GetControllerClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
  begin
    Result := sDefaultControllerName
  end else
  begin
    Result := Trim(edtClassName.Text);
  end;
end;

end.
