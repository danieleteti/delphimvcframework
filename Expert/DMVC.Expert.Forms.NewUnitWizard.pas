unit DMVC.Expert.Forms.NewUnitWizard;

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
  TfrmDMVCNewUnit = class(TForm)
    GroupBox1: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblClassName: TLabel;
    edtClassName: TEdit;
    chkCreateIndexMethod: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetCreateIndexMethod: boolean;
    function GetControllerClassName: string;
    { Private declarations }
  public
    { Public declarations }
    property ControllerClassName : string read GetControllerClassName;
    property CreateIndexMethod : boolean read GetCreateIndexMethod;

  end;

var
  frmDMVCNewUnit: TfrmDMVCNewUnit;

implementation
uses
  DMVC.Expert.CodeGen.Templates;

{$R *.dfm}

procedure TfrmDMVCNewUnit.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := sDefaultControllerName;
end;

function TfrmDMVCNewUnit.GetCreateIndexMethod: boolean;
begin
  Result := chkCreateIndexMethod.Checked;
end;

function TfrmDMVCNewUnit.GetControllerClassName: string;
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
