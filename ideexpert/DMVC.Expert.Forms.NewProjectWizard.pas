unit DMVC.Expert.Forms.NewProjectWizard;

interface

uses
  WinAPI.Windows,
  WinAPI.Messages,
  WinAPI.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.Imaging.pngimage, VCL.ExtCtrls;

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
    chkCreateActionFiltersMethods: TCheckBox;
    edtServerPort: TEdit;
    Label2: TLabel;
    Image1: TImage;
    lblFrameworkVersion: TLabel;
    chkCreateCRUDMethods: TCheckBox;
    chkAnalyticsMiddleware: TCheckBox;
    procedure chkCreateControllerUnitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
    function GetAddToProjectGroup: boolean;
    function GetCreateIndexMethod: boolean;
    function GetCreateControllerUnit: boolean;
    function GetControllerClassName: string;
    function GetWebModuleClassName: string;
    function GetCreateActionFiltersMethods: boolean;
    function GetServerPort: Integer;
    function GetCreateCRUDMethods: boolean;
    function GetAnalyticsSupport: boolean;
    function GetMiddlewares: TArray<String>;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property ControllerClassName: string read GetControllerClassName;
    property CreateControllerUnit: boolean read GetCreateControllerUnit;
    property AddToProjectGroup: boolean read GetAddToProjectGroup;
    property CreateIndexMethod: boolean read GetCreateIndexMethod;
    property CreateCRUDMethods: boolean read GetCreateCRUDMethods;
    property AnalyticsSupport: boolean read GetAnalyticsSupport;
    property Middlewares: TArray<String> read GetMiddlewares;
    property CreateActionFiltersMethods: boolean
      read GetCreateActionFiltersMethods;
    property WebModuleClassName: string read GetWebModuleClassName;
    property ServerPort: Integer read GetServerPort;
  end;

var
  frmDMVCNewProject: TfrmDMVCNewProject;

implementation

uses
  DMVC.Expert.CodeGen.Templates,
  MVCFramework.Commons;

{$R *.dfm}

procedure TfrmDMVCNewProject.chkCreateControllerUnitClick(Sender: TObject);
begin
  gbControllerUnitOptions.Enabled := chkCreateIndexMethod.Checked;
  chkCreateIndexMethod.Enabled := chkCreateControllerUnit.Checked;
  chkCreateActionFiltersMethods.Enabled := chkCreateControllerUnit.Checked;
  edtClassName.Enabled := chkCreateControllerUnit.Checked;
end;

procedure TfrmDMVCNewProject.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := sDefaultControllerName;
  edtWebModuleName.TextHint := sDefaultWebModuleName;
  edtServerPort.TextHint := sDefaultServerPort;
  lblFrameworkVersion.Caption := DMVCFRAMEWORK_VERSION;
end;

function TfrmDMVCNewProject.GetAddToProjectGroup: boolean;
begin
  Result := chkAddToProjectGroup.Checked;
end;

function TfrmDMVCNewProject.GetAnalyticsSupport: boolean;
begin
  Result := chkAnalyticsMiddleware.Checked;
end;

function TfrmDMVCNewProject.GetCreateIndexMethod: boolean;
begin
  Result := chkCreateIndexMethod.Checked;
end;

function TfrmDMVCNewProject.GetMiddlewares: TArray<String>;
begin
  Result := [];
  if AnalyticsSupport then
  begin
    Result := Result + ['TMVCAnalyticsMiddleware.Create(GetLoggerForAnalytics)'];
  end;
end;

function TfrmDMVCNewProject.GetServerPort: Integer;
var
  lServerPort: Integer;
begin
  Result := StrToInt(sDefaultServerPort);
  if (Trim(edtServerPort.Text) <> '') and TryStrToInt(edtServerPort.Text,
    lServerPort) then
  begin
    if (lServerPort > 0) and (lServerPort < 65535) then
      Result := lServerPort;
  end;
end;

function TfrmDMVCNewProject.GetWebModuleClassName: string;
begin
  if Trim(edtWebModuleName.Text) = '' then
  begin
    Result := sDefaultWebModuleName
  end
  else
  begin
    Result := Trim(edtWebModuleName.Text);
  end;
end;

procedure TfrmDMVCNewProject.Image1Click(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),
    PChar('https://www.gitbook.com/book/danieleteti/delphimvcframework/details'),
    nil, nil, SW_SHOW);
end;

function TfrmDMVCNewProject.GetCreateActionFiltersMethods: boolean;
begin
  Result := chkCreateActionFiltersMethods.Checked;
end;

function TfrmDMVCNewProject.GetCreateControllerUnit: boolean;
begin
  Result := chkCreateControllerUnit.Checked;
end;

function TfrmDMVCNewProject.GetCreateCRUDMethods: boolean;
begin
  Result := chkCreateCRUDMethods.Checked;
end;

function TfrmDMVCNewProject.GetControllerClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
  begin
    Result := sDefaultControllerName
  end
  else
  begin
    Result := Trim(edtClassName.Text);
  end;
end;

end.
