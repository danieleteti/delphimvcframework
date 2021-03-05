unit MainFormU;

interface

uses
  REST.RestServer,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TStartServerProc = procedure(const Port: Integer); stdcall;
  TStopServerProc = procedure; stdcall;

  TMainForm = class(TForm)
    btnStartInProcess: TButton;
    Button3: TButton;
    btnStopInProcess: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnStartInProcessClick(Sender: TObject);
    procedure btnStopInProcessClick(Sender: TObject);
    procedure Label2MouseEnter(Sender: TObject);
    procedure Label2MouseLeave(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    fDll: THandle;
    fStartServer: TStartServerProc;
    fStopServer: TStopServerProc;
    fServer: TDMVCRestServer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


uses
  Winapi.ShellApi, System.UITypes;

procedure TMainForm.btnStartInProcessClick(Sender: TObject);
begin
  fServer := TDMVCRestServer.Create(8090);
  fServer.Activate;
end;

procedure TMainForm.btnStopInProcessClick(Sender: TObject);
begin
  FreeAndNil(fServer);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  fStopServer();
  FreeLibrary(fDll);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  fDll := LoadLibrary('DMVCServerDLL.dll');
  fStartServer := GetProcAddress(fDll, 'RunServer');
  if not Assigned(fStartServer) then
    raise Exception.Create('Cannot find "RunServer" dll function');
  fStopServer := GetProcAddress(fDll, 'StopServer');
  if not Assigned(fStopServer) then
    raise Exception.Create('Cannot find "StopServer" dll function');
  fStartServer(8080);
end;

procedure TMainForm.Label2Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar((Sender as TLabel).Caption), nil, nil, SW_SHOW);
end;

procedure TMainForm.Label2MouseEnter(Sender: TObject);
var
  lLabel: TLabel;
begin
  lLabel := Sender as TLabel;
  lLabel.Font.Color := clBlue;
  lLabel.Font.Style := lLabel.Font.Style + [fsUnderline];
  Screen.Cursor := crHandPoint;
end;

procedure TMainForm.Label2MouseLeave(Sender: TObject);
var
  lLabel: TLabel;
begin
  lLabel := Sender as TLabel;
  lLabel.Font.Color := clWindowText;
  lLabel.Font.Style := lLabel.Font.Style - [fsUnderline];
  Screen.Cursor := crDefault;
end;

end.
