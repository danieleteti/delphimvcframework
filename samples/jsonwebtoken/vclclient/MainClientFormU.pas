unit MainClientFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    gbLogin: TGroupBox;
    lblUser: TLabel;
    lblPass: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    btnLogin: TButton;
    btnLoginJSON: TButton;
    chkRememberMe: TCheckBox;
    gbActions: TGroupBox;
    btnGetRole1: TButton;
    btnGetRole2: TButton;
    btnGetPublic: TButton;
    btnLoginException: TButton;
    btnClear: TButton;
    gbToken: TGroupBox;
    mmoToken: TMemo;
    gbResponse: TGroupBox;
    mmoResponse: TMemo;
    StatusBar: TStatusBar;
    procedure btnLoginClick(Sender: TObject);
    procedure btnLoginJSONClick(Sender: TObject);
    procedure btnGetRole1Click(Sender: TObject);
    procedure btnGetRole2Click(Sender: TObject);
    procedure btnGetPublicClick(Sender: TObject);
    procedure btnLoginExceptionClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FJWT: string;
    procedure SetJWT(const Value: string);
    procedure LogRequest(const AMethod, APath: string);
    procedure LogResponse(AStatusCode: Integer; const AContent: string; ASuccess: Boolean);
    procedure DoLogin(const AUser, APass: string; AUseJSONBody: Boolean);
    procedure DoGet(const APath: string; AUseAuth: Boolean);
    property JWT: string read FJWT write SetJWT;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  MVCFramework.SystemJSONUtils,
  System.JSON,
  System.DateUtils;

const
  SERVER_HOST = 'localhost';
  SERVER_PORT = 8080;

{ TMainForm }

procedure TMainForm.SetJWT(const Value: string);
begin
  FJWT := Value;
  mmoToken.Lines.Text := Value;
  btnGetRole1.Enabled := not Value.IsEmpty;
  btnGetRole2.Enabled := not Value.IsEmpty;
  if not Value.IsEmpty then
    StatusBar.Panels[0].Text := 'Authenticated as ' + edtUsername.Text
  else
    StatusBar.Panels[0].Text := 'Not authenticated';
end;

procedure TMainForm.LogRequest(const AMethod, APath: string);
begin
  mmoResponse.Lines.Add('--- ' + AMethod + ' ' + APath + ' ---');
end;

procedure TMainForm.LogResponse(AStatusCode: Integer; const AContent: string; ASuccess: Boolean);
begin
  if ASuccess then
    mmoResponse.Lines.Add('HTTP ' + AStatusCode.ToString + ' OK')
  else
    mmoResponse.Lines.Add('HTTP ' + AStatusCode.ToString + ' ERROR');
  mmoResponse.Lines.Add(AContent);
  mmoResponse.Lines.Add('');
end;

procedure TMainForm.DoLogin(const AUser, APass: string; AUseJSONBody: Boolean);
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
  LJSON: TJSONObject;
  LSegment: string;
begin
  LSegment := '';
  if chkRememberMe.Checked then
    LSegment := '?rememberme=1';

  LClient := TMVCRESTClient.New.BaseURL(SERVER_HOST, SERVER_PORT);
  LClient.ReadTimeOut(0);

  if AUseJSONBody then
  begin
    LogRequest('POST', '/login' + LSegment + ' (JSON body)');
    LResp := LClient.Post('/login' + LSegment,
      '{"jwtusername":"' + AUser + '","jwtpassword":"' + APass + '"}');
  end
  else
  begin
    LogRequest('POST', '/login' + LSegment + ' (headers)');
    LClient.SetBasicAuthorization(AUser, APass);
    LResp := LClient.Post('/login' + LSegment);
  end;

  if not LResp.Success then
  begin
    LogResponse(LResp.StatusCode, LResp.Content, False);
    JWT := '';
    Exit;
  end;

  LogResponse(LResp.StatusCode, LResp.Content, True);

  LJSON := TSystemJSON.StringAsJSONObject(LResp.Content);
  try
    JWT := LJSON.GetValue('token').Value;
  finally
    LJSON.Free;
  end;
end;

procedure TMainForm.DoGet(const APath: string; AUseAuth: Boolean);
var
  LClient: IMVCRESTClient;
  LResp: IMVCRESTResponse;
begin
  LogRequest('GET', APath);

  LClient := TMVCRESTClient.New.BaseURL(SERVER_HOST, SERVER_PORT);
  LClient.ReadTimeOut(0);
  if AUseAuth and (not FJWT.IsEmpty) then
    LClient.SetBearerAuthorization(FJWT);

  LResp := LClient.Accept('application/json').Get(APath);
  LogResponse(LResp.StatusCode, LResp.Content, LResp.Success);
end;

// --- Button handlers ---

procedure TMainForm.btnLoginClick(Sender: TObject);
begin
  DoLogin(edtUsername.Text, edtPassword.Text, False);
end;

procedure TMainForm.btnLoginJSONClick(Sender: TObject);
begin
  DoLogin(edtUsername.Text, edtPassword.Text, True);
end;

procedure TMainForm.btnGetRole1Click(Sender: TObject);
begin
  DoGet('/admin/role1', True);
end;

procedure TMainForm.btnGetRole2Click(Sender: TObject);
begin
  DoGet('/admin/role2', True);
end;

procedure TMainForm.btnGetPublicClick(Sender: TObject);
begin
  DoGet('/public', False);
end;

procedure TMainForm.btnLoginExceptionClick(Sender: TObject);
begin
  DoLogin('user_raise_exception', 'user_raise_exception', False);
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  mmoResponse.Clear;
  mmoToken.Clear;
  JWT := '';
end;

end.
