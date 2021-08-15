unit MainClientFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  MVCFramework.Middleware.JWT,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    MemoJWT: TMemo;
    MemoRawResponse: TMemo;
    Panel1: TPanel;
    btnGet: TButton;
    btnLOGIN: TButton;
    Splitter2: TSplitter;
    btnLogout: TButton;
    btnPublicResource: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    Label2: TLabel;
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnPublicResourceClick(Sender: TObject);
  private
    FJWT: string;
    procedure SetJWT(const Value: string);
    property JWT: string read FJWT write SetJWT;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


uses
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  MVCFramework.SystemJSONUtils,
  System.JSON, MVCFramework.Commons;

procedure TMainForm.btnGetClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  { Getting JSON response }
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  if not FJWT.IsEmpty then
  begin
    lClient.SetBearerAuthorization(FJWT);
  end;

  lClient
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .AddQueryStringParam('firstname', 'Daniele')
    .AddQueryStringParam('lastname', 'Teti');
  lResp := lClient.Get('/admin/role1');
  if not lResp.Success then
    ShowMessage(lResp.Content);

  MemoRawResponse.Lines.Text := lResp.Content;
end;

procedure TMainForm.btnLOGINClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  lClient.SetBasicAuthorization('user1', 'user1');
  lRest := lClient.Post('/login');
  MemoRawResponse.Lines.Text := lRest.Content;

  if not lRest.Success then
  begin
    ShowMessage(
      'HTTP ERROR: ' + lRest.StatusCode.ToString + sLineBreak +
      'HTTP ERROR MESSAGE: ' + lRest.StatusText + sLineBreak +
      'ERROR MESSAGE: ' + lRest.Content);
    Exit;
  end;

  lJSON := TSystemJSON.StringAsJSONObject(lRest.Content);
  try
    JWT := lJSON.GetValue('token').Value;
  finally
    lJSON.Free;
  end;
end;

procedure TMainForm.btnLogoutClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  if FJWT.IsEmpty then
  begin
    ShowMessage('Please, do a login first');
    Exit;
  end;

  lClient.SetBearerAuthorization(FJWT);
  lResp := lClient.Get('/logout');
  MemoRawResponse.Lines.Text := lResp.Content;

  if lResp.Success then
  begin
    ShowMessage('Now your current JWT has been blacklisted by the server. Any subsequent request with this token is forbidden');
  end;
end;

procedure TMainForm.btnPublicResourceClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  { Getting JSON response }
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lResp := lClient
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .Get('/public');
  MemoRawResponse.Lines.Text := lResp.Content;
end;

procedure TMainForm.SetJWT(const Value: string);
begin
  FJWT := Value;
  MemoJWT.Lines.Text := Value;
end;

end.
