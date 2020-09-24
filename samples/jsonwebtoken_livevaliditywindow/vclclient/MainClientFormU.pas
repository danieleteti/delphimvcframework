unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    btnGet: TButton;
    btnLOGIN: TButton;
    Splitter1: TSplitter;
    Label1: TLabel;
    btnLoginWithHeaderBasic: TButton;
    Button1: TButton;
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
    procedure btnLoginWithHeaderBasicClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  MVCFramework.Middleware.JWT,
  MVCFramework.Commons,
  MVCFramework.SystemJSONUtils,
  System.NetEncoding,
  JSONDataObjects;

procedure TMainForm.btnGetClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
  tokenOld, tokenNew: string; // NEW CODE
begin
  tokenOld := FJWT; // NEW CODE
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  if not FJWT.IsEmpty then
  begin
    lClient.SetBearerAuthorization(FJWT);
  end;
  lClient
    .AddQueryStringParam('firstname', 'Daniele')
    .AddQueryStringParam('lastname', 'Teti');
  lResp := lClient.Get('/admin/role1');

  if not lResp.Success then
    ShowMessage(lResp.StatusCode.ToString + sLineBreak + lResp.Content);

  Memo2.Lines.Text := lResp.Content;

  // NEW CODE
  tokenNew := lResp.HeaderValue(TMVCJWTDefaults.AUTHORIZATION_HEADER);
  if tokenNew.StartsWith('Bearer', True) then
  begin
    tokenNew := tokenNew.Remove(0, 'Bearer'.Length).Trim;
    tokenNew := TNetEncoding.URL.URLDecode(tokenNew).Trim;
    JWT := tokenNew;
  end; // END NEW CODE
end;

procedure TMainForm.btnLOGINClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  lClient.AddHeader(TMVCJWTDefaults.USERNAME_HEADER, 'user1').AddHeader(TMVCJWTDefaults.PASSWORD_HEADER, 'user1');
  lRest := lClient.Get('/login'); { any HTTP verbs is OK }
  lJSON := StrToJSONObject(lRest.Content);
  try
    JWT := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
end;

procedure TMainForm.btnLoginWithHeaderBasicClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  lClient.SetBasicAuthorization('user1', 'user1');
  lRest := lClient.Post('/login');
  lJSON := StrToJSONObject(lRest.Content);
  try
    JWT := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.ReadTimeOut(0);
  lRest := lClient.Post('/login', '{"jwtusername":"user1","jwtpassword":"user1"}');
  lJSON := StrToJSONObject(lRest.Content);
  try
    JWT := lJSON.S['token'];
  finally
    lJSON.Free;
  end;
end;

procedure TMainForm.SetJWT(const Value: string);
begin
  FJWT := Value;
  Memo1.Lines.Text := Value;
end;

end.
