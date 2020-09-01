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
  TForm5 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    btnGet: TButton;
    btnLOGIN: TButton;
    Splitter1: TSplitter;
    Memo3: TMemo;
    Splitter2: TSplitter;
    btnLoginWithException: TButton;
    btnLoginJsonObject: TButton;
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
    procedure btnLoginWithExceptionClick(Sender: TObject);
    procedure btnLoginJsonObjectClick(Sender: TObject);
  private
    FJWT: string;
    procedure SetJWT(const Value: string);
    property JWT: string read FJWT write SetJWT;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}


uses
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.SystemJSONUtils,
  System.JSON;

procedure TForm5.btnGetClick(Sender: TObject);
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
  lResp := lClient
    .AddQueryStringParam('firstname', 'Daniele')
    .AddQueryStringParam('lastname', 'Teti')
    .Get('/admin/role1');
  if not lResp.Success then
    ShowMessage(lResp.Content);
  Memo2.Lines.Text := lResp.Content;

  { Getting HTML response }
    // when the JWT authorization header is named "Authorization", the basic authorization must be disabled
  if not FJWT.IsEmpty then
  begin
    lClient.SetBearerAuthorization(FJWT);
  end;
  lResp := lClient
    .AddQueryStringParam('firstname', 'Daniele')
    .AddQueryStringParam('lastname', 'Teti')
    .Accept('text/html')
    .Get('/admin/role1');
  if not lResp.Success then
    ShowMessage(lResp.Content);
  Memo3.Lines.Text := lResp.Content;
end;

procedure TForm5.btnLOGINClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New
    .BaseURL('localhost', 8080)
    .ReadTimeOut(0)
    .SetBasicAuthorization('user1', 'user1');
  lRest := lClient.Post('/login');
  if not lRest.Success then
  begin
    ShowMessage(
      'HTTP ERROR: ' + lRest.StatusCode.ToString + sLineBreak +
      'CONTENT MESSAGE: ' + lRest.Content);
    Exit;
  end;

  lJSON := TSystemJSON.StringAsJSONObject(lRest.Content);
  try
    JWT := lJSON.GetValue('token').Value;
  finally
    lJSON.Free;
  end;
end;

procedure TForm5.btnLoginJsonObjectClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New
    .BaseURL('localhost', 8080)
    .ReadTimeOut(0);
  lRest := lClient.Post('/login', '{"jwtusername":"user1","jwtpassword":"user1"}');
  if not lRest.Success then
  begin
    ShowMessage(
      'HTTP ERROR: ' + lRest.StatusCode.ToString + sLineBreak +
      'CONTENT MESSAGE: ' + lRest.Content);
    Exit;
  end;

  lJSON := TSystemJSON.StringAsJSONObject(lRest.Content);
  try
    JWT := lJSON.GetValue('token').Value;
  finally
    lJSON.Free;
  end;
end;

procedure TForm5.btnLoginWithExceptionClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TMVCRESTClient.New
    .BaseURL('localhost', 8080)
    .ReadTimeOut(0)
    .SetBasicAuthorization('user_raise_exception', 'user_raise_exception');

  lRest := lClient.Post('/login');
  if not lRest.Success then
  begin
    ShowMessage(
      'HTTP ERROR: ' + lRest.StatusCode.ToString + sLineBreak +
      'CONTENT MESSAGE: ' + lRest.Content);
    Exit;
  end;

  lJSON := TSystemJSON.StringAsJSONObject(lRest.Content);
  try
    JWT := lJSON.GetValue('token').Value;
  finally
    lJSON.Free;
  end;
end;

procedure TForm5.SetJWT(const Value: string);
begin
  FJWT := Value;
  Memo1.Lines.Text := Value;
end;

end.
