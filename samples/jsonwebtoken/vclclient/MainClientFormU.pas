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
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
    procedure btnLoginWithExceptionClick(Sender: TObject);
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
  MVCFramework.SystemJSONUtils,
  System.JSON;

procedure TForm5.btnGetClick(Sender: TObject);
var
  lClient: TRESTClient;
  lResp: IRESTResponse;
  lQueryStringParams: TStringList;
begin
  { Getting JSON response }
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.UseBasicAuthentication := False;
    lClient.ReadTimeOut(0);
    if not FJWT.IsEmpty then
    begin
      lClient.RequestHeaders.Values[TMVCJWTDefaults.AUTHORIZATION_HEADER] := 'Bearer ' + FJWT;
    end;
    lQueryStringParams := TStringList.Create;
    try
      lQueryStringParams.Values['firstname'] := 'Daniele';
      lQueryStringParams.Values['lastname'] := 'Teti';
      lResp := lClient.doGET('/admin/role1', [], lQueryStringParams);
      if lResp.HasError then
        ShowMessage(lResp.Error.ExceptionMessage);
    finally
      lQueryStringParams.Free;
    end;
    Memo2.Lines.Text := lResp.BodyAsString;
  finally
    lClient.Free;
  end;

  { Getting HTML response }
  lClient := TRESTClient.Create('localhost', 8080);
  try
    // when the JWT authorization header is named "Authorization", the basic authorization must be disabled
    lClient.UseBasicAuthentication := False;

    lClient.ReadTimeOut(0);
    if not FJWT.IsEmpty then
      lClient.RequestHeaders.Values[TMVCJWTDefaults.AUTHORIZATION_HEADER] := 'Bearer ' + FJWT;
    lQueryStringParams := TStringList.Create;
    try
      lQueryStringParams.Values['firstname'] := 'Daniele';
      lQueryStringParams.Values['lastname'] := 'Teti';
      lResp := lClient.Accept('text/html').doGET('/admin/role1', [], lQueryStringParams);
      if lResp.HasError then
        ShowMessage(lResp.Error.ExceptionMessage);
    finally
      lQueryStringParams.Free;
    end;
    Memo3.Lines.Text := lResp.BodyAsString;
  finally
    lClient.Free;
  end;

end;

procedure TForm5.btnLOGINClick(Sender: TObject);
var
  lClient: TRESTClient;
  lRest: IRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.ReadTimeOut(0);
    lClient.Authentication('user1', 'user1');
    lRest := lClient.doPOST('/login', []);
    if lRest.HasError then
    begin
      ShowMessage(
        'HTTP ERROR: ' + lRest.Error.HTTPError.ToString + sLineBreak +
        'APPLICATION ERROR CODE: ' + lRest.Error.ErrorNumber.ToString + sLineBreak +
        'EXCEPTION MESSAGE: ' + lRest.Error.ExceptionMessage);

      Exit;
    end;

    lJSON := TSystemJSON.StringAsJSONObject(lRest.BodyAsString);
    try
      JWT := lJSON.GetValue('token').Value;
    finally
      lJSON.Free;
    end;
  finally
    lClient.Free;
  end;
end;

procedure TForm5.btnLoginWithExceptionClick(Sender: TObject);
var
  lClient: TRESTClient;
  lRest: IRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.ReadTimeOut(0);
    lClient.Authentication('user_raise_exception', 'user_raise_exception');
    lRest := lClient.doPOST('/login', []);
    if lRest.HasError then
    begin
      ShowMessage(
        'HTTP ERROR: ' + lRest.Error.HTTPError.ToString + sLineBreak +
        'APPLICATION ERROR CODE: ' + lRest.Error.ErrorNumber.ToString + sLineBreak +
        'EXCEPTION MESSAGE: ' + lRest.Error.ExceptionMessage);
      Exit;
    end;

    lJSON := TSystemJSON.StringAsJSONObject(lRest.BodyAsString);
    try
      JWT := lJSON.GetValue('token').Value;
    finally
      lJSON.Free;
    end;
  finally
    lClient.Free;
  end;
end;

procedure TForm5.SetJWT(const Value: string);
begin
  FJWT := Value;
  Memo1.Lines.Text := Value;
end;

end.
