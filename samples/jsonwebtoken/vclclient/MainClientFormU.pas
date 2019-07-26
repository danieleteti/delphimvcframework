unit MainClientFormU;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, MVCFramework.Middleware.JWT,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
    procedure SetJWT(const AValue: string);
    property JWT: string read FJWT write SetJWT;
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses MVCFramework.RESTClient, MVCFramework.SystemJSONUtils, System.JSON;

procedure TForm5.btnGetClick(Sender: TObject);
var
  LClient: TRESTClient;
  LResp: IRESTResponse;
  LQueryStringParams: TStringList;
begin
  { Getting JSON response }
  LClient := TRESTClient.Create('localhost', 8080);
  try
    LClient.UseBasicAuthentication := False;
    LClient.ReadTimeOut(0);
    if not FJWT.IsEmpty then
      LClient.RequestHeaders.Values[TMVCJWTDefaults.AUTHORIZATION_HEADER] := 'Bearer ' + FJWT;
    LQueryStringParams := TStringList.Create;
    try
      LQueryStringParams.Values['firstname'] := 'Daniele';
      LQueryStringParams.Values['lastname'] := 'Teti';
      LResp := LClient.doGET('/admin/role1', [], LQueryStringParams);
      if LResp.HasError then
        ShowMessage(LResp.Error.ExceptionMessage);
    finally
      LQueryStringParams.Free;
    end;
    Memo2.Lines.Text := LResp.BodyAsString;
  finally
    LClient.Free;
  end;

  { Getting HTML response }
  LClient := TRESTClient.Create('localhost', 8080);
  try
    // when the JWT authorization header is named "Authorization", the basic authorization must be disabled
    LClient.UseBasicAuthentication := False;

    LClient.ReadTimeOut(0);
    if not FJWT.IsEmpty then
      LClient.RequestHeaders.Values[TMVCJWTDefaults.AUTHORIZATION_HEADER] := 'bearer ' + FJWT;
    LQueryStringParams := TStringList.Create;
    try
      LQueryStringParams.Values['firstname'] := 'Daniele';
      LQueryStringParams.Values['lastname'] := 'Teti';
      LResp := LClient.Accept('text/html').doGET('/admin/role1', [], LQueryStringParams);
      if LResp.HasError then
        ShowMessage(LResp.Error.ExceptionMessage);
    finally
      LQueryStringParams.Free;
    end;
    Memo3.Lines.Text := LResp.BodyAsString;
  finally
    LClient.Free;
  end;
end;

procedure TForm5.btnLOGINClick(Sender: TObject);
var
  LClient: TRESTClient;
  LRest: IRESTResponse;
  LJSON: TJSONObject;
begin
  LClient := TRESTClient.Create('localhost', 8080);
  try
    LClient.ReadTimeOut(0);
    LClient.Authentication('user1', 'user1');
    LRest := LClient.doPOST('/login', []);
    if LRest.HasError then
    begin
      ShowMessage(
        'HTTP ERROR: ' + LRest.Error.HTTPError.ToString + sLineBreak +
        'APPLICATION ERROR CODE: ' + LRest.Error.ErrorNumber.ToString + sLineBreak +
        'EXCEPTION MESSAGE: ' + LRest.Error.ExceptionMessage);
      Exit;
    end;

    LJSON := TSystemJSON.StringAsJSONObject(LRest.BodyAsString);
    try
      JWT := LJSON.GetValue('token').Value;
    finally
      LJSON.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TForm5.btnLoginWithExceptionClick(Sender: TObject);
var
  LClient: TRESTClient;
  LRest: IRESTResponse;
  LJSON: TJSONObject;
begin
  LClient := TRESTClient.Create('localhost', 8080);
  try
    LClient.ReadTimeOut(0);
    LClient.Authentication('user_raise_exception', 'user_raise_exception');
    LRest := LClient.doPOST('/login', []);
    if LRest.HasError then
    begin
      ShowMessage(
        'HTTP ERROR: ' + LRest.Error.HTTPError.ToString + sLineBreak +
        'APPLICATION ERROR CODE: ' + LRest.Error.ErrorNumber.ToString + sLineBreak +
        'EXCEPTION MESSAGE: ' + LRest.Error.ExceptionMessage);
      Exit;
    end;

    LJSON := TSystemJSON.StringAsJSONObject(LRest.BodyAsString);
    try
      JWT := LJSON.GetValue('token').Value;
    finally
      LJSON.Free;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TForm5.SetJWT(const AValue: string);
begin
  FJWT := AValue;
  Memo1.Lines.Text := AValue;
end;

end.
