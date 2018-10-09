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
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
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
  MVCFramework.RESTClient,
  MVCFramework.SystemJSONUtils,
  MVCFramework.TypesAliases,
  System.NetEncoding;

procedure TMainForm.btnGetClick(Sender: TObject);
var
  lClient: TRESTClient;
  lResp: IRESTResponse;
  lQueryStringParams: TStringList;
  tokenOld, tokenNew: string; // NEW CODE
begin
  tokenOld := FJWT; // NEW CODE
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.ReadTimeOut(0);
    if not FJWT.IsEmpty then
      lClient.RequestHeaders.Values['Authentication'] := 'bearer ' + FJWT;
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

    // NEW CODE
    tokenNew := lResp.HeaderValue('Authentication');
    if tokenNew.StartsWith('bearer', True) then
    begin
      tokenNew := tokenNew.Remove(0, 'bearer'.Length).Trim;
      tokenNew := TNetEncoding.URL.URLDecode(tokenNew).Trim;
      JWT := tokenNew;
    end; // END NEW CODE
  finally
    lClient.Free;
  end;
end;

procedure TMainForm.btnLOGINClick(Sender: TObject);
var
  lClient: TRESTClient;
  lRest: IRESTResponse;
  lJSON: TJSONObject;
begin
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.ReadTimeOut(0);
    lClient
      .Header('jwtusername', 'user1')
      .Header('jwtpassword', 'user1');
    lRest := lClient.doPOST('/login', []);
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

procedure TMainForm.SetJWT(const Value: string);
begin
  FJWT := Value;
  Memo1.Lines.Text := Value;

end;

end.
