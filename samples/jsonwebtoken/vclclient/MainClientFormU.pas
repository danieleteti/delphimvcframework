unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm5 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    btnGet: TButton;
    btnLOGIN: TButton;
    Splitter1: TSplitter;
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
  Form5: TForm5;

implementation

{$R *.dfm}


uses
  MVCFramework.RESTClient, ObjectsMappers,
  MVCFramework.SystemJSONUtils,
  MVCFramework.TypesAliases;

procedure TForm5.btnGetClick(Sender: TObject);
var
  lClient: TRESTClient;
  lResp: IRESTResponse;
  lQueryStringParams: TStringList;
begin
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
    lClient
      .Header('jwtusername', 'user1')
      .Header('jwtpassword', 'user1');
    lRest := lClient.doPOST('/login', []);
    lJSON := TSystemJSON.BodyAsJSONObject(lRest);
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
