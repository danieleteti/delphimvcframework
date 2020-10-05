unit MainClientFormU;

interface

uses
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
  TForm7 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FToken: String;
    procedure SetToken(const Value: String);
    { Private declarations }
  public
    property Token: String read FToken write SetToken;
  end;

var
  Form7: TForm7;

implementation

uses
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient;

{$R *.dfm}

procedure TForm7.Button1Click(Sender: TObject);

var
  lClt: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClt := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lResp := lClt.Post('/login');
  Token := lResp.Content;
  ShowMessage('In the next 15 seconds you can request protected resources. After your token will expires!');
end;

procedure TForm7.Button2Click(Sender: TObject);
var
  lClt: IMVCRESTClient;
  lResp: IMVCRESTResponse;
begin
  lClt := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClt.AddHeader('Authentication', 'bearer ' + FToken);
  lResp := lClt.Get('/');
  ShowMessage(lResp.StatusText + sLineBreak +
    lResp.Content);
end;

procedure TForm7.SetToken(const Value: String);
begin
  FToken := Value;
  Memo1.Lines.Text := Value;
end;

end.
