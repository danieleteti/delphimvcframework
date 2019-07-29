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
  mvcframework.restclient;

{$R *.dfm}

procedure TForm7.Button1Click(Sender: TObject);

var
  lClt: TRESTClient;
  lResp: IRESTResponse;
begin
  lClt := TRESTClient.Create('localhost', 8080);
  try
    lResp := lClt.doPOST('/login', [], '');
    Token := lResp.BodyAsString;
  finally
    lClt.Free;
  end;
  ShowMessage
    ('In the next 15 seconds you can request protected resources. After your token will expires!');
end;

procedure TForm7.Button2Click(Sender: TObject);
var
  lClt: TRESTClient;
  lResp: IRESTResponse;
begin
  lClt := TRESTClient.Create('localhost', 8080);
  try
    lClt.Header('Authentication', 'bearer ' + FToken);
    lResp := lClt.doGET('/', []);
    ShowMessage(lResp.ResponseText + sLineBreak +
      lResp.BodyAsString);
  finally
    lClt.Free;
  end;

end;

procedure TForm7.SetToken(const Value: String);
begin
  FToken := Value;
  Memo1.Lines.Text := Value;
end;

end.
