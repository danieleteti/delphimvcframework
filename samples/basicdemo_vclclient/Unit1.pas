unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
 MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient;

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  Clt: IMVCRESTClient;
begin
  Clt := TMVCRESTClient.New.BaseURL('http://localhost', 8080);
  ShowMessage(Clt.Get('/div/10/20').Content);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Clt: IMVCRESTClient;
begin
  Clt := TMVCRESTClient.New.BaseURL('http://localhost', 8080);
  ShowMessage(Clt.Post('/hello', '{"name":"Bob äöüß"}').Content);
end;

end.
