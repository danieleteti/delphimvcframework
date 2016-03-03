unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  MVCFramework.RESTClient;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Clt: TRestClient;
begin
  Clt := MVCFramework.RESTClient.TRESTClient.Create('http://localhost', 3000, nil);
  try
//    Clt.ProxyServer := 'localhost';
//    Clt.ProxyPort := 8888;
    ShowMessage(Clt.doGET('/div/10/20', []).BodyAsString);
  finally
    Clt.Free;
  end;
end;

end.
