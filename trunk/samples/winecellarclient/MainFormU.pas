unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses MVCFramework.RESTClient;

procedure TForm5.Button1Click(Sender: TObject);
var
  rest: TRESTClient;
  response: IRESTResponse;
begin
  rest := TRESTClient.Create('localhost', 3000);
  try
    response := rest.doGET('/wines', []);
    Memo1.Lines.Text := response.BodyAsString;
  finally
    rest.free;
  end;
end;

end.
