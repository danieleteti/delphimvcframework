unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    btnGet: TButton;
    procedure btnGetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses
  MVCFramework.RESTClient;

procedure TForm5.btnGetClick(Sender: TObject);
var
  lClient: TRESTClient;
  lRest: IRESTResponse;
begin
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient.Authentication('user1', 'user1');
    lRest := lClient.doGET('/admin/role1?par1=daniele', []);
    ShowMessage(lRest.BodyAsString);
  finally
    lClient.Free;
  end;
end;

end.
