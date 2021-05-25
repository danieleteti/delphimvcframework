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
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient;

procedure TForm5.btnGetClick(Sender: TObject);
var
  lClient: IMVCRESTClient;
  lRest: IMVCRESTResponse;
begin
  lClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
  lClient.SetBasicAuthorization('user1', 'user1');
  lRest := lClient.Get('/admin/role1?par1=daniele');
  ShowMessage(
    Format('%d %s', [lRest.StatusCode, lRest.StatusText]) + sLineBreak +
    lRest.Content);
end;

end.
