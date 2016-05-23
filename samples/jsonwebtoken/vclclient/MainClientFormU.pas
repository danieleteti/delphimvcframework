unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    btnGet: TButton;
    btnLOGIN: TButton;
    procedure btnGetClick(Sender: TObject);
    procedure btnLOGINClick(Sender: TObject);
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
    lRest := lClient.doGET('/', []);
    ShowMessage(lRest.BodyAsString);
  finally
    lClient.Free;
  end;
end;

procedure TForm5.btnLOGINClick(Sender: TObject);
var
  lClient: TRESTClient;
  lRest: IRESTResponse;
begin
  lClient := TRESTClient.Create('localhost', 8080);
  try
    lClient
      .Header('jwtusername', 'daniele')
      .Header('jwtpassword', 'daniele');
    lRest := lClient.doPOST('/login', []);
    ShowMessage(lRest.BodyAsString);
  finally
    lClient.Free;
  end;
end;

end.
