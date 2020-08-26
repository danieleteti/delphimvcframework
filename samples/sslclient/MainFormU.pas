unit MainFormU;

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
  IPPeerClient,
  Vcl.StdCtrls,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  REST.Client,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  REST.Types;

type
  TForm9 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Clt: IMVCRESTClient;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
begin
  RESTRequest1.ExecuteAsync(
    procedure
    begin
      Memo1.Lines.Text := RESTRequest1.Response.JSONValue.ToString;
    end);
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  Clt.Async(
    procedure(Resp: IMVCRESTResponse)
    begin
      Memo1.Lines.Text := Resp.Content;
      Memo1.Lines.Add('Request Terminated');
    end,
    procedure(E: Exception)
    begin
      ShowMessage(E.Message);
    end,
    True
    )
    .Get('/people');
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  Clt := TMVCRESTClient.New.BaseURL('https://localhost', 443);
end;

end.
