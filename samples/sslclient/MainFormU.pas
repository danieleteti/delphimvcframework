unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, Vcl.StdCtrls, MVCFramework.RESTClient, REST.Client,
  Data.Bind.Components,
  Data.Bind.ObjectScope, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Clt: MVCFramework.RESTClient.TRESTClient;
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
  Clt.Asynch(
    procedure(Resp: IRESTResponse)
    begin
      Memo1.Lines.Text := Resp.BodyAsString;
    end, nil, nil, true).doGET('/people', []);
end;

procedure TForm9.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Clt.Free;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  Clt := MVCFramework.RESTClient.TRESTClient.Create('https://localhost', 8080, IdSSLIOHandlerSocketOpenSSL1);
end;

end.
