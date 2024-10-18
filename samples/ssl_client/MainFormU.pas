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
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  REST.Client,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  REST.Types;

type
  TForm9 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
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
  Clt
    .SetValidateServerCertificateProc(
    procedure(const Sender: TObject; const ARequest: TURLRequest;
    const Certificate: TCertificate; var Accepted: Boolean)
    begin
      //
      Accepted := True;
    end
    )
    .Async(
    procedure(Resp: IMVCRESTResponse)
    begin
      Memo1.Lines.Text := Resp.Content;
      Memo1.Lines.Add('Request Terminated successfully')
    end,
    procedure(E: Exception)
    begin
      ShowMessage(E.Message);
      Memo1.Lines.Add('Request Terminated with errors')
    end, True).Get('/people');
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  Clt := TMVCRESTClient.New.BaseURL('https://localhost');
end;

end.
