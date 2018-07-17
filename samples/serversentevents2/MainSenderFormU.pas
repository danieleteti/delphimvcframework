unit MainSenderFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TForm10 = class(TForm)
    edtMessage: TEdit;
    Label1: TLabel;
    btnSend: TButton;
    HTTPSend: TNetHTTPClient;
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses
  JsonDataObjects;

{$R *.dfm}

procedure TForm10.btnSendClick(Sender: TObject);
var
  lStream: TStringStream;
  lJSON: TJsonObject;
  lRes: IHTTPResponse;
begin
  lStream := TStringStream.Create;
  try
    lJSON := TJsonObject.Create;
    try
      lJSON.S['value'] := edtMessage.Text;
      lStream.WriteString(lJSON.ToJSON());
    finally
      lJSON.Free;
    end;
    lStream.Position := 0;
    lRes := HTTPSend.Post('http://localhost:8080/api/notifications', lStream, nil, [TNetHEader.Create('content-type', 'application/json')]);
    if lRes.StatusCode <> 201 then
    begin
      ShowMessage(Format('%d: %s (%s)', [lRes.StatusCode, lres.StatusText, lres.ContentAsString]));
    end;
  finally
    lStream.Free;
  end;
end;

end.
