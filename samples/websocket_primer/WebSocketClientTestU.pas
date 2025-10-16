unit WebSocketClientTestU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  MVCFramework.WebSocket.Client;

type
  TFormWebSocketClient = class(TForm)
    MemoLog: TMemo;
    PanelTop: TPanel;
    EditURL: TEdit;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    PanelBottom: TPanel;
    EditMessage: TEdit;
    ButtonSend: TButton;
    Label1: TLabel;
    CheckAutoReconnect: TCheckBox;
    ButtonPing: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonPingClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: Char);
  private
    FWebSocketClient: TMVCWebSocketClient;
    procedure OnConnect(Sender: TMVCWebSocketClient);
    procedure OnDisconnect(Sender: TMVCWebSocketClient);
    procedure OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
    procedure OnBinaryMessage(Sender: TMVCWebSocketClient; const AData: TBytes);
    procedure OnError(Sender: TMVCWebSocketClient; const AError: Exception);
    procedure Log(const AMessage: string);
  public
  end;

var
  FormWebSocketClient: TFormWebSocketClient;

implementation

{$R *.dfm}

procedure TFormWebSocketClient.FormCreate(Sender: TObject);
begin
  MemoLog.Clear;
  EditURL.Text := 'ws://localhost:8080/ws/echo';
  ButtonDisconnect.Enabled := False;
  ButtonSend.Enabled := False;
  ButtonPing.Enabled := False;
end;

procedure TFormWebSocketClient.FormDestroy(Sender: TObject);
begin
  if Assigned(FWebSocketClient) then
  begin
    FWebSocketClient.Disconnect;
    FWebSocketClient.Free;
  end;
end;

procedure TFormWebSocketClient.ButtonConnectClick(Sender: TObject);
begin
  try
    Log('Connecting to ' + EditURL.Text + '...');

    if Assigned(FWebSocketClient) then
      FWebSocketClient.Free;

    FWebSocketClient := TMVCWebSocketClient.Create(EditURL.Text);
    FWebSocketClient.OnConnect := OnConnect;
    FWebSocketClient.OnDisconnect := OnDisconnect;
    FWebSocketClient.OnTextMessage := OnTextMessage;
    FWebSocketClient.OnBinaryMessage := OnBinaryMessage;
    FWebSocketClient.OnError := OnError;
    FWebSocketClient.AutoReconnect := CheckAutoReconnect.Checked;
    FWebSocketClient.ReconnectInterval := 5;

    FWebSocketClient.Connect;

    ButtonConnect.Enabled := False;
    ButtonDisconnect.Enabled := True;
    ButtonSend.Enabled := True;
    ButtonPing.Enabled := True;
    EditURL.Enabled := False;

  except
    on E: Exception do
    begin
      Log('Connection error: ' + E.Message);
      ShowMessage('Connection failed: ' + E.Message);
    end;
  end;
end;

procedure TFormWebSocketClient.ButtonDisconnectClick(Sender: TObject);
begin
  if Assigned(FWebSocketClient) then
  begin
    Log('Disconnecting...');
    FWebSocketClient.Disconnect;

    ButtonConnect.Enabled := True;
    ButtonDisconnect.Enabled := False;
    ButtonSend.Enabled := False;
    ButtonPing.Enabled := False;
    EditURL.Enabled := True;
  end;
end;

procedure TFormWebSocketClient.ButtonSendClick(Sender: TObject);
begin
  if Assigned(FWebSocketClient) and FWebSocketClient.IsConnected then
  begin
    if EditMessage.Text <> '' then
    begin
      Log('Sending: ' + EditMessage.Text);
      FWebSocketClient.SendText(EditMessage.Text);
      EditMessage.Clear;
    end;
  end
  else
  begin
    ShowMessage('Not connected!');
  end;
end;

procedure TFormWebSocketClient.ButtonPingClick(Sender: TObject);
begin
  if Assigned(FWebSocketClient) and FWebSocketClient.IsConnected then
  begin
    Log('Sending ping...');
    FWebSocketClient.SendPing;
  end;
end;

procedure TFormWebSocketClient.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ButtonSendClick(nil);
  end;
end;

procedure TFormWebSocketClient.OnConnect(Sender: TMVCWebSocketClient);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Log('Connected!');
    end);
end;

procedure TFormWebSocketClient.OnDisconnect(Sender: TMVCWebSocketClient);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Log('Disconnected!');
      ButtonConnect.Enabled := True;
      ButtonDisconnect.Enabled := False;
      ButtonSend.Enabled := False;
      ButtonPing.Enabled := False;
      EditURL.Enabled := True;
    end);
end;

procedure TFormWebSocketClient.OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Log('< ' + AMessage);
    end);
end;

procedure TFormWebSocketClient.OnBinaryMessage(Sender: TMVCWebSocketClient; const AData: TBytes);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Log(Format('< Received %d bytes of binary data', [Length(AData)]));
    end);
end;

procedure TFormWebSocketClient.OnError(Sender: TMVCWebSocketClient; const AError: Exception);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Log('ERROR: ' + AError.Message);
    end);
end;

procedure TFormWebSocketClient.Log(const AMessage: string);
begin
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + AMessage);
end;

end.
