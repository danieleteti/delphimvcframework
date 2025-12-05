unit WebSocketChatClientU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  MVCFramework.WebSocket.Client, MVCFramework.WebSocket;

type
  TFormWebSocketChat = class(TForm)
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
    procedure OnDisconnect(Sender: TMVCWebSocketClient; ACode: TMVCWebSocketCloseCode; const AReason: string);
    procedure OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
    procedure OnBinaryMessage(Sender: TMVCWebSocketClient; const AData: TBytes);
    procedure OnError(Sender: TMVCWebSocketClient; const AError: Exception);
    procedure OnPong(Sender: TMVCWebSocketClient);
    procedure Log(const AMessage: string);
  public
  end;

var
  FormWebSocketChat: TFormWebSocketChat;

implementation

{$R *.dfm}

procedure TFormWebSocketChat.FormCreate(Sender: TObject);
begin
  MemoLog.Clear;
  EditURL.Text := 'ws://localhost:9092/';
  Caption := 'WebSocket Chat Client';
  ButtonDisconnect.Enabled := False;
  ButtonSend.Enabled := False;
  ButtonPing.Enabled := False;
end;

procedure TFormWebSocketChat.FormDestroy(Sender: TObject);
begin
  if Assigned(FWebSocketClient) then
  begin
    FWebSocketClient.Disconnect;
    FWebSocketClient.Free;
  end;
end;

procedure TFormWebSocketChat.ButtonConnectClick(Sender: TObject);
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
    FWebSocketClient.OnPong := OnPong;
    FWebSocketClient.AutoReconnect := CheckAutoReconnect.Checked;
    FWebSocketClient.ReconnectInterval := 5;

    FWebSocketClient.Connect;

    ButtonConnect.Enabled := False;
    ButtonDisconnect.Enabled := True;
    ButtonSend.Enabled := True;
    ButtonPing.Enabled := True;
    EditMessage.Enabled := True;
    EditURL.Enabled := False;

  except
    on E: Exception do
    begin
      Log('Connection error: ' + E.Message);
      ShowMessage('Connection failed: ' + E.Message);
    end;
  end;
end;

procedure TFormWebSocketChat.ButtonDisconnectClick(Sender: TObject);
begin
  if Assigned(FWebSocketClient) then
  begin
    Log('Disconnecting...');
    FWebSocketClient.Disconnect;

    ButtonConnect.Enabled := True;
    ButtonDisconnect.Enabled := False;
    ButtonSend.Enabled := False;
    ButtonPing.Enabled := False;
    EditMessage.Enabled := False;
    EditURL.Enabled := True;
  end;
end;

procedure TFormWebSocketChat.ButtonSendClick(Sender: TObject);
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

procedure TFormWebSocketChat.ButtonPingClick(Sender: TObject);
begin
  if Assigned(FWebSocketClient) and FWebSocketClient.IsConnected then
  begin
    Log('Sending ping...');
    FWebSocketClient.SendPing;
  end;
end;

procedure TFormWebSocketChat.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ButtonSendClick(nil);
  end;
end;

procedure TFormWebSocketChat.OnConnect(Sender: TMVCWebSocketClient);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Connected!');
    end);
end;

procedure TFormWebSocketChat.OnDisconnect(Sender: TMVCWebSocketClient;
  ACode: TMVCWebSocketCloseCode; const AReason: string);
var
  LCode: Integer;
  LReason: string;
begin
  LCode := Ord(ACode);
  LReason := AReason;
  TThread.Queue(nil,
    procedure
    begin
      if LReason <> '' then
        Log(Format('Disconnected! Code: %d, Reason: %s', [LCode, LReason]))
      else
        Log(Format('Disconnected! Code: %d', [LCode]));
      ButtonConnect.Enabled := True;
      ButtonDisconnect.Enabled := False;
      ButtonSend.Enabled := False;
      ButtonPing.Enabled := False;
      EditMessage.Enabled := False;
      EditURL.Enabled := True;
    end);
end;

procedure TFormWebSocketChat.OnTextMessage(Sender: TMVCWebSocketClient; const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('< ' + AMessage);
    end);
end;

procedure TFormWebSocketChat.OnBinaryMessage(Sender: TMVCWebSocketClient; const AData: TBytes);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log(Format('< Received %d bytes of binary data', [Length(AData)]));
    end);
end;

procedure TFormWebSocketChat.OnError(Sender: TMVCWebSocketClient; const AError: Exception);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('ERROR: ' + AError.Message);
    end);
end;

procedure TFormWebSocketChat.OnPong(Sender: TMVCWebSocketClient);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Pong received!');
    end);
end;

procedure TFormWebSocketChat.Log(const AMessage: string);
begin
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + AMessage);
end;

end.
