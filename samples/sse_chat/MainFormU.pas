unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, MVCFramework.SSEClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlChat: TPanel;
    pnlUsers: TPanel;
    lblUsername: TLabel;
    edtUsername: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    memChat: TMemo;
    lbUsers: TListBox;
    lblOnline: TLabel;
    edtMessage: TEdit;
    btnSend: TButton;
    lblTarget: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure edtMessageKeyPress(Sender: TObject; var Key: Char);
    procedure lbUsersClick(Sender: TObject);
  private
    fSSEClient: TMVCSSEClient;
    procedure SetConnectedState(AConnected: Boolean);
    procedure Log(const AText: string);
    procedure DoDisconnect;
    procedure HandleChatEvent(const AData: string);
    procedure HandleUserListEvent(const AData: string);
    procedure HandleSystemEvent(const AData: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.JSON, System.Net.URLClient, System.Generics.Collections, System.NetConsts;

{ Helpers }

procedure TMainForm.Log(const AText: string);
begin
  memChat.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + AText);
end;

procedure TMainForm.SetConnectedState(AConnected: Boolean);
begin
  btnConnect.Enabled := not AConnected;
  btnDisconnect.Enabled := AConnected;
  edtUsername.Enabled := not AConnected;
  edtMessage.Enabled := AConnected;
  btnSend.Enabled := AConnected;
  if AConnected then
    edtMessage.SetFocus;
end;

{ SSE Event Handlers }

procedure TMainForm.HandleChatEvent(const AData: string);
var
  LObj: TJSONObject;
  LFrom, LText, LTo: string;
begin
  LObj := TJSONObject.ParseJSONValue(AData) as TJSONObject;
  if LObj = nil then
    Exit;
  try
    LFrom := LObj.GetValue<string>('from', '');
    LText := LObj.GetValue<string>('text', '');
    if LObj.GetValue<Boolean>('private', False) then
    begin
      LTo := LObj.GetValue<string>('to', '');
      if SameText(LTo, edtUsername.Text) then
        Log('[PM from ' + LFrom + '] ' + LText)
      else
        Log('[PM to ' + LTo + '] ' + LText);
    end
    else
      Log('[' + LFrom + '] ' + LText);
  finally
    LObj.Free;
  end;
end;

procedure TMainForm.HandleUserListEvent(const AData: string);
var
  LArr: TJSONArray;
  I: Integer;
begin
  LArr := TJSONObject.ParseJSONValue(AData) as TJSONArray;
  if LArr = nil then
    Exit;
  try
    lbUsers.Items.BeginUpdate;
    try
      lbUsers.Items.Clear;
      lbUsers.Items.Add('(Everyone)');
      for I := 0 to LArr.Count - 1 do
        lbUsers.Items.Add(LArr.Items[I].Value);
    finally
      lbUsers.Items.EndUpdate;
    end;
    lbUsers.ItemIndex := 0;
    lbUsersClick(nil);
  finally
    LArr.Free;
  end;
end;

procedure TMainForm.HandleSystemEvent(const AData: string);
var
  LVal: TJSONValue;
  LMsg: string;
begin
  LVal := TJSONObject.ParseJSONValue(AData);
  if LVal <> nil then
  try
    LMsg := LVal.Value;
  finally
    LVal.Free;
  end
  else
    LMsg := AData;
  Log('*** ' + LMsg + ' ***');
end;

{ Connect / Disconnect }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fSSEClient := nil;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
var
  LUsername: string;
begin
  LUsername := Trim(edtUsername.Text);
  if LUsername.IsEmpty then
  begin
    ShowMessage('Please enter a username');
    edtUsername.SetFocus;
    Exit;
  end;

  fSSEClient := TMVCSSEClient.Create(
    'http://localhost:8080/chat?username=' + LUsername);

  fSSEClient.OnEvent :=
    procedure(const AId, AEvent, AData: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          if AEvent = 'chat' then
            HandleChatEvent(AData)
          else if AEvent = 'userlist' then
            HandleUserListEvent(AData)
          else if AEvent = 'system' then
            HandleSystemEvent(AData);
        end);
    end;

  fSSEClient.OnError :=
    procedure(const AError: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          Log('ERROR: ' + AError);
        end);
    end;

  fSSEClient.OnOpen :=
    procedure
    begin
      TThread.Queue(nil,
        procedure
        begin
          Log('--- Connected as ' + edtUsername.Text + ' ---');
        end);
    end;

  fSSEClient.Start;
  SetConnectedState(True);
end;

procedure TMainForm.DoDisconnect;
var
  LClient: TMVCSSEClient;
begin
  if fSSEClient = nil then
    Exit;
  LClient := fSSEClient;
  fSSEClient := nil;
  LClient.Stop;
  LClient.Free;
end;

procedure TMainForm.btnDisconnectClick(Sender: TObject);
begin
  DoDisconnect;
  lbUsers.Items.Clear;
  SetConnectedState(False);
  Log('--- Disconnected ---');
end;

{ Send }

procedure TMainForm.btnSendClick(Sender: TObject);
begin
  if Trim(edtMessage.Text).IsEmpty then
    Exit;

  TThread.CreateAnonymousThread(
    procedure
    var
      LClient: THTTPClient;
      LBody: TJSONObject;
      LContent: TStringStream;
      LResponse: IHTTPResponse;
      LText, LTo, LUsername: string;
    begin
      // Capture UI values before switching to background
      TThread.Synchronize(nil,
        procedure
        begin
          LUsername := edtUsername.Text;
          LText := edtMessage.Text;
          if lbUsers.ItemIndex > 0 then
            LTo := lbUsers.Items[lbUsers.ItemIndex]
          else
            LTo := '';
          edtMessage.Text := '';
          edtMessage.SetFocus;
        end);

      LClient := THTTPClient.Create;
      try
        LBody := TJSONObject.Create;
        try
          LBody.AddPair('username', LUsername);
          LBody.AddPair('text', LText);
          if not LTo.IsEmpty then
            LBody.AddPair('to', LTo);
          LContent := TStringStream.Create(LBody.ToJSON, TEncoding.UTF8);
          try
            LClient.ContentType := 'application/json';
            LResponse := LClient.Post('http://localhost:8080/api/messages', LContent);
            if LResponse.StatusCode >= 400 then
              TThread.Queue(nil,
                procedure
                begin
                  Log('ERROR: Failed to send (' + LResponse.StatusCode.ToString + ')');
                end);
          finally
            LContent.Free;
          end;
        finally
          LBody.Free;
        end;
      finally
        LClient.Free;
      end;
    end).Start;
end;

procedure TMainForm.edtMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnSendClick(Sender);
  end;
end;

procedure TMainForm.lbUsersClick(Sender: TObject);
begin
  if lbUsers.ItemIndex <= 0 then
    lblTarget.Caption := 'Broadcast to all'
  else
    lblTarget.Caption := 'Private to: ' + lbUsers.Items[lbUsers.ItemIndex];
end;

end.
