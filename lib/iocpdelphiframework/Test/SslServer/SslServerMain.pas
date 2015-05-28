unit SslServerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SyncObjs,
  Iocp.TcpSocket, Iocp.SSLSocket, Iocp.Utils, Iocp.Logger;

{$define __SND_FIXED_SIZE__}

const
  TEST_PACK_SIZE = 256;

const
  SSL_SERVER_CERT: AnsiString =
    '-----BEGIN CERTIFICATE-----' + sLineBreak +
    'MIIDRDCCAiwCAf8wDQYJKoZIhvcNAQEFBQAwaDELMAkGA1UEBhMCVVMxCzAJBgNV' + sLineBreak +
    'BAgMAkNBMQswCQYDVQQHDAJMQTEVMBMGA1UECgwMVGVzdCBSb290IENBMQswCQYD' + sLineBreak +
    'VQQLDAJJVDEbMBkGA1UEAwwSd3d3LnRlc3Ryb290Y2EuY29tMB4XDTEzMDIyNzE4' + sLineBreak +
    'MjE1NloXDTIzMDIyNTE4MjE1NlowaDELMAkGA1UEBhMCVVMxCzAJBgNVBAgMAkNB' + sLineBreak +
    'MQswCQYDVQQHDAJMQTEVMBMGA1UECgwMVGVzdCBDb21wYW55MQswCQYDVQQLDAJJ' + sLineBreak +
    'VDEbMBkGA1UEAwwSd3d3LnRlc3RzZXJ2ZXIuY29tMIIBIjANBgkqhkiG9w0BAQEF' + sLineBreak +
    'AAOCAQ8AMIIBCgKCAQEAzkHv+S30g5Dc+F1RJ1PUq9Hbh1YkEUJdYEj7ti+UfONV' + sLineBreak +
    'NOT24hXzg8zaNSVO2Bhm+l8vzOVYMnjK9xcGSq5R5I633+lEeFdxURfsSJv9Vymq' + sLineBreak +
    'tHUj5eNkmjzWBVrf4HvnZTJtRJljs941zYUgyJT9tkQXaerGFKJ6sfdXYfhGrkuK' + sLineBreak +
    'gA1e71TwpRFYcfyYbQ3htENTh2CFBv7l5gjrITcmEJwpcU3U4nx4ZTr0IPLmV2kr' + sLineBreak +
    'K8IJysY4dqgRcmduEI5ZgbYGkdG8L7QjggFXA6QNDPu8DfmXeeqS0gIffEm22bk7' + sLineBreak +
    'b2fMnPfnFsJLsDdyhgrdYktnWhtZNij0y80tV4YCTwIDAQABMA0GCSqGSIb3DQEB' + sLineBreak +
    'BQUAA4IBAQDMLn9VnUQt6BWx73J1lExYO/LWulMOnMR/WSVFy9dSwry+E807ekMY' + sLineBreak +
    'WC8b3gpgDIqfkZjmttE9VtAdss2Baten+oBW+K13339sxHvcn30OxOs/Bln0yvaZ' + sLineBreak +
    'Be+Zir7iE450b1IdYI98PMTSKgrK2e3vx/uUOCgG2yvs6/1v5rz5er/M1SQNzdMS' + sLineBreak +
    'blelHWRQ1/ExwoUWBfIBkx/A4lTPmLgoC9fnXSiLhHKbZdfCJD8KLzEV0Se+ocn/' + sLineBreak +
    'vl+6tlcUznap0TsRQpC67T/NGUimxdAhb6G1/U6z9bq0QQIuDxpOIpvwIgLvfRFx' + sLineBreak +
    'qZQxmxOcK28fejHngmek7ZJNYKQbNewP' + sLineBreak +
    '-----END CERTIFICATE-----' + sLineBreak;

  SSL_SERVER_PKEY: AnsiString =
    '-----BEGIN PRIVATE KEY-----' + sLineBreak +
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDOQe/5LfSDkNz4' + sLineBreak +
    'XVEnU9Sr0duHViQRQl1gSPu2L5R841U05PbiFfODzNo1JU7YGGb6Xy/M5VgyeMr3' + sLineBreak +
    'FwZKrlHkjrff6UR4V3FRF+xIm/1XKaq0dSPl42SaPNYFWt/ge+dlMm1EmWOz3jXN' + sLineBreak +
    'hSDIlP22RBdp6sYUonqx91dh+EauS4qADV7vVPClEVhx/JhtDeG0Q1OHYIUG/uXm' + sLineBreak +
    'COshNyYQnClxTdTifHhlOvQg8uZXaSsrwgnKxjh2qBFyZ24QjlmBtgaR0bwvtCOC' + sLineBreak +
    'AVcDpA0M+7wN+Zd56pLSAh98SbbZuTtvZ8yc9+cWwkuwN3KGCt1iS2daG1k2KPTL' + sLineBreak +
    'zS1XhgJPAgMBAAECggEAIT83s27Y7yw2skI4hqJYsamOPW6BOdb8vjyFdoSM5uSu' + sLineBreak +
    'I2yU7zSioCgxNEfjQaoNT2ZwihKd+OTHsrSfawJWaQUoVot/YfaWaX/1sm6Sk64/' + sLineBreak +
    'uf733mKdIM+VoB9Z3xGZ5xIN0vT2wVOcUJiZBDwf+XVYYNZbP5BBPtaj20LuAcIZ' + sLineBreak +
    'OmW9uigdXQkQ1dylUkRPitjJ92bbysrTr621JTBSmvKnF7ctcF/Ql6VfS5RcqzYI' + sLineBreak +
    '6U1vozoFkjmUnExlYZHC6qKCFG73Z+IcC7ojdMpzMp4/EqiveV/9EVdFlLRB1YAa' + sLineBreak +
    'tND93xU9mo7L26XQzy79Xf2dWRUgUvaJ/7EvLA1RoQKBgQD2ZhJ9ogqfQ0ahq0D6' + sLineBreak +
    '5neZo6bPbckEKshv1GKR5ixnYpPp1kCIxM8oIzb9fOvTX4MOMeRzPJyrJNwhVgfY' + sLineBreak +
    'otWLrvkNviGHXN0frmkdj/Y/WSWh7clzzwXmGbB/8NPG4yzREvQ8vhKBkAmZln6K' + sLineBreak +
    'ICl8J5NxOxF6GgYJ793GcsfZVQKBgQDWS3DYMVQ3eRgFajkQ/8+Gacgdu+8/SyM1' + sLineBreak +
    'WptHOlPvKfqg3nZYPlAjMnVmk0Q7l/d2EtFBPP07/Jz0IvC/pMz0S8XfW/NigcRn' + sLineBreak +
    '0R5Nci3BXbmQEjxNGt0m0sX4C4/Bx8ei8pugipX96OemT/bWP05RskL6tWsofGsb' + sLineBreak +
    '8zgIQcldEwKBgCyx90iyzBp3qahJ2E+q3qcP+IJH9965pAIlFHxCtGtMhmg0ZSBq' + sLineBreak +
    'EunE+YSh1GVTPgKlKjt9Ey44UXX6lRHG99WOt762bn6Pac0FZivmoVR8Z0coSxKm' + sLineBreak +
    'yvsiTdHnbYL2UnraZVNfZxv5dMRXeDy1+NB8nVI81L7BWbcTu7bzuyzBAoGAY0j4' + sLineBreak +
    's3HHbxwvwPKCFhovcDs6eGxGYLDTUzjzkIC5uqlccYQgmKnmPyh1tFyu1F2ITbBS' + sLineBreak +
    'O0OioFRd887sdB5KxzUELIRRs2YkNWVyALfR8zEVdGa+gYrcw8wL5OyWYlXJbPmy' + sLineBreak +
    'mSMcc1OhYDDUUFdsVfWdisLbLxrWFVEOuOSiAvkCgYEA2viHsxoFxOrhnZQOhaLT' + sLineBreak +
    'RPrgaSojv9pooHQ6fJwplewt91tb1OchDIeZk9Sl1hqPAXB0167of43GDOw2vfnq' + sLineBreak +
    'Ust7RtiyJhQhSkz0qp4aH4P9l+dZJIWnpgjcyWkcz893br9gEuVnQgh13V/lcxOn' + sLineBreak +
    'JtpaCFuHNTU3PcFiuQW+cN0=' + sLineBreak +
    '-----END PRIVATE KEY-----' + sLineBreak;

type
  TTestIocpServer = class(TIocpSSLSocket)
  protected
    procedure TriggerClientRecvData(Client: TIocpSocketConnection; buf: Pointer; len: Integer); override;
  end;

  TfmIocpServer = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    edtPort: TLabeledEdit;
    lbConnections: TLabel;
    lbSentBytes: TLabel;
    lbRecvBytes: TLabel;
    lbHandleUsedMemory: TLabel;
    lbHandleFreeMemory: TLabel;
    lbIoUsedMemory: TLabel;
    lbIoFreeMemory: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbRunTime: TLabel;
    Label13: TLabel;
    lbSndQueueUsedMemory: TLabel;
    lbSndQueueFreeMemory: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    cbConsole: TCheckBox;
    lbPendingRequest: TLabel;
    Label7: TLabel;
    edtTimeout: TLabeledEdit;
    edtLife: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cbConsoleClick(Sender: TObject);
  private
    FServer: TTestIocpServer;
    FStartTick: DWORD;
    FLastTick: DWORD;
    FLastSentBytes, FLastRecvBytes: Int64;

    procedure UpdateInfo;
  public
    { Public declarations }
  end;

var
  fmIocpServer: TfmIocpServer;

implementation

{$R *.dfm}

const
  KBYTES = Int64(1024);
  MBYTES = KBYTES * 1024;
  GBYTES = MBYTES * 1024;
  TBYTES = GBYTES * 1024;

function BytesToStr(Bytes: Int64): string;
begin
  if (Bytes = 0) then
    Result := ''
  else if (Bytes < KBYTES) then
    Result := Format('%dB', [Bytes])
  else if (Bytes < MBYTES) then
    Result := Format('%dK ', [Bytes div KBYTES]) + BytesToStr(Bytes mod KBYTES)
  else if (Bytes < GBYTES) then
    Result := Format('%dM ', [Bytes div MBYTES]) + BytesToStr(Bytes mod MBYTES)
  else if (Bytes < TBYTES) then
    Result := Format('%dG ', [Bytes div GBYTES]) + BytesToStr(Bytes mod GBYTES)
  else
    Result := Format('%dT ', [Bytes div TBYTES]) + BytesToStr(Bytes mod TBYTES);
end;

procedure TfmIocpServer.btnStartClick(Sender: TObject);
begin
  if (edtPort.Text = '') then
  begin
    zMsgWarning(Handle, 'Configuration is not complete');
    Exit;
  end;

  FServer.Timeout := StrToIntDef(edtTimeout.Text, 0) * 1000;
  FServer.ClientLife := StrToIntDef(edtLife.Text, 0) * 1000;
  FServer.StartupWorkers;
  if not FServer.Listen(StrToInt(edtPort.Text), 1) then
    raise Exception.CreateFmt('Listen error on port %d', [StrToInt(edtPort.Text)]);
  btnStart.Enabled := False;
  btnStop.Enabled := True;
end;

procedure TfmIocpServer.btnStopClick(Sender: TObject);
begin
  FServer.ShutdownWorkers;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TfmIocpServer.cbConsoleClick(Sender: TObject);
begin
  ShowConsoleLog(cbConsole.Checked);
end;

procedure TfmIocpServer.FormCreate(Sender: TObject);
begin
  {$ifdef WIN64}
  Caption := Caption + ' (x64)';
  {$else}
  Caption := Caption + ' (x86)';
  {$endif}
  FStartTick := GetTickCount;
  FServer := TTestIocpServer.Create(nil);
  FServer.SetCert(PAnsiChar(SSL_SERVER_CERT), Length(SSL_SERVER_CERT),
    PAnsiChar(SSL_SERVER_PKEY), Length(SSL_SERVER_PKEY));
end;

procedure TfmIocpServer.FormDestroy(Sender: TObject);
begin
  FServer.Free;
end;

procedure TfmIocpServer.Timer1Timer(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfmIocpServer.UpdateInfo;
  function SpeedInfo(NowBytes, LastBytes: Int64; LastTick: DWORD): string;
  var
    t: DWORD;
  begin
    Result := BytesToStr(NowBytes);

    if (NowBytes > LastBytes) then
    begin
      t := CalcTickDiff(LastTick, GetTickCount);
      if (t > 0) then
        Result := Result + ', ' + BytesToStr(Round((NowBytes-LastBytes)/t)*1000) + '/s';
    end;
  end;
begin
  if (csDestroying in ComponentState) then Exit;
  if not Assigned(FServer) then Exit;

  lbRunTime.Caption := TickToTimeStr(CalcTickDiff(FStartTick, GetTickCount));
  lbConnections.Caption := IntToStr(FServer.ConnectionList.Count);
  lbSentBytes.Caption := SpeedInfo(FServer.SentBytes, FLastSentBytes, FLastTick);
  lbRecvBytes.Caption := SpeedInfo(FServer.RecvBytes, FLastRecvBytes, FLastTick);
  lbHandleUsedMemory.Caption := BytesToStr(FServer.ConnectionUsedMemory);
  lbHandleFreeMemory.Caption := BytesToStr(FServer.ConnectionFreeMemory);
  lbIoUsedMemory.Caption := BytesToStr(FServer.PerIoUsedMemory);
  lbIoFreeMemory.Caption := BytesToStr(FServer.PerIoFreeMemory);
  lbSndQueueUsedMemory.Caption := BytesToStr(FServer.IoCacheUsedMemory);
  lbSndQueueFreeMemory.Caption := BytesToStr(FServer.IoCacheFreeMemory);
  lbPendingRequest.Caption := IntToStr(FServer.PendingRequest);

  FLastSentBytes := FServer.SentBytes;
  FLastRecvBytes := FServer.RecvBytes;
  FLastTick := GetTickCount;
end;

{ TTestIocpServer }

procedure TTestIocpServer.TriggerClientRecvData(Client: TIocpSocketConnection;
  buf: Pointer; len: Integer);
var
{$ifdef __SND_FIXED_SIZE__}
  NewBuf: array [0..TEST_PACK_SIZE - 1] of Byte;
{$else}
  NewBuf: PByteArray;
{$endif}
  i: Integer;
begin
  inherited TriggerClientRecvData(Client, buf, len);

{$ifdef __SND_FIXED_SIZE__}
  for i := 0 to TEST_PACK_SIZE - 1 do
  begin
    NewBuf[i] := i mod 256;
  end;
  Client.Send(@NewBuf[0], TEST_PACK_SIZE);
{$else}
  GetMem(NewBuf, len);
  for i := 0 to len - 1 do
  begin
    NewBuf[i] := i mod 256;
  end;
  Result := (Client.Send(NewBuf, len) > 0);
  FreeMem(NewBuf);
{$endif}
end;

end.
