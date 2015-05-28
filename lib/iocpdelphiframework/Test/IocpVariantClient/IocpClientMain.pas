unit IocpClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SyncObjs,
  Iocp.TcpSocket, Iocp.VariantPacket, Iocp.VariantSocket, Iocp.Logger, Iocp.Utils;

{$define __SND_FIXED_SIZE__}

const
  TEST_PACK_SIZE = 4096;

type
  TTestIocpClient = class(TIocpVariantClient)
  protected
    procedure DoOnResponse(Client: TIocpVariantClientConnection; Request, Response: TIocpVariantPacket); override;
  end;

  TfmIocpClient = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
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
    Panel2: TPanel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    edtPort: TLabeledEdit;
    edtHost: TLabeledEdit;
    edtThreads: TLabeledEdit;
    edtSendDelay: TLabeledEdit;
    lbRunTime: TLabel;
    Label13: TLabel;
    lbSndQueueUsedMemory: TLabel;
    lbSndQueueFreeMemory: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    cbConsole: TCheckBox;
    Label12: TLabel;
    lbPendingRequest: TLabel;
    edtTimeout: TLabeledEdit;
    edtLife: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure edtSendDelayChange(Sender: TObject);
    procedure cbConsoleClick(Sender: TObject);
  private
    FClient: TTestIocpClient;
    FSendDelay: DWORD;
    FStartTick: DWORD;
    FLastTick: DWORD;
    FLastSentBytes, FLastRecvBytes: Int64;

    procedure UpdateInfo;
  public
    { Public declarations }
  end;

var
  fmIocpClient: TfmIocpClient;

implementation

{$R *.dfm}

{ TTestIocpClient }

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

procedure TfmIocpClient.btnConnectClick(Sender: TObject);
var
  i: Integer;
  Request: TIocpVariantPacket;
  a: TArray<Integer>;
begin
  if (edtPort.Text = '') or (edtHost.Text = '') then
  begin
    zMsgWarning(Handle, 'Configuration is not complete');
    Exit;
  end;

  FClient.StartupWorkers;
  FClient.ServerAddr := edtHost.Text;
  FClient.ServerPort := StrToInt(edtPort.Text);
  FClient.Timeout := StrToIntDef(edtTimeout.Text, 0) * 1000;
  FClient.ClientLife := StrToIntDef(edtLife.Text, 0) * 1000;

  SetLength(a, 3);
  a[0] := 11;
  a[1] := 33;
  a[2] := 99;
  Request := TIocpVariantPacket.Create;
  for i := 1 to StrToIntDef(edtThreads.Text, 1) do
  begin
    Request.Cmd := 'testcmd ' + IntToStr(i);
    Request.Params['Time'] := Now;
    Request.Params['Status'] := 'test variant packet';
    Request.Params['Array'] := a;
    FClient.AsyncRequest(Request);
  end;
  Request.Free;
end;

procedure TfmIocpClient.btnDisconnectClick(Sender: TObject);
begin
  FClient.ShutdownWorkers;
end;

procedure TfmIocpClient.cbConsoleClick(Sender: TObject);
begin
  ShowConsoleLog(cbConsole.Checked);
end;

procedure TfmIocpClient.edtSendDelayChange(Sender: TObject);
begin
  FSendDelay := StrToIntDef(edtSendDelay.Text, 0);
end;

procedure TfmIocpClient.FormCreate(Sender: TObject);
begin
  {$ifdef WIN64}
  Caption := Caption + ' (x64)';
  {$else}
  Caption := Caption + ' (x86)';
  {$endif}
  FStartTick := GetTickCount;
  FClient := TTestIocpClient.Create(nil);
  edtSendDelayChange(nil);
end;

procedure TfmIocpClient.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

procedure TfmIocpClient.Timer1Timer(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfmIocpClient.UpdateInfo;
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
  lbRunTime.Caption := TickToTimeStr(CalcTickDiff(FStartTick, GetTickCount));
  lbConnections.Caption := IntToStr(FClient.ConnectionList.Count);
  lbSentBytes.Caption := SpeedInfo(FClient.SentBytes, FLastSentBytes, FLastTick);
  lbRecvBytes.Caption := SpeedInfo(FClient.RecvBytes, FLastRecvBytes, FLastTick);
  lbHandleUsedMemory.Caption := BytesToStr(FClient.ConnectionUsedMemory);
  lbHandleFreeMemory.Caption := BytesToStr(FClient.ConnectionFreeMemory);
  lbIoUsedMemory.Caption := BytesToStr(FClient.PerIoUsedMemory);
  lbIoFreeMemory.Caption := BytesToStr(FClient.PerIoFreeMemory);
  lbSndQueueUsedMemory.Caption := BytesToStr(FClient.IoCacheUsedMemory);
  lbSndQueueFreeMemory.Caption := BytesToStr(FClient.IoCacheFreeMemory);
  lbPendingRequest.Caption := IntToStr(FClient.PendingRequest);

  FLastSentBytes := FClient.SentBytes;
  FLastRecvBytes := FClient.RecvBytes;
  FLastTick := GetTickCount;
end;

{ TTestIocpClient }

procedure TTestIocpClient.DoOnResponse(Client: TIocpVariantClientConnection;
  Request, Response: TIocpVariantPacket);
begin
  inherited;

end;

end.
