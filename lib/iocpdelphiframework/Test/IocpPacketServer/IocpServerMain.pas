unit IocpServerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IoUtils, SyncObjs,
  Iocp.Utils, Iocp.TcpSocket, Iocp.PacketSocket, Iocp.Logger, uGlobalVars;

type
  TTestIocpServer = class(TIocpPacketServer)
  private
    FSuccessCnt, FCrcErrorCnt: Int64;

    procedure SendTestData(Client: TIocpSocketConnection);
    procedure SavePacketHeader(const Packet: TIocpPacket);
    procedure SavePacketData(const Packet: TIocpPacket);
  protected
    procedure TriggerPacketRecv(Client: TIocpPacketConnection; const Packet: TIocpPacket); override;
    procedure TriggerPacketHeaderCrcError(Client: TIocpPacketConnection; const Packet: TIocpPacket); override;
    procedure TriggerPacketDataCrcError(Client: TIocpPacketConnection; const Packet: TIocpPacket); override;
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
    lbPacketCrcOk: TLabel;
    Label6: TLabel;
    lbPacketCrcErr: TLabel;
    Label11: TLabel;
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

{function BytesToStr(Bytes: Int64): string;
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
end;}

function BytesToStr(Bytes: Int64): string;
begin
  if (Bytes = 0) then
    Result := ''
  else if (Bytes < KBYTES) then
    Result := Format('%dB', [Bytes])
  else if (Bytes < MBYTES) then
    Result := Format('%.2fK ', [Bytes / KBYTES])
  else if (Bytes < GBYTES) then
    Result := Format('%.2fM ', [Bytes / MBYTES])
  else if (Bytes < TBYTES) then
    Result := Format('%.2fG ', [Bytes / GBYTES])
  else
    Result := Format('%.2fT ', [Bytes / TBYTES]);
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
  FServer.Port := StrToInt(edtPort.Text);
  FServer.FSuccessCnt := 0;
  FServer.FCrcErrorCnt := 0;
  btnStart.Enabled := not FServer.Start;
  btnStop.Enabled := not btnStart.Enabled;
end;

procedure TfmIocpServer.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled := FServer.Stop;
  btnStop.Enabled := not btnStart.Enabled;
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
  lbPacketCrcOk.Caption := IntToStr(FServer.FSuccessCnt);
  lbPacketCrcErr.Caption := IntToStr(FServer.FCrcErrorCnt);

  FLastSentBytes := FServer.SentBytes;
  FLastRecvBytes := FServer.RecvBytes;
  FLastTick := GetTickCount;
end;

{ TTestIocpServer }

procedure TTestIocpServer.SendTestData(Client: TIocpSocketConnection);
var
  Buf: Pointer;
  Size, i: Integer;
begin
  Size := RandRange(256, 4096);
  GetMem(Buf, Size);
  if (Buf = nil) then Exit;

  for i := 0 to Size - 1 do
    PByteArray(Buf)[i] := i mod 256;

  Client.Send(Buf, Size);
  FreeMem(Buf);
end;

procedure TTestIocpServer.SavePacketHeader(const Packet: TIocpPacket);
var
  Stream: TFileStream;
begin
  ForceDirectories(gAppPath + 'ERR-PACK');
  Stream := TFileStream.Create(GetTempFileName(gAppPath + 'ERR-PACK', Format('ERR-HEADER-%d-', [GetTickCount]), ''), fmCreate);
  Stream.Write(Packet.Header, SizeOf(Packet.Header));
  Stream.Free;
end;

procedure TTestIocpServer.SavePacketData(const Packet: TIocpPacket);
var
  Stream: TFileStream;
begin
  ForceDirectories(gAppPath + 'ERR-PACK');
  Stream := TFileStream.Create(GetTempFileName(gAppPath + 'ERR-PACK', Format('ERR-DATA-%d-', [GetTickCount]), ''), fmCreate);
  Stream.Write(Packet.Data^, Packet.Header.DataSize);
  Stream.Free;
end;

procedure TTestIocpServer.TriggerPacketRecv(Client: TIocpPacketConnection;
  const Packet: TIocpPacket);
begin
  TInterlocked.Increment(FSuccessCnt);
  SendTestData(Client);
end;

procedure TTestIocpServer.TriggerPacketHeaderCrcError(
  Client: TIocpPacketConnection; const Packet: TIocpPacket);
begin
  TInterlocked.Increment(FCrcErrorCnt);
  SavePacketHeader(Packet);
end;

procedure TTestIocpServer.TriggerPacketDataCrcError(Client: TIocpPacketConnection;
  const Packet: TIocpPacket);
begin
  TInterlocked.Increment(FCrcErrorCnt);
  SavePacketData(Packet);
end;

end.
