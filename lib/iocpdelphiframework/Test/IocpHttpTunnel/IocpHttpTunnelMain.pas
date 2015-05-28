unit IocpHttpTunnelMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FileCtrl, System.Generics.Collections, Vcl.Buttons,
  Iocp.Utils, Iocp.TcpSocket, Iocp.HttpTunnel, Iocp.HttpUtils, Iocp.Logger;

type
  TTestIocpHttpTunnel = class(TIocpHttpTunnel)
  private
    FDestHost: string;
    FDestPort: Word;
  protected
    function TriggerConfirmForward(Client: TIocpHttpTunnelConnection;
      out ServerAddr: string; out ServerPort: Word): Boolean; override;
  end;

  TfmIocpHttpTunnel = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    btnStart: TButton;
    btnStop: TButton;
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
    edtPort: TLabeledEdit;
    edtDestHost: TLabeledEdit;
    edtDestPort: TLabeledEdit;
    Label7: TLabel;
    lbPendingRequest: TLabel;
    edtTimeout: TLabeledEdit;
    edtLife: TLabeledEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cbConsoleClick(Sender: TObject);
  private
    FServer: TTestIocpHttpTunnel;
    FStartTick: DWORD;
    FLastTick: DWORD;
    FLastSentBytes, FLastRecvBytes: Int64;

    procedure UpdateInfo;
  public
    { Public declarations }
  end;

var
  fmIocpHttpTunnel: TfmIocpHttpTunnel;

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

procedure TfmIocpHttpTunnel.btnStartClick(Sender: TObject);
begin
{  if (edtPort.Text = '') or (edtDestHost.Text = '') or (edtDestPort.Text = '') then
  begin
    zMsgWarning(Handle, 'Configuration is not complete');
    Exit;
  end;}

  FServer.Port := StrToIntDef(edtPort.Text, 80);
  FServer.Timeout := StrToIntDef(edtTimeout.Text, 0);
  FServer.ClientLife := StrToIntDef(edtLife.Text, 0);
  FServer.FDestHost := edtDestHost.Text;
  FServer.FDestPort := StrToInt(edtDestPort.Text);
  btnStart.Enabled := not FServer.Start;
  btnStop.Enabled := not btnStart.Enabled;
end;

procedure TfmIocpHttpTunnel.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled := FServer.Stop;
  btnStop.Enabled := not btnStart.Enabled;
end;

procedure TfmIocpHttpTunnel.cbConsoleClick(Sender: TObject);
begin
  ShowConsoleLog(cbConsole.Checked);
end;

procedure TfmIocpHttpTunnel.FormCreate(Sender: TObject);
begin
  {$ifdef WIN64}
  Caption := Caption + ' (x64)';
  {$else}
  Caption := Caption + ' (x86)';
  {$endif}
  FStartTick := GetTickCount;
  FServer := TTestIocpHttpTunnel.Create(nil);
end;

procedure TfmIocpHttpTunnel.FormDestroy(Sender: TObject);
begin
  FServer.Free;
end;

procedure TfmIocpHttpTunnel.Timer1Timer(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TfmIocpHttpTunnel.UpdateInfo;
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

  FLastSentBytes := FServer.SentBytes;
  FLastRecvBytes := FServer.RecvBytes;
  FLastTick := GetTickCount;
end;

{ TTestIocpHttpTunnel }

function TTestIocpHttpTunnel.TriggerConfirmForward(
  Client: TIocpHttpTunnelConnection;
  out ServerAddr: string; out ServerPort: Word): Boolean;
begin
  ServerAddr := FDestHost;
  ServerPort := FDestPort;

  Result := True;
end;

end.
