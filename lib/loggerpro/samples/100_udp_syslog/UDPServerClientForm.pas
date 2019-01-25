unit UDPServerClientForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdUDPBase, IdUDPServer, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  IdGlobal, IdSocketHandle, LoggerPro.UDPSyslogAppender;

type
  TFUDPServerClientForm = class(TForm)
    IdUDPServer: TIdUDPServer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    UDPServerReceived: TMemo;
    Panel1: TPanel;
    UDPServerControl: TRadioGroup;
    UDPClientControl: TRadioGroup;
    UDPClientPort: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    UDPServerPort: TSpinEdit;
    UDPClientTimer: TTimer;
    procedure UDPClientTimerTimer(Sender: TObject);
    procedure UDPClientControlClick(Sender: TObject);
    procedure UDPServerControlClick(Sender: TObject);
    procedure IdUDPServerUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
  public
    { Public declarations }
  end;

var
  FUDPServerClientForm: TFUDPServerClientForm;

implementation

uses
  LoggerPro, LoggerProConfig, Math;

const
  STATUS_ON = 0;
  STATUS_OFF = 1;

{$R *.dfm}

function GetPIDByHWND(const hWnd: THandle): THandle;
var
   PID: DWORD;
begin
  Result := 0;
  if hWnd = 0 then Exit;
  GetWindowThreadProcessID(hWnd, @PID);
  Result := PID;
end;

procedure TFUDPServerClientForm.IdUDPServerUDPRead(
  AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
begin
  UDPServerReceived.Lines.Add(BytesToString(AData, IndyTextEncoding_UTF8));
  UDPServerReceived.Lines.Add('----------------');
  SendMessage(UDPServerReceived.Handle, EM_LINESCROLL, 0, UDPServerReceived.Lines.Count);
end;

procedure TFUDPServerClientForm.UDPClientControlClick(Sender: TObject);
begin
  UDPClientTimer.Enabled := UDPClientControl.ItemIndex = STATUS_ON;
  if not UDPClientTimer.Enabled then Exit;

  Appender.ProcID := IntToStr(GetPIDByHWND(Application.Handle));
  Appender.Port := UDPClientPort.Value;
  Appender.UserName := GetEnvironmentVariable('USERNAME');
  Appender.HostName := GetEnvironmentVariable('COMPUTERNAME');
  Appender.Application := ExtractFileName(ParamStr(0));
end;

procedure TFUDPServerClientForm.UDPClientTimerTimer(Sender: TObject);
begin
  case RandomRange(0, 5) of
    0: Log.Debug('debug message', 'DEBUG');
    1: Log.Info('info message', 'INFO');
    2: Log.Warn('warn message', 'WARN');
    3: Log.Error('error message', 'ERROR');
    4: Log.Info('Some Access Violation', 'INFO');
  end;
end;

procedure TFUDPServerClientForm.UDPServerControlClick(Sender: TObject);
begin
  IdUDPServer.Bindings[0].Port := UDPServerPort.Value;
  IdUDPServer.Active := UDPServerControl.ItemIndex = STATUS_ON;
end;

end.
