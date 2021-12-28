unit LoggerPro.WindowsEventLogAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProWindowsEventLogAppender))
  @author(David Cornelius) }

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'This unit only works with Windows'}
{$ENDIF}

interface

uses
  Vcl.SvcMgr,
  LoggerPro;

type
  { @abstract(This appender is for logging from Windows Services to the Windows Event Log) }
  TLoggerProWindowsEventLogAppender = class(TLoggerProAppenderBase)
  private
    FService: TService;
  public
    constructor Create(AService: TService); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  Winapi.Windows;

constructor TLoggerProWindowsEventLogAppender.Create(AService: TService);
begin
  inherited Create;
  FService := AService;
end;

procedure TLoggerProWindowsEventLogAppender.Setup;
begin
  // do nothing
end;

procedure TLoggerProWindowsEventLogAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProWindowsEventLogAppender.WriteLog(const aLogItem: TLogItem);
begin
  case aLogItem.LogType of
    TLogType.Debug,
    TLogType.Info:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_INFORMATION_TYPE);
    TLogType.Warning:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_WARNING_TYPE);
    TLogType.Error:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_ERROR_TYPE);
  end;
end;

end.
