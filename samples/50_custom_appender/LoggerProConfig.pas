unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.ConsoleAppender, System.SysUtils;

type
  TMyCustomAppender = class(TLoggerProAppenderBase)
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

{ TMyCustomAppender }
procedure TMyCustomAppender.Setup;
begin
  //
end;

procedure TMyCustomAppender.TearDown;
begin
  //
end;

procedure TMyCustomAppender.WriteLog(const aLogItem: TLogItem);
begin
  // uncomment this to see what happend if an appender raise an exception

  // raise Exception.Create('Kill LoggerPro');

  // uncomment this to see what happend if an appender is slow
  Sleep(2000);
  WriteLn('[THE BAD APPENDER] ' + aLogItem.LogMessage);
end;

initialization

_Log := BuildLogWriter([
  TLoggerProConsoleAppender.Create,
  TMyCustomAppender.Create])

end.
