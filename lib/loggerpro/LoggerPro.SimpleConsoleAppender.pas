unit LoggerPro.SimpleConsoleAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProSimpleConsoleAppender))
  @author(David Cornelius) }

{$IFNDEF CONSOLE}
{$MESSAGE FATAL 'This unit should only be used in console applications'}
{$ENDIF}

interface

uses
  System.Classes, System.SysUtils,
  LoggerPro;

type
  { @abstract(This appender assumes the application is running from the console and simply uses Writeln
    without any dependency on Windows to send logs to the current console; this allows console logging from Linux)
    To learn how to use this appender, check the sample @code(SimpleConsole_appender.dproj)
  }
  TLoggerProSimpleConsoleAppender = class(TLoggerProAppenderBase)
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation


procedure TLoggerProSimpleConsoleAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProSimpleConsoleAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProSimpleConsoleAppender.WriteLog(const aLogItem: TLogItem);
begin
  Writeln(FormatLog(aLogItem));
end;

end.
