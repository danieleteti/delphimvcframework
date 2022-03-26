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
  private
    FFormatSettings: TFormatSettings;
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation


procedure TLoggerProSimpleConsoleAppender.Setup;
begin
  FFormatSettings := LoggerPro.GetDefaultFormatSettings;
end;

procedure TLoggerProSimpleConsoleAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProSimpleConsoleAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
  ds: string;
begin
  ds := DateTimeToStr(aLogItem.TimeStamp, FFormatSettings);
  lText := Format('[%-8s] %s [%2:-10s] %s', [aLogItem.LogTag, ds, aLogItem.LogTypeAsString, aLogItem.LogMessage],
                   FFormatSettings);
  Writeln(lText);
end;

end.
