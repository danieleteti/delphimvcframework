unit LoggerProConfig;

interface

uses
  LoggerPro, LoggerPro.Renderers;

function Log: ILogWriter;

implementation

uses
  LoggerPro.ConsoleAppender,
  LoggerPro.FileAppender,
  Winapi.Windows;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization



LoggerPro.Renderers.gDefaultLogItemRenderer := TLogItemRendererNoTag; //optional

_Log := BuildLogWriter([
  TLoggerProConsoleLogFmtAppender.Create,
  TLoggerProLogFmtFileAppender.Create]);
if not IsConsole then
  AllocConsole;

end.
