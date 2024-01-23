unit LoggerProConfig;

interface

uses
  LoggerPro, LoggerPro.Renderers;

function Log: ILogWriter;

implementation

uses
  LoggerPro.ConsoleAppender, Winapi.Windows;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization



LoggerPro.Renderers.gDefaultLogItemRenderer := TLogItemRendererNoTag; //optional

_Log := BuildLogWriter([TLoggerProConsoleAppender.Create]);
if not IsConsole then
  AllocConsole;

end.
