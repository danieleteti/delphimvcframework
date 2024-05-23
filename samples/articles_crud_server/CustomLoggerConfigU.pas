unit CustomLoggerConfigU;

interface

uses
  LoggerPro; // loggerpro core

function GetLogger: ILogWriter;

implementation

uses
  System.IOUtils
  , LoggerPro.FileAppender // loggerpro file appender (logs to file)
  , LoggerPro.ConsoleAppender // loggerpro simple console appender (logs to console with colors)
  , LoggerPro.DBAppender.FireDAC // loggerpro DB Appender (logs to database)
  {$IFDEF MSWINDOWS}
  , LoggerPro.OutputdebugStringAppender// loggerpro outputdebugstring appender (logs to the debugger)
  {$ENDIF}
  , Commons, System.SysUtils;

const
  FailedDBWriteTag = 'FailedDBWrite';

var _FallbackLog: ILogWriter;

function GetFallBackLogger: ILogWriter;
begin
  if _FallbackLog = nil then
  begin
    _FallbackLog := BuildLogWriter([
      TLoggerProSimpleFileAppender.Create(10, 2048, 'logs', TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT)
    ]);
  end;
  Result := _FallbackLog;
end;

function GetLogger: ILogWriter;
begin
  Result := BuildLogWriter([
    TLoggerProFileAppender.Create(10, 1000, TPath.Combine('MyFolder', 'MyLogs')),
    TLoggerProDBAppenderFireDAC.Create(
      CON_DEF_NAME,
      'sp_loggerpro_writer',
      // error handler, just write to disk on the server for later analysis
      procedure(const Sender: TObject; const LogItem: TLogItem; const DBError: Exception; var RetryCount: Integer)
      var
        lIntf: ILogItemRenderer;
      begin
        lIntf := TLogItemRenderer.GetDefaultLogItemRenderer();
        GetFallBackLogger.Error('DBAppender Is Failing (%d): %s %s', [RetryCount, DBError.ClassName, DBError.Message], FailedDBWriteTag);
        GetFallBackLogger.Error(lIntf.RenderLogItem(LogItem), FailedDBWriteTag);
      end),
      TLoggerProConsoleAppender.Create
     {$IFDEF MSWINDOWS}, TLoggerProOutputDebugStringAppender.Create{$ENDIF}
    ], nil, [
      TLogType.Debug,
      TLogType.Info, {writes on DB only for INFO+}
      TLogType.Debug
      {$IFDEF MSWINDOWS}, TLogType.Debug{$ENDIF}
      ]);
end;

end.
