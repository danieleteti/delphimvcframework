unit LoggerProConfig;

// instantiate and call the Logging appender that writes to the Logging database

interface

uses
  LoggerPro;

///<summary>global function pointer tha returns a DB Logger instance</summary>
var
  Log: function: ILogWriter;

implementation

uses
  System.SysUtils,
  System.Classes,
  LoggerPro.DBAppender.FireDAC,
  LoggerPro.FileAppender,
  Data.DB,
  System.IOUtils,
  System.NetEncoding,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  FDConnectionConfigU,
  LoggerPro.Renderers;

var
  _Log: ILogWriter;
  _FallbackLog: ILogWriter;

const
  FailedDBWriteTag = 'FailedDBWrite';


function GetFallBackLogger: ILogWriter;
begin
  if _FallbackLog = nil then
  begin
    _FallbackLog := BuildLogWriter([
      TLoggerProSimpleFileAppender.Create(10, 2048, 'logs')
    ]);
  end;
  Result := _FallbackLog;
end;

function GetLogger: ILogWriter;
begin

  if _Log = nil then
  begin
    GetFallBackLogger.Info('Initializing db appender', FailedDBWriteTag);

    _Log := BuildLogWriter([TLoggerProDBAppenderFireDAC.Create(
      // create an ADO DB Connection
      function: TCustomConnection
      begin
        Result := TFDConnection.Create(nil);
        Result.LoginPrompt := False;
        // todo:  set the connection string in here, typically read from env variables or config file
        TFDConnection(Result).ConnectionDefName := CON_DEF_NAME;
      end,
    // create a stored proc
      function(Connection: TCustomConnection): TFDStoredProc
      begin
        Result := TFDStoredProc.Create(nil);
        Result.StoredProcName := 'sp_loggerpro_writer';
        Result.Connection := Connection as TFDConnection;
      end,
    // populate the stored proc
      procedure(SP: TFDStoredProc; LogItem: TLogItem)
      begin
        SP.ParamByName('p_log_type').Value := Integer(LogItem.LogType);
        SP.ParamByName('p_log_tag').Value := LogItem.LogTag;
        SP.ParamByName('p_log_message').Value := LogItem.LogMessage;
        SP.ParamByName('p_log_timestamp').Value := LogItem.TimeStamp;
        SP.ParamByName('p_log_thread_id').Value := LogItem.ThreadID;
      end,
      // error handler, just write to disk on the server for later analysis
      procedure(const Sender: TObject; const LogItem: TLogItem; const DBError: Exception; var RetryCount: Integer)
      var
        lIntf: ILogItemRenderer;
      begin
        lIntf := GetDefaultLogItemRenderer();
        GetFallBackLogger.Error('DBAppender Is Failing (%d): %s %s', [RetryCount, DBError.ClassName, DBError.Message], FailedDBWriteTag);
        GetFallBackLogger.Error(lIntf.RenderLogItem(LogItem), FailedDBWriteTag);
      end)]);
  end;
  Result := _Log;
end;

initialization

Log := GetLogger;

finalization

_Log := nil;
_FallbackLog := nil;

end.
