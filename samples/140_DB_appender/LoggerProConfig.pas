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
  LoggerPro.DBAppender.ADO,
  Data.DB, Data.Win.ADODB,
  System.IOUtils,
  Winapi.ActiveX, System.NetEncoding;

var
  _Log: ILogWriter;

const
  FailedDBWriteTag = 'FailedDBWrite';

function GetLogger: ILogWriter;
begin

  if _Log = nil then
  begin
    _Log := BuildLogWriter([TLoggerProDBAppenderADO.Create(
      // create an ADO DB Connection
      function: TCustomConnection
      begin
        Result := TADOConnection.Create(nil);
        Result.LoginPrompt := False;
        // todo:  set the connection string in here, typically read from env variables or config file
        TADOConnection(Result).ConnectionString := '';
      end,
    // create a stored proc
      function(Connection: TCustomConnection): TADOStoredProc
      begin
        Result := TADOStoredProc.Create(nil);
        Result.ProcedureName := 'todo: set the stored proc name here';
        Result.Connection := Connection as TADOConnection;
      end,
    // populate the stored proc
      procedure(SP: TADOStoredProc; LogItem: TLogItem)
      begin
        SP.Parameters.ParamByName('@LogType').Value := Integer(LogItem.LogType);
        SP.Parameters.ParamByName('@LogTag').Value := LogItem.LogTag;
        SP.Parameters.ParamByName('@LogMessage').Value := LogItem.LogMessage;
        SP.Parameters.ParamByName('@Timestamp').Value := LogItem.TimeStamp;
        SP.Parameters.ParamByName('@TID').Value := LogItem.ThreadID;
      end,
    // error handler, just write to disk on the server for later analysis
      procedure(const Sender: TObject; const LogItem: TLogItem; const DBError: Exception; var RetryCount: Integer)
      begin
        // write code in here to write out using system default logger to a local file
        // Log.Error('Could not write Viewer Request: %s', [DBError.Message], FailedDBWriteTag);
        // Log.Error('Data: %s', [LogItem.LogMessage], FailedDBWriteTag);
      end)]);

  end;
  Result := _Log;
end;

initialization

Log := GetLogger;

finalization

_Log := nil;

end.
