unit LoggerPro.DBAppender.FireDAC;

// DB log appender for FireDAC

interface

uses
  System.Classes,
  LoggerPro, System.SysUtils, Data.DB,
  LoggerPro.DBAppender,
  FireDAC.Stan.Error,
  FireDAC.DApt,
  FireDAC.Phys,
  FireDAC.Stan.Param,
  FireDAC.Comp.Client;

type
  TLoggerProDBAppenderFireDACCustom = class abstract(TLoggerProDBAppender<TFDStoredProc>)
  protected
    procedure RefreshParams(DataObj: TFDStoredProc); override;
    procedure ExecuteDataObject(DataObj: TFDStoredProc); override;
  end;

  /// <summary>LoggerPro that persists to DB via a FireDAC stored procedure</summary>
  TLoggerProDBAppenderFireDAC = class(TLoggerProDBAppenderFireDACCustom)
  public
    constructor Create(
      const ConnectionDefName: String;
      const StoredProcName: String;
      const OnDBWriteError: TOnDBWriteError); reintroduce; overload;
    constructor Create(
      const ConnectionDefName: String;
      const StoredProcName: String;
      const SetParams: TSetParams<TFDStoredProc>;
      const OnDBWriteError: TOnDBWriteError); reintroduce; overload;
  end;


implementation

{ TLoggerProDBAppenderFireDAC }

constructor TLoggerProDBAppenderFireDAC.Create(const ConnectionDefName,
  StoredProcName: String; const SetParams: TSetParams<TFDStoredProc>;
  const OnDBWriteError: TOnDBWriteError);
begin
  inherited Create(
      function: TCustomConnection
      begin
        Result := TFDConnection.Create(nil);
        Result.LoginPrompt := False;
        TFDConnection(Result).ConnectionDefName := ConnectionDefName;
      end,
      function(Connection: TCustomConnection): TFDStoredProc
      begin
        Result := TFDStoredProc.Create(nil);
        Result.StoredProcName := StoredProcName;
        Result.Connection := Connection as TFDConnection;
      end,
      SetParams,
      OnDBWriteError);
end;

procedure TLoggerProDBAppenderFireDACCustom.ExecuteDataObject(DataObj: TFDStoredProc);
begin
  DataObj.ExecProc;
end;

procedure TLoggerProDBAppenderFireDACCustom.RefreshParams(DataObj: TFDStoredProc);
begin
  DataObj.Prepare;
end;

constructor TLoggerProDBAppenderFireDAC.Create(const ConnectionDefName,
  StoredProcName: String; const OnDBWriteError: TOnDBWriteError);
begin
  inherited Create(
      function: TCustomConnection
      begin
        Result := TFDConnection.Create(nil);
        Result.LoginPrompt := False;
        TFDConnection(Result).ConnectionDefName := ConnectionDefName;
      end,
      function(Connection: TCustomConnection): TFDStoredProc
      begin
        Result := TFDStoredProc.Create(nil);
        Result.StoredProcName := StoredProcName;
        Result.Connection := Connection as TFDConnection;
      end,
      procedure(SP: TFDStoredProc; LogItem: TLogItem)
      begin
        SP.ParamByName('p_log_type').Value := Integer(LogItem.LogType);
        SP.ParamByName('p_log_tag').Value := LogItem.LogTag;
        SP.ParamByName('p_log_message').Value := LogItem.LogMessage;
        SP.ParamByName('p_log_timestamp').Value := LogItem.TimeStamp;
        SP.ParamByName('p_log_thread_id').Value := LogItem.ThreadID;
      end,
      OnDBWriteError);
end;

end.
