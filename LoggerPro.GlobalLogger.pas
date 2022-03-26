unit LoggerPro.GlobalLogger;
{<@abstract(Contains the global logger as a thread safe singleton)
  Use the global logger for fast&dirty logging, but consider to use your own
  instance of @link(ILogWriter) (created using @link(BuildLogWriter)) for all your serious logging needs.
  @author(Daniele Teti - d.teti@bittime.it)
}

interface

uses
  LoggerPro;
{ @abstract(The global logger. Just uses @link(Logger.GlobalLogger) and you can start to log using @code(Log) function.)
  The global logger is configured with a @link(TLoggerProFileAppender) using default settings.
}
function Log: ILogWriter;

{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads, so call this before unload DLL)
  Use this also in ISAPI dll. Check the @code(loggerproisapisample.dll) sample
}
procedure ReleaseGlobalLogger;

implementation

uses
  LoggerPro.FileAppender;

var
  _Logger: ILogWriter;
  _Lock: TObject = nil;
  _ShuttedDown: boolean = false;

function Log: ILogWriter;
begin
  if _Logger = nil then
  begin
    if not _ShuttedDown then
    begin
      TMonitor.Enter(_Lock);
      try
        if _Logger = nil then // double check
        begin
          _Logger := BuildLogWriter([TLoggerProFileAppender.Create]);
        end;
      finally
        TMonitor.Exit(_Lock);
      end;
    end;
  end;
  Result := _Logger;
end;

procedure ReleaseGlobalLogger;
begin
  if _Logger <> nil then
  begin
    TMonitor.Enter(_Lock);
    try
      if _Logger <> nil then // double check
      begin
        _Logger := nil;
        _ShuttedDown := True;
      end;
    finally
      TMonitor.Exit(_Lock);
    end;
  end;
end;

initialization

_Lock := TObject.Create;

finalization

_Lock.Free;

end.
