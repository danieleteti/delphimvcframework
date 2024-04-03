unit LoggerPro.DBAppender;

// Abstract LoggerPro Appender that writes to a database
// Subclass to use with DB connection stack of choice e.g. ADO, FireDAC etc
// only supports parameterised stored procedures for security and performance

interface

uses
  System.Classes, LoggerPro, System.SysUtils, Data.DB;

type
  TOnDBWriteError = reference to procedure(const Sender: TObject; const LogItem: TLogItem; const DBError: Exception;
    var RetryCount: Integer);

  TGetDBConnection = reference to function: TCustomConnection;

  TGetStoredProc<T: class> = reference to function(Connection: TCustomConnection): T;

  TSetParams<T: class> = reference to procedure(DataObject: T; LogItem: TLogItem);


  ///<summary>Abstract class for writing logs to database</summary>
  /// <remarks>Subclass with your choice of stored procedure class to get a working logger </remarks>
  TLoggerProDBAppender<T: class> = class(TLoggerProAppenderBase)
  protected
    const MAX_RETRY_COUNT = 5;
  protected
    FOnDBWriteError: TOnDBWriteError;
    FGetDBConnection: TGetDBConnection;
    FGetStoredProc: TGetStoredProc<T>;
    FSetParams: TSetParams<T>;

    FDBConnection: TCustomConnection;
    FDBObject: T;
    procedure RefreshParams(DataObj: T); virtual; abstract;
    procedure ExecuteDataObject(DataObj: T); virtual; abstract;
  public
    constructor Create(GetDBConnection: TGetDBConnection; GetStoredProc: TGetStoredProc<T>; SetParams: TSetParams<T>;
      OnDBWriteError: TOnDBWriteError); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure TryToRestart(var Restarted: Boolean); override;
    procedure WriteLog(const ALogItem: TLogItem); override;
  end;

implementation

{ TLoggerProDBAppender }

///<summary>Create an instance of the DB logger</summary>
/// <param name="GetDBConnection">anonymous function that returns a configured DB Connection</param>
/// <param name="GetStoredProc">anonymous function that returns a stored proc that can write to the DB</param>
/// <param name="SetParams">anonymous procedure that populates parameters before the stored proc is executed</param>
/// <param name="OnDBWriteError">anonymous procedure to handle retry of a failed operaation</param>
constructor TLoggerProDBAppender<T>.Create(GetDBConnection: TGetDBConnection; GetStoredProc: TGetStoredProc<T>;
  SetParams: TSetParams<T>; OnDBWriteError: TOnDBWriteError);
begin
  inherited Create;
  FGetDBConnection := GetDBConnection;
  FGetStoredProc := GetStoredProc;
  FSetParams := SetParams;
  FOnDBWriteError := OnDBWriteError;
end;

procedure TLoggerProDBAppender<T>.Setup;
begin
  inherited;
  FDBConnection := FGetDBConnection;
end;

procedure TLoggerProDBAppender<T>.TearDown;
begin
  inherited;
  if FDBObject <> nil then
    FDBObject.Free;

  if FDBConnection <> nil then
  begin
    FDBConnection.Connected := False;
    FDBConnection.Free;
  end;
end;

procedure TLoggerProDBAppender<T>.TryToRestart(var Restarted: Boolean);
begin
  try
    // remove the DB Object
    if FDBObject <> nil then
    begin
      FDBObject.Free;
      FDBObject := nil;
    end;

    // reset the DB connection
    if FDBConnection <> nil then
    begin
      FDBConnection.Connected := False;
      FDBConnection.Free;
      FDBConnection := nil;
    end;
  except
    // no point catching the exception
  end;

  // now try to restart it
  FDBConnection := FGetDBConnection;
  Restarted := True;
end;

procedure TLoggerProDBAppender<T>.WriteLog(const ALogItem: TLogItem);
var
  RetryCount: Integer;
begin
  RetryCount := 0;
  repeat
    try
      if FDBObject = nil then
      begin
        FDBConnection.Connected := True;  //force an exception if needed
        FDBObject := FGetStoredProc(FDBConnection);
        RefreshParams(FDBObject); //this may not raise unhandled exception even in case of disconnection
      end;
      FSetParams(FDBObject, ALogItem);
      ExecuteDataObject(FDBObject);
      Break;
    except
      on E: Exception do
      begin
        // if there is an event handler for DB exception, call it
        if Assigned(FOnDBWriteError) then
          FOnDBWriteError(Self, ALogItem, E, RetryCount);
        Inc(RetryCount);
        // if the handler has set FRetryCount to a positive value then retry the call
        if RetryCount >= MAX_RETRY_COUNT then
          raise;
      end;
    end;
  until False;
end;

end.
