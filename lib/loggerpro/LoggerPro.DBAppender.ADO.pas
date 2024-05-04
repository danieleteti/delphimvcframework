unit LoggerPro.DBAppender.ADO;

// DB log appender - using dbGo aka ADO

interface

uses
  System.Classes,
  LoggerPro, System.SysUtils, Data.DB,
  LoggerPro.DBAppender,
  Data.Win.ADODB;

type
  ESQLException = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(AMessage: string; AErrorCode: Integer); overload;
    constructor Create(StoredProc: TADOStoredProc); overload;
  end;

  /// <summary>LoggerPro that persists to DB via ADO stored procedure</summary>
  TLoggerProDBAppenderADO = class(TLoggerProDBAppender<TADOStoredProc>)
  protected
    procedure RefreshParams(DataObj: TADOStoredProc); override;
    procedure ExecuteDataObject(DataObj: TADOStoredProc); override;
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

implementation

uses
  System.IOUtils, Winapi.ActiveX;

{ ESQLException }

constructor ESQLException.Create(AMessage: string; AErrorCode: Integer);
begin
  inherited Create(AMessage);
  FErrorCode := AErrorCode;
end;

constructor ESQLException.Create(StoredProc: TADOStoredProc);
var
  LastErrorIdx: Integer;
begin
  if StoredProc.Connection <> nil then
  begin
    LastErrorIdx := StoredProc.Connection.Errors.Count - 1;
    Create(StoredProc.Connection.Errors.Item[LastErrorIdx].Description, StoredProc.Connection.Errors.Item[LastErrorIdx]
      .NativeError);
  end;
end;

{ TLoggerProDBAppenderADO }

procedure TLoggerProDBAppenderADO.ExecuteDataObject(DataObj: TADOStoredProc);
begin
  DataObj.ExecProc;

  if DataObj.Connection.Errors.Count > 0 then
    raise ESQLException.Create(DataObj);
end;

procedure TLoggerProDBAppenderADO.RefreshParams(DataObj: TADOStoredProc);
begin
  DataObj.Parameters.Refresh;
end;

procedure TLoggerProDBAppenderADO.Setup;
begin
  CoInitialize(nil);
  inherited;
end;

procedure TLoggerProDBAppenderADO.TearDown;
begin
  inherited;
  CoUninitialize;
end;


end.
