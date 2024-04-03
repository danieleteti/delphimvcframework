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
  /// <summary>LoggerPro that persists to DB via a FireDAC stored procedure</summary>
  TLoggerProDBAppenderFireDAC = class(TLoggerProDBAppender<TFDStoredProc>)
  protected
    procedure RefreshParams(DataObj: TFDStoredProc); override;
    procedure ExecuteDataObject(DataObj: TFDStoredProc); override;
  end;

implementation

{ TLoggerProDBAppenderFireDAC }

procedure TLoggerProDBAppenderFireDAC.ExecuteDataObject(DataObj: TFDStoredProc);
begin
  DataObj.ExecProc;
end;

procedure TLoggerProDBAppenderFireDAC.RefreshParams(DataObj: TFDStoredProc);
begin
  DataObj.Prepare;
end;

end.
