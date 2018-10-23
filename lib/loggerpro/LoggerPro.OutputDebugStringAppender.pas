unit LoggerPro.OutputDebugStringAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProOutputDebugStringAppender))
  @author(Daniele Teti) }

interface

uses
  LoggerPro, System.Classes;

type
  { @abstract(This appenders sends logs to the @code(OutputDebugString) function on Windows OSes)
    To learn how to use this appender, check the sample @code(outputdebugstring_appender.dproj)
  }
  TLoggerProOutputDebugStringAppender = class(TLoggerProAppenderBase)
  private
    FModuleName: string;
  public
    constructor Create; override;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows, Winapi.Messages, System.IOUtils;

{ TStringsLogAppender }
const
  DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-10s] %3:s [%4:s]';

constructor TLoggerProOutputDebugStringAppender.Create;
begin
  inherited Create;
end;

procedure TLoggerProOutputDebugStringAppender.Setup;
begin
  FModuleName := TPath.GetFileName(GetModuleName(HInstance));
end;

procedure TLoggerProOutputDebugStringAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProOutputDebugStringAppender.WriteLog(const aLogItem
  : TLogItem);
var
  lLog: string;
begin
  lLog := '(' + FModuleName + ') ' + Format(DEFAULT_LOG_FORMAT,
    [datetimetostr(aLogItem.TimeStamp), aLogItem.ThreadID,
    aLogItem.LogTypeAsString, aLogItem.LogMessage, aLogItem.LogTag]);
  OutputDebugString(PChar(lLog));
end;

end.
