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
    {$IFDEF MSWINDOWS}
    FModuleName: string;
    {$ENDIF}
  public
    constructor Create(ALogFormat: string = DEFAULT_LOG_FORMAT); override;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils
{$IFDEF MSWINDOWS}
    , Winapi.Windows
    , Winapi.Messages
{$ENDIF}
  ;

{ TLoggerProOutputDebugStringAppender }

constructor TLoggerProOutputDebugStringAppender.Create(ALogFormat: string);
begin
  inherited;
end;

procedure TLoggerProOutputDebugStringAppender.Setup;
begin
  inherited;
{$IFDEF MSWINDOWS}
  FModuleName := TPath.GetFileName(GetModuleName(HInstance));
{$ENDIF}
end;

procedure TLoggerProOutputDebugStringAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProOutputDebugStringAppender.WriteLog(const aLogItem : TLogItem);
{$IFDEF MSWINDOWS}
var
  lLog: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  lLog := '(' + FModuleName + ') ' + FormatLog(aLogItem);
  OutputDebugString(PChar(lLog));
{$ENDIF}
end;

end.
