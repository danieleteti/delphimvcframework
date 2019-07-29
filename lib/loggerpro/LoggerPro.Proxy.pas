unit LoggerPro.Proxy;

interface

uses Classes, System.SysUtils, LoggerPro;

type
  ILogAppenderProxy=interface
    ['{34816F83-9FBF-461E-8913-F10F9460D712}']
    function GetInternalAppender: ILogAppender;
    property InternalAppender: ILogAppender read GetInternalAppender;
  end;

  TLoggerProFilter = class abstract
    class function Build(Appender: ILogAppender; Filter: TFunc<TLogItem, boolean>): ILogAppender;
  end;


implementation

type
  TLoggerProAppenderFilterImpl = class(TLoggerProAppenderBase, ILogAppender, ILogAppenderProxy)
  private
    FAppender: ILogAppender;
    FFilter: TFunc<TLogItem, boolean>;
    function GetInternalAppender: ILogAppender;
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    property InternalAppender: ILogAppender read GetInternalAppender;
    constructor Create(Appender: ILogAppender; Filter: TFunc<TLogItem, boolean>);  reintroduce;
  end;

{ TLoggerProAppenderFilterImpl }

constructor TLoggerProAppenderFilterImpl.Create(Appender: ILogAppender;
  Filter: TFunc<TLogItem, boolean>);
begin
  inherited Create;
  self.FFilter := Filter;
  self.FAppender := Appender;
end;

function TLoggerProAppenderFilterImpl.GetInternalAppender: ILogAppender;
begin
  result := FAppender;
end;

procedure TLoggerProAppenderFilterImpl.Setup;
begin
  FAppender.Setup;
end;

procedure TLoggerProAppenderFilterImpl.TearDown;
begin
  FAppender.TearDown;
end;

procedure TLoggerProAppenderFilterImpl.WriteLog(const aLogItem: TLogItem);
begin
  if FFilter(aLogItem) then
    FAppender.WriteLog(aLogItem);
end;


class function TLoggerProFilter.Build(Appender: ILogAppender;
  Filter: TFunc<TLogItem, boolean>): ILogAppender;
begin
  result := TLoggerProAppenderFilterImpl.Create(Appender, Filter);
end;


end.
