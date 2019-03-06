unit LoggerPro.RedisAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProRedisAppender))
  @author(Daniele Teti) }

interface

uses
  LoggerPro, System.Classes, System.DateUtils,
  Redis.Commons {Redis.Commons is included in DelphiRedisClient, also available with GETIT} ,
  Redis.NetLib.INDY {Redis.NetLib.INDY is included in DelphiRedisClient, also available with GETIT};

type
  {
    @abstract(Logs to a Redis instance)
    To learn how to use this appender, check the sample @code(remote_redis_appender.dproj)
    @author(Daniele Teti - d.teti@bittime.it)
  }
  TLoggerProRedisAppender = class(TLoggerProAppenderBase)
  private
    FRedis: IRedisClient;
    FLogKeyPrefix: string;
    FMaxSize: Int64;
  public
    constructor Create(aRedis: IRedisClient; aMaxSize: Int64 = 5000; aKeyPrefix: string = 'loggerpro'); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;
  end;

implementation

uses
  System.SysUtils;

const
  DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-8s] %3:s [%4:s]';

constructor TLoggerProRedisAppender.Create(aRedis: IRedisClient; aMaxSize: Int64; aKeyPrefix: string);
begin
  inherited Create;
  FRedis := aRedis;
  FLogKeyPrefix := aKeyPrefix;
  FMaxSize := aMaxSize;
end;

procedure TLoggerProRedisAppender.Setup;
begin
  FRedis.Connect;
end;

procedure TLoggerProRedisAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProRedisAppender.TryToRestart(var Restarted: Boolean);
begin
  inherited;
  Restarted := False;
  try
    FRedis.Disconnect
  except
  end;
  try
    FRedis.Connect;
    Restarted := True;
  except
  end;
end;

procedure TLoggerProRedisAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
  lKey: string;
begin
  lText := Format(DEFAULT_LOG_FORMAT, [datetimetostr(aLogItem.TimeStamp),
    aLogItem.ThreadID, aLogItem.LogTypeAsString, aLogItem.LogMessage,
    aLogItem.LogTag]);
  lKey := FLogKeyPrefix + '::logs'; // + aLogItem.LogTypeAsString.ToLower;
  // Push the log item to the right of the list (logs:info, logs:warning, log:error)
  FRedis.RPUSH(lKey, [lText]);
  // Trim the list to the FMaxSize last elements
  FRedis.LTRIM(lKey, -FMaxSize, -1);
end;

end.
