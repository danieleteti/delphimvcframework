// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit LoggerPro.RedisAppender;

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
    constructor Create(aRedis: IRedisClient; aMaxSize: Int64 = 5000; aKeyPrefix: string = 'loggerpro'; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TryToRestart(var Restarted: Boolean); override;
  end;

implementation

uses
  System.SysUtils;

constructor TLoggerProRedisAppender.Create(aRedis: IRedisClient; aMaxSize: Int64; aKeyPrefix: string; aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  FRedis := aRedis;
  FLogKeyPrefix := aKeyPrefix;
  FMaxSize := aMaxSize;
end;

procedure TLoggerProRedisAppender.Setup;
begin
  inherited;
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
  lText := FormatLog(aLogItem);
  lKey := FLogKeyPrefix + '::logs'; // + aLogItem.LogTypeAsString.ToLower;
  // Push the log item to the right of the list (logs:info, logs:warning, log:error)
  FRedis.RPUSH(lKey, [lText]);
  // Trim the list to the FMaxSize last elements
  FRedis.LTRIM(lKey, -FMaxSize, -1);
end;

end.
