// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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
// *************************************************************************** }

unit MVCFramework.Middleware.RateLimit.Redis;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.DateUtils,
  Redis.Client,
  Redis.Values,
  Redis.Commons,
  Redis.NetLib.INDY,
  MVCFramework.Middleware.RateLimit;

type
  /// <summary>
  /// Redis-based rate limit storage implementation
  /// Requires DelphiRedisClient library
  /// </summary>
  TMVCRedisRateLimitStorage = class(TInterfacedObject, IMVCRateLimitStorage)
  private
    FRedisHost: string;
    FRedisPort: Integer;
    FRedisPassword: string;
    FKeyPrefix: string;
    function GetRedisKey(const AKey: string): string;
    function GetRedisClient: IRedisClient;
  public
    constructor Create(const ARedisHost: string = '127.0.0.1';
      const ARedisPort: Integer = 6379;
      const ARedisPassword: string = '';
      const AKeyPrefix: string = 'ratelimit:');
    function CheckRateLimit(const AKey: string; const ALimit: Integer;
      const AWindowSeconds: Integer; out ARemaining: Integer;
      out AResetTime: TDateTime): Boolean;
    procedure ResetKey(const AKey: string);
    procedure Clear;
  end;

implementation

{ TMVCRedisRateLimitStorage }

constructor TMVCRedisRateLimitStorage.Create(const ARedisHost: string;
  const ARedisPort: Integer; const ARedisPassword: string;
  const AKeyPrefix: string);
begin
  inherited Create;
  FRedisHost := ARedisHost;
  FRedisPort := ARedisPort;
  FRedisPassword := ARedisPassword;
  FKeyPrefix := AKeyPrefix;
end;

function TMVCRedisRateLimitStorage.GetRedisKey(const AKey: string): string;
begin
  Result := FKeyPrefix + AKey;
end;

function TMVCRedisRateLimitStorage.GetRedisClient: IRedisClient;
begin
  Result := NewRedisClient(FRedisHost, FRedisPort);
  if not FRedisPassword.IsEmpty then
    Result.AUTH(FRedisPassword);
end;

function TMVCRedisRateLimitStorage.CheckRateLimit(const AKey: string;
  const ALimit: Integer; const AWindowSeconds: Integer; out ARemaining: Integer;
  out AResetTime: TDateTime): Boolean;
var
  LRedis: IRedisClient;
  LRedisKey: string;
  LCountResponse: TRedisInteger;
  LTTLResponse: TRedisInteger;
  LCount: Int64;
  LTTL: Integer;
begin
  LRedis := GetRedisClient;
  LRedisKey := GetRedisKey(AKey);

  try
    // Use Redis INCR command - atomic operation
    LCountResponse := LRedis.INCR(LRedisKey);
    if not LCountResponse.HasValue then
    begin
      // Error - fail open
      ARemaining := ALimit;
      AResetTime := IncSecond(Now, AWindowSeconds);
      Result := False;
      Exit;
    end;

    LCount := LCountResponse.Value;

    if LCount = 1 then
    begin
      // First request - set expiration
      LRedis.EXPIRE(LRedisKey, AWindowSeconds);
      LTTL := AWindowSeconds;
    end
    else
    begin
      // Get remaining time to expiration
      LTTLResponse := LRedis.TTL(LRedisKey);
      if LTTLResponse.HasValue then
        LTTL := LTTLResponse.Value
      else
        LTTL := AWindowSeconds;

      if LTTL < 0 then
      begin
        // Key expired between INCR and TTL - reset
        LRedis.DEL([LRedisKey]);
        LRedis.INCR(LRedisKey);
        LRedis.EXPIRE(LRedisKey, AWindowSeconds);
        LTTL := AWindowSeconds;
        LCount := 1;
      end;
    end;

    AResetTime := IncSecond(Now, LTTL);
    ARemaining := ALimit - LCount;

    Result := LCount > ALimit;

  except
    on E: Exception do
    begin
      // On Redis error, allow the request (fail-open)
      ARemaining := ALimit;
      AResetTime := IncSecond(Now, AWindowSeconds);
      Result := False;
    end;
  end;
end;

procedure TMVCRedisRateLimitStorage.ResetKey(const AKey: string);
var
  LRedis: IRedisClient;
begin
  LRedis := GetRedisClient;
  LRedis.DEL([GetRedisKey(AKey)]);
end;

procedure TMVCRedisRateLimitStorage.Clear;
var
  LRedis: IRedisClient;
  LKeysResponse: TRedisArray;
  LKeys: TArray<string>;
begin
  LRedis := GetRedisClient;

  try
    // Get all keys matching pattern
    LKeysResponse := LRedis.KEYS(FKeyPrefix + '*');

    if LKeysResponse.HasValue then
    begin
      // Use helper method to convert TRedisArray to TArray<string>
      LKeys := LKeysResponse.ToArray;

      // Delete all keys
      if Length(LKeys) > 0 then
        LRedis.DEL(LKeys);
    end;
  except
    // Ignore errors on clear
  end;
end;

end.
