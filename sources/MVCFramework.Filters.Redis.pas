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
// ***************************************************************************
//
// Redis-backed filter helpers for the minimal-API surface. Kept in a
// separate unit so MVCFramework.Filters does not pull DelphiRedisClient as
// a hard dependency — only callers that actually use a Redis-backed filter
// take the dep.
//
// ***************************************************************************

unit MVCFramework.Filters.Redis;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI;

// HTTPFilter that enforces a fixed-window rate limit per client IP, with
// counters stored in Redis. Distributed equivalent of RateLimit() —
// suitable for multi-instance deployments behind a load balancer where the
// in-memory per-process map would let each replica grant its own quota.
// Exceeded buckets short-circuit with HTTP 429 + Retry-After.
//
// Fail-open: on Redis connectivity errors the request is allowed through
// (matches the classic TMVCRedisRateLimitStorage semantics — better to
// serve a request than to take the site down because Redis blinked).
//
//   uses MVCFramework.Filters.Redis;
//   ...
//   lEngine.UseHTTPFilter(RateLimitRedis(100, 60));   // 100 req / minute / IP
//   lEngine.UseHTTPFilter(RateLimitRedis(100, 60,
//     '10.0.0.7', 6379, 'secret', 'myapp:rl:'));
function RateLimitRedis(
  const AMaxRequests: Integer = 60;
  const AWindowSeconds: Integer = 60;
  const ARedisHost: string = '127.0.0.1';
  const ARedisPort: Integer = 6379;
  const ARedisPassword: string = '';
  const AKeyPrefix: string = 'ratelimit:'): TMVCHTTPFilter;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  Redis.Client,
  Redis.Values,
  Redis.Commons,
  Redis.NetLib.INDY;

type
  // Per-filter Redis client factory. Captured by the closure via an
  // interface ref so a single configuration travels with the filter for
  // the engine's lifetime. A fresh IRedisClient is built per request
  // (matches the original TMVCRedisRateLimitStorage shape — no shared
  // connection, no pool); swap for a pooled wrapper if it ever becomes
  // a hotspot.
  IRedisRateLimitConfig = interface
    ['{C3F0F1B4-3D1B-4E20-9F2C-6C8B7A1A0E11}']
    function NewClient: IRedisClient;
    function KeyPrefix: string;
  end;

  TRedisRateLimitConfig = class(TInterfacedObject, IRedisRateLimitConfig)
  strict private
    fHost: string;
    fPort: Integer;
    fPassword: string;
    fKeyPrefix: string;
  public
    constructor Create(const AHost: string; const APort: Integer;
      const APassword, AKeyPrefix: string);
    function NewClient: IRedisClient;
    function KeyPrefix: string;
  end;

constructor TRedisRateLimitConfig.Create(const AHost: string;
  const APort: Integer; const APassword, AKeyPrefix: string);
begin
  inherited Create;
  fHost := AHost;
  fPort := APort;
  fPassword := APassword;
  fKeyPrefix := AKeyPrefix;
end;

function TRedisRateLimitConfig.NewClient: IRedisClient;
begin
  Result := NewRedisClient(fHost, fPort);
  if not fPassword.IsEmpty then
    Result.AUTH(fPassword);
end;

function TRedisRateLimitConfig.KeyPrefix: string;
begin
  Result := fKeyPrefix;
end;

function RateLimitRedis(const AMaxRequests: Integer;
  const AWindowSeconds: Integer;
  const ARedisHost: string;
  const ARedisPort: Integer;
  const ARedisPassword: string;
  const AKeyPrefix: string): TMVCHTTPFilter;
var
  lConfig: IRedisRateLimitConfig;
begin
  lConfig := TRedisRateLimitConfig.Create(
    ARedisHost, ARedisPort, ARedisPassword, AKeyPrefix);

  Result :=
    procedure (const AContext: TWebContext;
               const ANext: TMVCHTTPFilterNext)
    var
      lRedis: IRedisClient;
      lKey: string;
      lCountResp, lTTLResp: TRedisInteger;
      lCount: Int64;
      lTTL: Integer;
      lExceeded: Boolean;
    begin
      lExceeded := False;
      try
        lRedis := lConfig.NewClient;
        lKey := lConfig.KeyPrefix + AContext.Request.ClientIp;

        // Atomic INCR (Redis returns post-increment value). Fails open if
        // Redis is unreachable / returns nil.
        lCountResp := lRedis.INCR(lKey);
        if not lCountResp.HasValue then
        begin
          ANext();
          Exit;
        end;
        lCount := lCountResp.Value;

        if lCount = 1 then
        begin
          // First hit in this window — stamp the TTL.
          lRedis.EXPIRE(lKey, AWindowSeconds);
          lTTL := AWindowSeconds;
        end
        else
        begin
          lTTLResp := lRedis.TTL(lKey);
          if lTTLResp.HasValue then
            lTTL := lTTLResp.Value
          else
            lTTL := AWindowSeconds;

          if lTTL < 0 then
          begin
            // Key expired between INCR and TTL — restart the window.
            lRedis.DEL([lKey]);
            lRedis.INCR(lKey);
            lRedis.EXPIRE(lKey, AWindowSeconds);
            lTTL := AWindowSeconds;
            lCount := 1;
          end;
        end;

        if lCount > AMaxRequests then
        begin
          // Never advertise Retry-After: 0 — that tells the client to retry
          // immediately, contradicting the 429. Clamp to at least 1 second.
          if lTTL < 1 then
            lTTL := 1;
          AContext.Response.SetCustomHeader('Retry-After', IntToStr(lTTL));
          AContext.Response.StatusCode := 429;
          lExceeded := True;
        end;
      except
        // Fail open — never take the site down because Redis is unhappy.
        on E: Exception do
          lExceeded := False;
      end;

      if lExceeded then
        Exit; // short-circuit — no Next()
      ANext();
    end;
end;

end.
