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

unit MVCFramework.WebSocket.RateLimiter;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.DateUtils;

type
  /// <summary>
  /// Token bucket rate limiter for WebSocket connections
  /// Implements the token bucket algorithm for smooth rate limiting
  /// </summary>
  TMVCWebSocketRateLimiter = class
  private
    FMaxTokens: Integer;
    FRefillRate: Double; // Tokens per second
    FTokens: Double;
    FLastRefill: TDateTime;
    FLock: TCriticalSection;
    procedure Refill;
  public
    /// <summary>
    /// Creates a new rate limiter
    /// </summary>
    /// <param name="AMaxTokens">Maximum number of tokens (burst capacity)</param>
    /// <param name="ARefillRate">Tokens added per second (sustained rate)</param>
    constructor Create(AMaxTokens: Integer; ARefillRate: Double);
    destructor Destroy; override;

    /// <summary>
    /// Attempts to consume tokens
    /// </summary>
    /// <param name="ATokens">Number of tokens to consume (default: 1)</param>
    /// <returns>True if tokens were available and consumed, False otherwise</returns>
    function TryConsume(ATokens: Integer = 1): Boolean;

    /// <summary>
    /// Resets the rate limiter to full capacity
    /// </summary>
    procedure Reset;

    /// <summary>
    /// Gets the current number of available tokens
    /// </summary>
    function GetAvailableTokens: Integer;
  end;

  /// <summary>
  /// Per-connection rate limiter that tracks multiple rate limits
  /// </summary>
  TMVCConnectionRateLimiter = class
  private
    FMessageLimiter: TMVCWebSocketRateLimiter; // Messages per second
    FBytesLimiter: TMVCWebSocketRateLimiter;   // Bytes per second
    FConnectionId: string;
  public
    constructor Create(const AConnectionId: string;
      AMaxMessagesPerSecond: Integer;
      AMaxBytesPerSecond: Integer);
    destructor Destroy; override;

    /// <summary>
    /// Checks if a message can be sent
    /// </summary>
    /// <param name="AMessageSize">Size of the message in bytes</param>
    /// <returns>True if the message is within rate limits</returns>
    function CanSendMessage(AMessageSize: Integer): Boolean;

    /// <summary>
    /// Records that a message was sent
    /// </summary>
    /// <param name="AMessageSize">Size of the message in bytes</param>
    /// <returns>True if within limits, False if rate limited</returns>
    function RecordMessage(AMessageSize: Integer): Boolean;

    property ConnectionId: string read FConnectionId;
  end;

implementation

uses
  System.Math;

{ TMVCWebSocketRateLimiter }

constructor TMVCWebSocketRateLimiter.Create(AMaxTokens: Integer; ARefillRate: Double);
begin
  inherited Create;
  FMaxTokens := AMaxTokens;
  FRefillRate := ARefillRate;
  FTokens := AMaxTokens;
  FLastRefill := Now;
  FLock := TCriticalSection.Create;
end;

destructor TMVCWebSocketRateLimiter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMVCWebSocketRateLimiter.Refill;
var
  CurrentTime: TDateTime;
  ElapsedSeconds: Double;
  TokensToAdd: Double;
begin
  CurrentTime := Now;
  ElapsedSeconds := SecondsBetween(CurrentTime, FLastRefill);

  if ElapsedSeconds > 0 then
  begin
    // Calculate tokens to add based on elapsed time
    TokensToAdd := FRefillRate * ElapsedSeconds;
    FTokens := Min(FMaxTokens, FTokens + TokensToAdd);
    FLastRefill := CurrentTime;
  end;
end;

function TMVCWebSocketRateLimiter.TryConsume(ATokens: Integer): Boolean;
begin
  FLock.Enter;
  try
    Refill;

    if FTokens >= ATokens then
    begin
      FTokens := FTokens - ATokens;
      Result := True;
    end
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

procedure TMVCWebSocketRateLimiter.Reset;
begin
  FLock.Enter;
  try
    FTokens := FMaxTokens;
    FLastRefill := Now;
  finally
    FLock.Leave;
  end;
end;

function TMVCWebSocketRateLimiter.GetAvailableTokens: Integer;
begin
  FLock.Enter;
  try
    Refill;
    Result := Trunc(FTokens);
  finally
    FLock.Leave;
  end;
end;

{ TMVCConnectionRateLimiter }

constructor TMVCConnectionRateLimiter.Create(const AConnectionId: string;
  AMaxMessagesPerSecond: Integer; AMaxBytesPerSecond: Integer);
begin
  inherited Create;
  FConnectionId := AConnectionId;

  // Create message rate limiter
  // Burst capacity: 2x the per-second rate (allows short bursts)
  FMessageLimiter := TMVCWebSocketRateLimiter.Create(
    AMaxMessagesPerSecond * 2,  // Burst capacity
    AMaxMessagesPerSecond        // Sustained rate
  );

  // Create bytes rate limiter
  FBytesLimiter := TMVCWebSocketRateLimiter.Create(
    AMaxBytesPerSecond * 2,  // Burst capacity
    AMaxBytesPerSecond        // Sustained rate
  );
end;

destructor TMVCConnectionRateLimiter.Destroy;
begin
  FMessageLimiter.Free;
  FBytesLimiter.Free;
  inherited;
end;

function TMVCConnectionRateLimiter.CanSendMessage(AMessageSize: Integer): Boolean;
begin
  // Check both message count and byte limits without consuming tokens
  Result := (FMessageLimiter.GetAvailableTokens >= 1) and
            (FBytesLimiter.GetAvailableTokens >= AMessageSize);
end;

function TMVCConnectionRateLimiter.RecordMessage(AMessageSize: Integer): Boolean;
begin
  // Try to consume tokens for both message count and bytes
  // Both must succeed for the message to be allowed
  if FMessageLimiter.TryConsume(1) then
  begin
    if FBytesLimiter.TryConsume(AMessageSize) then
      Result := True
    else
    begin
      // Bytes limit exceeded - need to refund the message token
      // Since we can't easily refund, we just return false
      // The message token will be naturally refilled over time
      Result := False;
    end;
  end
  else
    Result := False;
end;

end.
