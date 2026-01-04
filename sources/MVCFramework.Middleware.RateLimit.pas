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

unit MVCFramework.Middleware.RateLimit;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  System.SyncObjs,
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Rate limit strategy type
  /// </summary>
  TMVCRateLimitStrategy = (
    /// <summary>
    /// Fixed window counter - simple counter that resets at fixed intervals
    /// </summary>
    rlsFixedWindow,
    /// <summary>
    /// Sliding window counter - more accurate, considers request timestamps
    /// </summary>
    rlsSlidingWindow,
    /// <summary>
    /// Token bucket - allows bursts, tokens refill over time
    /// </summary>
    rlsTokenBucket
  );

  /// <summary>
  /// Rate limit key generator type
  /// </summary>
  TMVCRateLimitKeyType = (
    /// <summary>
    /// Rate limit by client IP address
    /// </summary>
    rlkIPAddress,
    /// <summary>
    /// Rate limit by user identifier (from session or JWT)
    /// </summary>
    rlkUserID,
    /// <summary>
    /// Rate limit by API key (from header)
    /// </summary>
    rlkAPIKey,
    /// <summary>
    /// Rate limit by custom key (using callback)
    /// </summary>
    rlkCustom
  );

  /// <summary>
  /// Custom key extractor function
  /// </summary>
  TMVCRateLimitKeyExtractor = reference to function(const AContext: TWebContext): string;

  /// <summary>
  /// Rate limit exceeded callback
  /// </summary>
  TMVCRateLimitExceededProc = reference to procedure(
    const AContext: TWebContext;
    const AKey: string;
    const ALimit: Integer;
    const AWindowSeconds: Integer
  );

  /// <summary>
  /// Rate limit storage interface - can be implemented for Redis, Memcached, etc.
  /// </summary>
  IMVCRateLimitStorage = interface
    ['{8E5F9A7C-2D4B-4B8E-9F3A-1C6D8E9A7B5C}']
    /// <summary>
    /// Check if rate limit is exceeded and increment counter
    /// Returns True if limit is exceeded
    /// </summary>
    function CheckRateLimit(const AKey: string; const ALimit: Integer;
      const AWindowSeconds: Integer; out ARemaining: Integer;
      out AResetTime: TDateTime): Boolean;
    /// <summary>
    /// Reset rate limit for a specific key
    /// </summary>
    procedure ResetKey(const AKey: string);
    /// <summary>
    /// Clear all rate limit data
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  /// In-memory rate limit storage implementation
  /// </summary>
  TMVCInMemoryRateLimitStorage = class(TInterfacedObject, IMVCRateLimitStorage)
  private
    type
      TRateLimitEntry = record
        Count: Integer;
        WindowStart: TDateTime;
        ResetTime: TDateTime;
      end;
  private
    FStorage: TDictionary<string, TRateLimitEntry>;
    FLock: TCriticalSection;
    FStrategy: TMVCRateLimitStrategy;
    procedure CleanupExpiredEntries;
  public
    constructor Create(const AStrategy: TMVCRateLimitStrategy = rlsFixedWindow);
    destructor Destroy; override;
    function CheckRateLimit(const AKey: string; const ALimit: Integer;
      const AWindowSeconds: Integer; out ARemaining: Integer;
      out AResetTime: TDateTime): Boolean;
    procedure ResetKey(const AKey: string);
    procedure Clear;
  end;

  /// <summary>
  /// Rate limiting middleware with support for different strategies and storage backends
  /// </summary>
  TMVCRateLimitMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FStorage: IMVCRateLimitStorage;
    FLimit: Integer;
    FWindowSeconds: Integer;
    FKeyType: TMVCRateLimitKeyType;
    FKeyExtractor: TMVCRateLimitKeyExtractor;
    FHeaderPrefix: string;
    FOnRateLimitExceeded: TMVCRateLimitExceededProc;
    FExcludedPaths: TList<string>;
    FAPIKeyHeaderName: string;
    function ExtractKey(const AContext: TWebContext): string;
    function IsPathExcluded(const APath: string): Boolean;
    procedure SetRateLimitHeaders(const AContext: TWebContext;
      const ALimit, ARemaining: Integer; const AResetTime: TDateTime);
  public
    constructor Create(
      const ALimit: Integer;
      const AWindowSeconds: Integer;
      const AKeyType: TMVCRateLimitKeyType = rlkIPAddress;
      const AStorage: IMVCRateLimitStorage = nil
    );
    destructor Destroy; override;

    /// <summary>
    /// Set custom key extractor function (only for rlkCustom key type)
    /// </summary>
    procedure SetKeyExtractor(const AExtractor: TMVCRateLimitKeyExtractor);

    /// <summary>
    /// Set callback for rate limit exceeded event
    /// </summary>
    procedure SetOnRateLimitExceeded(const AProc: TMVCRateLimitExceededProc);

    /// <summary>
    /// Add path to exclusion list (e.g., '/health', '/metrics')
    /// </summary>
    procedure AddExcludedPath(const APath: string);

    /// <summary>
    /// Set custom API key header name (default: 'X-API-Key')
    /// </summary>
    procedure SetAPIKeyHeaderName(const AHeaderName: string);

    /// <summary>
    /// Set custom header prefix (default: 'X-RateLimit-')
    /// </summary>
    procedure SetHeaderPrefix(const APrefix: string);

    { IMVCMiddleware }
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  System.Math,
  System.StrUtils;

{ TMVCInMemoryRateLimitStorage }

constructor TMVCInMemoryRateLimitStorage.Create(const AStrategy: TMVCRateLimitStrategy);
begin
  inherited Create;
  FStorage := TDictionary<string, TRateLimitEntry>.Create;
  FLock := TCriticalSection.Create;
  FStrategy := AStrategy;
end;

destructor TMVCInMemoryRateLimitStorage.Destroy;
begin
  FStorage.Free;
  FLock.Free;
  inherited;
end;

procedure TMVCInMemoryRateLimitStorage.CleanupExpiredEntries;
var
  LKey: string;
  LEntry: TRateLimitEntry;
  LKeysToRemove: TArray<string>;
  LNow: TDateTime;
begin
  // This is called within a lock, so no need for additional synchronization
  LNow := Now;
  SetLength(LKeysToRemove, 0);

  for LKey in FStorage.Keys do
  begin
    if FStorage.TryGetValue(LKey, LEntry) then
    begin
      if LNow > LEntry.ResetTime then
      begin
        SetLength(LKeysToRemove, Length(LKeysToRemove) + 1);
        LKeysToRemove[High(LKeysToRemove)] := LKey;
      end;
    end;
  end;

  for LKey in LKeysToRemove do
    FStorage.Remove(LKey);
end;

function TMVCInMemoryRateLimitStorage.CheckRateLimit(const AKey: string;
  const ALimit: Integer; const AWindowSeconds: Integer; out ARemaining: Integer;
  out AResetTime: TDateTime): Boolean;
var
  LEntry: TRateLimitEntry;
  LNow: TDateTime;
  LNewEntry: TRateLimitEntry;
begin
  FLock.Acquire;
  try
    CleanupExpiredEntries;

    LNow := Now;

    if not FStorage.TryGetValue(AKey, LEntry) then
    begin
      // First request from this key
      LNewEntry.Count := 1;
      LNewEntry.WindowStart := LNow;
      LNewEntry.ResetTime := IncSecond(LNow, AWindowSeconds);
      FStorage.AddOrSetValue(AKey, LNewEntry);

      ARemaining := ALimit - 1;
      AResetTime := LNewEntry.ResetTime;
      Result := False;
      Exit;
    end;

    // Check if window has expired
    if LNow >= LEntry.ResetTime then
    begin
      // Start new window
      LNewEntry.Count := 1;
      LNewEntry.WindowStart := LNow;
      LNewEntry.ResetTime := IncSecond(LNow, AWindowSeconds);
      FStorage.AddOrSetValue(AKey, LNewEntry);

      ARemaining := ALimit - 1;
      AResetTime := LNewEntry.ResetTime;
      Result := False;
      Exit;
    end;

    // Within current window
    if LEntry.Count >= ALimit then
    begin
      // Rate limit exceeded
      ARemaining := 0;
      AResetTime := LEntry.ResetTime;
      Result := True;
      Exit;
    end;

    // Increment counter
    LEntry.Count := LEntry.Count + 1;
    FStorage.AddOrSetValue(AKey, LEntry);

    ARemaining := ALimit - LEntry.Count;
    AResetTime := LEntry.ResetTime;
    Result := False;
  finally
    FLock.Release;
  end;
end;

procedure TMVCInMemoryRateLimitStorage.ResetKey(const AKey: string);
begin
  FLock.Acquire;
  try
    FStorage.Remove(AKey);
  finally
    FLock.Release;
  end;
end;

procedure TMVCInMemoryRateLimitStorage.Clear;
begin
  FLock.Acquire;
  try
    FStorage.Clear;
  finally
    FLock.Release;
  end;
end;

{ TMVCRateLimitMiddleware }

constructor TMVCRateLimitMiddleware.Create(const ALimit: Integer;
  const AWindowSeconds: Integer; const AKeyType: TMVCRateLimitKeyType;
  const AStorage: IMVCRateLimitStorage);
begin
  inherited Create;
  FLimit := ALimit;
  FWindowSeconds := AWindowSeconds;
  FKeyType := AKeyType;
  FHeaderPrefix := 'X-RateLimit-';
  FAPIKeyHeaderName := 'X-API-Key';
  FExcludedPaths := TList<string>.Create;

  if Assigned(AStorage) then
    FStorage := AStorage
  else
    FStorage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);
end;

destructor TMVCRateLimitMiddleware.Destroy;
begin
  FExcludedPaths.Free;
  inherited;
end;

procedure TMVCRateLimitMiddleware.SetKeyExtractor(const AExtractor: TMVCRateLimitKeyExtractor);
begin
  FKeyExtractor := AExtractor;
end;

procedure TMVCRateLimitMiddleware.SetOnRateLimitExceeded(const AProc: TMVCRateLimitExceededProc);
begin
  FOnRateLimitExceeded := AProc;
end;

procedure TMVCRateLimitMiddleware.AddExcludedPath(const APath: string);
begin
  FExcludedPaths.Add(APath.ToLower);
end;

procedure TMVCRateLimitMiddleware.SetAPIKeyHeaderName(const AHeaderName: string);
begin
  FAPIKeyHeaderName := AHeaderName;
end;

procedure TMVCRateLimitMiddleware.SetHeaderPrefix(const APrefix: string);
begin
  FHeaderPrefix := APrefix;
end;

function TMVCRateLimitMiddleware.ExtractKey(const AContext: TWebContext): string;
begin
  case FKeyType of
    rlkIPAddress:
      Result := AContext.Request.ClientIP;

    rlkUserID:
      begin
        // Try to get user ID from session
        if AContext.LoggedUser.IsValid then
          Result := AContext.LoggedUser.UserName
        else
        begin
          // Fallback to IP if no user is logged in
          Result := AContext.Request.ClientIP;
        end;
      end;

    rlkAPIKey:
      begin
        Result := AContext.Request.Headers[FAPIKeyHeaderName];
        if Result.IsEmpty then
          Result := AContext.Request.ClientIP; // Fallback to IP
      end;

    rlkCustom:
      begin
        if Assigned(FKeyExtractor) then
          Result := FKeyExtractor(AContext)
        else
          Result := AContext.Request.ClientIP; // Fallback to IP
      end;
  else
    Result := AContext.Request.ClientIP;
  end;

  // Add key type prefix to avoid collisions
  Result := Format('%d:%s', [Ord(FKeyType), Result]);
end;

function TMVCRateLimitMiddleware.IsPathExcluded(const APath: string): Boolean;
var
  LExcludedPath: string;
  LNormalizedPath: string;
begin
  Result := False;
  LNormalizedPath := APath.ToLower;

  for LExcludedPath in FExcludedPaths do
  begin
    if LNormalizedPath.StartsWith(LExcludedPath) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TMVCRateLimitMiddleware.SetRateLimitHeaders(const AContext: TWebContext;
  const ALimit, ARemaining: Integer; const AResetTime: TDateTime);
var
  LResetUnixTime: Int64;
begin
  // Standard rate limit headers
  AContext.Response.SetCustomHeader(FHeaderPrefix + 'Limit', ALimit.ToString);
  AContext.Response.SetCustomHeader(FHeaderPrefix + 'Remaining', Max(0, ARemaining).ToString);

  // Reset time as Unix timestamp
  LResetUnixTime := DateTimeToUnix(AResetTime, False);
  AContext.Response.SetCustomHeader(FHeaderPrefix + 'Reset', LResetUnixTime.ToString);
end;

procedure TMVCRateLimitMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  LKey: string;
  LRemaining: Integer;
  LResetTime: TDateTime;
  LExceeded: Boolean;
begin
  // Check if path is excluded
  if IsPathExcluded(AContext.Request.PathInfo) then
  begin
    AHandled := False;
    Exit;
  end;

  // Extract rate limit key
  LKey := ExtractKey(AContext);

  // Check rate limit
  LExceeded := FStorage.CheckRateLimit(LKey, FLimit, FWindowSeconds, LRemaining, LResetTime);

  // Set rate limit headers
  SetRateLimitHeaders(AContext, FLimit, LRemaining, LResetTime);

  if LExceeded then
  begin
    // Call custom callback if set
    if Assigned(FOnRateLimitExceeded) then
      FOnRateLimitExceeded(AContext, LKey, FLimit, FWindowSeconds);

    // Return 429 Too Many Requests
    AContext.Response.StatusCode := HTTP_STATUS.TooManyRequests;
    AContext.Response.ReasonString := 'Too Many Requests';
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := Format(
      '{"error":"Rate limit exceeded","limit":%d,"window_seconds":%d,"retry_after":%d}',
      [FLimit, FWindowSeconds, SecondsBetween(LResetTime, Now)]
    );

    // Add Retry-After header (RFC 6585)
    AContext.Response.SetCustomHeader('Retry-After', SecondsBetween(LResetTime, Now).ToString);

    AHandled := True;
  end
  else
  begin
    AHandled := False;
  end;
end;

procedure TMVCRateLimitMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  // Nothing to do here
end;

procedure TMVCRateLimitMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; const AHandled: Boolean);
begin
  // Nothing to do here
end;

procedure TMVCRateLimitMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // Nothing to do here
end;

end.
