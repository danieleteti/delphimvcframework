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

{$WARN SYMBOL_DEPRECATED OFF} // the class is deprecated for its users, not inside its own unit

interface

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Filters;

type
  // The types moved to MVCFramework.Filters: the aliases keep the code that uses this unit compiling
  TMVCRateLimitStrategy = MVCFramework.Filters.TMVCRateLimitStrategy;
  TMVCRateLimitKeyType = MVCFramework.Filters.TMVCRateLimitKeyType;
  TMVCRateLimitKeyExtractor = MVCFramework.Filters.TMVCRateLimitKeyExtractor;
  TMVCRateLimitExceededProc = MVCFramework.Filters.TMVCRateLimitExceededProc;
  IMVCRateLimitStorage = MVCFramework.Filters.IMVCRateLimitStorage;
  TMVCInMemoryRateLimitStorage = MVCFramework.Filters.TMVCInMemoryRateLimitStorage;

const
  rlsFixedWindow = MVCFramework.Filters.rlsFixedWindow;
  rlsSlidingWindow = MVCFramework.Filters.rlsSlidingWindow;
  rlsTokenBucket = MVCFramework.Filters.rlsTokenBucket;

  rlkIPAddress = MVCFramework.Filters.rlkIPAddress;
  rlkUserID = MVCFramework.Filters.rlkUserID;
  rlkAPIKey = MVCFramework.Filters.rlkAPIKey;
  rlkCustom = MVCFramework.Filters.rlkCustom;

type
  /// <summary>
  /// Rate limiting middleware with support for different strategies and storage backends
  /// </summary>
  TMVCRateLimitMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    // what MVCRateLimitExceeded checks, as the setters leave it (an empty prefix stays empty)
    fOptions: TMVCRateLimitOptions;
  public
    constructor Create(
      const ALimit: Integer;
      const AWindowSeconds: Integer;
      const AKeyType: TMVCRateLimitKeyType = rlkIPAddress;
      const AStorage: IMVCRateLimitStorage = nil
    );

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
  end deprecated 'Use the RateLimit HTTP filter (MVCFramework.Filters): AEngine.Use(RateLimitHTTPFilter(...))';

implementation

uses
  MVCFramework.Filters.Internal; // MVCRateLimitExceeded

{ TMVCRateLimitMiddleware }

constructor TMVCRateLimitMiddleware.Create(const ALimit: Integer;
  const AWindowSeconds: Integer; const AKeyType: TMVCRateLimitKeyType;
  const AStorage: IMVCRateLimitStorage);
begin
  inherited Create;
  fOptions.MaxRequests := ALimit;
  fOptions.WindowSeconds := AWindowSeconds;
  fOptions.KeyType := AKeyType;
  fOptions.HeaderPrefix := 'X-RateLimit-';
  fOptions.APIKeyHeaderName := 'X-API-Key';

  if Assigned(AStorage) then
    fOptions.Storage := AStorage
  else
    fOptions.Storage := TMVCInMemoryRateLimitStorage.Create(rlsFixedWindow);
end;

procedure TMVCRateLimitMiddleware.SetKeyExtractor(const AExtractor: TMVCRateLimitKeyExtractor);
begin
  fOptions.KeyExtractor := AExtractor;
end;

procedure TMVCRateLimitMiddleware.SetOnRateLimitExceeded(const AProc: TMVCRateLimitExceededProc);
begin
  fOptions.OnExceeded := AProc;
end;

procedure TMVCRateLimitMiddleware.AddExcludedPath(const APath: string);
begin
  SetLength(fOptions.ExcludedPaths, Length(fOptions.ExcludedPaths) + 1);
  fOptions.ExcludedPaths[High(fOptions.ExcludedPaths)] := APath.ToLower;
end;

procedure TMVCRateLimitMiddleware.SetAPIKeyHeaderName(const AHeaderName: string);
begin
  fOptions.APIKeyHeaderName := AHeaderName;
end;

procedure TMVCRateLimitMiddleware.SetHeaderPrefix(const APrefix: string);
begin
  fOptions.HeaderPrefix := APrefix;
end;

procedure TMVCRateLimitMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AHandled := MVCRateLimitExceeded(AContext, fOptions); // True: the 429 is written
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
