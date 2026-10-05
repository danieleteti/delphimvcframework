// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Router;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Rtti.Utils,
  IdURI, System.Classes;

type
  TMVCActionParamCacheItem = class
  private
    FValue: string;
    FParams: TList<TPair<String, String>>;
    FRegEx: TRegEx;
  public
    constructor Create(aValue: string; aParams: TList<TPair<String, String>>); virtual;
    destructor Destroy; override;
    function Value: string;
    function Params: TList<TPair<String, String>>; // this should be read-only...
    function Match(const Value: String): TMatch; inline;
  end;

  TMVCRouterResult = record
    MethodToCall: TRttiMethod;
    ControllerClazz: TMVCControllerClazz;
    ControllerCreateAction: TMVCControllerCreateAction;
    ControllerInjectableConstructor: TRttiMethod;
    ResponseContentMediaType: string;
    ResponseContentCharset: string;
    function GetQualifiedActionName: string;
  end;

  { [PERF] Pre-compiled route descriptor. One TMVCCompiledRoute per
    (controller, controller-url-segment, action-method, action-path)
    combination. Built once at AddController time by TMVCRouteTable;
    read-only from the request hot path.

    ActionAttributes is the same TArray<TCustomAttribute> that
    TRttiMethod.GetAttributes would return - cached here so
    IsHTTPAcceptCompatible / IsHTTPContentTypeCompatible do not
    re-allocate it per request. The per-method check is not needed
    at this stage because the route table is already indexed by
    HTTP method; any route reaching the match routine has already
    passed the method gate. }
  TMVCCompiledRoute = class
  public
    ControllerClazz: TMVCControllerClazz;
    CreateAction: TMVCControllerCreateAction;
    InjectableConstructor: TRttiMethod;
    ActionMethod: TRttiMethod;
    ActionAttributes: TArray<TCustomAttribute>;
    FullPath: string;               // APathPrefix + URLSegment + MVCPath.Path
    IsParametric: Boolean;          // contains "(" -> needs regex match
    ProducesMediaType: string;      // resolved at build time
    ProducesCharset: string;
  end;

  { [PERF] Route table indexed the way the request searches:
    first by HTTP method, then by path. Static paths hit a string
    dictionary in O(1); parametric paths fall into a short per-method
    list filtered via the already-cached gMVCGlobalActionParamsCache
    regexes. Routes that declare multiple HTTP verbs (e.g. GET+HEAD)
    are registered under each verb so no per-request verb filter runs.
    Built once per engine at first request, cached thereafter. }
  TMVCRouteTable = class
  private
    FRoutes: TObjectList<TMVCCompiledRoute>;     // owns all route instances
    FStaticByMethod: array[TMVCHTTPMethodType] of TDictionary<string, TList<TMVCCompiledRoute>>;
    FParametricByMethod: array[TMVCHTTPMethodType] of TList<TMVCCompiledRoute>;
    procedure AddRoute(ARoute: TMVCCompiledRoute; const AMethods: TMVCHTTPMethods);
    procedure BuildFrom(
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const APathPrefix, ADefaultContentType, ADefaultContentCharset: string);
  public
    constructor Create(
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const APathPrefix, ADefaultContentType, ADefaultContentCharset: string);
    destructor Destroy; override;
  end;

  TMVCRouter = class(TMVCCustomRouter)
  private
    class function GetAttribute<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>): T; static;

    class function GetFirstMediaType(const AContentType: string): string; static;

    class function IsHTTPContentTypeCompatible(
      const ARequestMethodType: TMVCHTTPMethodType;
      var AContentType: string;
      const AAttributes: TArray<TCustomAttribute>): Boolean; static;

    class function IsHTTPAcceptCompatible(
      const ARequestMethodType: TMVCHTTPMethodType;
      var AAccept: string;
      const AAttributes: TArray<TCustomAttribute>): Boolean; static;

    class function IsCompatiblePath(
      const AMVCPath: string;
      const APath: string;
      var aParams: TMVCRequestParamsTable): Boolean; static;

    class function GetParametersNames(
      const V: string): TList<TPair<string, string>>; static;
  protected
    class procedure FillControllerMappedPaths(
      const aControllerName: string;
      const aControllerAttributes: TArray<TCustomAttribute>;
      const aControllerMappedPaths: TStringList); static;
  public
    class function StringMethodToHTTPMetod(const aValue: string): TMVCHTTPMethodType; static;
    { The verbs an action answers: the union of its [MVCHTTPMethod] attributes,
      or every verb when it declares none. The Swagger and OpenAPI emitters call
      this too, so what is documented cannot drift from what is routed. }
    class function AllowedMethods(const AAttributes: TArray<TCustomAttribute>): TMVCHTTPMethods; static;
    { [PERF] Fast overload used by TMVCEngine. The engine owns ARouteTable
      across its lifetime and invalidates it when AddController is called,
      so the table is built once and reused. }
    class function ExecuteRouting(const ARequestPathInfo: string;
      const ARequestMethodType: TMVCHTTPMethodType;
      const ARequestContentType, ARequestAccept: string;
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const ADefaultContentType: string;
      const ADefaultContentCharset: string;
      const APathPrefix: string;
      var ARequestParams: TMVCRequestParamsTable;
      out ARouterResult: TMVCRouterResult;
      var ARouteTable: TMVCRouteTable): Boolean; overload; static;
    { Back-compat overload: builds and frees a throw-away route table
      per call. Used by unit tests and any external caller that does not
      own a TMVCRouteTable instance. }
    class function ExecuteRouting(const ARequestPathInfo: string;
      const ARequestMethodType: TMVCHTTPMethodType;
      const ARequestContentType, ARequestAccept: string;
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const ADefaultContentType: string;
      const ADefaultContentCharset: string;
      const APathPrefix: string;
      var ARequestParams: TMVCRequestParamsTable;
      out ARouterResult: TMVCRouterResult): Boolean; overload; static;
  end;

/// <summary>
///   The kind of a route parameter, "($name:kind)", the same for controller routes and Minimal API
///   routes: int, int64, float, bool, guid, date (yyyy-mm-dd), time (hh:nn[:ss]) and datetime (ISO 8601)
///   accept only a value of that shape; sqids decodes
///   the value into its integer. Returns False when the value does not fit, and the route does not
///   match (404 if no other route does). An unknown kind raises EMVCException.
/// </summary>
function MVCRouteParamOfKind(const AKind, AValue: string; out AResult: string): Boolean;

/// <summary>
///   True for '' and for the kinds MVCRouteParamOfKind knows (any case); lets a router reject
///   a misspelled kind when the route is registered instead of at every request.
/// </summary>
function MVCIsRouteParamKind(const AKind: string): Boolean;

/// <summary>
///   Raises EMVCException for a route path with a parameter without a name, an unknown kind or a
///   catch-all "($x:*)" that is not the last segment (or at all, when AAllowCatchAll is False).
///   Called when a route is registered - controllers (TMVCEngine.AddController) and Minimal API -
///   so the mistake stops the server at startup instead of turning requests into 500s.
/// </summary>
procedure MVCCheckRoutePath(const APath: string; const AAllowCatchAll: Boolean);

implementation

uses
  System.TypInfo,
  System.NetEncoding,
  System.DateUtils,
  MVCFramework.Container;

function MVCIsRouteParamKind(const AKind: string): Boolean;
begin
  // the same list as MVCRouteParamOfKind below
  Result := (AKind = '') or SameText(AKind, 'int') or SameText(AKind, 'int64') or SameText(AKind, 'float') or
    SameText(AKind, 'bool') or SameText(AKind, 'guid') or SameText(AKind, 'date') or SameText(AKind, 'time') or
    SameText(AKind, 'datetime') or SameText(AKind, 'sqids');
end;

procedure MVCCheckRoutePath(const APath: string; const AAllowCatchAll: Boolean);
var
  lMatch: TMatch;
  lInner, lName, lKind: string;
  lColon: Integer;
begin
  for lMatch in TRegEx.Matches(APath, '\(\$([^)]*)\)') do
  begin
    lInner := lMatch.Groups[1].Value;
    lColon := Pos(':', lInner);
    if lColon > 0 then
    begin
      lName := Copy(lInner, 1, lColon - 1);
      lKind := Copy(lInner, lColon + 1, MaxInt);
    end
    else
    begin
      lName := lInner;
      lKind := '';
    end;
    if lName = '' then
      raise EMVCException.CreateFmt('Route "%s": a parameter without a name, %s', [APath, lMatch.Value]);
    if lKind = '*' then
    begin
      if not AAllowCatchAll then
        raise EMVCException.CreateFmt('Route "%s": the catch-all %s is available in Minimal API routes only',
          [APath, lMatch.Value]);
      if not APath.TrimRight(['/']).EndsWith(lMatch.Value) then
        raise EMVCException.CreateFmt('Route "%s": the catch-all %s must be the last segment', [APath, lMatch.Value]);
    end
    else if not MVCIsRouteParamKind(lKind) then
      raise EMVCException.CreateFmt('Route "%s": unknown route parameter kind [%s]', [APath, lKind]);
  end;
end;

function MVCRouteParamOfKind(const AKind, AValue: string; out AResult: string): Boolean;

  function IsDecimalInteger(const S: string): Boolean;
  var
    I, lStart: Integer;
  begin
    // "-" and digits only: TryStrToInt also reads "$10", "0x10" and " 10", other URLs for the same number
    lStart := 1;
    if (S <> '') and (S[1] = '-') then
      lStart := 2;
    Result := Length(S) >= lStart;
    for I := lStart to Length(S) do
      if not CharInSet(S[I], ['0'..'9']) then
        Exit(False);
  end;

var
  lInt: Integer;
  lInt64: Int64;
  lFloat: Double;
  lDate: TDateTime;
begin
  AResult := AValue;
  if AKind = '' then
    Exit(True);
  if SameText(AKind, 'int') then
    Exit(IsDecimalInteger(AValue) and TryStrToInt(AValue, lInt));
  if SameText(AKind, 'int64') then
    Exit(IsDecimalInteger(AValue) and TryStrToInt64(AValue, lInt64));
  if SameText(AKind, 'float') then
    Exit(TryStrToFloat(AValue, lFloat, TFormatSettings.Invariant));
  if SameText(AKind, 'bool') then
    Exit(SameText(AValue, 'true') or SameText(AValue, 'false') or (AValue = '0') or (AValue = '1'));
  if SameText(AKind, 'guid') then
  begin
    try
      StringToGUID('{' + AValue.Replace('{', '').Replace('}', '') + '}');
      Exit(True);
    except
      Exit(False);
    end;
  end;
  if SameText(AKind, 'date') then
    // yyyy-mm-dd: a path segment cannot hold the slashes of a locale date
    Exit((AValue.Length = 10) and TryISO8601ToDate(AValue, lDate, True));
  if SameText(AKind, 'time') then
    // hh:nn or hh:nn:ss, 24 hours
    Exit(TRegEx.IsMatch(AValue, '^([01][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$'));
  if SameText(AKind, 'datetime') then
    // yyyy-mm-ddThh:nn:ss, optional fraction and Z or +hh:nn (send "+" as %2B)
    Exit((AValue.Length >= 19) and (AValue.Chars[10] = 'T') and TryISO8601ToDate(AValue, lDate, True));
  if SameText(AKind, 'sqids') then
  begin
    try
      AResult := TMVCSqids.SqidToInt(AValue).ToString;
      Exit(True);
    except
      Exit(False);
    end;
  end;
  raise EMVCException.CreateFmt('Unknown route parameter kind [%s]', [AKind]);
end;

var
  gMVCGlobalActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem> = nil;
  gRttiCtx: TRttiContext;

{ TMVCCompiledRoute / TMVCRouteTable - forward utilities }

function IsParametricPath(const APath: string): Boolean; inline;
begin
  { Any MVCPath that needs regex interpretation at request time goes in
    the parametric bucket. Parameter markers are written "($name)" and
    literal regex metacharacters are typically escaped with a backslash
    (e.g. '/patient/\$match' for a literal '$'), so checking for either
    marker catches both cases. Pure literal paths hit the O(1) static
    dictionary. }
  Result := (Pos('($', APath) > 0) or (Pos('\', APath) > 0);
end;

class function TMVCRouter.AllowedMethods(const AAttributes: TArray<TCustomAttribute>): TMVCHTTPMethods;
var
  I: Integer;
  LFound: Boolean;
begin
  LFound := False;
  Result := [];
  for I := 0 to High(AAttributes) do
    if AAttributes[I] is MVCHTTPMethodAttribute then
    begin
      Result := Result + MVCHTTPMethodAttribute(AAttributes[I]).MVCHTTPMethods;
      LFound := True;
    end;
  if not LFound then
    Result := [httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpHEAD, httpOPTIONS, httpTRACE,
      httpQUERY];
end;

{ TMVCRouteTable }

constructor TMVCRouteTable.Create(
  const AControllers: TObjectList<TMVCControllerDelegate>;
  const APathPrefix, ADefaultContentType, ADefaultContentCharset: string);
var
  M: TMVCHTTPMethodType;
begin
  inherited Create;
  FRoutes := TObjectList<TMVCCompiledRoute>.Create(True);
  for M := Low(TMVCHTTPMethodType) to High(TMVCHTTPMethodType) do
  begin
    FStaticByMethod[M] := TDictionary<string, TList<TMVCCompiledRoute>>.Create;
    FParametricByMethod[M] := TList<TMVCCompiledRoute>.Create;
  end;
  BuildFrom(AControllers, APathPrefix, ADefaultContentType, ADefaultContentCharset);
end;

destructor TMVCRouteTable.Destroy;
var
  M: TMVCHTTPMethodType;
  LBucket: TList<TMVCCompiledRoute>;
begin
  for M := Low(TMVCHTTPMethodType) to High(TMVCHTTPMethodType) do
  begin
    if Assigned(FStaticByMethod[M]) then
    begin
      for LBucket in FStaticByMethod[M].Values do
        LBucket.Free;
      FStaticByMethod[M].Free;
    end;
    FParametricByMethod[M].Free;
  end;
  FRoutes.Free;
  inherited;
end;

procedure TMVCRouteTable.AddRoute(ARoute: TMVCCompiledRoute;
  const AMethods: TMVCHTTPMethods);
var
  M: TMVCHTTPMethodType;
  LKey: string;
  LBucket: TList<TMVCCompiledRoute>;
begin
  FRoutes.Add(ARoute);
  LKey := LowerCase(ARoute.FullPath);
  for M := Low(TMVCHTTPMethodType) to High(TMVCHTTPMethodType) do
  begin
    if not (M in AMethods) then
      Continue;
    if ARoute.IsParametric then
    begin
      FParametricByMethod[M].Add(ARoute);
    end
    else
    begin
      if not FStaticByMethod[M].TryGetValue(LKey, LBucket) then
      begin
        LBucket := TList<TMVCCompiledRoute>.Create;
        FStaticByMethod[M].Add(LKey, LBucket);
      end;
      LBucket.Add(ARoute);
    end;
  end;
end;

procedure TMVCRouteTable.BuildFrom(
  const AControllers: TObjectList<TMVCControllerDelegate>;
  const APathPrefix, ADefaultContentType, ADefaultContentCharset: string);
var
  LControllerDelegate: TMVCControllerDelegate;
  LRttiType: TRttiType;
  LClassAttributes: TArray<TCustomAttribute>;
  LMappedPaths: TStringList;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LMethodAttrs: TArray<TCustomAttribute>;
  LAtt: TCustomAttribute;
  LURLSegment: string;
  LControllerMappedPath: string;
  LMethodPath: string;
  LProduces: MVCProducesAttribute;
  LRoute: TMVCCompiledRoute;
  LItem: string;
  LFullPath: string;
begin
  LMappedPaths := TStringList.Create;
  try
    for LControllerDelegate in AControllers do
    begin
      LMappedPaths.Clear;
      LRttiType := gRttiCtx.GetType(LControllerDelegate.Clazz.ClassInfo);
      if not Assigned(LRttiType) then
        Continue;

      LURLSegment := LControllerDelegate.URLSegment;
      if LURLSegment.IsEmpty then
      begin
        LClassAttributes := LRttiType.GetAttributes;
        if Length(LClassAttributes) = 0 then
          Continue;
        TMVCRouter.FillControllerMappedPaths(LRttiType.Name, LClassAttributes, LMappedPaths);
      end
      else
      begin
        LMappedPaths.Add(LURLSegment);
      end;

      LMethods := LRttiType.GetMethods;
      for LMethod in LMethods do
      begin
        if LMethod.Visibility <> mvPublic then
          Continue;
        if not (LMethod.MethodKind in [mkProcedure, mkFunction]) then
          Continue;
        LMethodAttrs := LMethod.GetAttributes;
        if Length(LMethodAttrs) = 0 then
          Continue;

        for LAtt in LMethodAttrs do
        begin
          if LAtt is MVCPathAttribute then
          begin
            LMethodPath := MVCPathAttribute(LAtt).Path;
            for LItem in LMappedPaths do
            begin
              LControllerMappedPath := LItem;
              if LControllerMappedPath = '/' then
                LControllerMappedPath := '';
              LFullPath := APathPrefix + LControllerMappedPath + LMethodPath;

              LRoute := TMVCCompiledRoute.Create;
              LRoute.ControllerClazz := LControllerDelegate.Clazz;
              LRoute.CreateAction := LControllerDelegate.CreateAction;
              LRoute.ActionMethod := LMethod;
              LRoute.ActionAttributes := LMethodAttrs;
              { Normalise an empty full path to "/" so a request to "/"
                matches the dictionary key directly. IsCompatiblePath has
                a special case for ('/', '') that we sidestep by making
                the registered key match the request. }
              if LFullPath = '' then
                LFullPath := '/';
              LRoute.FullPath := LFullPath;
              LRoute.IsParametric := IsParametricPath(LFullPath);

              { Leave ProducesMediaType empty when the action has no
                MVCProduces attribute; TryMatchRoute substitutes the
                per-call defaults. Keeping defaults out of the cached
                route lets the same table serve engines configured with
                different DefaultContentType / DefaultContentCharset. }
              LProduces := TMVCRouter.GetAttribute<MVCProducesAttribute>(LMethodAttrs);
              if Assigned(LProduces) then
              begin
                LRoute.ProducesMediaType := LProduces.Value;
                LRoute.ProducesCharset := LProduces.Charset;
              end;

              if not Assigned(LRoute.CreateAction) then
                LRoute.InjectableConstructor :=
                  TRttiUtils.GetConstructorWithAttribute<MVCInjectAttribute>(LRttiType);

              AddRoute(LRoute, TMVCRouter.AllowedMethods(LMethodAttrs));
            end;
          end;
        end;
      end;
    end;
  finally
    LMappedPaths.Free;
  end;
end;

{ TMVCRouter }

function TryMatchRoute(
  const ARoute: TMVCCompiledRoute;
  const ARequestMethodType: TMVCHTTPMethodType;
  var ARequestContentType, ARequestAccept: string;
  const ARequestPathInfo: string;
  const ACheckPath: Boolean;
  const ADefaultContentType, ADefaultContentCharset: string;
  var ARequestParams: TMVCRequestParamsTable;
  out ARouterResult: TMVCRouterResult): Boolean;
begin
  { Method was already matched by the route-table index; remaining checks
    are content-type, accept, and (for parametric routes only) path regex. }
  Result := False;
  if not TMVCRouter.IsHTTPContentTypeCompatible(ARequestMethodType, ARequestContentType, ARoute.ActionAttributes) then
    Exit;
  if not TMVCRouter.IsHTTPAcceptCompatible(ARequestMethodType, ARequestAccept, ARoute.ActionAttributes) then
    Exit;
  if ACheckPath and
     not TMVCRouter.IsCompatiblePath(ARoute.FullPath, ARequestPathInfo, ARequestParams) then
    Exit;

  ARouterResult.MethodToCall := ARoute.ActionMethod;
  ARouterResult.ControllerClazz := ARoute.ControllerClazz;
  ARouterResult.ControllerCreateAction := ARoute.CreateAction;
  ARouterResult.ControllerInjectableConstructor := ARoute.InjectableConstructor;
  if ARoute.ProducesMediaType <> '' then
  begin
    ARouterResult.ResponseContentMediaType := ARoute.ProducesMediaType;
    ARouterResult.ResponseContentCharset := ARoute.ProducesCharset;
  end
  else
  begin
    ARouterResult.ResponseContentMediaType := ADefaultContentType;
    ARouterResult.ResponseContentCharset := ADefaultContentCharset;
  end;
  Result := True;
end;

class function TMVCRouter.ExecuteRouting(const ARequestPathInfo: string;
  const ARequestMethodType: TMVCHTTPMethodType;
  const ARequestContentType, ARequestAccept: string;
  const AControllers: TObjectList<TMVCControllerDelegate>;
  const ADefaultContentType: string;
  const ADefaultContentCharset: string;
  const APathPrefix: string;
  var ARequestParams: TMVCRequestParamsTable;
  out ARouterResult: TMVCRouterResult;
  var ARouteTable: TMVCRouteTable): Boolean;
var
  LRequestPathInfo: string;
  LRequestAccept: string;
  LRequestContentType: string;
  LBucket: TList<TMVCCompiledRoute>;
  I: Integer;
begin
  Result := False;

  LRequestAccept := ARequestAccept;
  LRequestContentType := ARequestContentType;
  LRequestPathInfo := ARequestPathInfo;
  if (Trim(LRequestPathInfo) = EmptyStr) then
    LRequestPathInfo := '/'
  else if not LRequestPathInfo.StartsWith('/') then
    LRequestPathInfo := '/' + LRequestPathInfo;
  { A path still carrying a dot-segment is refused rather than resolved.

    Resolving would be the wrong direction: HTTP.sys hands us a path the kernel
    has already collapsed, so /public/../admin arrives as /admin and executes,
    while Indy and WebBroker pass the literal string through and simply do not
    match. Normalising here would make all three behave like the permissive one
    and hand the same bypass to every host - a reverse proxy that denies /admin
    never saw /admin. Refusing keeps Indy and WebBroker exactly as they are and
    can only tighten HTTP.sys, where anything that survived kernel
    canonicalisation had to have been encoded on purpose. }
  if MVCPathHasDotSegment(LRequestPathInfo) then
    Exit(False);

  LRequestPathInfo := TIdURI.PathEncode(Trim(LRequestPathInfo)); //regression introduced in fix for issue 492

  { Build the table on first call; engine owns subsequent reuse. }
  if ARouteTable = nil then
    ARouteTable := TMVCRouteTable.Create(AControllers, APathPrefix,
      ADefaultContentType, ADefaultContentCharset);

  // 1. Static: dictionary keyed by the request's method + path.
  if ARouteTable.FStaticByMethod[ARequestMethodType].TryGetValue(LowerCase(LRequestPathInfo), LBucket) then
  begin
    for I := 0 to LBucket.Count - 1 do
      if TryMatchRoute(LBucket[I], ARequestMethodType,
                        LRequestContentType, LRequestAccept,
                        LRequestPathInfo, False,
                        ADefaultContentType, ADefaultContentCharset,
                        ARequestParams, ARouterResult) then
        Exit(True);
  end;

  // 2. Parametric fallback: iterate the candidates for this verb only.
  LBucket := ARouteTable.FParametricByMethod[ARequestMethodType];
  for I := 0 to LBucket.Count - 1 do
    if TryMatchRoute(LBucket[I], ARequestMethodType,
                      LRequestContentType, LRequestAccept,
                      LRequestPathInfo, True,
                      ADefaultContentType, ADefaultContentCharset,
                      ARequestParams, ARouterResult) then
      Exit(True);
end;

class function TMVCRouter.ExecuteRouting(const ARequestPathInfo: string;
  const ARequestMethodType: TMVCHTTPMethodType;
  const ARequestContentType, ARequestAccept: string;
  const AControllers: TObjectList<TMVCControllerDelegate>;
  const ADefaultContentType: string;
  const ADefaultContentCharset: string;
  const APathPrefix: string;
  var ARequestParams: TMVCRequestParamsTable;
  out ARouterResult: TMVCRouterResult): Boolean;
var
  LTable: TMVCRouteTable;
begin
  LTable := nil;
  try
    Result := ExecuteRouting(ARequestPathInfo, ARequestMethodType,
      ARequestContentType, ARequestAccept, AControllers,
      ADefaultContentType, ADefaultContentCharset, APathPrefix,
      ARequestParams, ARouterResult, LTable);
  finally
    LTable.Free;
  end;
end;

class function TMVCRouter.GetAttribute<T>(const AAttributes: TArray<TCustomAttribute>): T;
var
  Att: TCustomAttribute;
begin
  Result := nil;
  for Att in AAttributes do
    if Att is T then
      Exit(T(Att));
end;

class procedure TMVCRouter.FillControllerMappedPaths(
      const aControllerName: string;
      const aControllerAttributes: TArray<TCustomAttribute>;
      const aControllerMappedPaths: TStringList);
var
  LFound: Boolean;
  LAtt: TCustomAttribute;
begin
  LFound := False;
  for LAtt in aControllerAttributes do
  begin
    if LAtt is MVCPathAttribute then
    begin
      LFound := True;
      aControllerMappedPaths.Add(MVCPathAttribute(LAtt).Path);
    end;
  end;
  if not LFound then
  begin
    raise EMVCException.CreateFmt('Controller %s does not have MVCPath attribute', [aControllerName]);
  end;
end;

class function TMVCRouter.GetFirstMediaType(const AContentType: string): string;
begin
  Result := AContentType;
  while Pos(',', Result) > 0 do
    Result := Copy(Result, 1, Pos(',', Result) - 1);
  while Pos(';', Result) > 0 do
    Result := Copy(Result, 1, Pos(';', Result) - 1);
end;

class function TMVCRouter.IsCompatiblePath(
  const AMVCPath: string;
  const APath: string;
  var aParams: TMVCRequestParamsTable): Boolean;

  function ToPattern(const V: string; const Names: TList<TPair<String, String>>): string;
  var
    S: TPair<String, String>;
  begin
    Result := V;
    if Names.Count > 0 then
    begin
      for S in Names do
      begin
        Result := StringReplace(
          Result,
          '($' + S.Key + S.Value + ')', '([' + TMVCConstants.URL_MAPPED_PARAMS_ALLOWED_CHARS + ']*)',
          [rfReplaceAll]);
      end;
    end;
  end;

var
  lMatch: TMatch;
  lPattern: string;
  I, J: Integer;
  lNames: TList<TPair<String, String>>;
  lCacheItem: TMVCActionParamCacheItem;
  P: TPair<string, string>;
  lKindValue: string;
  lParValue: String;
begin
  if (APath = AMVCPath) or ((APath = '/') and (AMVCPath = '')) then
  begin
    Exit(True);
  end;

  if not gMVCGlobalActionParamsCache.TryGetValue(AMVCPath, lCacheItem) then
  begin
    TMonitor.Enter(gMVCGlobalActionParamsCache);
    try
      if not gMVCGlobalActionParamsCache.TryGetValue(AMVCPath, lCacheItem) then
      begin
        lNames := GetParametersNames(AMVCPath);
        lPattern := ToPattern(AMVCPath, lNames);
        lCacheItem := TMVCActionParamCacheItem.Create('^' + lPattern + '$', lNames);
        gMVCGlobalActionParamsCache.Add(AMVCPath, lCacheItem);
      end;
    finally
      TMonitor.Exit(gMVCGlobalActionParamsCache);
    end;
  end;

  lMatch := lCacheItem.Match(APath);
  Result := lMatch.Success;
  if Result then
  begin
    for I := 1 to Pred(lMatch.Groups.Count) do
    begin
      P := lCacheItem.Params[I - 1];

      {
        P.Key = Parameter name
        P.Value = ":kind" of the parameter (eg. :int, :sqids) or empty
      }

      lParValue := TIdURI.URLDecode(lMatch.Groups[I].Value);
      { The decode is the point where %2F becomes a separator and %2E%2E a dot
        segment: the regex matched one segment, the action gets three. The check
        upstream in ExecuteRouting cannot see this - it runs on the still-encoded
        path. A parameter that merely contains a slash is left alone, because
        carrying an encoded one is a documented use; a dot segment is not.
        A value that does not fit the kind of the parameter is not a match either. }
      if MVCPathHasDotSegment(lParValue) or
        not MVCRouteParamOfKind(Copy(P.Value, 2, MaxInt), lParValue, lKindValue) then
      begin
        { Undo what this call already put in the table. aParams is shared by every
          candidate route and is a TDictionary: leaving the parameters of the
          groups matched before this one behind makes the NEXT candidate with the
          same parameter names raise EListError on Add - a 500 where the request
          should simply not match. This is the only place the invariant "match
          fully or add nothing" has to be restored by hand. }
        for J := 1 to I - 1 do
          aParams.Remove(lCacheItem.Params[J - 1].Key);
        Exit(False);
      end;
      aParams.Add(P.Key, lKindValue);
    end;
  end;
end;

class function TMVCRouter.GetParametersNames(const V: string): TList<TPair<string, string>>;
var
  S: string;
  Matches: TMatchCollection;
  M: TMatch;
  I: Integer;
  lList: TList<TPair<string, string>>;
  lNameFound: Boolean;
  lKind: string;
  lName: string;
begin
  lList := TList<TPair<string, string>>.Create;
  try
    S := '\(\$([A-Za-z0-9\_]+)(\:[a-z][a-z0-9]*)?\)';
    Matches := TRegEx.Matches(V, S, [roIgnoreCase, roCompiled, roSingleLine]);
    for M in Matches do
    begin
      lNameFound := False;
      lKind := '';
      for I := 0 to M.Groups.Count - 1 do
      begin
        S := M.Groups[I].Value;
        if Length(S) > 0 then
        begin
          if (not lNameFound) and (S.Chars[0] <> '(') and (S.Chars[0] <> ':') then
          begin
            lName := S;
            lNameFound := True;
            Continue;
          end;
          if lNameFound and (S.Chars[0] = ':') then
          begin
            lKind := S;
          end;
        end;
      end;
      if lNameFound then
      begin
        lList.Add(TPair<string,string>.Create(lName,lKind));
      end;
    end;
    Result := lList;
  except
    lList.Free;
    raise;
  end;
end;

class function TMVCRouter.IsHTTPAcceptCompatible(
  const ARequestMethodType: TMVCHTTPMethodType;
  var AAccept: string;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MethodAccept: string;
  FoundOneAttProduces: Boolean;
begin
  Result := False;
  if AAccept.IsEmpty or AAccept.Contains('*/*') then // 2020-08-08, 2025-04-17
  begin
    Exit(True);
  end;

  FoundOneAttProduces := False;
  for I := 0 to high(AAttributes) do
    if AAttributes[I] is MVCProducesAttribute then
    begin
      FoundOneAttProduces := True;
      MethodAccept := MVCProducesAttribute(AAttributes[I]).Value;
      AAccept := GetFirstMediaType(AAccept);
      Result := SameText(AAccept, MethodAccept, loInvariantLocale);
      if Result then
        Break;
    end;

  Result := (not FoundOneAttProduces) or (FoundOneAttProduces and Result);
end;

class function TMVCRouter.IsHTTPContentTypeCompatible(
  const ARequestMethodType: TMVCHTTPMethodType;
  var AContentType: string;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MethodContentType: string;
  FoundOneAttConsumes: Boolean;
begin
  if ARequestMethodType in MVC_HTTP_METHODS_WITHOUT_CONTENT then
    Exit(True);

  Result := False;

  FoundOneAttConsumes := False;
  for I := 0 to high(AAttributes) do
    if AAttributes[I] is MVCConsumesAttribute then
    begin
      FoundOneAttConsumes := True;
      MethodContentType := MVCConsumesAttribute(AAttributes[I]).Value;
      AContentType := GetFirstMediaType(AContentType);
      Result := SameText(AContentType, MethodContentType, loInvariantLocale);
      if Result then
        Break;
    end;

  Result := (not FoundOneAttConsumes) or (FoundOneAttConsumes and Result);
end;

class function TMVCRouter.StringMethodToHTTPMetod(const aValue: string): TMVCHTTPMethodType;
begin
  if aValue = 'GET' then
    Exit(httpGET);
  if aValue = 'POST' then
    Exit(httpPOST);
  if aValue = 'DELETE' then
    Exit(httpDELETE);
  if aValue = 'PUT' then
    Exit(httpPUT);
  if aValue = 'HEAD' then
    Exit(httpHEAD);
  if aValue = 'OPTIONS' then
    Exit(httpOPTIONS);
  if aValue = 'PATCH' then
    Exit(httpPATCH);
  if aValue = 'TRACE' then
    Exit(httpTRACE);
  if aValue = 'QUERY' then
    Exit(httpQUERY);
  raise EMVCException.CreateFmt('Unknown HTTP method [%s]', [aValue]);
end;

{ TMVCActionParamCacheItem }

constructor TMVCActionParamCacheItem.Create(aValue: string;
  aParams: TList<TPair<String, String>>);
begin
  inherited Create;
  fValue := aValue;
  fParams := aParams;
  fRegEx := TRegEx.Create(FValue, [roIgnoreCase, roCompiled, roSingleLine]);
end;

destructor TMVCActionParamCacheItem.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TMVCActionParamCacheItem.Match(const Value: String): TMatch;
begin
  TMonitor.Enter(Self);
  try
    // See https://stackoverflow.com/questions/53016707/is-system-regularexpressions-tregex-thread-safe
    Result := fRegEx.Match(Value);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TMVCActionParamCacheItem.Params: TList<TPair<String, String>>;
begin
  Result := FParams;
end;

function TMVCActionParamCacheItem.Value: string;
begin
  Result := FValue;
end;


{ TMVCRouterResult }

function TMVCRouterResult.GetQualifiedActionName: string;
begin
  Result := Self.ControllerClazz.QualifiedClassName + '.' + Self.MethodToCall.Name;
end;

initialization

gMVCGlobalActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
gRttiCtx := TRttiContext.Create;

finalization

FreeAndNil(gMVCGlobalActionParamsCache);
gRTTICtx.Free;

end.
