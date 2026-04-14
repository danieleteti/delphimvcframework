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
    IsHTTPMethodCompatible / IsHTTPAcceptCompatible / IsHTTPContentTypeCompatible
    do not re-allocate it per request. }
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

    class function IsHTTPMethodCompatible(
      const AMethodType: TMVCHTTPMethodType;
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

implementation

uses
  System.TypInfo,
  System.NetEncoding,
  MVCFramework.Container;

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

function AllowedMethods(const AAttributes: TArray<TCustomAttribute>): TMVCHTTPMethods;
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
    Result := [httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpHEAD, httpOPTIONS, httpTRACE];
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

              AddRoute(LRoute, AllowedMethods(LMethodAttrs));
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
  I: Integer;
  lNames: TList<TPair<String, String>>;
  lCacheItem: TMVCActionParamCacheItem;
  P: TPair<string, string>;
  lConv: string;
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
        P.Value = Converter applied to the value before to be injected (eg. :sqid)
      }

      lParValue := TIdURI.URLDecode(lMatch.Groups[I].Value);
      if P.Value.IsEmpty then
      begin
        {no converter}
        aParams.Add(P.Key, lParValue);
      end
      else
      begin
        lConv := P.Value;
        if SameText(lConv, ':sqids') then
        begin
          {sqids converter (so far the only one)}
          aParams.Add(P.Key, TMVCSqids.SqidToInt(lParValue).ToString);
        end
        else
        begin
          raise EMVCException.CreateFmt('Unknown converter [%s]', [lConv]);
        end;
      end;
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
  lConverter: string;
  lName: string;
begin
  lList := TList<TPair<string, string>>.Create;
  try
    S := '\(\$([A-Za-z0-9\_]+)(\:[a-z]+)?\)';
    Matches := TRegEx.Matches(V, S, [roIgnoreCase, roCompiled, roSingleLine]);
    for M in Matches do
    begin
      lNameFound := False;
      lConverter := '';
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
            lConverter := S;
          end;
        end;
      end;
      if lNameFound then
      begin
        lList.Add(TPair<string,string>.Create(lName,lConverter));
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

class function TMVCRouter.IsHTTPMethodCompatible(
  const AMethodType: TMVCHTTPMethodType;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MustBeCompatible: Boolean;
  CompatibleMethods: TMVCHTTPMethods;
begin
  Result := False;

  MustBeCompatible := False;
  for I := 0 to high(AAttributes) do
    if AAttributes[I] is MVCHTTPMethodAttribute then
    begin
      MustBeCompatible := True;
      CompatibleMethods := MVCHTTPMethodAttribute(AAttributes[I]).MVCHTTPMethods;
      Result := (AMethodType in CompatibleMethods);
    end;

  Result := (not MustBeCompatible) or (MustBeCompatible and Result);
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
