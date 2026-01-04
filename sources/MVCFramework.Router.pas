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
    class function ExecuteRouting(const ARequestPathInfo: string;
      const ARequestMethodType: TMVCHTTPMethodType;
      const ARequestContentType, ARequestAccept: string;
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const ADefaultContentType: string;
      const ADefaultContentCharset: string;
      const APathPrefix: string;
      var ARequestParams: TMVCRequestParamsTable;
      out ARouterResult: TMVCRouterResult): Boolean; static;
  end;

implementation

uses
  System.TypInfo,
  System.NetEncoding,
  MVCFramework.Container;

var
  gMVCGlobalActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem> = nil;
  gRttiCtx: TRttiContext;

{ TMVCRouter }

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
  LRequestPathInfo: string;
  LRequestAccept: string;
  LRequestContentType: string;
  LControllerMappedPath: string;
  LControllerMappedPaths: TStringList;
  LControllerDelegate: TMVCControllerDelegate;
  LAttributes: TArray<TCustomAttribute>;
  LAtt: TCustomAttribute;
  LRttiType: TRttiType;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LMethodPath: string;
  LProduceAttribute: MVCProducesAttribute;
  lURLSegment: string;
  LItem: String;
begin
  Result := False;

  LRequestAccept := ARequestAccept;
  LRequestContentType := ARequestContentType;
  LRequestPathInfo := ARequestPathInfo;
  if (Trim(LRequestPathInfo) = EmptyStr) then
    LRequestPathInfo := '/'
  else
  begin
    if not LRequestPathInfo.StartsWith('/') then
    begin
      LRequestPathInfo := '/' + LRequestPathInfo;
    end;
  end;
  LRequestPathInfo := TIdURI.PathEncode(Trim(LRequestPathInfo)); //regression introduced in fix for issue 492

  LControllerMappedPaths := TStringList.Create;
  try
    for LControllerDelegate in AControllers do
    begin
      LControllerMappedPaths.Clear;
      SetLength(LAttributes, 0);
      LRttiType := gRttiCtx.GetType(LControllerDelegate.Clazz.ClassInfo);

      lURLSegment := LControllerDelegate.URLSegment;
      if lURLSegment.IsEmpty then
      begin
        LAttributes := LRttiType.GetAttributes;
        if (LAttributes = nil) then
          Continue;
        FillControllerMappedPaths(LRttiType.Name, LAttributes, LControllerMappedPaths);
      end
      else
      begin
        LControllerMappedPaths.Add(lURLSegment);
      end;

      for LItem in LControllerMappedPaths do
      begin
        LControllerMappedPath := LItem;
        if (LControllerMappedPath = '/') then
        begin
          LControllerMappedPath := '';
        end;

  {$IF defined(TOKYOORBETTER)}
        if not LRequestPathInfo.StartsWith(APathPrefix + LControllerMappedPath, True) then
  {$ELSE}
        if not TMVCStringHelper.StartsWith(APathPrefix + LControllerMappedPath, LRequestPathInfo, True) then
  {$ENDIF}
        begin
          Continue;
        end;
        LMethods := LRttiType.GetMethods; { do not use GetDeclaredMethods because JSON-RPC rely on this!! }
        for LMethod in LMethods do
        begin
          if LMethod.Visibility <> mvPublic then // 2020-08-08
            Continue;
          if not (LMethod.MethodKind in [mkProcedure, mkFunction]) then
            Continue;

          LAttributes := LMethod.GetAttributes;
          if Length(LAttributes) = 0 then
            Continue;

          for LAtt in LAttributes do
          begin
            if LAtt is MVCPathAttribute then
            begin
              // THIS BLOCK IS HERE JUST FOR DEBUG
              // if LMethod.Name.Contains('GetProject') then
              // begin
              // lMethodCompatible := True; //debug here
              // end;
              // lMethodCompatible := IsHTTPMethodCompatible(ARequestMethodType, LAttributes);
              // lContentTypeCompatible := IsHTTPContentTypeCompatible(ARequestMethodType, LRequestContentType, LAttributes);
              // lAcceptCompatible :=  IsHTTPAcceptCompatible(ARequestMethodType, LRequestAccept, LAttributes);

              if IsHTTPMethodCompatible(ARequestMethodType, LAttributes) and
                IsHTTPContentTypeCompatible(ARequestMethodType, LRequestContentType, LAttributes) and
                IsHTTPAcceptCompatible(ARequestMethodType, LRequestAccept, LAttributes) then
              begin
                LMethodPath := MVCPathAttribute(LAtt).Path;
                if IsCompatiblePath(APathPrefix + LControllerMappedPath + LMethodPath,
                  LRequestPathInfo, ARequestParams) then
                begin
                  ARouterResult.MethodToCall := LMethod;
//                    FMethodToCall := LMethod;
                  ARouterResult.ControllerClazz := LControllerDelegate.Clazz;
                  //FControllerClazz := LControllerDelegate.Clazz;

                  ARouterResult.ControllerCreateAction := LControllerDelegate.CreateAction;
                  //FControllerCreateAction := LControllerDelegate.CreateAction;

                  ARouterResult.ControllerInjectableConstructor := nil;
                  //FControllerInjectableConstructor := nil;

                  // select the constructor with the most mumber of parameters
                  if not Assigned(ARouterResult.ControllerCreateAction) then
                  begin
                    ARouterResult.ControllerInjectableConstructor := TRttiUtils.GetConstructorWithAttribute<MVCInjectAttribute>(LRttiType);
                  end;
                  // end - select the constructor with the most mumber of parameters

                  LProduceAttribute := GetAttribute<MVCProducesAttribute>(LAttributes);
                  if LProduceAttribute <> nil then
                  begin
                    aRouterResult.ResponseContentMediaType := LProduceAttribute.Value;
                    aRouterResult.ResponseContentCharset := LProduceAttribute.Charset;
                  end
                  else
                  begin
                    aRouterResult.ResponseContentMediaType := ADefaultContentType;
                    aRouterResult.ResponseContentCharset := ADefaultContentCharset;
                  end;
                  Exit(True);
                end;
              end;
            end; // if MVCPathAttribute
          end; // for in Attributes
        end; // for in Methods
      end;
    end; // for in Controllers
  finally
    LControllerMappedPaths.Free;
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
