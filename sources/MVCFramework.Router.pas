// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Router;

interface

uses
  Web.HTTPApp,
  RTTIUtilsU,
  MVCFramework.Commons,
  System.RTTI,
  MVCFramework,
  System.Generics.Collections;

type
  TMVCRouter = class
  private
    FCTX: TRttiContext;
    FMethodToCall: TRTTIMethod;
    FMVCControllerClass: TMVCControllerClass;
    FMVCControllerDelegate: TMVCControllerDelegate;
    FMVCConfig: TMVCConfig;
    function IsHTTPContentTypeCompatible(AWebRequestMethodType: TMVCHTTPMethodType;
      AContentType: String; AAttributes: TArray<TCustomAttribute>): Boolean;
    function IsHTTPAcceptCompatible(AWebRequestMethodType: TMVCHTTPMethodType; AAccept: string;
      AAttributes: TArray<TCustomAttribute>): Boolean;
    function GetFirstMimeType(const AContentType: string): string;
  protected
    function IsHTTPMethodCompatible(AMethodType: TMVCHTTPMethodType;
      AAttributes: TArray<TCustomAttribute>): Boolean; virtual;
    function IsCompatiblePath(AMVCPath: string; APath: string; var AParams: TMVCRequestParamsTable)
      : Boolean; virtual;
    function GetAttribute<T: TCustomAttribute>(AAttributes: TArray<TCustomAttribute>): T;

  public
    class function StringMethodToHTTPMetod(const Value: AnsiString): TMVCHTTPMethodType;
    constructor Create(AMVCConfig: TMVCConfig);
    function ExecuteRouting(AWebRequestPathInfo: AnsiString;
      AWebRequestMethodType: TMVCHTTPMethodType; AWebRequestContentType: AnsiString;
      AWebRequestAccept: AnsiString; AMVCControllers: TObjectList<TMVCControllerRoutable>;
      ADefaultContentType: string; ADefaultContentCharset: string;
      var AMVCRequestParams: TMVCRequestParamsTable; out AResponseContentType: string;
      out AResponseContentEncoding: string): Boolean; overload;
    property MethodToCall: TRTTIMethod read FMethodToCall;
    property MVCControllerClass: TMVCControllerClass read FMVCControllerClass;
    property MVCControllerDelegate: TMVCControllerDelegate read FMVCControllerDelegate;
  end;

implementation

uses
  System.AnsiStrings,
  System.StrUtils,
  System.RegularExpressions,
  System.SysUtils,
  idURI;

{ TMVCRouter }

constructor TMVCRouter.Create(AMVCConfig: TMVCConfig);
begin
  inherited Create;
  FMVCConfig := AMVCConfig;
end;

function TMVCRouter.ExecuteRouting(AWebRequestPathInfo: AnsiString;
  AWebRequestMethodType: TMVCHTTPMethodType; AWebRequestContentType: AnsiString;
  AWebRequestAccept: AnsiString; AMVCControllers: TObjectList<TMVCControllerRoutable>;
  ADefaultContentType, ADefaultContentCharset: string;
  var AMVCRequestParams: TMVCRequestParamsTable; out AResponseContentType: string;
  out AResponseContentEncoding: string): Boolean;
var
  controllerRoutable: TMVCControllerRoutable;
  _type: TRttiType;
  _methods: TArray<TRTTIMethod>;
  _method: TRTTIMethod;
  _attribute: TCustomAttribute;
  _attributes: TArray<TCustomAttribute>;
  i: Integer;
  ControllerMappedPath: string;
  MethodPathAttribute: string;
  MVCProduceAttr: MVCProducesAttribute;
  Found: Boolean;
  LWebRequestPathInfo: string;
  LWebRequestAccept: String;
begin
  FMethodToCall := nil;
  FMVCControllerClass := nil;
  FMVCControllerDelegate := nil;
  LWebRequestAccept := String(AWebRequestAccept);

  LWebRequestPathInfo := string(AWebRequestPathInfo);
  if Trim(LWebRequestPathInfo) = EmptyStr then
    LWebRequestPathInfo := '/'
  else
  begin
    if LWebRequestPathInfo[1] <> '/' then
      LWebRequestPathInfo := '/' + LWebRequestPathInfo;
  end;

  // FIX https://github.com/danieleteti/delphimvcframework/issues/17
  LWebRequestPathInfo := TIdURI.PathEncode(LWebRequestPathInfo);

  { ISAPI CHANGE THE REQUEST PATH INFO START }
  if IsLibrary then
  begin
    if string(LWebRequestPathInfo).StartsWith(FMVCConfig.Value[TMVCConfigKey.ISAPIPath]) then
      LWebRequestPathInfo := LWebRequestPathInfo.Remove(0,
        FMVCConfig.Value[TMVCConfigKey.ISAPIPath].Length);
    if Length(LWebRequestPathInfo) = 0 then
      LWebRequestPathInfo := '/';
  end;
  { ISAPI CHANGE THE REQUEST PATH INFO END }

  TMonitor.Enter(Lock); // start of lock
  try

    Result := False;
    ControllerMappedPath := '';
    for controllerRoutable in AMVCControllers do
    begin
      SetLength(_attributes, 0);
      _type := FCTX.GetType(controllerRoutable.&Class.ClassInfo);
      _attributes := _type.GetAttributes;
      if _attributes = nil then
        Continue;

      Found := False;
      for _attribute in _attributes do
        if _attribute is MVCPathAttribute then
        begin
          Found := True;
          ControllerMappedPath := MVCPathAttribute(_attribute).Path;
          Break;
        end;

      if not Found then
        raise EMVCException.Create('Controller ' + _type.Name + ' doesn''t have MVCPath attribute');

      if ControllerMappedPath = '/' then // WE WANT TO AVOID '//' AS MVCPATH
        ControllerMappedPath := '';

      if (not ControllerMappedPath.IsEmpty) and (Pos(ControllerMappedPath, LWebRequestPathInfo) <> 1)
      then
        Continue;

      _methods := _type.GetMethods;
      for _method in _methods do
      begin
        _attributes := _method.GetAttributes;
        for i := 0 to Length(_attributes) - 1 do
        begin
          _attribute := _attributes[i];
          if _attribute is MVCPathAttribute then
          begin
            if IsHTTPMethodCompatible(AWebRequestMethodType, _attributes) and
              IsHTTPContentTypeCompatible(AWebRequestMethodType, String(AWebRequestContentType),
              _attributes) and IsHTTPAcceptCompatible(AWebRequestMethodType, LWebRequestAccept,
              _attributes) then
            begin
              MethodPathAttribute := MVCPathAttribute(_attribute).Path;
              if IsCompatiblePath(ControllerMappedPath + MethodPathAttribute, LWebRequestPathInfo,
                AMVCRequestParams) then
              begin
                FMethodToCall := _method;
                FMVCControllerClass := controllerRoutable.&Class;
                FMVCControllerDelegate := controllerRoutable.Delegate;
                // getting the default contenttype using MVCProduceAttribute
                MVCProduceAttr := GetAttribute<MVCProducesAttribute>(_attributes);
                if MVCProduceAttr <> nil then
                begin
                  AResponseContentType := MVCProduceAttr.Value;
                  AResponseContentEncoding := MVCProduceAttr.ProduceEncoding;
                end
                else
                begin
                  AResponseContentType := ADefaultContentType;
                  AResponseContentEncoding := ADefaultContentCharset;
                end;
                Exit(True);
              end; // if is compatible path
            end; // if is compatible method, contenttype and accept
          end; // if attribute is mvcpath
        end; // for each attributes on method
      end; // for each methods
    end; // for each controllers
  finally
    TMonitor.Exit(Lock);
  end;
end;

function TMVCRouter.GetAttribute<T>(AAttributes: TArray<TCustomAttribute>): T;
var
  a: TCustomAttribute;
begin
  Result := nil;
  for a in AAttributes do
    if a is T then
      Exit(T(a));
end;

function TMVCRouter.GetFirstMimeType(const AContentType: string): string;
begin
  Result := AContentType;
  while Pos(',', Result) > 0 do
    Result := Copy(Result, 1, Pos(',', Result) - 1);
  while Pos(';', Result) > 0 do
    Result := Copy(Result, 1, Pos(';', Result) - 1);
  // application/json;charset=UTF-8 {daniele}
end;

function TMVCRouter.IsCompatiblePath(AMVCPath: string; APath: string;
  var AParams: TMVCRequestParamsTable): Boolean;
  function ToPattern(const V: string; Names: TList<string>): string;
  var
    s: string;
  begin
    Result := V;
    for s in Names do
      Result := StringReplace(Result, '($' + s + ')', '([ ������@\.\_\,%\w\d\x2D\x3A]*)',
        [rfReplaceAll]);
  end;

  function GetParametersNames(const V: string): TList<string>;
  var
    s: string;
    matches: TMatchCollection;
    match: TMatch;
    i: Integer;
  begin
    Result := TList<string>.Create;
    s := '\(\$([A-Za-z0-9_]+)\)';
    matches := TRegEx.matches(V, s, [roIgnoreCase, roCompiled, roSingleLine]);
    for match in matches do
      for i := 0 to match.Groups.Count - 1 do
      begin
        s := match.Groups[i].Value;
        if (Length(s) > 0) and (s[1] <> '(') then
        begin
          Result.Add(s);
          Break;
        end;
      end;
  end;

var
  re: TRegEx;
  m: TMatch;
  pattern: string;
  i: Integer;
  Names: TList<string>;
begin
  Names := GetParametersNames(AMVCPath);
  try
    pattern := ToPattern(AMVCPath, Names);
    if APath = AMVCPath then
      Exit(True)
    else
    begin
      re := TRegEx.Create('^' + pattern + '$', [roIgnoreCase, roCompiled, roSingleLine]);
      m := re.match(APath);
      Result := m.Success;
      if Result then
        for i := 1 to pred(m.Groups.Count) do
          AParams.Add(Names[i - 1], TIdURI.URLDecode(m.Groups[i].Value));
    end;
  finally
    Names.Free;
  end;
end;

function TMVCRouter.IsHTTPAcceptCompatible(AWebRequestMethodType: TMVCHTTPMethodType;
  AAccept: string; AAttributes: TArray<TCustomAttribute>): Boolean;
var
  i: Integer;
  MethodAccept: string;
  FoundOneAttribProduces: Boolean;
begin
  Result := False;
  FoundOneAttribProduces := False;
  for i := 0 to high(AAttributes) do
  begin
    if AAttributes[i] is MVCProducesAttribute then
    begin
      FoundOneAttribProduces := True;
      MethodAccept := MVCProducesAttribute(AAttributes[i]).Value;
      AAccept := GetFirstMimeType(AAccept);
      // while Pos(',', AAccept) > 0 do
      // AAccept := Copy(AAccept, 1, Pos(',', AAccept) - 1);

      Result := SameText(AAccept, MethodAccept, loInvariantLocale);
      if Result then
        Break;
    end;
  end;
  Result := (not FoundOneAttribProduces) or (FoundOneAttribProduces and Result);
end;

function TMVCRouter.IsHTTPContentTypeCompatible(AWebRequestMethodType: TMVCHTTPMethodType;
  AContentType: String; AAttributes: TArray<TCustomAttribute>): Boolean;
var
  i: Integer;
  MethodContentType: string;
  FoundOneAttribConsumes: Boolean;
begin
  // content type is applicable only for PUT, POST and PATCH
  if AWebRequestMethodType in [httpGET, httpDELETE, httpHEAD, httpOPTIONS] then
    Exit(True);

  Result := False;
  FoundOneAttribConsumes := False;
  for i := 0 to high(AAttributes) do
  begin
    if AAttributes[i] is MVCConsumesAttribute then
    begin
      FoundOneAttribConsumes := True;
      MethodContentType := MVCConsumesAttribute(AAttributes[i]).Value;
      AContentType := GetFirstMimeType(AContentType);
      Result := SameText(AContentType, MethodContentType, loInvariantLocale);
      if Result then
        Break;
    end;
  end;
  Result := (not FoundOneAttribConsumes) or (FoundOneAttribConsumes and Result);
end;

function TMVCRouter.IsHTTPMethodCompatible(AMethodType: TMVCHTTPMethodType;
  AAttributes: TArray<TCustomAttribute>): Boolean;
var
  i: Integer;
  MustBeCompatible: Boolean;
  CompatibleMethods: TMVCHTTPMethods;
begin
  Result := False;
  // if there aren't MVCHTTPMethod attributes defined, the action is compatibile with all methods
  MustBeCompatible := False;
  for i := 0 to high(AAttributes) do
  begin
    if AAttributes[i] is MVCHTTPMethodAttribute then
    begin
      MustBeCompatible := True;
      CompatibleMethods := MVCHTTPMethodAttribute(AAttributes[i]).MVCHTTPMethods;
      Result := (AMethodType in CompatibleMethods);
    end;
  end;
  Result := (not MustBeCompatible) or (MustBeCompatible and Result);
end;

class function TMVCRouter.StringMethodToHTTPMetod(const Value: AnsiString): TMVCHTTPMethodType;
begin
  if Value = 'GET' then
    Exit(httpGET);
  if Value = 'POST' then
    Exit(httpPOST);
  if Value = 'DELETE' then
    Exit(httpDELETE);
  if Value = 'PUT' then
    Exit(httpPUT);
  if Value = 'HEAD' then
    Exit(httpHEAD);
  if Value = 'OPTIONS' then
    Exit(httpOPTIONS);
  if Value = 'PATCH' then
    Exit(httpPATCH);
  if Value = 'TRACE' then
    Exit(httpTRACE);
  raise EMVCException.CreateFmt('Unknown HTTP method [%s]', [Value]);
end;

end.
