// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
  {$IFNDEF LINUX}
  System.AnsiStrings,
  {$ENDIF}
  MVCFramework,
  MVCFramework.Commons,
  IdURI;

type

  TMVCRouter = class
  private
    FRttiContext: TRttiContext;
    FConfig: TMVCConfig;
    FMethodToCall: TRttiMethod;
    FControllerClazz: TMVCControllerClazz;
    FControllerCreateAction: TMVCControllerCreateAction;
    function GetAttribute<T: TCustomAttribute>(const AAttributes: TArray<TCustomAttribute>): T;
    function GetFirstMediaType(const AContentType: string): string;

    function IsHTTPContentTypeCompatible(
      const ARequestMethodType: TMVCHTTPMethodType;
      var AContentType: string;
      const AAttributes: TArray<TCustomAttribute>): Boolean;

    function IsHTTPAcceptCompatible(
      const ARequestMethodType: TMVCHTTPMethodType;
      var AAccept: string;
      const AAttributes: TArray<TCustomAttribute>): Boolean;

    function IsHTTPMethodCompatible(
      const AMethodType: TMVCHTTPMethodType;
      const AAttributes: TArray<TCustomAttribute>): Boolean;

    function IsCompatiblePath(
      const AMVCPath: string;
      const APath: string;
      var AParams: TMVCRequestParamsTable): Boolean;
  protected
    { protected declarations }
  public
    class function StringMethodToHTTPMetod(const AValue: string): TMVCHTTPMethodType; static;
  public
    constructor Create(const AConfig: TMVCConfig);
    destructor Destroy; override;

    function ExecuteRouting(
      const ARequestPathInfo: string;
      const ARequestMethodType: TMVCHTTPMethodType;
      const ARequestContentType: string;
      const ARequestAccept: string;
      const AControllers: TObjectList<TMVCControllerDelegate>;
      const ADefaultContentType: string;
      const ADefaultContentCharset: string;
      var ARequestParams: TMVCRequestParamsTable;
      out AResponseContentType: string;
      out AResponseContentEncoding: string): Boolean;

    property MethodToCall: TRttiMethod read FMethodToCall;
    property ControllerClazz: TMVCControllerClazz read FControllerClazz;
    property ControllerCreateAction: TMVCControllerCreateAction read FControllerCreateAction;
  end;

implementation

{ TMVCRouter }

constructor TMVCRouter.Create(const AConfig: TMVCConfig);
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
  FConfig := AConfig;
  FMethodToCall := nil;
  FControllerClazz := nil;
  FControllerCreateAction := nil;
end;

destructor TMVCRouter.Destroy;
begin
  FRttiContext.Free;
  inherited Destroy;
end;

function TMVCRouter.ExecuteRouting(const ARequestPathInfo: string;
  const ARequestMethodType: TMVCHTTPMethodType;
  const ARequestContentType, ARequestAccept: string;
  const AControllers: TObjectList<TMVCControllerDelegate>;
  const ADefaultContentType: string;
  const ADefaultContentCharset: string;
  var ARequestParams: TMVCRequestParamsTable;
  out AResponseContentType: string;
  out AResponseContentEncoding: string): Boolean;
var
  LRequestPathInfo: string;
  LRequestAccept: string;
  LRequestContentType: string;
  LControllerMappedPath: string;
  LControllerDelegate: TMVCControllerDelegate;
  LAttributes: TArray<TCustomAttribute>;
  LAtt: TCustomAttribute;
  LRttiType: TRttiType;
  LMethods: TArray<TRTTIMethod>;
  LMethod: TRTTIMethod;
  LFound: Boolean;
  LMethodPath: string;
  LProduceAttribute: MVCProducesAttribute;
begin
  Result := False;

  FMethodToCall := nil;
  FControllerClazz := nil;
  FControllerCreateAction := nil;

  LRequestAccept := ARequestAccept;
  LRequestContentType := ARequestContentType;
  LRequestPathInfo := ARequestPathInfo;
  if (Trim(LRequestPathInfo) = EmptyStr) then
    LRequestPathInfo := '/'
  else
  begin
    if LRequestPathInfo[1] <> '/' then
      LRequestPathInfo := '/' + LRequestPathInfo;
  end;
  LRequestPathInfo := TIdURI.PathEncode(LRequestPathInfo);

  { ISAPI CHANGE THE REQUEST PATH INFO START }
  if IsLibrary then
  begin
    if string(LRequestPathInfo).StartsWith(FConfig.Value[TMVCConfigKey.ISAPIPath]) then
      LRequestPathInfo := LRequestPathInfo.Remove(0, FConfig.Value[TMVCConfigKey.ISAPIPath].Length);
    if Length(LRequestPathInfo) = 0 then
      LRequestPathInfo := '/';
  end;
  { ISAPI CHANGE THE REQUEST PATH INFO END }

  TMonitor.Enter(Lock);
  try
    LControllerMappedPath := EmptyStr;
    for LControllerDelegate in AControllers do
    begin
      SetLength(LAttributes, 0);
      LRttiType := FRttiContext.GetType(LControllerDelegate.Clazz.ClassInfo);
      LAttributes := LRttiType.GetAttributes;
      if (LAttributes = nil) then
        Continue;

      LFound := False;
      for LAtt in LAttributes do
        if LAtt is MVCPathAttribute then
        begin
          LFound := True;
          LControllerMappedPath := MVCPathAttribute(LAtt).Path;
          Break;
        end;

      if not LFound then
        raise EMVCException.CreateFmt('Controller %s does not have MVCPath attribute', [LRttiType.Name]);

      if (LControllerMappedPath = '/') then
        LControllerMappedPath := '';

      if (not LControllerMappedPath.IsEmpty) and (Pos(LControllerMappedPath, LRequestPathInfo) <> 1) then
        Continue;

      LMethods := LRttiType.GetMethods;
      for LMethod in LMethods do
      begin
        LAttributes := LMethod.GetAttributes;
        for LAtt in LAttributes do
          if LAtt is MVCPathAttribute then
            if IsHTTPMethodCompatible(ARequestMethodType, LAttributes) and
              IsHTTPContentTypeCompatible(ARequestMethodType, LRequestContentType, LAttributes) and
              IsHTTPAcceptCompatible(ARequestMethodType, LRequestAccept, LAttributes) then
            begin
              LMethodPath := MVCPathAttribute(LAtt).Path;
              if IsCompatiblePath(LControllerMappedPath + LMethodPath, LRequestPathInfo, ARequestParams) then
              begin
                FMethodToCall := LMethod;
                FControllerClazz := LControllerDelegate.Clazz;
                FControllerCreateAction := LControllerDelegate.CreateAction;
                LProduceAttribute := GetAttribute<MVCProducesAttribute>(LAttributes);
                if Assigned(LProduceAttribute) then
                begin
                  AResponseContentType := LProduceAttribute.Value;
                  AResponseContentEncoding := LProduceAttribute.Encoding;
                end
                else
                begin
                  AResponseContentType := ADefaultContentType;
                  AResponseContentEncoding := ADefaultContentCharset;
                end;
                Exit(True);
              end;
            end;
      end;

    end;
  finally
    TMonitor.Exit(Lock);
  end;
end;

function TMVCRouter.GetAttribute<T>(const AAttributes: TArray<TCustomAttribute>): T;
var
  Att: TCustomAttribute;
begin
  Result := nil;
  for Att in AAttributes do
    if Att is T then
      Exit(T(Att));
end;

function TMVCRouter.GetFirstMediaType(const AContentType: string): string;
begin
  Result := AContentType;
  while Pos(',', Result) > 0 do
    Result := Copy(Result, 1, Pos(',', Result) - 1);
  while Pos(';', Result) > 0 do
    Result := Copy(Result, 1, Pos(';', Result) - 1);
end;

function TMVCRouter.IsCompatiblePath(
  const AMVCPath: string;
  const APath: string;
  var AParams: TMVCRequestParamsTable): Boolean;

  function ToPattern(const V: string; Names: TList<string>): string;
  var
    S: string;
  begin
    Result := V;
    for S in Names do
      Result := StringReplace(Result, '($' + S + ')', '([ àèéùòì@\.\_\,%\w\d\x2D\x3A]*)', [rfReplaceAll]);
  end;

  function GetParametersNames(const V: string): TList<string>;
  var
    S: string;
    Matches: TMatchCollection;
    M: TMatch;
    I: Integer;
  begin
    Result := TList<string>.Create;
    S := '\(\$([A-Za-z0-9\_]+)\)';
    Matches := TRegEx.Matches(V, S, [roIgnoreCase, roCompiled, roSingleLine]);
    for M in Matches do
      for I := 0 to M.Groups.Count - 1 do
      begin
        S := M.Groups[I].Value;
        if (Length(S) > 0) and (S[1] <> '(') then
        begin
          Result.Add(S);
          Break;
        end;
      end;
  end;

var
  RegEx: TRegEx;
  Macth: TMatch;
  Pattern: string;
  I: Integer;
  Names: TList<string>;
begin
  Names := GetParametersNames(AMVCPath);
  try
    Pattern := ToPattern(AMVCPath, Names);
    if (APath = AMVCPath) then
      Exit(True)
    else
    begin
      RegEx := TRegEx.Create('^' + Pattern + '$', [roIgnoreCase, roCompiled, roSingleLine]);
      Macth := RegEx.match(APath);
      Result := Macth.Success;
      if Result then
        for I := 1 to pred(Macth.Groups.Count) do
          AParams.Add(Names[I - 1], TIdURI.URLDecode(Macth.Groups[I].Value));
    end;
  finally
    Names.Free;
  end;
end;

function TMVCRouter.IsHTTPAcceptCompatible(
  const ARequestMethodType: TMVCHTTPMethodType;
  var AAccept: string;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MethodAccept: string;
  FoundOneAttProduces: Boolean;
begin
  Result := False;

  FoundOneAttProduces := False;
  for I := 0 to High(AAttributes) do
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

function TMVCRouter.IsHTTPContentTypeCompatible(
  const ARequestMethodType: TMVCHTTPMethodType;
  var AContentType: string;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MethodContentType: string;
  FoundOneAttConsumes: Boolean;
begin
  if ARequestMethodType in [httpGET, httpDELETE, httpHEAD, httpOPTIONS] then
    Exit(True);

  Result := False;

  FoundOneAttConsumes := False;
  for I := 0 to High(AAttributes) do
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

function TMVCRouter.IsHTTPMethodCompatible(
  const AMethodType: TMVCHTTPMethodType;
  const AAttributes: TArray<TCustomAttribute>): Boolean;
var
  I: Integer;
  MustBeCompatible: Boolean;
  CompatibleMethods: TMVCHTTPMethods;
begin
  Result := False;

  MustBeCompatible := False;
  for I := 0 to High(AAttributes) do
    if AAttributes[I] is MVCHTTPMethodAttribute then
    begin
      MustBeCompatible := True;
      CompatibleMethods := MVCHTTPMethodAttribute(AAttributes[I]).MVCHTTPMethods;
      Result := (AMethodType in CompatibleMethods);
    end;

  Result := (not MustBeCompatible) or (MustBeCompatible and Result);
end;

class function TMVCRouter.StringMethodToHTTPMetod(const AValue: string): TMVCHTTPMethodType;
begin
  if AValue = 'GET' then
    Exit(httpGET);
  if AValue = 'POST' then
    Exit(httpPOST);
  if AValue = 'DELETE' then
    Exit(httpDELETE);
  if AValue = 'PUT' then
    Exit(httpPUT);
  if AValue = 'HEAD' then
    Exit(httpHEAD);
  if AValue = 'OPTIONS' then
    Exit(httpOPTIONS);
  if AValue = 'PATCH' then
    Exit(httpPATCH);
  if AValue = 'TRACE' then
    Exit(httpTRACE);
  raise EMVCException.CreateFmt('Unknown HTTP method [%s]', [AValue]);
end;

end.
