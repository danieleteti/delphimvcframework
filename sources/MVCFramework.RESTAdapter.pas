{ *******************************************************************************
  Copyright 2010-2015 Daniele Teti
  Copyright 2010-2015 Daniele Spinetti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  ******************************************************************************** }

unit MVCFramework.RESTAdapter;

interface

uses
  System.Rtti, System.TypInfo, MVCFramework.RESTClient, MVCFramework,
  IdIOHandler, System.Classes;

type

  RESTResourceAttribute = class(MVCHTTPMethodAttribute)
  private
    FURL: string;
    FHTTPMethodType: TMVCHTTPMethodType;
    procedure SetURL(const Value: string);
    procedure SetHTTPMethodType(const Value: TMVCHTTPMethodType);
  public
    constructor Create(AMVCHTTPMethod: TMVCHTTPMethodType; AURL: string);
    property URL: string read FURL write SetURL;
    property HTTPMethodType: TMVCHTTPMethodType read FHTTPMethodType
      write SetHTTPMethodType;
  end;

  BodyAttribute = class(TCustomAttribute)
  private
    FOwnsObject: boolean;
    procedure SetOwnsObject(const Value: boolean);
  public
    constructor Create(AOwnsObject: boolean = true);
    property OwnsObject: boolean read FOwnsObject write SetOwnsObject;
  end;

  ParamAttribute = class(TCustomAttribute)
  private
    FParamType: string;
    FCustomFormat: string;
    procedure SetCustomFormat(const Value: string);
    procedure SetParamType(const Value: string);
  public
    constructor Create(AParamType: string = ''; ACustomFormat: string = '');
    property ParamType: string read FParamType write SetParamType;
    property CustomFormat: string read FCustomFormat write SetCustomFormat;
  end;

  HeadersAttribute = class(TCustomAttribute)
  private
    FKey: string;
    FValue: string;
    procedure SetKey(const Value: string);
    procedure SetValue(const Value: string);
  public
    constructor Create(AKey: string; AValue: string);
    property Key: string read FKey write SetKey;
    property Value: string read FValue write SetValue;
  end;

  IRESTAdapter<T> = interface
    ['{AAA41F40-69DB-419B-9922-F59F990CBDB5}']
    function ResourcesService: T;
    procedure AddRequestHeaders(AObj: TRttiObject);
    procedure AddRequestHeader(AKey: string; AValue: string);
    procedure MapResult(AResp: IRESTResponse; AMethod: TRttiMethod;
      out AResult: TValue);
  end;

  TVIAdapter<T: IInvokable> = class(TVirtualInterface)
  protected
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
    procedure DoInvokeImpl(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue); virtual; abstract;
  public
    constructor Create;
  end;

  TRESTAdapter<T: IInvokable> = class(TVIAdapter<T>, IRESTAdapter<T>)
  private
    FRESTClient: TRESTClient;
    procedure SetRESTClient(const Value: TRESTClient);
  protected
    procedure DoInvokeImpl(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue); override;
    procedure AddRequestHeaders(AObj: TRttiObject);
    procedure AddRequestHeader(AKey: string; AValue: string);
    function MapAsParam(AParam: ParamAttribute; const Arg: TValue): string;
    function MapAsBody(const Arg: TValue): string;
    procedure MapParams(Method: TRttiMethod; const Args: TArray<TValue>;
      out ABody: string; out AParams: TArray<string>);
    procedure MapResult(AResp: IRESTResponse; AMethod: TRttiMethod;
      out AResult: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    function Build(ARESTClient: TRESTClient): T; overload;
    function Build(const AServerName: string; const AServerPort: Word = 80;
      AIOHandler: TIdIOHandler = nil): T; overload;

    function ResourcesService: T;
    property RESTClient: TRESTClient read FRESTClient write SetRESTClient;
  end;

implementation

uses
  System.SysUtils, ObjectsMappers,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
  Data.SqlExpr,
  DBXCommon,
{$ELSE}
  System.JSON,
{$ENDIF}
  RTTIUtilsU, DuckListU,
  Generics.Collections;

{ TRESTAdapter }

function TRESTAdapter<T>.Build(ARESTClient: TRESTClient): T;
begin
  RESTClient := ARESTClient;
  Result := ResourcesService;
end;

{ TVIAdapter }

constructor TVIAdapter<T>.Create;
begin
  inherited Create(TypeInfo(T), DoInvoke);
end;

procedure TVIAdapter<T>.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
begin
  DoInvokeImpl(Method, Args, Result);
end;

procedure TRESTAdapter<T>.AddRequestHeader(AKey, AValue: string);
begin
  if CompareText(AKey, 'ContentType') = 0 then
    FRESTClient.ContentType(AValue)
  else if CompareText(AKey, 'ContentEncoding') = 0 then
    FRESTClient.ContentEncoding(AValue)
  else if CompareText(AKey, 'Accept') = 0 then
    FRESTClient.Accept(AValue)
  else
    FRESTClient.RequestHeaders.Values[AKey] := AValue;
end;

procedure TRESTAdapter<T>.AddRequestHeaders(AObj: TRttiObject);
var
  _attr: TCustomAttribute;
begin
  for _attr in AObj.GetAttributes do
    if _attr is HeadersAttribute then
      AddRequestHeader(HeadersAttribute(_attr).Key,
        HeadersAttribute(_attr).Value);
end;

function TRESTAdapter<T>.Build(const AServerName: string;
  const AServerPort: Word; AIOHandler: TIdIOHandler): T;
var
  ARESTClient: TRESTClient;
begin
  ARESTClient := TRESTClient.Create(AServerName, AServerPort, AIOHandler);
  Result := Build(ARESTClient);
end;

constructor TRESTAdapter<T>.Create;
begin
  inherited Create;
end;

destructor TRESTAdapter<T>.Destroy;
begin
  if Assigned(FRESTClient) then
    FRESTClient.Free;
  inherited;
end;

procedure TRESTAdapter<T>.DoInvokeImpl(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  Resp: IRESTResponse;
  _restresourceattr: RESTResourceAttribute;
  _attrlistof: MapperListOf;
  BodyString: string;
  _parameter: TRttiParameter;
  Params: TArray<string>;
begin
  // Implementation of RESTClient DoGet DoPut ecc...
  if not TRTTIUtils.HasAttribute<RESTResourceAttribute>(Method,
    _restresourceattr) then
    raise Exception.CreateFmt('No REST Resource specified in method %s',
      [Method.Name]);

  // MapParams BodyAttribute and ParamAttribute
  MapParams(Method, Args, BodyString, Params);

  // headers can be more than one
  FRESTClient.RequestHeaders.Clear;
  // Interface
  AddRequestHeaders(TRTTIUtils.ctx.GetType(TypeInfo(T)));
  // Method
  AddRequestHeaders(Method);

  case _restresourceattr.HTTPMethodType of
    httpGET:
      Resp := FRESTClient.doGET(_restresourceattr.URL, Params);
    httpPUT:
      Resp := FRESTClient.doPUT(_restresourceattr.URL, Params, BodyString);
    httpPOST:
      Resp := FRESTClient.doPOST(_restresourceattr.URL, Params, BodyString);
  end;

  // if is a procedure no need a return type
  if Assigned(Method.ReturnType) then
    MapResult(Resp, Method, Result);

end;

function TRESTAdapter<T>.MapAsBody(const Arg: TValue): string;
begin
  if Arg.IsObject then
    Result := Mapper.ObjectToJSONObjectString(Arg.AsObject)
  else
    Result := TRTTIUtils.TValueAsString(Arg, '', '');
end;

function TRESTAdapter<T>.MapAsParam(AParam: ParamAttribute;
  const Arg: TValue): string;
begin
  Result := TRTTIUtils.TValueAsString(Arg, AParam.ParamType,
    AParam.CustomFormat);
end;

procedure TRESTAdapter<T>.MapParams(Method: TRttiMethod;
  const Args: TArray<TValue>; out ABody: string; out AParams: TArray<string>);
var
  I: Integer;
  _parameter: TRttiParameter;
  _param: ParamAttribute;
begin
  for I := 0 to Length(Method.GetParameters) - 1 do
  begin
    _parameter := Method.GetParameters[I];
    if TRTTIUtils.HasAttribute<BodyAttribute>(_parameter) then
      ABody := MapAsBody(Args[I]);
    if TRTTIUtils.HasAttribute<ParamAttribute>(_parameter, _param) then
      AParams := AParams + [MapAsParam(_param, Args[I])];
  end;
end;

procedure TRESTAdapter<T>.MapResult(AResp: IRESTResponse; AMethod: TRttiMethod;
  out AResult: TValue);
var
  _attrlistof: MapperListOf;
begin
  if AMethod.ReturnType.QualifiedName = TRTTIUtils.ctx.GetType
    (TypeInfo(IRESTResponse)).QualifiedName then
    AResult := AResult.From(AResp)
  else if TRTTIUtils.HasAttribute<MapperListOf>(AMethod, _attrlistof) then
  begin
    AResult := TRTTIUtils.CreateObject(AMethod.ReturnType.QualifiedName);
    Mapper.JSONArrayToObjectList(WrapAsList(AResult.AsObject),
      _attrlistof.Value, AResp.BodyAsJsonValue as TJSONArray, false);
  end
  else if AMethod.ReturnType.QualifiedName.Contains('JSON') then
    AResult := AResult.From(AResp.BodyAsJsonValue)
  else if AMethod.ReturnType.TypeKind = tkClass then
    AResult := Mapper.JSONObjectToObject(AMethod.ReturnType.QualifiedName,
      AResp.BodyAsJsonObject)
  else
    AResult := AResp.BodyAsString
end;

function TRESTAdapter<T>.ResourcesService: T;
var
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(T);
  if QueryInterface(GetTypeData(pInfo).Guid, Result) <> 0 then
  begin
    raise Exception.CreateFmt
      ('RESTAdapter is unable to cast %s to its interface',
      [string(pInfo.Name)]);
  end;
end;

procedure TRESTAdapter<T>.SetRESTClient(const Value: TRESTClient);
begin
  FRESTClient := Value;
end;

{ RESTResourceAttribute }

constructor RESTResourceAttribute.Create(AMVCHTTPMethod: TMVCHTTPMethodType;
  AURL: string);
begin
  FURL := AURL;
  FHTTPMethodType := AMVCHTTPMethod;
end;

procedure RESTResourceAttribute.SetHTTPMethodType
  (const Value: TMVCHTTPMethodType);
begin
  FHTTPMethodType := Value;
end;

procedure RESTResourceAttribute.SetURL(const Value: string);
begin
  FURL := Value;
end;

{ BodyAttribute }

constructor BodyAttribute.Create(AOwnsObject: boolean);
begin
  FOwnsObject := AOwnsObject;
end;

procedure BodyAttribute.SetOwnsObject(const Value: boolean);
begin
  FOwnsObject := Value;
end;

{ ParamAttribute }

constructor ParamAttribute.Create(AParamType, ACustomFormat: string);
begin
  FParamType := AParamType;
  FCustomFormat := ACustomFormat;
end;

procedure ParamAttribute.SetCustomFormat(const Value: string);
begin
  FCustomFormat := Value;
end;

procedure ParamAttribute.SetParamType(const Value: string);
begin
  FParamType := Value;
end;

{ HeadersAttribute }

constructor HeadersAttribute.Create(AKey: string; AValue: string);
begin
  FKey := AKey;
  FValue := AValue;
end;

procedure HeadersAttribute.SetKey(const Value: string);
begin
  FKey := Value;
end;

procedure HeadersAttribute.SetValue(const Value: string);
begin
  FValue := Value;
end;

end.
