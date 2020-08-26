// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RESTAdapter;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.TypInfo,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Commons,
  IdIOHandler,
  System.Classes,
  System.SysUtils;

const
  URL_SEPARATOR = '/';

type

  RESTResourceAttribute = class(TCustomAttribute)
  private
    fURL: string;
    fHTTPMethodType: TMVCHTTPMethodType;
    procedure SetURL(const Value: string);
    procedure SetHTTPMethodType(const Value: TMVCHTTPMethodType);
  public
    constructor Create(AMVCHTTPMethod: TMVCHTTPMethodType; AURL: string);
    property URL: string read fURL write SetURL;
    property HTTPMethodType: TMVCHTTPMethodType read fHTTPMethodType write SetHTTPMethodType;
  end;

  BodyAttribute = class(TCustomAttribute)
  private
    FOwnsObject: Boolean;
    procedure SetOwnsObject(const Value: Boolean);
  public
    constructor Create(AOwnsObject: Boolean = True);
    property OwnsObject: Boolean read FOwnsObject write SetOwnsObject;
  end;

  ParamAttribute = class(TCustomAttribute)
  private
    fParamType: string;
    fCustomFormat: string;
    fParamMatch: string;
    procedure SetCustomFormat(const Value: string);
    procedure SetParamType(const Value: string);
    procedure SetParamMatch(const Value: string);
  public
    constructor Create(AParamMatch: string; AParamType: string = ''; ACustomFormat: string = '');
    property ParamMatch: string read fParamMatch write SetParamMatch;
    property ParamType: string read fParamType write SetParamType;
    property CustomFormat: string read fCustomFormat write SetCustomFormat;
    function FmtParamMatch: string;
  end;

  HeadersAttribute = class(TCustomAttribute)
  private
    fKey: string;
    fValue: string;
    procedure SetKey(const Value: string);
    procedure SetValue(const Value: string);
  public
    constructor Create(AKey: string; AValue: string);
    property Key: string read fKey write SetKey;
    property Value: string read fValue write SetValue;
  end;

  MappingAttribute = class(TCustomAttribute)
  private
    fClass: TClass;
  public
    constructor Create(AClass: TClass);
    function GetType: TRttiType;
  end;

  IRESTAdapter<T> = interface
    ['{AAA41F40-69DB-419B-9922-F59F990CBDB5}']
    function ResourcesService: T;
    procedure AddRequestHeaders(AObj: TRttiObject);
    procedure AddRequestHeader(AKey: string; AValue: string);
    procedure MapResult(AResp: IMVCRESTResponse; AMethod: TRttiMethod; ARTTIType: TRttiType; out AResult: TValue);
  end;

  TRESTAdapter<T: IInvokable> = class(TVirtualInterface, IRESTAdapter<T>)
  private
    fRESTClient: IMVCRESTClient;
    fRESTClientOwner: Boolean;
  protected
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure AddRequestHeaders(AObj: TRttiObject);
    procedure AddRequestHeader(AKey: string; AValue: string);
    procedure MapResult(AResp: IMVCRESTResponse; AMethod: TRttiMethod; ARTTIType: TRttiType; out AResult: TValue);
    function GetURL(AMethod: TRttiMethod; const Args: TArray<TValue>): string;
    function GetBodyAsString(AMethod: TRttiMethod; const Args: TArray<TValue>): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Build(ARESTClient: IMVCRESTClient; const ARESTClientOwner: Boolean = false): T; overload;
    function Build(const AServerName: string; const AServerPort: Word = 80): T; overload;

    function ResourcesService: T;
    property RESTClient: IMVCRESTClient read fRESTClient write fRESTClient;
    property RESTClientOwner: Boolean read fRESTClientOwner write fRESTClientOwner;
  end;

  IAsynchRequest = interface
    ['{3E720356-F2B7-4C32-8051-B7723263740F}']
    procedure SetErrorProc(const Value: TProc<Exception>);
    procedure SetSuccessProc(const Value: TProc<TValue>);
    procedure SetSynchronized(const Value: Boolean);

    function GetErrorProc: TProc<Exception>;
    function GetSuccessProc: TProc<TValue>;
    function GetSynchronized: Boolean;

    property SuccessProc: TProc<TValue> read GetSuccessProc write SetSuccessProc;
    property ErrorProc: TProc<Exception> read GetErrorProc write SetErrorProc;
    property Synchronized: Boolean read GetSynchronized write SetSynchronized;
  end;

  TAsynchRequest = class(TInterfacedObject, IAsynchRequest)
  private
    fSynchronized: Boolean;
    fSuccessProc: TProc<TValue>;
    fErrorProc: TProc<Exception>;
    procedure SetErrorProc(const Value: TProc<Exception>);
    procedure SetSuccessProc(const Value: TProc<TValue>);
    procedure SetSynchronized(const Value: Boolean);
    function GetErrorProc: TProc<Exception>;
    function GetSuccessProc: TProc<TValue>;
    function GetSynchronized: Boolean;
  public
    constructor Create(ASuccProc: TProc<TValue> = nil; AProcErr: TProc<Exception> = nil; ASynchronized: Boolean = False);
    property SuccessProc: TProc<TValue> read GetSuccessProc write SetSuccessProc;
    property ErrorProc: TProc<Exception> read GetErrorProc write SetErrorProc;
    property Synchronized: Boolean read GetSynchronized write SetSynchronized;
  end;

implementation

uses
  // ObjectsMappers,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Defaults,

  {$IFDEF SYSTEMJSON}
  System.JSON,

  {$ELSE}
  Data.DBXJSON,
  Data.SqlExpr,
  DBXCommon,

  {$ENDIF}
  MVCFramework.Rtti.Utils,
  MVCFramework.DuckTyping,
  System.Generics.Collections;

{ TRESTAdapter }

function TRESTAdapter<T>.Build(ARESTClient: IMVCRESTClient; const ARESTClientOwner: Boolean = False): T;
begin
  RESTClient := ARESTClient;
  RESTClientOwner := ARESTClientOwner;
  Result := ResourcesService;
end;

procedure TRESTAdapter<T>.AddRequestHeader(AKey, AValue: string);
begin
//  if CompareText(AKey, 'ContentType') = 0 then
//    fRESTClient.ContentType(AValue)
//  else if CompareText(AKey, 'ContentEncoding') = 0 then
//    fRESTClient.ContentEncoding(AValue)
//  else
  if CompareText(AKey, 'Accept') = 0 then
    fRESTClient.Accept(AValue)
  else if CompareText(AKey, 'AcceptCharset') = 0 then
    fRESTClient.AcceptCharset(AValue)
  else if CompareText(AKey, 'AcceptEncoding') = 0 then
    fRESTClient.AcceptEncoding(AValue)
  else
    fRESTClient.AddHeader(AKey, AValue);
end;

procedure TRESTAdapter<T>.AddRequestHeaders(AObj: TRttiObject);
var
  lAttr: TCustomAttribute;
begin
  for lAttr in AObj.GetAttributes do
  begin
    if lAttr is HeadersAttribute then
    begin
      AddRequestHeader(HeadersAttribute(lAttr).Key, HeadersAttribute(lAttr).Value);
    end;
  end;
end;

function TRESTAdapter<T>.Build(const AServerName: string; const AServerPort: Word): T;
var
  lRESTClient: IMVCRESTClient;
begin
  lRESTClient := TMVCRESTClient.New.BaseURL(AServerName, AServerPort);
  Result := Build(lRESTClient, True);
end;

constructor TRESTAdapter<T>.Create;
begin
  inherited Create(TypeInfo(T), DoInvoke);
end;

destructor TRESTAdapter<T>.Destroy;
begin
  // Ezequiel J. Müller (If it is created outside, it must be destroyed out)
  // d.spinetti added RESTClientOwner to manage desctruction of RESTClient and free its associated memory
  fRESTClient := nil;
  inherited;
end;

procedure TRESTAdapter<T>.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
var
  lResp: IMVCRESTResponse;
  lRestResourceAttr: RESTResourceAttribute;
  lURL: string;
  lBody: string;
  lAsynchClass: IAsynchRequest;
  lMappingAttr: MappingAttribute;
begin
  // Implementation of RESTClient DoGet DoPut ecc...
  if not TRttiUtils.HasAttribute<RESTResourceAttribute>(Method, lRestResourceAttr) then
    raise Exception.CreateFmt('No REST Resource specified in method %s', [Method.Name]);

  // headers can be more than one
  // fRESTClient.RequestHeaders.Clear; //Ezequiel J. Müller (You can not clear the header, because I can use other.)
  // Interface
  AddRequestHeaders(TRttiUtils.GlContext.GetType(TypeInfo(T)));
  // Method
  AddRequestHeaders(Method);

  // lURL and lBody
  lURL := GetURL(Method, Args);
  lBody := GetBodyAsString(Method, Args);

  // Asynch way to do
  if Args[Length(Args) - 1].TryAsType<IAsynchRequest>(lAsynchClass) then
  begin
    fRESTClient.Async(
      procedure(ARESTResponse: IMVCRESTResponse)
      var
        lResValue: TValue;
      begin
        if TRttiUtils.HasAttribute<MappingAttribute>(Method, lMappingAttr) then
          MapResult(ARESTResponse, Method, lMappingAttr.GetType, lResValue)
        else
          lResValue := TValue.From(ARESTResponse);
        if Assigned(lAsynchClass.SuccessProc) then
          lAsynchClass.SuccessProc(lResValue);
      end, lAsynchClass.ErrorProc,
      lAsynchClass.Synchronized);
  end;

  case lRestResourceAttr.HTTPMethodType of
    httpGET:
      lResp := fRESTClient.Get(lURL);
    httpPUT:
      lResp := fRESTClient.Put(lURL, lBody);
    httpPOST:
      lResp := fRESTClient.Post(lURL, lBody);
    httpDELETE:
      lResp := fRESTClient.Delete(lURL);
  end;

  // if the response code is > 400 raise exception
  // if lResp.ResponseCode >= 400 then
  // raise Exception.CreateFmt
  // ('Error on execute request ''%s''. Message: %d %s ',
  // [lURL, lResp.ResponseCode, lResp.BodyAsString]);

  // if is a procedure no need a return type
  if Assigned(Method.ReturnType) then
    MapResult(lResp, Method, Method.ReturnType, Result);

end;

function TRESTAdapter<T>.GetBodyAsString(AMethod: TRttiMethod;
const Args: TArray<TValue>): string;
var
  _parameters: TArray<TRttiParameter>;
  I: Integer;
  _parameter: TRttiParameter;
  _param: BodyAttribute;
  _attrlistof: MVCListOfAttribute;
  Arg: TValue;
begin
  _parameters := AMethod.GetParameters;
  for I := 0 to Length(_parameters) - 1 do
  begin
    _parameter := _parameters[I];
    // ARG := ARGS[I+1] because
    // Args	RTTI for the arguments of the interface method that has been called. The first argument (located at index 0) represents the interface instance itself.
    Arg := Args[I + 1];
    if TRttiUtils.HasAttribute<BodyAttribute>(_parameter, _param) then
      try
        if Arg.IsObject then
        begin

          if TRttiUtils.HasAttribute<MVCListOfAttribute>(AMethod, _attrlistof) then
            Exit(
              GetDefaultSerializer.SerializeCollection(Arg.AsObject)
            { Mapper.ObjectListToJSONArrayString(WrapAsList(Arg.AsObject), True) }
              )
          else
            Exit(
              GetDefaultSerializer.SerializeObject(Arg.AsObject)
            { Mapper.ObjectToJSONObjectString(Arg.AsObject) }
              );
        end
        else
          Exit(TRttiUtils.TValueAsString(Arg, '', ''));
      finally
        if _param.OwnsObject and Arg.IsObject then
        begin

          {$HINTS OFF}
          Arg.AsObject.Free;

          {$HINTS ON}
        end;
      end;
  end;
end;

function TRESTAdapter<T>.GetURL(AMethod: TRttiMethod;
const Args: TArray<TValue>): string;
var
  _restresourceattr: RESTResourceAttribute;
  IURL: string;
  SplitUrl: TArray<string>;
  URLDict: TDictionary<string, string>;
  Split: string;
  _parameters: TArray<TRttiParameter>;
  I: Integer;
  _parameter: TRttiParameter;
  _param: ParamAttribute;
  Arg: TValue;
begin
  _restresourceattr := TRttiUtils.GetAttribute<RESTResourceAttribute>(AMethod);
  IURL := _restresourceattr.URL;
  SplitUrl := IURL.Split([URL_SEPARATOR]);
  URLDict := TDictionary<string, string>.Create;
  try
    for Split in SplitUrl do
      if not Split.IsEmpty then
        URLDict.Add(Split, Split);
    _parameters := AMethod.GetParameters;
    // ARG := ARGS[I+1] because
    // Args	RTTI for the arguments of the interface method that has been called. The first argument (located at index 0) represents the interface instance itself.
    for I := 0 to Length(_parameters) - 1 do
    begin
      _parameter := _parameters[I];
      Arg := Args[I + 1];
      if TRttiUtils.HasAttribute<ParamAttribute>(_parameter, _param) then
        URLDict[_param.FmtParamMatch] := TRttiUtils.TValueAsString(Arg,
          _param.ParamType, _param.CustomFormat);
    end;

    for Split in SplitUrl do
      if not Split.IsEmpty then
        Result := Result + URL_SEPARATOR + URLDict[Split];

    if IURL.EndsWith(URL_SEPARATOR) and not (Result.EndsWith(URL_SEPARATOR)) then
      Result := Result + URL_SEPARATOR;

  finally
    URLDict.Free;
  end;
end;

procedure TRESTAdapter<T>.MapResult(AResp: IMVCRESTResponse; AMethod: TRttiMethod; ARTTIType: TRttiType;
out AResult: TValue);
var
  _attrlistof: MVCListOfAttribute;
begin
  if ARTTIType.TypeKind = tkClass then
  begin
    // ListOf
    if TRttiUtils.HasAttribute<MVCListOfAttribute>(AMethod, _attrlistof) then
    begin
      AResult := TRttiUtils.CreateObject(ARTTIType.QualifiedName);
      GetDefaultSerializer.DeserializeCollection(AResp.Content, AResult.AsObject, _attrlistof.Value);
    end
    // JSONValue
    else if ARTTIType.AsInstance.MetaclassType.InheritsFrom(TJSONValue) then
    begin
      AResult := TJSONObject.ParseJSONValue(AResp.Content);
      // Object
    end
    else
    begin
      AResult := TRttiUtils.CreateObject(ARTTIType.QualifiedName);
      GetDefaultSerializer.DeserializeObject(AResp.Content, AResult.AsObject);
    end;
  end
  else
    // IMVCRESTResponse
    if ARTTIType.QualifiedName = TRttiUtils.GlContext.GetType(TypeInfo(IMVCRESTResponse))
      .QualifiedName then
    begin
      AResult := AResult.From(AResp)
    end
    else // else a simple Content
    begin
      AResult := AResp.Content
    end;
end;

function TRESTAdapter<T>.ResourcesService: T;
var
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(T);
  if QueryInterface(GetTypeData(pInfo).Guid, Result) <> 0 then
  begin
    raise Exception.Create('RESTAdapter is unable to cast to its interface');
  end;
end;

{ RESTResourceAttribute }

constructor RESTResourceAttribute.Create(AMVCHTTPMethod: TMVCHTTPMethodType;
AURL: string);
begin
  fURL := AURL;
  fHTTPMethodType := AMVCHTTPMethod;
end;

procedure RESTResourceAttribute.SetHTTPMethodType
  (const Value: TMVCHTTPMethodType);
begin
  fHTTPMethodType := Value;
end;

procedure RESTResourceAttribute.SetURL(const Value: string);
begin
  fURL := Value;
end;

{ BodyAttribute }

constructor BodyAttribute.Create(AOwnsObject: Boolean);
begin
  inherited Create;
  FOwnsObject := AOwnsObject;
end;

procedure BodyAttribute.SetOwnsObject(const Value: Boolean);
begin
  FOwnsObject := Value;
end;

{ ParamAttribute }

constructor ParamAttribute.Create(AParamMatch: string;
AParamType, ACustomFormat: string);
begin
  inherited Create;
  fParamMatch := AParamMatch;
  fParamType := AParamType;
  fCustomFormat := ACustomFormat;
end;

function ParamAttribute.FmtParamMatch: string;
begin
  Result := '{' + ParamMatch + '}';
end;

procedure ParamAttribute.SetCustomFormat(const Value: string);
begin
  fCustomFormat := Value;
end;

procedure ParamAttribute.SetParamMatch(const Value: string);
begin
  fParamMatch := Value;
end;

procedure ParamAttribute.SetParamType(const Value: string);
begin
  fParamType := Value;
end;

{ HeadersAttribute }

constructor HeadersAttribute.Create(AKey: string; AValue: string);
begin
  fKey := AKey;
  fValue := AValue;
end;

procedure HeadersAttribute.SetKey(const Value: string);
begin
  fKey := Value;
end;

procedure HeadersAttribute.SetValue(const Value: string);
begin
  fValue := Value;
end;

{ TAsynchRequest }

constructor TAsynchRequest.Create(ASuccProc: TProc<TValue>; AProcErr: TProc<Exception>; ASynchronized: Boolean);
begin
  inherited Create;
  fSuccessProc := ASuccProc;
  fErrorProc := AProcErr;
  fSynchronized := ASynchronized;
end;

function TAsynchRequest.GetErrorProc: TProc<Exception>;
begin
  Result := fErrorProc;
end;

function TAsynchRequest.GetSuccessProc: TProc<TValue>;
begin
  Result := fSuccessProc;
end;

function TAsynchRequest.GetSynchronized: Boolean;
begin
  Result := fSynchronized;
end;

procedure TAsynchRequest.SetErrorProc(const Value: TProc<Exception>);
begin
  fErrorProc := Value;
end;

procedure TAsynchRequest.SetSuccessProc(const Value: TProc<TValue>);
begin
  fSuccessProc := Value;
end;

procedure TAsynchRequest.SetSynchronized(const Value: Boolean);
begin
  fSynchronized := Value;
end;

{ MappingAttribute }

constructor MappingAttribute.Create(AClass: TClass);
begin
  fClass := AClass;
end;

function MappingAttribute.GetType: TRttiType;
begin
  Result := TRttiUtils.GlContext.GetType(fClass);
end;

end.
