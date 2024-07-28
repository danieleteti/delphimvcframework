// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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
  public
    constructor Create(aMVCHTTPMethod: TMVCHTTPMethodType; aURL: string);
    property URL: string read fURL;
    property HTTPMethodType: TMVCHTTPMethodType read fHTTPMethodType;
  end;

  BodyAttribute = class(TCustomAttribute)
  private
    fOwnsObject: Boolean;
  public
    constructor Create(aOwnsObject: Boolean = True);
    property OwnsObject: Boolean read fOwnsObject;
  end;

  ParamAttribute = class(TCustomAttribute)
  private
    fParamType: string;
    fCustomFormat: string;
    fParamMatch: string;
  public
    constructor Create(aParamMatch: string; aParamType: string = ''; aCustomFormat: string = '');
    property ParamMatch: string read fParamMatch;
    property ParamType: string read fParamType;
    property CustomFormat: string read fCustomFormat;
    function FmtParamMatch: string;
  end;

  HeadersAttribute = class(TCustomAttribute)
  private
    fKey: string;
    fValue: string;
  public
    constructor Create(aKey: string; aValue: string);
    property Key: string read fKey;
    property Value: string read fValue;
  end;

  MappingAttribute = class(TCustomAttribute)
  private
    fClass: TClass;
  public
    constructor Create(aClass: TClass);
    function GetType: TRttiType;
  end;

  IRESTAdapter<T> = interface
    ['{AAA41F40-69DB-419B-9922-F59F990CBDB5}']
    function ResourcesService: T;
    procedure AddRequestHeaders(aObj: TRttiObject);
    procedure AddRequestHeader(aKey: string; aValue: string);
    procedure MapResult(aResp: IMVCRESTResponse; aMethod: TRttiMethod; aRttiType: TRttiType; out aResult: TValue);
  end;

  TRESTAdapter<T: IInvokable> = class(TVirtualInterface, IRESTAdapter<T>)
  private
    fRESTClient: IMVCRESTClient;
  protected
    procedure DoInvoke(aMethod: TRttiMethod; const aArgs: TArray<TValue>;  out aResult: TValue);
    procedure AddRequestHeaders(aObj: TRttiObject);
    procedure AddRequestHeader(aKey: string; aValue: string);
    procedure MapResult(aResp: IMVCRESTResponse; aMethod: TRttiMethod; aRttiType: TRttiType; out aResult: TValue);
    function GetURL(aMethod: TRttiMethod; const aArgs: TArray<TValue>): string;
    function GetBodyAsString(aMethod: TRttiMethod; const aArgs: TArray<TValue>): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Build(aRESTClient: IMVCRESTClient): T; overload;
    function Build(const aServerName: string; const aServerPort: Word = 80): T; overload;
    function ResourcesService: T;
    property RESTClient: IMVCRESTClient read fRESTClient write fRESTClient;
  end;

  IAsynchRequest = interface
    ['{3E720356-F2B7-4C32-8051-B7723263740F}']
    procedure SetErrorProc(const aValue: TProc<Exception>);
    procedure SetSuccessProc(const aValue: TProc<TValue>);
    procedure SetSynchronized(const aValue: Boolean);

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
    procedure SetErrorProc(const aValue: TProc<Exception>);
    procedure SetSuccessProc(const aValue: TProc<TValue>);
    procedure SetSynchronized(const aValue: Boolean);
    function GetErrorProc: TProc<Exception>;
    function GetSuccessProc: TProc<TValue>;
    function GetSynchronized: Boolean;
  public
    constructor Create(aSuccProc: TProc<TValue> = nil; aProcErr: TProc<Exception> = nil;
      aSynchronized: Boolean = False);
    property SuccessProc: TProc<TValue> read GetSuccessProc write SetSuccessProc;
    property ErrorProc: TProc<Exception> read GetErrorProc write SetErrorProc;
    property Synchronized: Boolean read GetSynchronized write SetSynchronized;
  end;

implementation

uses
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Rtti.Utils,
  MVCFramework.DuckTyping,
  Generics.Collections,
  MVCFramework.RESTClient,
  System.NetConsts;

{ TRESTAdapter }

function TRESTAdapter<T>.Build(aRESTClient: IMVCRESTClient): T;
begin
  fRESTClient := aRESTClient;
  Result := ResourcesService;
end;

procedure TRESTAdapter<T>.AddRequestHeader(aKey, aValue: string);
begin
  if CompareText(aKey, sAccept) = 0 then
    fRESTClient.Accept(aValue)
  else if CompareText(aKey, sAcceptCharset) = 0 then
    fRESTClient.AcceptCharset(aValue)
  else if CompareText(aKey, sAcceptEncoding) = 0 then
    fRESTClient.AcceptEncoding(aValue)
  else
    fRESTClient.AddHeader(aKey, aValue);
end;

procedure TRESTAdapter<T>.AddRequestHeaders(aObj: TRttiObject);
var
  lAttr: TCustomAttribute;
begin
  for lAttr in aObj.GetAttributes do
  begin
    if lAttr is HeadersAttribute then
      AddRequestHeader(HeadersAttribute(lAttr).Key, HeadersAttribute(lAttr).Value);
  end;
end;

function TRESTAdapter<T>.Build(const aServerName: string; const aServerPort: Word): T;
begin
  Result := Build(TMVCRESTClient.New.BaseURL(aServerName, aServerPort));
end;

constructor TRESTAdapter<T>.Create;
begin
  inherited Create(TypeInfo(T), DoInvoke);
end;

destructor TRESTAdapter<T>.Destroy;
begin
  // Ezequiel J. M�ller (If it is created outside, it must be destroyed out)
  // d.spinetti added RESTClientOwner to manage desctruction of RESTClient and free its associated memory
//  if RESTClientOwner and Assigned(fRESTClient) then
//    fRESTClient.Free;
  inherited;
end;

procedure TRESTAdapter<T>.DoInvoke(aMethod: TRttiMethod; const aArgs: TArray<TValue>; out aResult: TValue);
var
  lResp: IMVCRESTResponse;
  lRestResourceAttr: RESTResourceAttribute;
  lURL: string;
  lBody: string;
  lAsyncClass: IAsynchRequest;
  lMappingAttr: MappingAttribute;
begin
  // Implementation of RESTClient DoGet DoPut ecc...
  if not TRttiUtils.HasAttribute<RESTResourceAttribute>(aMethod, lRestResourceAttr) then
    raise Exception.CreateFmt('No REST Resource specified in method %s', [aMethod.Name]);

  // headers can be more than one
  AddRequestHeaders(TRttiUtils.GlContext.GetType(TypeInfo(T)));
  // aMethod
  AddRequestHeaders(aMethod);

  // lURL and lBody
  lURL := GetURL(aMethod, aArgs);
  lBody := GetBodyAsString(aMethod, aArgs);

  // Asynch way to do
  if aArgs[Length(aArgs) - 1].TryAsType<IAsynchRequest>(lAsyncClass) then
  begin
    fRESTClient.Async(
      procedure(ARESTResponse: IMVCRESTResponse)
      var
        lResValue: TValue;
      begin
        if TRttiUtils.HasAttribute<MappingAttribute>(aMethod, lMappingAttr) then
          MapResult(ARESTResponse, aMethod, lMappingAttr.GetType, lResValue)
        else
          lResValue := TValue.From(ARESTResponse);
        if Assigned(lAsyncClass.SuccessProc) then
          lAsyncClass.SuccessProc(lResValue);
      end,
      lAsyncClass.ErrorProc,
      lAsyncClass.Synchronized);
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
    httpPATCH:
      lResp := fRESTClient.Patch(lURL, lBody);
    httpOPTIONS:
      lResp := fRESTClient.Options(lURL);
    httpHEAD:
      lResp := fRESTClient.Head(lURL);
    else
    begin
      raise Exception.Create('Invalid HTTP method');
    end;
  end;

  // if is a procedure no need a return type
  if Assigned(aMethod.ReturnType) then
    MapResult(lResp, aMethod, aMethod.ReturnType, aResult);
end;

function TRESTAdapter<T>.GetBodyAsString(aMethod: TRttiMethod; const aArgs: TArray<TValue>): string;
var
  lParameters: TArray<TRttiParameter>;
  I: Integer;
  lParameter: TRttiParameter;
  lParam: BodyAttribute;
  lAttrListOf: MVCListOfAttribute;
  lArg: TValue;
begin
  lParameters := aMethod.GetParameters;
  for I := 0 to Length(lParameters) - 1 do
  begin
    lParameter := lParameters[I];
    // lArg := aArgs[I+1] because
    // aArgs	RTTI for the arguments of the interface method that has been called. The first argument (located at index 0) represents the interface instance itself.
    lArg := aArgs[I + 1];
    if TRttiUtils.HasAttribute<BodyAttribute>(lParameter, lParam) then
      try
        if lArg.IsObject then
        begin

          if TRttiUtils.HasAttribute<MVCListOfAttribute>(aMethod, lAttrListOf) then
            Exit(
              GetDefaultSerializer.SerializeCollection(lArg.AsObject)
            { Mapper.ObjectListToJSONArrayString(WrapAsList(lArg.AsObject), True) }
              )
          else
            Exit(
              GetDefaultSerializer.SerializeObject(lArg.AsObject)
            { Mapper.ObjectToJSONObjectString(lArg.AsObject) }
              );
        end
        else
          Exit(TRttiUtils.TValueAsString(lArg, '', ''));
      finally
        if lParam.OwnsObject and lArg.IsObject then
        begin

{$HINTS OFF}
          lArg.AsObject.Free;

{$HINTS ON}
        end;
      end;
  end;
end;

function TRESTAdapter<T>.GetURL(aMethod: TRttiMethod; const aArgs: TArray<TValue>): string;
var
  lRestResourceAttr: RESTResourceAttribute;
  lURL: string;
  lSplitUrl: TArray<string>;
  lURLDict: TDictionary<string, string>;
  lSplit: string;
  lParameters: TArray<TRttiParameter>;
  I: Integer;
  lParameter: TRttiParameter;
  lParam: ParamAttribute;
  lArg: TValue;
begin
  lRestResourceAttr := TRttiUtils.GetAttribute<RESTResourceAttribute>(aMethod);
  lURL := lRestResourceAttr.URL;
  lSplitUrl := lURL.Split([URL_SEPARATOR]);
  lURLDict := TDictionary<string, string>.Create;
  try
    for lSplit in lSplitUrl do
    begin
      if not lSplit.IsEmpty then
      begin
        lURLDict.Add(lSplit, lSplit);
      end;
    end;
    lParameters := aMethod.GetParameters;
    // lArg := aArgs[I+1] because
    // aArgs	RTTI for the arguments of the interface method that has been called. The first argument (located at index 0) represents the interface instance itself.
    for I := 0 to Length(lParameters) - 1 do
    begin
      lParameter := lParameters[I];
      lArg := aArgs[I + 1];
      if TRttiUtils.HasAttribute<ParamAttribute>(lParameter, lParam) then
        lURLDict[lParam.FmtParamMatch] := TRttiUtils.TValueAsString(lArg,
          lParam.ParamType, lParam.CustomFormat);
    end;

    for lSplit in lSplitUrl do
    begin
      if not lSplit.IsEmpty then
      begin
        Result := Result + URL_SEPARATOR + lURLDict[lSplit];
      end;
    end;

    if lURL.EndsWith(URL_SEPARATOR) and not (Result.EndsWith(URL_SEPARATOR)) then
    begin
      Result := Result + URL_SEPARATOR;
    end;
  finally
    lURLDict.Free;
  end;
end;

procedure TRESTAdapter<T>.MapResult(aResp: IMVCRESTResponse; aMethod: TRttiMethod; aRttiType: TRttiType;
  out aResult: TValue);
var
  lAttrListOf: MVCListOfAttribute;
begin
  if aRttiType.TypeKind = tkClass then
  begin
    // ListOf
    if TRttiUtils.HasAttribute<MVCListOfAttribute>(aMethod, lAttrListOf) then
    begin
      aResult := TRttiUtils.CreateObject(aRttiType.QualifiedName);
      GetDefaultSerializer.DeserializeCollection(aResp.Content, aResult.AsObject, lAttrListOf.Value);
    end
    // JSONValue
    else if aRttiType.AsInstance.MetaclassType.InheritsFrom(TJsonBaseObject) then
    begin
      aResult := TJsonBaseObject.Parse(aResp.Content);
    end
    else
    // Object
    begin
      aResult := TRttiUtils.CreateObject(aRttiType.QualifiedName);
      GetDefaultSerializer.DeserializeObject(aResp.Content, aResult.AsObject);
    end;
  end
  // IRESTResponse
  else if aRttiType.QualifiedName = TRttiUtils.GlContext.GetType(TypeInfo(IMVCRESTResponse)).QualifiedName then
  begin
    aResult := aResult.From(aResp);
  end
  else // else a simple Content
  begin
    aResult := aResp.Content;
  end;
end;

function TRESTAdapter<T>.ResourcesService: T;
var
  lTypeInfo: PTypeInfo;
begin
  lTypeInfo := TypeInfo(T);
  if QueryInterface(GetTypeData(lTypeInfo).Guid, Result) <> 0 then
  begin
    raise Exception.Create('RESTAdapter is unable to cast to its interface');
  end;
end;

{ RESTResourceAttribute }

constructor RESTResourceAttribute.Create(aMVCHTTPMethod: TMVCHTTPMethodType; aURL: string);
begin
  fURL := aURL;
  fHTTPMethodType := aMVCHTTPMethod;
end;

{ BodyAttribute }

constructor BodyAttribute.Create(aOwnsObject: Boolean);
begin
  inherited Create;
  fOwnsObject := aOwnsObject;
end;

{ ParamAttribute }

constructor ParamAttribute.Create(aParamMatch, aParamType, aCustomFormat: string);
begin
  inherited Create;
  fParamMatch := aParamMatch;
  fParamType := aParamType;
  fCustomFormat := aCustomFormat;
end;

function ParamAttribute.FmtParamMatch: string;
begin
  Result := '{' + ParamMatch + '}';
end;

{ HeadersAttribute }

constructor HeadersAttribute.Create(aKey: string; aValue: string);
begin
  fKey := aKey;
  fValue := aValue;
end;

{ TAsynchRequest }

constructor TAsynchRequest.Create(aSuccProc: TProc<TValue> = nil; aProcErr: TProc<Exception> = nil;
  aSynchronized: Boolean = False);
begin
  inherited Create;
  fSuccessProc := aSuccProc;
  fErrorProc := aProcErr;
  fSynchronized := aSynchronized;
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

procedure TAsynchRequest.SetErrorProc(const aValue: TProc<Exception>);
begin
  fErrorProc := aValue;
end;

procedure TAsynchRequest.SetSuccessProc(const aValue: TProc<TValue>);
begin
  fSuccessProc := aValue;
end;

procedure TAsynchRequest.SetSynchronized(const aValue: Boolean);
begin
  fSynchronized := aValue;
end;

{ MappingAttribute }

constructor MappingAttribute.Create(aClass: TClass);
begin
  fClass := aClass;
end;

function MappingAttribute.GetType: TRttiType;
begin
  Result := TRttiUtils.GlContext.GetType(fClass);
end;

end.
