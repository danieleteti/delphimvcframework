// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.JSONRPC;

{
  JSON-RPC over HTTP implemented as described here
  https://www.jsonrpc.org/historical/json-rpc-over-http.html
}

interface

uses
  System.Classes,
  Data.DB,
  jsondataobjects,
  MVCFramework,
  MVCFramework.Commons,
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JsonDataObjects,
  System.SysUtils;

const
  JSONRPC_VERSION = '2.0';
  JSONRPC_HEADER = 'jsonrpc';
  JSONRPC_METHOD = 'method';
  JSONRPC_PARAMS = 'params';
  JSONRPC_ID = 'id';
  JSONRPC_RESULT = 'result';
  JSONRPC_ERROR = 'error';
  JSONRPC_CODE = 'code';
  JSONRPC_MESSAGE = 'message';
  JSONRPC_DATA = 'data';

const
  JSONRPC_HOOKS_ON_BEFORE_ROUTING = 'OnBeforeRoutingHook';
  JSONRPC_HOOKS_ON_BEFORE_CALL = 'OnBeforeCallHook';
  JSONRPC_HOOKS_ON_AFTER_CALL = 'OnAfterCallHook';
  JSONRPC_HOOKS_METHOD_NAMES: array [0 .. 2] of string = (JSONRPC_HOOKS_ON_BEFORE_ROUTING, JSONRPC_HOOKS_ON_BEFORE_CALL,
    JSONRPC_HOOKS_ON_AFTER_CALL);

  {
    http://www.jsonrpc.org/historical/json-rpc-over-http.html#response-codes
    HTTP Status	code	message
    500	-32700	Parse error.
    400	-32600	Invalid Request.
    404	-32601	Method not found.
    500	-32602	Invalid params.
    500	-32603	Internal error.
    500	-32099..-32000	Server error.
  }
  JSONRPC_ERR_PARSE_ERROR = -32700;
  JSONRPC_ERR_INVALID_REQUEST = -32600;
  JSONRPC_ERR_METHOD_NOT_FOUND = -32601;
  JSONRPC_ERR_INVALID_PARAMS = -32602;
  JSONRPC_ERR_INTERNAL_ERROR = -32603;
  JSONRPC_ERR_SERVER_ERROR_LOWERBOUND = -32099;
  JSONRPC_ERR_SERVER_ERROR_UPPERBOUND = -32000;
  JSONRPC_USER_ERROR = JSONRPC_ERR_SERVER_ERROR_LOWERBOUND;

type
  TJSONRPCHTTPVerb = (jrpcDefault, jrpcGET, jrpcPOST);

  MVCJSONRPCAllowGET = class(TCustomAttribute)
  end;

  IMVCJSONRPCMessage = interface
    ['{73B8D463-75E1-404B-8437-EF4B3C950D2F}']
    function AsJSONRPCMessage: string;
  end;

  TMVCJSONRPCMessage = class abstract(TInterfacedObject, IMVCJSONRPCMessage)
  private
    fJSON: TJDOJsonObject;
  protected
    class procedure CheckVersion(const aJSON: TJDOJsonObject);
    class procedure CheckMethod(const aJSON: TJDOJsonObject);
    class procedure CheckID(const aJSON: TJDOJsonObject; out aIsNotification: Boolean);
    constructor Create; overload;
    procedure Build(const aJSON: TJDOJsonObject); virtual; abstract;
    { IMVCJSONRPCMessage }
    function AsJSONRPCMessage: string;
    function AsJSON: TJDOJsonObject; virtual;
  end;

  IJSONRPCObject = interface
    ['{98E161EE-B106-4023-8722-3C2CB1B4CE87}']
    procedure SetJsonString(const Value: string);
    function GetJSONString: string;
    function GetJSON: TJDOJsonObject;
    function ToString(const Compact: Boolean): string;
    procedure SetJSON(const Value: TJDOJsonObject);
    property AsJSON: TJDOJsonObject read GetJSON write SetJSON;
    property AsJSONString: string read GetJSONString write SetJsonString;
  end;

  TJSONRPCObject = class(TInterfacedObject, IJSONRPCObject)
  protected
    procedure SetJsonString(const Value: string); virtual;
    function GetJSONString: string; virtual;
    function ToString(const Compact: Boolean): string; reintroduce; virtual;
    function GetJSON: TJDOJsonObject; virtual;
    procedure SetJSON(const Value: TJDOJsonObject); virtual;
    property AsJSON: TJDOJsonObject read GetJSON write SetJSON;
    property AsJSONString: string read GetJSONString write SetJsonString;
  public
    constructor Create; virtual;
  end;

  TJSONRPCParamDataType = (pdtString, pdtInteger, pdtLongInteger, pdTJDOJsonObject, pdtJSONArray, pdtBoolean, pdtDate,
    pdtTime, pdtDateTime, pdtFloat, pdtObject, pdtRecordOrArrayOfRecord);

  TJSONRPCRequestParams = class
  private
    function GetItem(const Index: Integer): TValue;
    function GetItemDataType(const Index: Integer): TJSONRPCParamDataType;
  protected
    fParamValues: TList<TValue>;
    fParamNames: TList<string>;
    fParamTypes: TList<TJSONRPCParamDataType>;
  private
    procedure CheckNotNames;
    procedure CheckBalancedParams;
    function GetItemName(const Index: Integer): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    property Items[const index: Integer]: TValue read GetItem; default;
    property ItemsType[const index: Integer]: TJSONRPCParamDataType read GetItemDataType;
    property ItemsName[const index: Integer]: string read GetItemName;
    function ToArray: TArray<TValue>;
    procedure Add(const Value: string); overload;
    procedure Add(const Value: Integer); overload;
    procedure Add(const Value: TJDOJsonObject); overload;
    procedure Add(const Value: TJDOJsonArray); overload;
    procedure Add(const Value: TObject); overload;
    procedure Add(const Value: Boolean); overload;
    procedure Add(const Value: TDate); overload;
    procedure Add(const Value: TTime); overload;
    procedure Add(const Value: TDateTime); overload;
    procedure Add(const Value: Double); overload;
    procedure Add(const Value: TValue; const ParamType: TJSONRPCParamDataType); overload;
    procedure AddByName(const Name: string; const Value: string); overload;
    procedure AddByName(const Name: string; const Value: Integer); overload;
    procedure AddByName(const Name: string; const Value: TJDOJsonObject); overload;
    procedure AddByName(const Name: string; const Value: TJDOJsonArray); overload;
    procedure AddByName(const Name: string; const Value: TObject); overload;
    procedure AddByName(const Name: string; const Value: Boolean); overload;
    procedure AddByName(const Name: string; const Value: TDate); overload;
    procedure AddByName(const Name: string; const Value: TTime); overload;
    procedure AddByName(const Name: string; const Value: TDateTime); overload;
    procedure AddByName(const Name: string; const Value: Double); overload;
    procedure AddByName(const Name: string; const Value: TValue; const ParamType: TJSONRPCParamDataType); overload;
  end;

  IJSONRPCNotification = interface(IJSONRPCObject)
    ['{FAA65A29-3305-4303-833E-825BDBD3FF7F}']
    procedure SetMethod(const Value: string);
    procedure FillParameters(const JSON: TJDOJsonObject; const RTTIMethod: TRTTIMethod);
    function GetMethod: string;
    function GetParams: TJSONRPCRequestParams;
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONRPCRequestParams read GetParams;
  end;

  TJSONRPCNotification = class(TJSONRPCObject, IJSONRPCObject, IJSONRPCNotification)
  protected
    FMethod: string;
    FParams: TJSONRPCRequestParams;
    procedure FillParameters(const JSON: TJDOJsonObject; const RTTIMethod: TRTTIMethod);
    procedure SetMethod(const Value: string);
    function GetMethod: string;
    function GetParams: TJSONRPCRequestParams;
    function GetJSON: TJDOJsonObject; override;
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONRPCRequestParams read GetParams;
  public
    constructor Create; overload; override;
    constructor Create(const aMethod: string); reintroduce; overload;
    destructor Destroy; override;
  end;

{$SCOPEDENUMS ON}

  TJSONRPCRequestType = (Request, Notification);

  IJSONRPCRequest = interface(IJSONRPCNotification)
    ['{D8318032-0261-4273-B99D-121899AD52FB}']
    function GetRequestType: TJSONRPCRequestType;
    function GetID: TValue;
    procedure SetID(const Value: TValue);
    property RequestType: TJSONRPCRequestType read GetRequestType;
    property RequestID: TValue read GetID write SetID;
  end;

  TJSONRPCRequest = class(TJSONRPCNotification, IJSONRPCRequest)
  private
    FID: TValue;
    function GetRequestType: TJSONRPCRequestType;
    function GetID: TValue;
  protected
    procedure SetJSON(const JSON: TJDOJsonObject); override;
    function GetJSON: TJDOJsonObject; override;
    procedure SetID(const Value: TValue);
    property RequestType: TJSONRPCRequestType read GetRequestType;
    property RequestID: TValue read GetID write SetID;
  public
    constructor Create(const aID: TValue; const aMethod: string); overload; virtual;
    constructor Create(const aID: TValue); overload; virtual;
    constructor Create; reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

  TJSONRPCResponseError = class
  private
    FCode: Integer;
    FMessage: string;
    FData: TValue;
    procedure SetCode(const Value: Integer);
    procedure SetMessage(const Value: string);
    procedure SetData(const Value: TValue);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Code: Integer read FCode write SetCode;
    property ErrMessage: string read FMessage write SetMessage;
    property Data: TValue read fData write SetData;
  end;

  IJSONRPCResponse = interface(IJSONRPCObject)
    ['{69B43409-14DC-4A36-9E12-425A1626FF3C}']
    function GetID: TValue;
    procedure SetID(const Value: TValue);
    function GetResult: TValue;
    procedure SetResult(const Value: TValue);
    function GetError: TJSONRPCResponseError;
    procedure SetError(const Value: TJSONRPCResponseError);
    function IsError: Boolean;
    function ResultAsJSONObject: TJDOJsonObject;
    function ResultAsJSONArray: TJDOJsonArray;
    procedure ResultAs(Obj: TObject);
    property Result: TValue read GetResult write SetResult;
    property Error: TJSONRPCResponseError read GetError write SetError;
    property RequestID: TValue read GetID write SetID;
  end;

  TJSONRPCResponse = class(TJSONRPCObject, IJSONRPCResponse)
  private
    FResult: TValue;
    FError: TJSONRPCResponseError;
    FID: TValue;
  protected
    function GetResult: TValue;
    function GetJSON: TJDOJsonObject; override;
    procedure SetJSON(const JSON: TJDOJsonObject); override;
    procedure SetID(const Value: TValue);
    procedure SetResult(const Value: TValue);
    procedure SetError(const Value: TJSONRPCResponseError);
    function GetError: TJSONRPCResponseError;
    function GetID: TValue;
    function ResultAsJSONObject: TJDOJsonObject;
    function ResultAsJSONArray: TJDOJsonArray;
    procedure ResultAs(Obj: TObject);
    function IsError: Boolean;
    property Result: TValue read GetResult write SetResult;
    property Error: TJSONRPCResponseError read GetError write SetError;
    property RequestID: TValue read GetID write SetID;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJSONRPCNullResponse = class(TJSONRPCObject, IJSONRPCResponse)
  private
    FError: TJSONRPCResponseError;
    procedure RaiseErrorForNullObject;
  protected
    function ToString(const Compact: Boolean): string; override;
    function GetJSONString: string; override;
    procedure SetJsonString(const Value: string); override;
    function GetJSON: TJDOJsonObject; override;
    procedure SetJSON(const JSON: TJDOJsonObject); override;
    procedure SetID(const Value: TValue);
    procedure SetResult(const Value: TValue);
    procedure SetError(const Value: TJSONRPCResponseError);
    function GetError: TJSONRPCResponseError;
    function GetID: TValue;
    function GetResult: TValue;
    procedure CheckForError;
    function ResultAsJSONObject: TJDOJsonObject;
    function ResultAsJSONArray: TJDOJsonArray;
    procedure ResultAs(Obj: TObject);
    function IsError: Boolean;
    property Result: TValue read GetResult write SetResult;
    property Error: TJSONRPCResponseError read GetError write SetError;
    property RequestID: TValue read GetID write SetID;
  end;

  EMVCJSONRPCInvalidVersion = class(Exception)

  end;

  EMVCJSONRPCException = class(Exception)

  end;

  EMVCJSONRPCRemoteException = class(EMVCJSONRPCException)
  private
    fErrData: TValue;
    fErrCode: Integer;
    fErrMessage: String;
  public
    constructor Create(const ErrCode: Integer; const ErrMessage: String; const ErrData: TValue); overload;
    constructor Create(const ErrCode: Integer; const ErrMessage: String); overload;
    property Data: TValue read fErrData;
    property ErrCode: Integer read fErrCode;
    property ErrMessage: String read fErrMessage;
  end;

  EMVCJSONRPCProtocolException = class(EMVCJSONRPCRemoteException)

  end;

  EMVCJSONRPCErrorResponse = class abstract(Exception)
  protected
    fJSONRPCErrorCode: Integer;
    fJSONRPCErrorData: TValue;
  public
    property JSONRPCErrorCode: Integer read fJSONRPCErrorCode;
    property JSONRPCErrorData: TValue read fJSONRPCErrorData;
  end;

  EMVCJSONRPCError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const ErrCode: Integer; const ErrMsg: string); overload;
    constructor Create(const ErrCode: Integer; const ErrMsg: string; const Data: TValue); overload;
    constructor CreateFmt(const ErrCode: Integer; const ErrMsg: string; const Args: array of const);
  end;

  EMVCJSONRPCParseError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create;
    procedure AfterConstruction; override;
  end;

  EMVCJSONRPCInvalidRequest = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const Message: string = ''); overload;
    procedure AfterConstruction; override;
  end;

  EMVCJSONRPCMethodNotFound = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const MethodName: string);
    procedure AfterConstruction; override;
  end;

  EMVCJSONRPCInvalidParams = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const Message: string);
    procedure AfterConstruction; override;
  end;

  EMVCJSONRPCInternalError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create;
    procedure AfterConstruction; override;
  end;

  { -32000 to -32099	Server error	Reserved for implementation-defined server-errors. }
  EMVCJSONRPCServerError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const JSONRPCError: Integer; const Message: string);
  end;

  TMVCJSONObject = TJDOJsonObject;
  TMVCJSONArray = TJDOJsonArray;

  TMVCJSONRPCController = class(TMVCController)
  private
    fExceptionHandler: TMVCJSONRPCExceptionHandlerProc;
    fSerializer: TMVCJsonDataObjectsSerializer;
    fRPCInstance: TObject;
    fOwsRPCInstance: Boolean;
    function GetSerializer: TMVCJsonDataObjectsSerializer;
    function GetDeclaredMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
    function GetInheritedMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
  protected
    function CreateError(const RequestID: TValue; const ErrorCode: Integer;
      const Message: string): TJDOJsonObject; overload;
    function CreateError(const RequestID: TValue; const ErrorCode: Integer;
      const Message: string; const Data: TValue): TJDOJsonObject; overload;
    function CreateResponse(const RequestID: TValue; const Value: TValue): TJSONRPCResponse;
    function CreateRequest(const JSON: TJDOJsonObject): IJSONRPCRequest;
    function JSONObjectAs<T: class, constructor>(const JSON: TJDOJsonObject): T;
    function CanBeRemotelyInvoked(const RTTIMethod: TRTTIMethod): Boolean;
    procedure ForEachInvokableMethod(const aProc: TProc<TRTTIMethod>);
    procedure TryToCallMethod(const aRTTIType: TRttiType; const MethodName: string; const Parameter: TJDOJsonObject);
    function GetJSONRPCPayload(const Request: TMVCWebRequest): TJsonObject;

    function InvokeMethod(
      const fRPCInstance: TObject;
      const RTTIType: TRTTIType;
      const RTTIMethod: TRTTIMethod;
      const JSON: TJSONObject;
      out BeforeCallHookHasBeenInvoked: Boolean): TValue;
  public
    [MVCPath]
    [MVCHTTPMethods([httpPOST, httpGET])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure Index; virtual;

    [MVCPath('/describe')]
    [MVCHTTPMethods([httpGET])]
    procedure GetPublishedMethodList; virtual;

    [MVCPath('/proxy')]
    [MVCHTTPMethods([httpGET])]
    procedure GetProxyCode; virtual;
	
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TMVCJSONRPCPublisher = class(TMVCJSONRPCController)
  public
    constructor Create(const RPCInstance: TObject; const Owns: Boolean = True; ExceptionHandler: TMVCJSONRPCExceptionHandlerProc = nil);
        reintroduce; overload;
  end;

  TJSONRPCProxyGenerator = class abstract
  public
    constructor Create; virtual;
    procedure StartGeneration(const aClassName: string); virtual;
    procedure EndGeneration; virtual;
    procedure VisitMethod(const aRTTIMethod: TRTTIMethod); virtual; abstract;
    function GetCode: string; virtual; abstract;
  end;

  TJSONRPCProxyGeneratorClass = class of TJSONRPCProxyGenerator;

  TJSONUtilsHelper = record helper for TJSONUtils
    class function JSONObjectToRecord<T: record >(const JSONRPCResponse: IInterface): T; overload; static;
    class function JSONArrayToArrayOfRecord<T: record >(const JSONRPCResponse: IInterface): TArray<T>; overload; static;
  end;

procedure RegisterJSONRPCProxyGenerator(const aLanguage: string; const aClass: TJSONRPCProxyGeneratorClass);

implementation

uses
  MVCFramework.Serializer.Intf,
  MVCFramework.Rtti.Utils,
  MVCFramework.Logger,
  System.TypInfo,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.JsonDataObjects.CustomTypes;

const
  CALL_TYPE: array [mkProcedure .. mkFunction] of string = ('PROCEDURE', 'FUNCTION');

var
  GProxyGeneratorsRegister: TDictionary<string, TJSONRPCProxyGeneratorClass>;

function IsReservedMethodName(const MethodName: string): Boolean;
var
  lMethod: string;
begin
  Result := False;
  for lMethod in JSONRPC_HOOKS_METHOD_NAMES do
  begin
    if SameText(MethodName, lMethod) then
    begin
      Exit(True);
    end;
  end;
end;

procedure AppendTValueToJsonArray(const Value: TValue; const ParamType: TJSONRPCParamDataType;
  const JSONArr: TJDOJsonArray);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lJArr: TJDOJsonArray;
  LJObj: TJDOJsonObject;
  lOrdinalValue: Int64;
  I: Integer;
begin
  case ParamType of
    pdtInteger:
      begin
        JSONArr.Add(Value.AsInteger);
      end;
    pdtFloat:
      begin
        JSONArr.Add(Value.AsExtended);
      end;
    pdtDateTime:
      begin
        JSONArr.Add(DateTimeToISOTimeStamp(FloatToDateTime(Value.AsExtended)));
      end;
    pdtDate:
      begin
        JSONArr.Add(DateToISODate(FloatToDateTime(Value.AsExtended)));
      end;
    pdtTime:
      begin
        JSONArr.Add(TimeToISOTime(FloatToDateTime(Value.AsExtended)));
      end;
    pdtString:
      begin
        JSONArr.Add(Value.AsString);
      end;
    pdtLongInteger:
      begin
        JSONArr.Add(Value.AsInt64);
      end;
    pdtBoolean:
      begin
        if not Value.TryAsOrdinal(lOrdinalValue) then
        begin
          raise EMVCException.Create('Invalid ordinal parameter');
        end;
        JSONArr.Add(lOrdinalValue = 1);
      end;
    pdTJDOJsonObject:
      begin
        JSONArr.Add((Value.AsObject as TJDOJsonObject).Clone as TJDOJsonObject);
      end;
    pdtJSONArray:
      begin
        JSONArr.Add((Value.AsObject as TJDOJsonArray).Clone as TJDOJsonArray);
      end;
    pdtObject:
      begin
        if Value.AsObject is TDataSet then
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            lJArr := TJDOJsonArray.Create;
            JSONArr.Add(lJArr);
            lSer.DataSetToJsonArray(TDataSet(Value.AsObject), lJArr, TMVCNameCase.ncLowerCase, []);
          finally
            lSer.Free;
          end
        end
        else
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            LJObj := lSer.SerializeObjectToJSON(Value.AsObject, TMVCSerializationType.stProperties, [], nil);
            JSONArr.Add(LJObj);
          finally
            lSer.Free;
          end;
        end;
      end;
    pdtRecordOrArrayOfRecord:
      begin
        lSer := TMVCJsonDataObjectsSerializer.Create;
        try
          if Value.IsArray then
          begin
            lJArr := TJsonArray.Create;
            JSONArr.Add(lJArr);
            for i := 0 to Value.GetArrayLength - 1 do
            begin
              lSer.RecordToJsonObject(
                Value.GetReferenceToRawArrayElement(i),
                Value.GetArrayElement(i).TypeInfo,
                lJArr.AddObject,
                TMVCSerializationType.stFields,
                nil
                );
            end;
          end
          else
          begin
            lSer.RecordToJsonObject(
              Value.GetReferenceToRawData,
              Value.TypeInfo,
              JSONArr.AddObject,
              TMVCSerializationType.stFields,
              nil
              );
          end;
        finally
          lSer.Free;
        end;
      end;
  else
    RaiseSerializationError(Format('Invalid TJSONRPCParamDataType: %s',
      [GetEnumName(TypeInfo(TJSONRPCParamDataType), Ord(ParamType))]));
  end;
end;

procedure AppendTValueToJsonObject(const Value: TValue; const Name: string; const ParamType: TJSONRPCParamDataType;
  const JSONObj: TJDOJsonObject);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lOrdinalValue: Int64;
begin
  case ParamType of
    pdtInteger:
      begin
        JSONObj.I[name] := Value.AsInteger;
      end;
    pdtFloat:
      begin
        JSONObj.F[name] := Value.AsExtended;
      end;
    pdtDateTime:
      begin
        JSONObj.S[name] := DateTimeToISOTimeStamp(FloatToDateTime(Value.AsExtended));
      end;
    pdtDate:
      begin
        JSONObj.S[name] := DateToISODate(FloatToDateTime(Value.AsExtended));
      end;
    pdtTime:
      begin
        JSONObj.S[name] := TimeToISOTime(FloatToDateTime(Value.AsExtended));
      end;
    pdtString:
      begin
        JSONObj.S[name] := Value.AsString;
      end;
    pdtLongInteger:
      begin
        JSONObj.L[name] := Value.AsInt64;
      end;
    pdtBoolean:
      begin
        if not Value.TryAsOrdinal(lOrdinalValue) then
        begin
          raise EMVCException.Create('Invalid ordinal parameter');
        end;
        JSONObj.B[name] := lOrdinalValue = 1;
      end;
    pdTJDOJsonObject:
      begin
        JSONObj.O[name] := (Value.AsObject as TJDOJsonObject).Clone as TJDOJsonObject;
      end;
    pdtJSONArray:
      begin
        JSONObj.A[name] := (Value.AsObject as TJDOJsonArray).Clone as TJDOJsonArray;
      end;
    pdtObject:
      begin
        if Value.AsObject is TDataSet then
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            lSer.DataSetToJsonArray(TDataSet(Value.AsObject), JSONObj.A[name], TMVCNameCase.ncLowerCase, []);
          finally
            lSer.Free;
          end
        end
        else
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create;
          try
            JSONObj.O[name] := lSer.SerializeObjectToJSON(Value.AsObject, TMVCSerializationType.stProperties, [], nil);
          finally
            lSer.Free;
          end;
        end;
      end;
  else
    RaiseSerializationError(Format('Invalid TJSONRPCParamDataType: %s',
      [GetEnumName(TypeInfo(TJSONRPCParamDataType), Ord(ParamType))]));
  end;
end;

function JSONDataValueToTValue(const JSONDataValue: TJsonDataValueHelper): TValue; overload;
begin
  case JSONDataValue.Typ of
    jdtString:
      begin
        Result := JSONDataValue.Value;
      end;
    jdtFloat:
      begin
        Result := JSONDataValue.FloatValue;
      end;
    jdtBool:
      begin
        Result := JSONDataValue.BoolValue;
      end;
    jdtArray:
      begin
        Result := JSONDataValue.ArrayValue.Clone as TJDOJsonArray;
      end;
    jdtObject:
      begin
        if JSONDataValue.IsNull then
          Result := nil
        else
          Result := JSONDataValue.ObjectValue.Clone as TJDOJsonObject;
      end;
    jdtInt:
      begin
        Result := JSONDataValue.IntValue;
      end;
    jdtLong:
      begin
        Result := JSONDataValue.LongValue;
      end;
    jdtULong:
      begin
        Result := JSONDataValue.ULongValue;
      end;
  else
    raise EMVCJSONRPCException.CreateFmt('Invalid parameter type [%d]', [Ord(JSONDataValue.Typ)]);
  end;
end;

function BuildDeclaration(const RTTIParameter: TRttiParameter): string;
begin
  Result := RTTIParameter.Name + ': ' + RTTIParameter.ParamType.Name;
end;

procedure JSONDataValueToTValueParam(
  const JSONDataValue: TJsonDataValueHelper;
  const RTTIParameter: TRttiParameter;
  const JSONRPCRequestParams: TJSONRPCRequestParams);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lBuf: PByte;
  lValue: TValue;
  lArr: TArray<TValue>;
  I: Integer;
begin
  case RTTIParameter.ParamType.TypeKind of
    tkString, tkUString {$IF CompilerVersion > 28}, tkAnsiString {$ENDIF}:
      begin
        if JSONDataValue.Typ <> jdtString then
        begin
          raise EMVCJSONRPCInvalidParams.Create('Invalid param type for [' + BuildDeclaration(RTTIParameter) + ']');
        end;
        JSONRPCRequestParams.Add(JSONDataValue.Value);
      end;
    tkFloat:
      begin
        if SameText(RTTIParameter.ParamType.Name, 'TDate') then
        begin
          JSONRPCRequestParams.Add(ISODateToDate(JSONDataValue.Value), pdtDate);
        end
        else if SameText(RTTIParameter.ParamType.Name, 'TDateTime') then
        begin
          if JSONDataValue.Value.IndexOf('T') = 10 then
            JSONRPCRequestParams.Add(ISOTimeStampToDateTime(JSONDataValue.Value), pdtDateTime)
          else
            JSONRPCRequestParams.Add(JSONDataValue.UtcDateTimeValue, pdtDateTime);
        end
        else if SameText(RTTIParameter.ParamType.Name, 'TTime') then
        begin
          JSONRPCRequestParams.Add(ISOTimeToTime(JSONDataValue.Value), pdtTime);
        end
        else
        begin
          // handle integer types passed where a float is expected
          // FIX https://github.com/danieleteti/delphimvcframework/issues/270
          case JSONDataValue.Typ of
            jdtInt:
              JSONRPCRequestParams.Add(JSONDataValue.IntValue, pdtFloat);
            jdtLong:
              JSONRPCRequestParams.Add(JSONDataValue.LongValue, pdtFloat);
            jdtULong:
              JSONRPCRequestParams.Add(JSONDataValue.ULongValue, pdtFloat);
          else
            begin
              if JSONDataValue.Typ <> jdtFloat then
              begin
                raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
              end;
              JSONRPCRequestParams.Add(JSONDataValue.FloatValue, pdtFloat);
            end;
          end;
        end
      end;
    tkEnumeration:
      begin
        if JSONDataValue.Typ <> jdtBool then
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
        JSONRPCRequestParams.Add(JSONDataValue.BoolValue, pdtBoolean);
      end;
    tkInteger:
      begin
        if JSONDataValue.Typ <> jdtInt then
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
        JSONRPCRequestParams.Add(JSONDataValue.IntValue, pdtInteger);
      end;
    tkInt64:
      begin
        if JSONDataValue.Typ = jdtInt then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.IntValue, pdtInteger);
        end
        else if JSONDataValue.Typ = jdtLong then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.LongValue, pdtLongInteger);
        end
        else if JSONDataValue.Typ = jdtULong then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ULongValue, pdtLongInteger);
        end
        else
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
      end;
    tkClass:
      begin
        if (SameText(RTTIParameter.ParamType.Name, TJDOJsonArray.ClassName)) then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ArrayValue.Clone, pdtJSONArray);
        end
        else if SameText(RTTIParameter.ParamType.Name, TJDOJsonObject.ClassName) then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ObjectValue.Clone as TJDOJsonObject, pdTJDOJsonObject);
        end
        else
        begin
          { TODO -oDanieleT -cGeneral : Automatically inject the dseserialized version of arbitrary object? }
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
      end;
    tkRecord:
      begin
        lSer := TMVCJsonDataObjectsSerializer.Create(nil);
        try
          lSer.JSONObjectToRecord(JSONDataValue.ObjectValue, RTTIParameter.ParamType.AsRecord, lBuf);
          TValue.Make(lBuf, RTTIParameter.ParamType.Handle, lValue);
          JSONRPCRequestParams.Add(lValue, pdtRecordOrArrayOfRecord);
        finally
          lSer.Free;
        end;
      end;
    tkDynArray:
      begin
        lSer := TMVCJsonDataObjectsSerializer.Create(nil);
        try
          SetLength(lArr, JSONDataValue.ArrayValue.Count);
          lValue := TValue.FromArray(RTTIParameter.ParamType.Handle, lArr);
          for I := Low(lArr) to High(lArr) do
          begin
            lSer.JSONObjectToRecord(
              JSONDataValue.ArrayValue.Items[I].ObjectValue,
              RTTIParameter.ParamType.AsRecord, lBuf);
            TValue.Make(lBuf, RTTIParameter.ParamType.Handle, lValue);
          end;
          JSONRPCRequestParams.Add(lValue, pdtRecordOrArrayOfRecord);
        finally
          lSer.Free;
        end;
      end
  else
    begin
      raise EMVCJSONRPCInvalidRequest.CreateFmt('Invalid parameter type for [%s]', [BuildDeclaration(RTTIParameter)]);
    end;
  end;
end;

procedure JSONDataValueToTValueParamEx(
  const JSONSerializer: TMVCJsonDataObjectsSerializer;
  const JSONDataValue: TJsonDataValueHelper;
  const RTTIParameter: TRttiParameter;
  var ParamValue: TValue;
  out ParamIsRecord: Boolean;
  out ParamRecordPointer: PByte;
  out ParamArrayLength: Integer);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lTValueArr: TArray<TValue>;
  lItemRTTIType: TRttiType;
  I: Integer;
begin
  ParamIsRecord := False;
  ParamRecordPointer := nil;
  case RTTIParameter.ParamType.TypeKind of
    tkString, tkUString {$IF CompilerVersion > 28}, tkAnsiString {$ENDIF}:
      begin
        if JSONDataValue.Typ <> jdtString then
        begin
          raise EMVCJSONRPCInvalidParams.Create('Invalid param type for [' + BuildDeclaration(RTTIParameter) + ']');
        end;
        ParamValue := JSONDataValue.Value;
      end;
    tkFloat:
      begin
        if SameText(RTTIParameter.ParamType.Name, 'TDate') then
        begin
          ParamValue :=  ISODateToDate(JSONDataValue.Value);
        end
        else if SameText(RTTIParameter.ParamType.Name, 'TDateTime') then
        begin
          if JSONDataValue.Value.IndexOf('T') = 10 then
            ParamValue := ISOTimeStampToDateTime(JSONDataValue.Value)
          else
            ParamValue := JSONDataValue.UtcDateTimeValue;
        end
        else if SameText(RTTIParameter.ParamType.Name, 'TTime') then
        begin
          ParamValue := ISOTimeToTime(JSONDataValue.Value);
        end
        else
        begin
          // handle integer types passed where a float is expected
          // FIX https://github.com/danieleteti/delphimvcframework/issues/270
          case JSONDataValue.Typ of
            jdtInt:
              ParamValue := JSONDataValue.IntValue;
            jdtLong:
              ParamValue := JSONDataValue.LongValue;
            jdtULong:
              ParamValue := JSONDataValue.ULongValue;
          else
            begin
              if JSONDataValue.Typ <> jdtFloat then
              begin
                raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
              end;
              ParamValue := JSONDataValue.FloatValue;
            end;
          end;
        end
      end;
    tkSet:
      begin
        if JSONDataValue.Typ <> jdtString then
        begin
          RaiseDeSerializationError('Cannot deserialize type ' + RTTIParameter.ParamType.Name);
        end;
        I := StringToSet(
          RTTIParameter.ParamType.Handle,
          StringReplace(JSONDataValue.Value, ' ', '', [rfReplaceAll]));
        TValue.Make(I, RTTIParameter.ParamType.Handle, ParamValue);
      end;
    tkEnumeration:
      begin
        if JSONDataValue.Typ = jdtBool then
        begin
          ParamValue := JSONDataValue.BoolValue;
        end
        else
        begin
          JSONSerializer.ParseStringAsTValueUsingMetadata(
            JSONDataValue.Value,
            RTTIParameter.ParamType.Handle,
            'type ' + RTTIParameter.ParamType.Name,
            RTTIParameter.ParamType.GetAttributes,
            ParamValue
            );
        end;

      end;
    tkInteger:
      begin
        if JSONDataValue.Typ <> jdtInt then
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
        ParamValue := JSONDataValue.IntValue;
      end;
    tkInt64:
      begin
        if JSONDataValue.Typ = jdtInt then
        begin
          ParamValue := JSONDataValue.IntValue;
        end
        else if JSONDataValue.Typ = jdtLong then
        begin
          ParamValue := JSONDataValue.LongValue;
        end
        else if JSONDataValue.Typ = jdtULong then
        begin
          ParamValue := JSONDataValue.ULongValue;
        end
        else
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
      end;
    tkClass:
      begin
        if (SameText(RTTIParameter.ParamType.Name, TJDOJsonArray.ClassName)) then
        begin
          ParamValue := JSONDataValue.ArrayValue.Clone;
        end
        else if SameText(RTTIParameter.ParamType.Name, TJDOJsonObject.ClassName) then
        begin
          ParamValue := JSONDataValue.ObjectValue.Clone as TJDOJsonObject;
        end
        else
        begin
          lSer := TMVCJsonDataObjectsSerializer.Create(nil);
          try
            ParamValue := TRTTIUtils.CreateObject(RTTIParameter.ParamType);
            try
              lSer.JsonObjectToObject(
                  JSONDataValue.ObjectValue,
                  ParamValue.AsObject,
                  TMVCSerializationType.stDefault,
                  nil
                );
            except
              ParamValue.AsObject.Free;
              raise;
            end;
          finally
            lSer.Free;
          end;

          { TODO -oDanieleT -cGeneral : Automatically inject the dseserialized version of arbitrary object? }
          //raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
      end;
    tkRecord:
      begin
        ParamIsRecord := True;
        lSer := TMVCJsonDataObjectsSerializer.Create(nil);
        try
          lSer.JSONObjectToRecord(
            JSONDataValue.ObjectValue,
            RTTIParameter.ParamType.AsRecord,
            ParamRecordPointer);
          TValue.MakeWithoutCopy(
            ParamRecordPointer,
            RTTIParameter.ParamType.Handle,
            ParamValue);
        finally
          lSer.Free;
        end;
      end;
    tkDynArray:
      begin
        if (JSONDataValue.Typ = jdtObject) and (JSONDataValue.IsNull) then
        begin
          ParamValue := TValue.FromArray(RTTIParameter.ParamType.Handle, []);
        end
        else
        begin
          lItemRTTIType := TRttiUtils.GetArrayContainedRTTIType(RTTIParameter.ParamType);
          SetLength(lTValueArr, JSONDataValue.ArrayValue.Count);
          ParamArrayLength := JSONDataValue.ArrayValue.Count;
          ParamIsRecord := False;
          lSer := TMVCJsonDataObjectsSerializer.Create(nil);
          try
            for I := 0 to Length(lTValueArr) - 1 do
            begin
              lSer.JSONObjectToRecord(
                JSONDataValue.ArrayValue.Items[i].ObjectValue,
                lItemRTTIType.AsRecord,
                ParamRecordPointer);
              TValue.MakeWithoutCopy(
                ParamRecordPointer,
                lItemRTTIType.AsRecord.Handle, // RTTIParameter.ParamType.Handle,
                lTValueArr[I]);
  //            lSer.JSONObjectToRecord(
  //              JSONDataValue.ObjectValue,
  //              RTTIParameter.ParamType.AsRecord,
  //              ParamRecordPointer);
  //            TValue.MakeWithoutCopy(
  //              ParamRecordPointer,
  //              RTTIParameter.ParamType.Handle,
  //              ParamValue);
            end;
          finally
            lSer.Free;
          end;
          ParamValue := TValue.FromArray(RTTIParameter.ParamType.Handle, lTValueArr);
        end;
      end;
  else
    begin
      raise EMVCJSONRPCInvalidRequest.CreateFmt('Invalid parameter type for [%s]', [BuildDeclaration(RTTIParameter)]);
    end;
  end;
end;


{ TMVCJSONRPCMessage }

function TMVCJSONRPCMessage.AsJSON: TJDOJsonObject;
begin
  Result := TMVCJSONObject.Create;
  Result.S[JSONRPC_HEADER] := JSONRPC_VERSION;
end;

function TMVCJSONRPCMessage.AsJSONRPCMessage: string;
begin
  Result := fJSON.ToJSON();
end;

class procedure TMVCJSONRPCMessage.CheckID(const aJSON: TMVCJSONObject; out aIsNotification: Boolean);
begin
  {
    id
    An identifier established by the Client that MUST contain a String, Number, or NULL value if included.
    If it is not included it is assumed to be a notification.
    The value SHOULD normally not be Null [1] and Numbers SHOULD NOT contain fractional parts [2]
  }
  aIsNotification := not aJSON.Contains(JSONRPC_ID);
  if not aIsNotification then
  begin
    if not(aJSON.Types[JSONRPC_ID] in [jdtString, jdtInt, jdtLong, jdtULong, jdtNone]) then
      raise EMVCJSONRPCException.Create('Message is not a notification but its ''id'' property is not valid');
  end;
end;

class procedure TMVCJSONRPCMessage.CheckMethod(const aJSON: TMVCJSONObject);
begin
  if (aJSON.Types[JSONRPC_METHOD] <> jdtString) then
    raise EMVCJSONRPCException.Create('Invalid ''method''');
end;

class procedure TMVCJSONRPCMessage.CheckVersion(const aJSON: TMVCJSONObject);
begin
  if not Assigned(aJSON) then
    raise EMVCJSONRPCException.Create('JSON not assigned');
  if aJSON.S[JSONRPC_HEADER] <> JSONRPC_VERSION then
    raise EMVCJSONRPCInvalidVersion.Create(JSONRPC_HEADER + ' must be "2.0"');
end;

constructor TMVCJSONRPCMessage.Create;
begin
  inherited Create;
end;

{ TMVCJSONRPCController }

constructor TMVCJSONRPCPublisher.Create(const RPCInstance: TObject; const Owns: Boolean = True; ExceptionHandler:
    TMVCJSONRPCExceptionHandlerProc = nil);
begin
  inherited Create;
  fRPCInstance := RPCInstance;
  fOwsRPCInstance := Owns;
  fExceptionHandler := ExceptionHandler;
end;

// procedure TMVCJSONRPCController.CheckInputParametersTypes(aRTTIMethod: TRTTIMethod);
// var
// lParam: TRttiParameter;
// begin
// for lParam in aRTTIMethod.GetParameters do
// begin
// if lParam.ParamType.TypeKind in [tkClass] then
// begin
// if not(SameText(lParam.ParamType.QualifiedName, 'JsonDataObjects.TJDOJsonObject') or
// SameText(lParam.ParamType.QualifiedClassName, 'JsonDataObjects.TJDOJsonArray')) then
// begin
// raise EMVCJSONRPCException.Create('Parameter [' + lParam.Name + ': ' + lParam.ParamType.QualifiedName +
// '] is not allowed as input parameter');
// end;
// end;
// end;
//
// end;

function TMVCJSONRPCController.CanBeRemotelyInvoked(const RTTIMethod: TRTTIMethod): Boolean;
begin
  Result := (RTTIMethod.Visibility = mvPublic) and (RTTIMethod.MethodKind in [mkProcedure, mkFunction]);
  Result := Result and not IsReservedMethodName(RTTIMethod.Name);
end;

constructor TMVCJSONRPCController.Create;
begin
  inherited Create;
  fRPCInstance := Self;
  fOwsRPCInstance := False;
end;

function TMVCJSONRPCController.CreateError(const RequestID: TValue; const ErrorCode: Integer; const Message: string)
  : TJDOJsonObject;
begin
  Result := CreateError(RequestID, ErrorCode, Message, TValue.Empty);
end;

function TMVCJSONRPCController.CreateError(const RequestID: TValue; const ErrorCode: Integer; const Message: string; const Data: TValue)
  : TJDOJsonObject;
var
  lErrResp: TJSONRPCResponse;
begin
  lErrResp := TJSONRPCResponse.Create;
  try
    lErrResp.RequestID := RequestID;
    lErrResp.Error := TJSONRPCResponseError.Create;
    lErrResp.Error.Code := ErrorCode;
    lErrResp.Error.ErrMessage := message;
    if not Data.IsEmpty then
    begin
      lErrResp.Error.Data := Data;
    end;
    Result := lErrResp.AsJSON;
  finally
    lErrResp.Free;
  end;
end;


function TMVCJSONRPCController.CreateRequest(const JSON: TJDOJsonObject): IJSONRPCRequest;
var
  lReqID: TValue;
  lMethodName: string;
begin
  if JSON.Types[JSONRPC_ID] = jdtString then
    lReqID := JSON.S[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtInt then
    lReqID := JSON.I[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtLong then
    lReqID := JSON.L[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtULong then
    lReqID := JSON.U[JSONRPC_ID]
  else
    lReqID := TValue.Empty;

  lMethodName := JSON.S[JSONRPC_METHOD];
  Result := TJSONRPCRequest.Create(lReqID, lMethodName);
  {
    if JSON.Types[JSONRPC_PARAMS] = jdtArray then
    begin
    lParams := JSON.A[JSONRPC_PARAMS];
    for I := 0 to lParams.Count - 1 do
    begin
    Result.Params.Add(JSONDataValueToTValue(lParams[I]));
    end;
    end
    else if JSON.Types[JSONRPC_PARAMS] <> jdtNone then
    begin
    raise EMVCJSONRPCException.Create('Params must be a JSON array or null');
    end;

  }
end;

function TMVCJSONRPCController.CreateResponse(const RequestID: TValue; const Value: TValue): TJSONRPCResponse;
begin
  Result := TJSONRPCResponse.Create;
  Result.RequestID := RequestID;
  Result.Result := Value;
end;

destructor TMVCJSONRPCController.Destroy;
begin
  if Assigned(fRPCInstance) and fOwsRPCInstance then
  begin
    fRPCInstance.Free;
  end;
  fSerializer.Free;
  inherited;
end;

function TMVCJSONRPCController.GetDeclaredMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
var
  lRTTIDeclaredMethods: TArray<TRTTIMethod>;
  I: Integer;
begin
  Result := nil;
  lRTTIDeclaredMethods := lRTTIType.GetDeclaredMethods;
  for I := 0 to Length(lRTTIType.GetDeclaredMethods) - 1 do
  begin
    if SameText(lRTTIDeclaredMethods[I].Name, lMethod) then
    begin
      Result := lRTTIDeclaredMethods[I];
      Break;
    end;
  end;
end;

function TMVCJSONRPCController.GetInheritedMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
var
  lRTTIMethod: TRTTIMethod;
begin
  Result := nil;
  lRTTIMethod := lRTTIType.GetMethod(lMethod);
  if Assigned(lRTTIMethod) then
  begin
    if TMVCSerializerHelper.HasAttribute<MVCInheritableAttribute>(lRTTIMethod) then
    begin
      Result := lRTTIMethod;
    end;
  end;
end;

function TMVCJSONRPCController.GetJSONRPCPayload(const Request: TMVCWebRequest): TJsonObject;
var
  lParams: string;
  lJ: TJsonBaseObject;
begin
  // https://www.simple-is-better.org/json-rpc/transport_http.html#get-request
  // http get :8080/jsonrpc jsonrpc==2 method==subtract params=={\"Value1\":10,\"Value2\":3} id==1234
  // http get :8080/jsonrpc jsonrpc==2 id==1234 method==subtract params==[10,3]
  Result := TJsonObject.Create;
  try
    Result.S['jsonrpc'] := Request.QueryStringParam('jsonrpc');
    Result.S['method'] := Request.QueryStringParam('method');
    if Request.QueryStringParamExists('id') then
    begin
      Result.S['id'] := Request.QueryStringParam('id');
    end;
    lParams := Request.QueryStringParam('params');
    lJ := TJsonObject.Parse(lParams);
    if lJ is TJsonArray then
      Result.A['params'] := TJsonArray(lJ)
    else
      Result.O['params'] := TJsonObject(lJ);
  except
    Result.Free;
    raise
  end;
end;

procedure TMVCJSONRPCController.ForEachInvokableMethod(const aProc: TProc<TRTTIMethod>);
var
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIMethodList: TArray<TRTTIMethod>;
  lRTTIMethod: TRTTIMethod;
  lGeneratedMethods: TList<String>;
  function MethodSign(const RTTIMethod: TRTTIMethod): String;
  begin
    Result := RTTIMethod.ToString.ToLower;
  end;

begin

  lGeneratedMethods := TList<String>.Create;
  try
    lRTTI := TRTTIContext.Create;
    try
      lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);
      lRTTIMethodList := lRTTIType.GetDeclaredMethods;
      for lRTTIMethod in lRTTIMethodList do
      begin
        if CanBeRemotelyInvoked(lRTTIMethod) then
        begin
          aProc(lRTTIMethod);
          lGeneratedMethods.Add(MethodSign(lRTTIMethod));
        end;
      end;

      lRTTIMethodList := lRTTIType.BaseType.GetMethods;
      for lRTTIMethod in lRTTIMethodList do
      begin
        if TMVCSerializerHelper.HasAttribute<MVCInheritableAttribute>(lRTTIMethod) and CanBeRemotelyInvoked(lRTTIMethod)
          and (not lGeneratedMethods.Contains(MethodSign(lRTTIMethod))) then
        begin
          aProc(lRTTIMethod);
        end;
      end;
    finally
      lRTTI.Free;
    end;
  finally
    lGeneratedMethods.Free;
  end;
end;

procedure TMVCJSONRPCController.GetProxyCode;
var
  lLanguage: string;
  lClass: TJSONRPCProxyGeneratorClass;
  lGenerator: TJSONRPCProxyGenerator;
  lRTTI: TRTTIContext;
  lContentType: string;
begin
  lLanguage := Context.Request.Params['language'].ToLower;
  if lLanguage.IsEmpty then
  begin
    lLanguage := 'delphi';
  end;

  if Context.Request.QueryStringParamExists('content-type') then
  begin
    lContentType := Context.Request.Params['content-type'];
  end
  else
  begin
    lContentType := 'text/plain';
  end;

  if not Assigned(GProxyGeneratorsRegister) then
  begin
    raise EMVCJSONRPCException.Create
      ('No Proxy Generators have been registered. [HINT] Use RegisterJSONRPCProxyGenerator function');
  end;

  if not GProxyGeneratorsRegister.TryGetValue(lLanguage, lClass) then
  begin
    raise EMVCJSONRPCException.CreateFmt('Unknown language [%s]', [lLanguage]);
  end;

  lGenerator := lClass.Create;
  try
    lRTTI := TRTTIContext.Create;
    try
      lGenerator.StartGeneration(fRPCInstance.ClassType.ClassName);
      ForEachInvokableMethod(
        procedure(aRTTIMethod: TRTTIMethod)
        begin
          lGenerator.VisitMethod(aRTTIMethod);
        end);
      lGenerator.EndGeneration();
      Context.Response.ContentType := lContentType;
      Render(lGenerator.GetCode);
    finally
      lRTTI.Free;
    end;
  finally
    lGenerator.Free;
  end;
end;

procedure TMVCJSONRPCController.GetPublishedMethodList;
begin
  ResponseStream.AppendLine('// ' + StringOfChar('*', 80));
  ResponseStream.AppendLine('// Generated by ' + DMVCFRAMEWORK_VERSION + ' at ' +
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  ResponseStream.AppendLine('// ' + StringOfChar('*', 80));
  ResponseStream.AppendLine('');
  ForEachInvokableMethod(
    procedure(aRTTIMethod: TRTTIMethod)
    var
      lAtt: MVCDocAttribute;
      lLines: TArray<String>;
      lLine: String;
    begin
      if IsReservedMethodName(aRTTIMethod.Name) then
      begin
        Exit;
      end;
      lAtt := TRTTIUtils.GetAttribute<MVCDocAttribute>(aRTTIMethod);
      if Assigned(lAtt) then
      begin
        lLines := lAtt.Value.Split([sLineBreak]);
        for lLine in lLines do
        begin
          ResponseStream.AppendLine('// ' + lLine);
        end;
      end;
      ResponseStream.AppendLine(aRTTIMethod.ToString + ';');
    end);
  RenderResponseStream;
end;

function TMVCJSONRPCController.GetSerializer: TMVCJsonDataObjectsSerializer;
begin
  if not Assigned(fSerializer) then
    fSerializer := TMVCJsonDataObjectsSerializer.Create;
  Result := fSerializer;
end;

procedure TMVCJSONRPCController.Index;
var
  lJSONRPCReq: IJSONRPCRequest;
  lMethod: string;
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIMethod: TRTTIMethod;
  lRes: TValue;
  lJSONRPCResponse: IJSONRPCResponse;
  lParamsToInject: TArray<TValue>;
  lReqID: TValue;
  lJSON: TJDOJsonObject;
  lJSONResp: TJDOJsonObject;
  lBeforeCallHookHasBeenInvoked: Boolean;
  lAfterCallHookHasBeenInvoked: Boolean;
  lTypeAttrs: TArray<TCustomAttribute>;
  lHTTPVerb: TMVCHTTPMethodType;
  lAllMethodsCallableWithGET: Boolean;
  lExceptionHandled: Boolean;
  lJSONRespErrorInfo: TMVCJSONRPCExceptionErrorInfo;
begin
  lBeforeCallHookHasBeenInvoked := False;
  lAfterCallHookHasBeenInvoked := False;
  lAllMethodsCallableWithGET := False;
  lRTTIType := nil;
  lReqID := TValue.Empty;
  SetLength(lParamsToInject, 0);
  lJSONResp := nil;
  lRTTI := TRTTIContext.Create;
  try
    try
      lHTTPVerb := Context.Request.HTTPMethod;
      case lHTTPVerb of
        httpGET:
          begin
            lJSON := GetJSONRPCPayload(Context.Request);
          end;
        httpPOST:
          begin
            lJSON := StrToJSONObject(Context.Request.Body);
          end;
      else
        raise EMVCJSONRPCInvalidRequest.Create('Only POST and GET Allowed');
      end;

      try
        if not Assigned(lJSON) then
        begin
          raise EMVCJSONRPCParseError.Create;
        end;
        lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);

        if lHTTPVerb = httpGET then
        begin
          lTypeAttrs := lRTTIType.GetAttributes;
          lAllMethodsCallableWithGET := (Length(lTypeAttrs) > 0) and
            TMVCSerializerHelper.AttributeExists<MVCJSONRPCAllowGET>(lTypeAttrs);
        end;

        lJSONRPCReq := CreateRequest(lJSON);
        lMethod := lJSONRPCReq.Method;

        if IsReservedMethodName(lMethod) then
        begin
          raise EMVCJSONRPCInvalidRequest.CreateFmt
            ('Requested method name [%s] is reserved and cannot be called remotely', [lMethod]);
        end;

        TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_ROUTING, lJSON);

        if lJSONRPCReq.RequestType = TJSONRPCRequestType.Request then
        begin
          if lJSONRPCReq.RequestID.IsEmpty then
            raise EMVCJSONRPCInvalidRequest.Create;
          lReqID := lJSONRPCReq.RequestID;
        end;

        lRTTIMethod := GetDeclaredMethod(lMethod, lRTTIType);
        if not Assigned(lRTTIMethod) then
        begin
          lRTTIMethod := GetInheritedMethod(lMethod, lRTTIType);
        end;

        if Assigned(lRTTIMethod) then
        begin
          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Request) and (lRTTIMethod.MethodKind <> mkFunction) then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a procedure using a JSON-RPC request - use requests for functions and notifications for procedures');
          end;

          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Notification) and (lRTTIMethod.MethodKind <> mkProcedure)
          then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a function using a JSON-RPC notification - use requests for functions and notifications for procedures');
          end;

          if not CanBeRemotelyInvoked(lRTTIMethod) then
          begin
            LogW(Format('Method [%s] cannot remotely invoked - only public functions or procedures can be called.',
              [lMethod]));
            raise EMVCJSONRPCMethodNotFound.Create(lMethod);
          end;

          if (lHTTPVerb = httpGET) and (not lAllMethodsCallableWithGET) then
          begin
            lTypeAttrs := lRTTIMethod.GetAttributes;
            if (Length(lTypeAttrs) = 0) or (not TMVCSerializerHelper.AttributeExists<MVCJSONRPCAllowGET>(lTypeAttrs))
            then
            begin
              raise EMVCJSONRPCError.Create(JSONRPC_ERR_INVALID_REQUEST, 'Method callable with POST only');
            end;
          end;
          lRes := InvokeMethod(fRPCInstance, lRTTIType, lRTTIMethod, lJSON, lBeforeCallHookHasBeenInvoked);
          case lJSONRPCReq.RequestType of
            TJSONRPCRequestType.Notification:
              begin
                ResponseStatus(HTTP_STATUS.NoContent);
              end;
            TJSONRPCRequestType.Request:
              begin
                lJSONRPCResponse := CreateResponse(lJSONRPCReq.RequestID, lRes);
                ResponseStatus(200);
                lJSONResp := lJSONRPCResponse.AsJSON;
              end;
          else
            raise EMVCJSONRPCException.Create('Invalid RequestType');
          end;
        end
        else
        begin
          LogW(Format('Method "%s" has not be found in %s. Only public methods can be invoked.',
            [lMethod, fRPCInstance.QualifiedClassName]));
          raise EMVCJSONRPCMethodNotFound.Create(lMethod);
        end;
      finally
        FreeAndNil(lJSON);
      end;
    except
      on E: EMVCJSONRPCErrorResponse do
      begin
        {
          http://www.jsonrpc.org/historical/json-rpc-over-http.html#response-codes
          HTTP Status	code	message
          500	-32700	Parse error.
          400	-32600	Invalid Request.
          404	-32601	Method not found.
          500	-32602	Invalid params.
          500	-32603	Internal error.
          500	-32099..-32000	Server error.
        }
        case E.JSONRPCErrorCode of
          JSONRPC_ERR_PARSE_ERROR:
            ResponseStatus(500);
          JSONRPC_ERR_INVALID_REQUEST:
            ResponseStatus(400);
          JSONRPC_ERR_METHOD_NOT_FOUND:
            ResponseStatus(404);
          JSONRPC_ERR_INVALID_PARAMS:
            ResponseStatus(500);
          JSONRPC_ERR_INTERNAL_ERROR:
            ResponseStatus(500);
          JSONRPC_ERR_SERVER_ERROR_LOWERBOUND .. JSONRPC_ERR_SERVER_ERROR_UPPERBOUND:
            ResponseStatus(500);
        end;
        lJSONResp := CreateError(lReqID, E.JSONRPCErrorCode, E.Message, E.JSONRPCErrorData);
        LogE(Format('[JSON-RPC][CLS %s][ERR %d][MSG "%s"]', [E.ClassName, E.JSONRPCErrorCode, E.Message]));
      end;
      on ExDeSer: EMVCDeserializationException do
      begin
        ResponseStatus(400);
        lJSONResp := CreateError(lReqID, JSONRPC_ERR_INVALID_REQUEST, ExDeSer.Message, ExDeSer.DetailedMessage);
        LogE(Format('[JSON-RPC][CLS %s][ERR %d][MSG "%s"]', [ExDeSer.ClassName, JSONRPC_ERR_INVALID_REQUEST, ExDeSer.Message]));
      end;
      on ExSer: EMVCSerializationException do
      begin
        ResponseStatus(400);
        lJSONResp := CreateError(lReqID, JSONRPC_ERR_INTERNAL_ERROR, ExSer.Message, ExSer.DetailedMessage);
        LogE(Format('[JSON-RPC][CLS %s][ERR %d][MSG "%s"]', [ExSer.ClassName, JSONRPC_ERR_INTERNAL_ERROR, ExSer.Message]));
      end;
      on Ex: Exception do // use another name for exception variable, otherwise E is nil!!
      begin
        //lJSONResp := CreateError(lReqID, 0, Ex.Message);
        LogE(Format('[JSON-RPC][CLS %s][MSG "%s"]', [Ex.ClassName, Ex.Message]));
        if Assigned(fExceptionHandler) then
        begin
          lExceptionHandled := False;
          lJSONRespErrorInfo.Code := 0;
          lJSONRespErrorInfo.Msg := Ex.Message;
          lJSONRespErrorInfo.Data := nil;
          fExceptionHandler(Ex, Context, lJSONRespErrorInfo,  lExceptionHandled);
          try
            if not lExceptionHandled then
            begin
              lJSONResp := CreateError(lReqID, 0, Ex.Message, Ex.ClassName);
            end
            else
            begin
              lJSONResp := CreateError(lReqID, lJSONRespErrorInfo.Code,
                lJSONRespErrorInfo.Msg, lJSONRespErrorInfo.Data);
            end;
          finally
            if not lExceptionHandled and not lJSONRespErrorInfo.Data.IsEmpty then
            begin
              if lJSONRespErrorInfo.Data.IsObjectInstance then
              begin
                 lJSONRespErrorInfo.Data.AsObject.Free;
              end;
            end;
          end;
        end
        else
        begin
          lJSONResp := CreateError(lReqID, 0, Ex.Message);
        end;
      end;
    end; // except
    if lBeforeCallHookHasBeenInvoked and (not lAfterCallHookHasBeenInvoked) then
    begin
      try
        TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_AFTER_CALL, lJSONResp);
      except
        on E: Exception do
        begin
          FreeAndNil(lJSONResp);
          if E is EMVCJSONRPCErrorResponse then
            lJSONResp := CreateError(lReqID, EMVCJSONRPCErrorResponse(E).JSONRPCErrorCode, E.Message)
          else
            lJSONResp := CreateError(lReqID, 0, E.Message);
        end;
      end;
    end;
    if lJSONResp <> nil then
    begin
      Render(lJSONResp, True);
    end;
  finally
    lRTTI.Free;
  end;
end;

function TMVCJSONRPCController.InvokeMethod(const fRPCInstance: TObject;
  const RTTIType: TRTTIType; const RTTIMethod: TRTTIMethod;
  const JSON: TJSONObject;
  out BeforeCallHookHasBeenInvoked: Boolean): TValue;
type
  TParamType = (ptNotARecord, ptRecord, ptArrayOfRecord);
var
  lRTTIMethodParams: TArray<TRttiParameter>;
  lRTTIMethodParam: TRttiParameter;
  lJSONParams: TJDOJsonArray;
  lJSONNamedParams: TJDOJsonObject;
  I, lParamsCount: Integer;
  lUseNamedParams: Boolean;
  lParamsArray: TArray<TValue>;
  lParamsIsRecord: TArray<Boolean>;
  lRecordsPointer: TArray<PByte>;
  lParamArrayLength: TArray<Integer>;
  function GetJsonDataValueHelper(const JSONNamedParams: TJsonObject; const JsonPropName: string): TJsonDataValueHelper;
  var
    I: Integer;
    lName: string;
  begin
    for I := 0 to JSONNamedParams.Count - 1 do
    begin
      lName := JSONNamedParams.Names[I];
      if SameText(lName, JsonPropName) then
      begin
        Exit(JSONNamedParams.Values[lName]);
      end;
    end;
    raise EJsonException.CreateFmt('Cannot find parameter [%s] in params object', [JsonPropName]);
  end;

begin
  lUseNamedParams := False;
  lJSONParams := nil;
  lJSONNamedParams := nil;
{$REGION 'Check params count and type'}
  if JSON.Types[JSONRPC_PARAMS] = jdtArray then
  begin
    lJSONParams := JSON.A[JSONRPC_PARAMS];
    lUseNamedParams := False;
  end
  else if JSON.Types[JSONRPC_PARAMS] = jdtObject then
  begin
    lJSONNamedParams := JSON.O[JSONRPC_PARAMS];
    lUseNamedParams := True;
  end
  else if JSON.Types[JSONRPC_PARAMS] <> jdtNone then
  begin
    raise EMVCJSONRPCException.Create('Params must be a JSON array or null');
  end;

  lRTTIMethodParams := RTTIMethod.GetParameters;
  lParamsCount := Length(lRTTIMethodParams);

  if lUseNamedParams then
  begin
    if (lParamsCount > 0) and (not Assigned(lJSONNamedParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].', [lParamsCount, 0]);

    if Assigned(lJSONNamedParams) and (lParamsCount <> lJSONNamedParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [lParamsCount, lJSONNamedParams.Count]);
  end
  else
  begin
    if (lParamsCount > 0) and (not Assigned(lJSONParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [lParamsCount, 0]);

    if Assigned(lJSONParams) and (lParamsCount <> lJSONParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [lParamsCount, lJSONParams.Count]);
  end;

  for lRTTIMethodParam in lRTTIMethodParams do
  begin
    if lRTTIMethodParam.Flags * [pfVar, pfOut, pfArray] <> [] then
      raise EMVCJSONRPCInvalidParams.CreateFmt
        ('Parameter modifier not supported for formal parameter [%s]. Only const and value modifiers are allowed.',
        [lRTTIMethodParam.Name]);
  end;

{$ENDREGION}

  BeforeCallHookHasBeenInvoked := False;
  SetLength(lParamsArray, lParamsCount);
  SetLength(lParamsIsRecord, lParamsCount);
  SetLength(lRecordsPointer, lParamsCount);
  SetLength(lParamArrayLength, lParamsCount);
  try
    // scroll json params and rttimethod params and find the best match
    if Assigned(lJSONParams) then
    begin
      // positional params
      for I := 0 to lJSONParams.Count - 1 do
      begin
        JSONDataValueToTValueParamEx(
          fSerializer,
          lJSONParams[I],
          lRTTIMethodParams[I],
          lParamsArray[I],
          lParamsIsRecord[I],
          lRecordsPointer[I],
          lParamArrayLength[i]
          );
      end;
    end
    else if Assigned(lJSONNamedParams) then
    begin
      // named params
      for I := 0 to lJSONNamedParams.Count - 1 do
      begin
        JSONDataValueToTValueParamEx(
          fSerializer,
          GetJsonDataValueHelper(lJSONNamedParams, lRTTIMethodParams[I].Name.ToLower),
          lRTTIMethodParams[I],
          lParamsArray[I],
          lParamsIsRecord[I],
          lRecordsPointer[I],
          lParamArrayLength[i]);
      end;
    end;

    TryToCallMethod(RTTIType, JSONRPC_HOOKS_ON_BEFORE_CALL, JSON);
    BeforeCallHookHasBeenInvoked := True;
    try
      LogD('[JSON-RPC][CALL][' + CALL_TYPE[RTTIMethod.MethodKind] + '][' + fRPCInstance.ClassName + '.' +
        RTTIMethod.Name + ']');
      Result := RTTIMethod.Invoke(fRPCInstance, lParamsArray);
    except
      on E: EInvalidCast do
      begin
        raise EMVCJSONRPCInvalidParams.Create('Check your input parameters types');
      end;
      on Ex: EMVCJSONRPCInvalidRequest do
      begin
        raise EMVCJSONRPCInvalidParams.Create(Ex.Message);
      end;
    end;
  finally
    for I := 0 to lParamsCount - 1 do
    begin
      if lParamsArray[I].IsObject then
      begin
        lParamsArray[I].AsObject.Free;
      end
      else if lParamsIsRecord[I] then
      begin
        FreeMem(lRecordsPointer[I], lRTTIMethodParams[I].ParamType.TypeSize);
      end;
    end;
  end;
end;

function TMVCJSONRPCController.JSONObjectAs<T>(const JSON: TJDOJsonObject): T;
begin
  Result := T.Create;
  try
    GetSerializer.JsonObjectToObject(JSON, Result, TMVCSerializationType.stProperties, []);
  except
    Result.Free;
    raise;
  end;
end;

procedure TMVCJSONRPCController.TryToCallMethod(const aRTTIType: TRttiType; const MethodName: string;
const Parameter: TJDOJsonObject);
var
  lHookMethod: TRTTIMethod;
  lHookSecondParam: TRttiParameter;
  lHookSecondParamType: string;
  lHookFirstParam: TRttiParameter;
  lHookFirstParamType: string;
begin
  if not Assigned(aRTTIType) then
  begin
    Exit;
  end;
  lHookMethod := aRTTIType.GetMethod(MethodName);
  if Assigned(lHookMethod) then
  begin
    if (Length(lHookMethod.GetParameters) <> 2) then
    begin
      raise EMVCJSONRPCException.CreateFmt('Invalid signature for [%s] Hook method [HINT: procedure ' +
        '%s.%s(const Context: TWebContext; const Value: TJDOJsonObject)',
        [MethodName, fRPCInstance.ClassName, MethodName]);
    end;

    lHookFirstParam := lHookMethod.GetParameters[0];
    lHookSecondParam := lHookMethod.GetParameters[1];

    lHookFirstParamType := lHookFirstParam.ParamType.ToString.ToLower;
    lHookSecondParamType := lHookSecondParam.ParamType.ToString.ToLower;

    if (lHookMethod.MethodKind <> mkProcedure) then
      raise EMVCJSONRPCException.CreateFmt
        ('Invalid signature for [%s] Hook method [HINT: Hook methods MUST have the following signature "procedure ' +
        '%s.%s(const Context: TWebContext; const Value: TJDOJsonObject)"',
        [MethodName, fRPCInstance.ClassName, MethodName]);

    if ((lHookSecondParamType <> 'tjdojsonobject') and (lHookSecondParamType <> 'tjsonobject')) or
      (lHookSecondParam.Flags * [pfConst, pfAddress] <> [pfConst, pfAddress]) then
      raise EMVCJSONRPCException.CreateFmt('Invalid signature for [%s] Hook method [HINT: procedure ' +
        '%s.%s(const Context: TWebContext; const Value: TJDOJsonObject)',
        [MethodName, fRPCInstance.ClassName, MethodName]);

    if (lHookFirstParamType <> 'twebcontext') or (lHookFirstParam.Flags * [pfConst, pfAddress] <> [pfConst, pfAddress])
    then
      raise EMVCJSONRPCException.CreateFmt('Invalid signature for [%s] Hook method [HINT: procedure ' +
        '%s.%s(const Context: TWebContext; const Value: TJDOJsonObject)',
        [MethodName, fRPCInstance.ClassName, MethodName]);

    LogD('[JSON-RPC][HOOK][' + fRPCInstance.ClassName + '.' + MethodName + ']');
    lHookMethod.Invoke(fRPCInstance, [Self.Context, Parameter])
  end;
end;

{ EMVCJSONRPCParseError }

procedure EMVCJSONRPCParseError.AfterConstruction;
begin
  inherited;
  fJSONRPCErrorCode := JSONRPC_ERR_PARSE_ERROR;
end;

constructor EMVCJSONRPCParseError.Create;
begin
  inherited Create
    ('Parse error. Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text');
end;

{ EMVCJSONRPCInvalidRequest }

procedure EMVCJSONRPCInvalidRequest.AfterConstruction;
begin
  inherited;
  fJSONRPCErrorCode := JSONRPC_ERR_INVALID_REQUEST;
end;

constructor EMVCJSONRPCInvalidRequest.Create(const Message: string);
var
  lMsg: string;
begin
  lMsg := 'Invalid Request. The JSON sent is not a valid Request object.';
  if not message.IsEmpty then
  begin
    lMsg := lMsg + ' [HINT] ' + message;
  end;
  inherited Create(lMsg);
end;

{ EMVCJSONRPCMethodNotFound }

procedure EMVCJSONRPCMethodNotFound.AfterConstruction;
begin
  inherited;
  fJSONRPCErrorCode := JSONRPC_ERR_METHOD_NOT_FOUND;
end;

constructor EMVCJSONRPCMethodNotFound.Create(const MethodName: string);
begin
  inherited CreateFmt('Method [%s] not found. The method does not exist or is not available.', [MethodName]);
end;

{ EMVCJSONRPCInvalidParams }

procedure EMVCJSONRPCInvalidParams.AfterConstruction;
begin
  inherited;
  fJSONRPCErrorCode := JSONRPC_ERR_INVALID_PARAMS;
end;

constructor EMVCJSONRPCInvalidParams.Create(const Message: string);
begin
  inherited Create('Invalid params. [hint: ' + message + ']');
end;

{ EMVCJSONRPCInternalError }

procedure EMVCJSONRPCInternalError.AfterConstruction;
begin
  inherited;
  fJSONRPCErrorCode := JSONRPC_ERR_INTERNAL_ERROR;
end;

constructor EMVCJSONRPCInternalError.Create;
begin
  inherited Create('Internal JSON-RPC error');
end;

{ EMVCJSONRPCServerError }

constructor EMVCJSONRPCServerError.Create(const JSONRPCError: Integer; const Message: string);
begin
  inherited Create(message);
  fJSONRPCErrorCode := JSONRPCError;
end;

{ TJSONRPCRequest }

constructor TJSONRPCRequest.Create(const aID: TValue; const aMethod: string);
begin
  inherited Create(aMethod);
  SetID(aID);
end;

constructor TJSONRPCRequest.Create(const aID: TValue);
begin
  inherited Create;
  SetID(aID);
end;

constructor TJSONRPCRequest.Create;
begin
  inherited Create;
  Self.FID := TValue.Empty;
end;

destructor TJSONRPCRequest.Destroy;
begin
  inherited;
end;

function TJSONRPCRequest.GetRequestType: TJSONRPCRequestType;
begin
  if FID.IsEmpty then
    Result := TJSONRPCRequestType.Notification
  else
    Result := TJSONRPCRequestType.Request;
end;

procedure TJSONRPCRequest.SetJSON(const JSON: TJDOJsonObject);
begin
  if JSON.Types[JSONRPC_ID] = jdtString then
    RequestID := JSON.S[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtInt then
    RequestID := JSON.I[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtLong then
    RequestID := JSON.L[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtULong then
    RequestID := JSON.U[JSONRPC_ID]
  else
    RequestID := TValue.Empty;
  Method := JSON.S[JSONRPC_METHOD];
  Params.Clear;
end;

constructor TJSONRPCNotification.Create(const aMethod: string);
begin
  Create;
  Method := aMethod;
end;

destructor TJSONRPCNotification.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TJSONRPCNotification.FillParameters(const JSON: TJDOJsonObject; const RTTIMethod: TRTTIMethod);
var
  lRTTIMethodParams: TArray<TRttiParameter>;
  lRTTIMethodParam: TRttiParameter;
  lJSONParams: TJDOJsonArray;
  lJSONNamedParams: TJDOJsonObject;
  I: Integer;
  lUseNamedParams: Boolean;
  function GetJsonDataValueHelper(const JSONNamedParams: TJsonObject; const JsonPropName: string): TJsonDataValueHelper;
  var
    I: Integer;
    lName: string;
  begin
    for I := 0 to JSONNamedParams.Count - 1 do
    begin
      lName := JSONNamedParams.Names[I];
      if SameText(lName, JsonPropName) then
      begin
        Exit(JSONNamedParams.Values[lName]);
      end;
    end;
    raise EJsonException.CreateFmt('Cannot find parameter [%s] in params object', [JsonPropName]);
  end;

begin
  lUseNamedParams := False;
  lJSONParams := nil;
  lJSONNamedParams := nil;
  Params.Clear;
  if JSON.Types[JSONRPC_PARAMS] = jdtArray then
  begin
    lJSONParams := JSON.A[JSONRPC_PARAMS];
    lUseNamedParams := False;
  end
  else if JSON.Types[JSONRPC_PARAMS] = jdtObject then
  begin
    lJSONNamedParams := JSON.O[JSONRPC_PARAMS];
    lUseNamedParams := True;
  end
  else if JSON.Types[JSONRPC_PARAMS] <> jdtNone then
  begin
    raise EMVCJSONRPCException.Create('Params must be a JSON array or null');
  end;

  lRTTIMethodParams := RTTIMethod.GetParameters;
  if lUseNamedParams then
  begin
    if (Length(lRTTIMethodParams) > 0) and (not Assigned(lJSONNamedParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [Length(lRTTIMethodParams), 0]);

    if Assigned(lJSONNamedParams) and (Length(lRTTIMethodParams) <> lJSONNamedParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [Length(lRTTIMethodParams), lJSONNamedParams.Count]);
  end
  else
  begin
    if (Length(lRTTIMethodParams) > 0) and (not Assigned(lJSONParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [Length(lRTTIMethodParams), 0]);

    if Assigned(lJSONParams) and (Length(lRTTIMethodParams) <> lJSONParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected [%d] got [%d].',
        [Length(lRTTIMethodParams), lJSONParams.Count]);
  end;

  for lRTTIMethodParam in lRTTIMethodParams do
  begin
    if lRTTIMethodParam.Flags * [pfVar, pfOut, pfArray] <> [] then
      raise EMVCJSONRPCInvalidParams.CreateFmt
        ('Parameter modifier not supported for formal parameter [%s]. Only const and value modifiers are allowed.',
        [lRTTIMethodParam.Name]);
  end;

  // scroll json params and rttimethod params and find the best match
  if Assigned(lJSONParams) then
  begin
    // positional params
    for I := 0 to lJSONParams.Count - 1 do
    begin
      JSONDataValueToTValueParam(lJSONParams[I], lRTTIMethodParams[I], Params);
    end;
  end
  else if Assigned(lJSONNamedParams) then
  begin
    // named params
    for I := 0 to lJSONNamedParams.Count - 1 do
    begin
      JSONDataValueToTValueParam(GetJsonDataValueHelper(lJSONNamedParams, lRTTIMethodParams[I].Name.ToLower),
      { lJSONNamedParams.Values[lRTTIMethodParams[I].Name.ToLower], }
      lRTTIMethodParams[I], Params);
    end;
  end;
end;

function TJSONRPCNotification.GetJSON: TJDOJsonObject;
var
  I: Integer;
begin
  if FMethod.IsEmpty then
    raise EMVCJSONRPCException.Create('JSON-RPC "Method" cannot be empty');
  Result := inherited;
  Result.S[JSONRPC_METHOD] := FMethod;
  if FParams.Count > 0 then
  begin
    if FParams.fParamNames.Count = 0 then
    begin // positional params
      for I := 0 to FParams.Count - 1 do
      begin
        AppendTValueToJsonArray(FParams.fParamValues[I], FParams.fParamTypes[I],
          Result.A[JSONRPC_PARAMS]);
      end;
    end
    else
    begin // named params
      for I := 0 to FParams.Count - 1 do
      begin
        AppendTValueToJsonObject(FParams.fParamValues[I], FParams.fParamNames[I],
          FParams.fParamTypes[I], Result.O[JSONRPC_PARAMS]);
      end;
    end;
  end;
end;

function TJSONRPCNotification.GetMethod: string;
begin
  Result := FMethod;
end;

function TJSONRPCNotification.GetParams: TJSONRPCRequestParams;
begin
  Result := FParams;
end;

procedure TJSONRPCNotification.SetMethod(const Value: string);
begin
  FMethod := Value;
end;

{ TJSONRCPResponse }
constructor TJSONRPCResponse.Create;
begin
  inherited;
  FError := nil;
end;

destructor TJSONRPCResponse.Destroy;
begin
  FreeAndNil(FError);
  if FResult.IsObject then
    FResult.AsObject.Free;
  inherited;
end;

function TJSONRPCResponse.GetError: TJSONRPCResponseError;
begin
  Result := FError;
end;

function TJSONRPCResponse.GetID: TValue;
begin
  Result := FID;
end;

function TJSONRPCResponse.GetJSON: TJDOJsonObject;
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  Result := inherited;
  // Must generate something like the following:
  // {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}

  if FID.IsEmpty then
  begin
    Result.Values[JSONRPC_ID] := jdtNone;
  end
  else if FID.IsType<string> then
  begin
    Result.S[JSONRPC_ID] := FID.AsString;
  end
  else if FID.IsType<Int32> then
  begin
    Result.I[JSONRPC_ID] := FID.AsInteger;
  end
  else if FID.IsType<Int64> then
  begin
    Result.I[JSONRPC_ID] := FID.AsInt64;
  end
  else
    raise EMVCJSONRPCException.Create('ID can be only Int32, Int64 or String');

  try
    if Assigned(FError) then
    begin
      Result.O[JSONRPC_ERROR].I[JSONRPC_CODE] := FError.Code;
      Result.O[JSONRPC_ERROR].S[JSONRPC_MESSAGE] := FError.ErrMessage;
      if not FError.Data.IsEmpty then
      begin
        TValueToJSONObjectPropertyEx(FError.Data, Result.O[JSONRPC_ERROR], JSONRPC_DATA);
      end;
    end
    else
    begin
      lSer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSer.TValueToJsonObjectProperty(Result, JSONRPC_RESULT, FResult, TMVCSerializationType.stDefault, [], nil);
      finally
        lSer.Free;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONRPCResponse.GetResult: TValue;
begin
  Result := FResult;
end;

function TJSONRPCResponse.IsError: Boolean;
begin
  Result := Assigned(FError);
end;

procedure TJSONRPCResponse.ResultAs(Obj: TObject);
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create(nil);
  try
    lSer.JsonObjectToObject(ResultAsJSONObject, Obj, TMVCSerializationType.stDefault, []);
  finally
    lSer.Free;
  end;
end;

function TJSONRPCResponse.ResultAsJSONArray: TJDOJsonArray;
begin
  Result := Self.Result.AsObject as TJDOJsonArray;
end;

function TJSONRPCResponse.ResultAsJSONObject: TJDOJsonObject;
begin
  // self.AsJSON
  if Self.Result.IsEmpty then
    Result := nil
  else
    Result := Self.Result.AsObject as TJDOJsonObject;
end;

procedure TJSONRPCResponse.SetError(const Value: TJSONRPCResponseError);
begin
  FError := Value;
end;

procedure TJSONRPCResponse.SetID(const Value: TValue);
begin
  FID := Value;
end;

procedure TJSONRPCResponse.SetJSON(const JSON: TJDOJsonObject);
begin
  if JSON.Types[JSONRPC_ID] = jdtString then
    RequestID := JSON.S[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtInt then
    RequestID := JSON.I[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtLong then
    RequestID := JSON.L[JSONRPC_ID]
  else if JSON.Types[JSONRPC_ID] = jdtULong then
    RequestID := JSON.U[JSONRPC_ID]
  else
    RequestID := TValue.Empty;

  if JSON.Contains(JSONRPC_RESULT) then
  begin
    FreeAndNil(FError);
    FResult := JSONDataValueToTValue(JSON.Values[JSONRPC_RESULT]);
  end
  else
  begin
    FResult := TValue.Empty;
    if JSON.Contains(JSONRPC_ERROR) then
    begin
      FError := TJSONRPCResponseError.Create;
      FError.Code := JSON.O[JSONRPC_ERROR].I[JSONRPC_CODE];
      FError.ErrMessage := JSON.O[JSONRPC_ERROR].S[JSONRPC_MESSAGE];
      if JSON.O[JSONRPC_ERROR].Contains(JSONRPC_DATA) then
      begin
        try
          FError.Data := JSONDataValueToTValue(JSON.O[JSONRPC_ERROR].Path[JSONRPC_DATA]);
        except
          FError.Data := JSON.O[JSONRPC_ERROR].Path[JSONRPC_DATA].Value;
        end;
      end;
    end
    else
    begin
      raise EMVCJSONRPCException.Create('Response message must have ''result'' or ''error''.' + sLineBreak +
        'Raw message is: ' + sLineBreak + JSON.ToJSON());
    end;
  end;
end;

procedure TJSONRPCResponse.SetResult(const Value: TValue);
begin
  FResult := Value;
end;

{ TJSONRPCNotification }

constructor TJSONRPCNotification.Create;
begin
  inherited;
  FParams := TJSONRPCRequestParams.Create;
end;

constructor TJSONRPCObject.Create;
begin
  inherited;
end;

function TJSONRPCObject.GetJSON: TJDOJsonObject;
begin
  Result := TJDOJsonObject.Create;
  Result.S[JSONRPC_HEADER] := JSONRPC_VERSION;
end;

function TJSONRPCObject.GetJSONString: string;
begin
  Result := ToString(True);
end;

procedure TJSONRPCRequest.SetID(const Value: TValue);
begin
  FID := Value;
end;

procedure TJSONRPCObject.SetJSON(const Value: TJDOJsonObject);
begin
  // not implemented
  raise Exception.Create('This method must be overwritten by child');
end;

procedure TJSONRPCObject.SetJsonString(const Value: string);
var
  lJSON: TJDOJsonObject;
begin
  try
    lJSON := TJDOJsonObject.Parse(Value) as TJDOJsonObject;
  except
    raise EMVCJSONRPCParseError.Create;
  end;
  try
    AsJSON := lJSON;
  finally
    lJSON.Free;
  end;
end;

function TJSONRPCObject.ToString(const Compact: Boolean): string;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := GetJSON;
  try
    Result := lJSON.ToJSON(Compact);
  finally
    lJSON.Free;
  end;
end;

{ TJSONRPCResponseError }

constructor TJSONRPCResponseError.Create;
begin
  inherited;
  FData := TValue.Empty;
end;

destructor TJSONRPCResponseError.Destroy;
begin
  if not FData.IsEmpty then
  begin
    if FData.IsObjectInstance then
    begin
      FData.AsObject.Free;
    end;
  end;
  inherited;
end;

procedure TJSONRPCResponseError.SetCode(const Value: Integer);
begin
  FCode := Value;
end;

procedure TJSONRPCResponseError.SetData(const Value: TValue);
begin
  if not FData.IsEmpty then
  begin
    if FData.IsObjectInstance then
    begin
      FData.AsObject.Free;
    end;
  end;
  fData := Value;
end;

procedure TJSONRPCResponseError.SetMessage(const Value: string);
begin
  FMessage := Value;
end;

{ TJSONRPCMessage }

function TJSONRPCRequest.GetID: TValue;
begin
  Result := FID;
end;

function TJSONRPCRequest.GetJSON: TJDOJsonObject;
begin
  Result := inherited GetJSON;
  try
    if not FID.IsEmpty then
    begin
      if FID.IsType<string> then
      begin
        Result.S[JSONRPC_ID] := FID.AsString;
      end
      else if FID.IsType<Int32> then
      begin
        Result.I[JSONRPC_ID] := FID.AsInteger;
      end
      else if FID.IsType<Int64> then
      begin
        Result.I[JSONRPC_ID] := FID.AsInt64;
      end
      else
        raise EMVCJSONRPCException.Create('ID can be only Int32, Int64 or String');
    end
    else
    begin
      raise EMVCJSONRPCException.Create('ID cannot be empty in a JSON-RPC request');
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TJSONRPCProxyGenerator }

constructor TJSONRPCProxyGenerator.Create;
begin
  inherited;
end;

procedure RegisterJSONRPCProxyGenerator(const aLanguage: string; const aClass: TJSONRPCProxyGeneratorClass);
begin
  if not Assigned(GProxyGeneratorsRegister) then
  begin
    GProxyGeneratorsRegister := TDictionary<string, TJSONRPCProxyGeneratorClass>.Create();
  end;
  GProxyGeneratorsRegister.AddOrSetValue(aLanguage.ToLower, aClass);
end;

procedure TJSONRPCProxyGenerator.EndGeneration;
begin
  // do nothing
end;

procedure TJSONRPCProxyGenerator.StartGeneration(const aClassName: string);
begin
  // do nothing
end;

{ TJSONRPCRequestParams }

procedure TJSONRPCRequestParams.Add(const Value: TJDOJsonArray);
begin
  Add(Value, pdtJSONArray);
end;

procedure TJSONRPCRequestParams.Add(const Value: TJDOJsonObject);
begin
  Add(Value, pdTJDOJsonObject);
end;

procedure TJSONRPCRequestParams.Add(const Value: Integer);
begin
  Add(Value, pdtInteger);
end;

procedure TJSONRPCRequestParams.Add(const Value: string);
begin
  Add(Value, pdtString);
end;

procedure TJSONRPCRequestParams.Add(const Value: Boolean);
begin
  Add(Value, pdtBoolean);
end;

procedure TJSONRPCRequestParams.Add(const Value: Double);
begin
  Add(Value, pdtFloat);
end;

procedure TJSONRPCRequestParams.Add(const Value: TDateTime);
begin
  Add(Value, pdtDateTime);
end;

procedure TJSONRPCRequestParams.Add(const Value: TTime);
begin
  Add(Value, pdtTime);
end;

procedure TJSONRPCRequestParams.Add(const Value: TDate);
begin
  Add(Value, pdtDate);
end;

procedure TJSONRPCRequestParams.CheckBalancedParams;
begin
  if fParamNames.Count <> fParamValues.Count then
  begin
    raise EMVCJSONRPCException.Create('Cannot mix positional with named parameters');
  end;
end;

procedure TJSONRPCRequestParams.CheckNotNames;
begin
  if fParamNames.Count > 0 then
  begin
    raise EMVCJSONRPCException.Create('Cannot mix positional with named parameters');
  end;
end;

procedure TJSONRPCRequestParams.Clear;
begin
  fParamValues.Clear;
  fParamTypes.Clear;
  fParamNames.Clear;
end;

function TJSONRPCRequestParams.Count: Integer;
begin
  Result := fParamValues.Count;
end;

constructor TJSONRPCRequestParams.Create;
begin
  inherited Create;
  fParamValues := TList<TValue>.Create;
  fParamTypes := TList<TJSONRPCParamDataType>.Create;
  fParamNames := TList<string>.Create;
end;

destructor TJSONRPCRequestParams.Destroy;
var
  lValue: TValue;
begin
  for lValue in fParamValues do
  begin
    if lValue.IsObject then
      lValue.AsObject.Free;
  end;
  fParamValues.Free;
  fParamTypes.Free;
  fParamNames.Free;
  inherited;
end;

function TJSONRPCRequestParams.GetItem(const Index: Integer): TValue;
begin
  Result := fParamValues[index];
end;

function TJSONRPCRequestParams.GetItemDataType(const Index: Integer): TJSONRPCParamDataType;
begin
  Result := fParamTypes[index];
end;

function TJSONRPCRequestParams.GetItemName(const Index: Integer): string;
begin
  Result := fParamNames[index];
end;

function TJSONRPCRequestParams.ToArray: TArray<TValue>;
begin
  Result := fParamValues.ToArray;
end;

procedure TJSONRPCRequestParams.Add(const Value: TValue; const ParamType: TJSONRPCParamDataType);
begin
  CheckNotNames;
  fParamValues.Add(Value);
  fParamTypes.Add(ParamType);
end;

procedure TJSONRPCRequestParams.Add(const Value: TObject);
begin
  Add(Value, pdtObject);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: Boolean);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtBoolean);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TJDOJsonArray);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtJSONArray);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TJDOJsonObject);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdTJDOJsonObject);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: Integer);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtInteger);
end;

procedure TJSONRPCRequestParams.AddByName(const Name, Value: string);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtString);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TValue;
const ParamType: TJSONRPCParamDataType);
begin
  CheckBalancedParams;
  fParamNames.Add(LowerCase(name));
  fParamValues.Add(Value);
  fParamTypes.Add(ParamType);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
  const Value: TObject);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtObject);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: Double);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtFloat);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TDateTime);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtDateTime);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TTime);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtTime);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string; const Value: TDate);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtDate);
end;

{ EMVCJSONRPCException }

constructor EMVCJSONRPCError.Create(const ErrCode: Integer; const ErrMsg: string);
begin
  inherited Create(ErrMsg);
  fJSONRPCErrorCode := ErrCode;
end;

{ TJSONRPCNullResponse }

procedure TJSONRPCNullResponse.CheckForError;
begin

end;

function TJSONRPCNullResponse.GetError: TJSONRPCResponseError;
begin
  Result := FError;
end;

function TJSONRPCNullResponse.GetID: TValue;
begin
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.GetJSON: TJDOJsonObject;
begin
  Result := nil;
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.GetJSONString: string;
begin
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.GetResult: TValue;
begin
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.IsError: Boolean;
begin
  Result := False;
end;

procedure TJSONRPCNullResponse.RaiseErrorForNullObject;
begin
  raise EMVCJSONRPCException.Create('Invalid Call for NULL object');
end;

procedure TJSONRPCNullResponse.ResultAs(Obj: TObject);
begin
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.ResultAsJSONArray: TJDOJsonArray;
begin
  Result := nil;
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.ResultAsJSONObject: TJDOJsonObject;
begin
  Result := nil;
  RaiseErrorForNullObject;
end;

procedure TJSONRPCNullResponse.SetError(const Value: TJSONRPCResponseError);
begin
  FError := Value;
end;

procedure TJSONRPCNullResponse.SetID(const Value: TValue);
begin
  RaiseErrorForNullObject;
end;

procedure TJSONRPCNullResponse.SetJSON(const JSON: TJDOJsonObject);
begin
  RaiseErrorForNullObject;
end;

procedure TJSONRPCNullResponse.SetJsonString(const Value: string);
begin
  RaiseErrorForNullObject;
end;

procedure TJSONRPCNullResponse.SetResult(const Value: TValue);
begin
  RaiseErrorForNullObject;
end;

function TJSONRPCNullResponse.ToString(const Compact: Boolean): string;
begin
  Result := '';
end;

constructor EMVCJSONRPCError.Create(const ErrCode: Integer; const ErrMsg: string; const Data: TValue);
begin
  Create(ErrCode, ErrMsg);
  fJSONRPCErrorData := Data;
end;

constructor EMVCJSONRPCError.CreateFmt(const ErrCode: Integer; const ErrMsg: string; const Args: array of const);
begin
  inherited CreateFmt(ErrMsg, Args);
  fJSONRPCErrorCode := ErrCode;
end;

{ EMVCJSONRPCRemoteException }

constructor EMVCJSONRPCRemoteException.Create(const ErrCode: Integer; const ErrMessage: String);
begin
  Create(ErrCode, ErrMessage, TValue.Empty);
end;

constructor EMVCJSONRPCRemoteException.Create(const ErrCode: Integer; const ErrMessage: String;
  const ErrData: TValue);
begin
  inherited Create(Format('[REMOTE EXCEPTION][CODE: %d] %s', [ErrCode, ErrMessage]));
  fErrData := ErrData;
  fErrCode := ErrCode;
  fErrMessage := ErrMessage;
end;

{ TJSONUtilsHelper }

class function TJSONUtilsHelper.JSONArrayToArrayOfRecord<T>(
  const JSONRPCResponse: IInterface): TArray<T>;
var
  lIntf: IJSONRPCResponse;
begin
  if Supports(JSONRPCResponse, IJSONRPCResponse, lIntf) then
  begin
    Result := TJSONUtils.JSONArrayToArrayOfRecord<T>(lIntf.ResultAsJSONArray);
  end
  else
  begin
    RaiseSerializationError('Parameter doesn''t support IJSONRPCResponse');
  end;
end;

class function TJSONUtilsHelper.JSONObjectToRecord<T>(
  const JSONRPCResponse: IInterface): T;
var
  lIntf: IJSONRPCResponse;
begin
  if Supports(JSONRPCResponse, IJSONRPCResponse, lIntf) then
  begin
    Result := TJSONUtils.JSONObjectToRecord<T>(lIntf.ResultAsJSONObject);
  end
  else
  begin
    RaiseSerializationError('Parameter doesn''t support IJSONRPCResponse');
  end;
end;

initialization

finalization

FreeAndNil(GProxyGeneratorsRegister);

end.
