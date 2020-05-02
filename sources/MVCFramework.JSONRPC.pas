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

unit MVCFramework.JSONRPC;

{
  JSON-RPC over HTTP implemented as described here
  https://www.jsonrpc.org/historical/json-rpc-over-http.html
}

interface

uses
  System.Classes,
  Data.DB,
  System.SysUtils,
  jsondataobjects,
  MVCFramework,
  MVCFramework.Commons,
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.jsondataobjects;

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
  JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE = 'OnBeforeSendResponseHook';
  JSONRPC_HOOKS_METHOD_NAMES: array [0 .. 2] of string = (
    JSONRPC_HOOKS_ON_BEFORE_ROUTING,
    JSONRPC_HOOKS_ON_BEFORE_CALL,
    JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE
    );

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

type
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
    function ToString(const Compact: Boolean): string; reintroduce;
    function GetJSON: TJDOJsonObject; virtual;
    procedure SetJSON(const Value: TJDOJsonObject); virtual;
    property AsJSON: TJDOJsonObject read GetJSON write SetJSON;
    property AsJSONString: string read GetJSONString write SetJsonString;
  public
    constructor Create; virtual;
  end;

  TJSONRPCParamDataType = (pdtString, pdtInteger, pdtLongInteger, pdTJDOJsonObject, pdtJSONArray,
    pdtBoolean, pdtDate,
    pdtTime, pdtDateTime, pdtFloat, pdtObject);

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
    procedure SetCode(const Value: Integer);
    procedure SetMessage(const Value: string);
  public
    property Code: Integer read FCode write SetCode;
    property ErrMessage: string read FMessage write SetMessage;
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
    property Result: TValue read GetResult write SetResult;
    property Error: TJSONRPCResponseError read GetError write SetError;
    property RequestID: TValue read GetID write SetID;
  end;

  TJSONRPCResponse = class(TJSONRPCObject, IJSONRPCResponse)
  private
    FResult: TValue;
    FError: TJSONRPCResponseError;
    FID: TValue;
    function GetResult: TValue;
  protected
    function GetJSON: TJDOJsonObject; override;
    procedure SetJSON(const JSON: TJDOJsonObject); override;
    procedure SetID(const Value: TValue);
    procedure SetResult(const Value: TValue);
    procedure SetError(const Value: TJSONRPCResponseError);
    function GetError: TJSONRPCResponseError;
    function GetID: TValue;
    function ResultAsJSONObject: TJDOJsonObject;
    function ResultAsJSONArray: TJDOJsonArray;
    function IsError: Boolean;
    property Result: TValue read GetResult write SetResult;
    property Error: TJSONRPCResponseError read GetError write SetError;
    property RequestID: TValue read GetID write SetID;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  EMVCJSONRPCInvalidVersion = class(Exception)

  end;

  EMVCJSONRPCException = class(Exception)

  end;

  EMVCJSONRPCErrorResponse = class abstract(Exception)
  protected
    fJSONRPCErrorCode: Integer;
  public
    property JSONRPCErrorCode: Integer read fJSONRPCErrorCode;
  end;

  EMVCJSONRPCError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const ErrCode: Integer; const Msg: string);
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
    fSerializer: TMVCJsonDataObjectsSerializer;
    fRPCInstance: TObject;
    fOwsRPCInstance: Boolean;
    function GetSerializer: TMVCJsonDataObjectsSerializer;
    function GetDeclaredMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
    function GetInheritedMethod(lMethod: string; lRTTIType: TRttiType): TRTTIMethod;
  protected
    function CreateError(
      const RequestID: TValue;
      const ErrorCode: Integer;
      const Message: string): TJDOJsonObject;
    function CreateResponse(const RequestID: TValue; const Value: TValue): TJSONRPCResponse;
    function CreateRequest(const JSON: TJDOJsonObject): IJSONRPCRequest;
    function JSONObjectAs<T: class, constructor>(const JSON: TJDOJsonObject): T;
    function CanBeRemotelyInvoked(const RTTIMethod: TRTTIMethod): Boolean;
    procedure ForEachInvokableMethod(const aProc: TProc<TRTTIMethod>);
    procedure TryToCallMethod(const aRTTIType: TRttiType; const MethodName: string;
      const Parameter: TJDOJsonObject; const ParameterName: string);
  public
    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure Index; virtual;

    [MVCPath('/describe')]
    [MVCHTTPMethods([httpGET])]
    procedure GetPublishedMethodList; virtual;

    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    procedure GetProxyCode; virtual;
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TMVCJSONRPCPublisher = class(TMVCJSONRPCController)
  public
    constructor Create(const RPCInstance: TObject; const Owns: Boolean = True);
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

procedure RegisterJSONRPCProxyGenerator(const aLanguage: string;
  const aClass: TJSONRPCProxyGeneratorClass);

implementation

uses
  MVCFramework.Serializer.Intf,
  MVCFramework.Logger,
  System.TypInfo,
  MVCFramework.Rtti.Utils,
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.jsondataobjects.CustomTypes;

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
            LJObj := lSer.SerializeObjectToJSON(Value.AsObject,
              TMVCSerializationType.stProperties, [], nil);
            JSONArr.Add(LJObj);
          finally
            lSer.Free;
          end;
        end;
      end;
  else
    raise EMVCException.Create('Invalid type');
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
            JSONObj.O[name] := lSer.SerializeObjectToJSON(Value.AsObject,
              TMVCSerializationType.stProperties, [], nil);
          finally
            lSer.Free;
          end;
        end;
      end;
  else
    raise EMVCException.Create('Invalid type');
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
    raise EMVCJSONRPCException.Create('Invalid parameter type');
  end;
end;

function BuildDeclaration(const RTTIParameter: TRttiParameter): string;
begin
  Result := RTTIParameter.Name + ': ' + RTTIParameter.ParamType.Name;
end;

procedure JSONDataValueToTValueParam(const JSONDataValue: TJsonDataValueHelper;
  const RTTIParameter: TRttiParameter;
  const JSONRPCRequestParams: TJSONRPCRequestParams);
begin
  case RTTIParameter.ParamType.TypeKind of
    tkString, tkUString {$IF CompilerVersion > 28}, tkAnsiString {$ENDIF}:
      begin
        if JSONDataValue.Typ <> jdtString then
        begin
          raise EMVCJSONRPCInvalidParams.Create('Invalid param type for [' +
            BuildDeclaration(RTTIParameter) + ']');
        end;
        JSONRPCRequestParams.Add(JSONDataValue.Value);
      end;
    tkFloat:
      begin
        if SameText(RTTIParameter.ParamType.Name, 'TDate') then
        begin
          JSONRPCRequestParams.Add(ISODateToDate(JSONDataValue.Value), pdtDate);
        end
        else
          if SameText(RTTIParameter.ParamType.Name, 'TDateTime') then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.UtcDateTimeValue, pdtDateTime);
        end
        else
          if SameText(RTTIParameter.ParamType.Name, 'TTime') then
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
    tkClass:
      begin
        if (SameText(RTTIParameter.ParamType.Name, TJDOJsonArray.ClassName)) then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ArrayValue.Clone, pdtJSONArray);
        end
        else
          if SameText(RTTIParameter.ParamType.Name, TJDOJsonObject.ClassName) then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ObjectValue.Clone as TJDOJsonObject,
            pdTJDOJsonObject);
        end
        else
        begin
          { TODO -oDanieleT -cGeneral : Automatically inject the dseserialized version of arbitrary object? }
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;
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
        else
          if JSONDataValue.Typ = jdtULong then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ULongValue, pdtLongInteger);
        end
        else
          if JSONDataValue.Typ = jdtULong then
        begin
          JSONRPCRequestParams.Add(JSONDataValue.ULongValue, pdtLongInteger);
        end
        else
        begin
          raise EMVCJSONRPCInvalidRequest.Create(BuildDeclaration(RTTIParameter));
        end;

      end;
  else
    raise EMVCJSONRPCInvalidRequest.Create('Invalid parameter type for ' +
      BuildDeclaration(RTTIParameter));
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

class procedure TMVCJSONRPCMessage.CheckID(const aJSON: TMVCJSONObject;
  out aIsNotification: Boolean);
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
      raise EMVCJSONRPCException.Create
        ('Message is not a notification but its ''id'' property is not valid');
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

constructor TMVCJSONRPCPublisher.Create(const RPCInstance: TObject; const Owns: Boolean);
begin
  inherited Create;
  fRPCInstance := RPCInstance;
  fOwsRPCInstance := Owns;
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

function TMVCJSONRPCController.CanBeRemotelyInvoked(
  const RTTIMethod: TRTTIMethod): Boolean;
begin
  Result := (RTTIMethod.Visibility = mvPublic) and
    (RTTIMethod.MethodKind in [mkProcedure, mkFunction]);
  Result := Result and not IsReservedMethodName(RTTIMethod.Name);
end;

constructor TMVCJSONRPCController.Create;
begin
  inherited Create;
  fRPCInstance := Self;
  fOwsRPCInstance := False;
end;

function TMVCJSONRPCController.CreateError(
  const RequestID: TValue;
  const ErrorCode: Integer;
  const Message: string): TJDOJsonObject;
var
  lErrResp: TJSONRPCResponse;
begin
  lErrResp := TJSONRPCResponse.Create;
  try
    lErrResp.RequestID := RequestID;
    lErrResp.Error := TJSONRPCResponseError.Create;
    lErrResp.Error.Code := ErrorCode;
    lErrResp.Error.ErrMessage := message;
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
  else
    if JSON.Types[JSONRPC_ID] = jdtInt then
    lReqID := JSON.I[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtLong then
    lReqID := JSON.L[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtULong then
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

function TMVCJSONRPCController.CreateResponse(const RequestID: TValue; const Value: TValue)
  : TJSONRPCResponse;
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

function TMVCJSONRPCController.GetDeclaredMethod(lMethod: string; lRTTIType: TRttiType)
  : TRTTIMethod;
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

function TMVCJSONRPCController.GetInheritedMethod(lMethod: string;
  lRTTIType: TRttiType): TRTTIMethod;
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

procedure TMVCJSONRPCController.ForEachInvokableMethod(const aProc: TProc<TRTTIMethod>);
var
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIMethodList: TArray<TRTTIMethod>;
  lRTTIMethod: TRTTIMethod;
begin
  lRTTI := TRTTIContext.Create;
  try
    lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);
    lRTTIMethodList := lRTTIType.GetDeclaredMethods;
    for lRTTIMethod in lRTTIMethodList do
    begin
      if CanBeRemotelyInvoked(lRTTIMethod) then
      begin
        aProc(lRTTIMethod);
      end;
    end;

    lRTTIMethodList := lRTTIType.BaseType.GetMethods;
    for lRTTIMethod in lRTTIMethodList do
    begin
      if TMVCSerializerHelper.HasAttribute<MVCInheritableAttribute>(lRTTIMethod) and
        CanBeRemotelyInvoked(lRTTIMethod) then
      begin
        aProc(lRTTIMethod);
      end;
    end;
  finally
    lRTTI.Free;
  end;
end;

procedure TMVCJSONRPCController.GetProxyCode;
var
  lLanguage: string;
  lClass: TJSONRPCProxyGeneratorClass;
  lGenerator: TJSONRPCProxyGenerator;
  lRTTI: TRTTIContext;
begin
  lLanguage := Context.Request.Params['language'].ToLower;
  if lLanguage.IsEmpty then
  begin
    lLanguage := 'delphi';
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
      Context.Response.ContentType := 'text/plain';
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
    begin
      if IsReservedMethodName(aRTTIMethod.Name) then
      begin
        Exit;
      end;
      lAtt := TRTTIUtils.GetAttribute<MVCDocAttribute>(aRTTIMethod);
      if Assigned(lAtt) then
      begin
        ResponseStream.AppendLine('// ' + lAtt.Value);
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
begin
  lRTTIType := nil;
  lReqID := TValue.Empty;
  SetLength(lParamsToInject, 0);
  lRTTI := TRTTIContext.Create;
  try
    try
      lJSON := StrToJSONObject(Context.Request.Body);
      try
        if not Assigned(lJSON) then
          raise EMVCJSONRPCParseError.Create;
        lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);
        TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_ROUTING, lJSON, 'JSON');
        lJSONRPCReq := CreateRequest(lJSON);
        lMethod := lJSONRPCReq.Method;

        if SameText(lMethod, JSONRPC_HOOKS_ON_BEFORE_ROUTING) or
          SameText(lMethod, JSONRPC_HOOKS_ON_BEFORE_CALL) or
          SameText(lMethod, JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE)
        then
        begin
          raise EMVCJSONRPCInvalidRequest.Create('Requested method name is reserved and cannot be called remotely');
        end;

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
          if not CanBeRemotelyInvoked(lRTTIMethod) then
          begin
            LogW(Format
              ('Method "%s" cannot be called. Only public functions or procedures can be called. ',
              [lMethod]));
            raise EMVCJSONRPCMethodNotFound.Create(lMethod);
          end;

          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Request) and
            (lRTTIMethod.MethodKind <> mkFunction) then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a procedure using a JSON-RPC request. [HINT] Use requests for functions and notifications for procedures');
          end;

          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Notification) and
            (lRTTIMethod.MethodKind <> mkProcedure)
          then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a function using a JSON-RPC notification. [HINT] Use requests for functions and notifications for procedures');
          end;

          try
            lJSONRPCReq.FillParameters(lJSON, lRTTIMethod);
          except
            on Ex: EMVCJSONRPCErrorResponse do
            begin
              raise EMVCJSONRPCInvalidParams.Create('Cannot map all parameters to remote method. ' + Ex.Message);
            end;
          end;

          try
            TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_CALL, lJSON, 'JSONRequest');
            LogD('[JSON-RPC][CALL][' + CALL_TYPE[lRTTIMethod.MethodKind] + '] ' + lRTTIMethod.Name);
            lRes := lRTTIMethod.Invoke(fRPCInstance, lJSONRPCReq.Params.ToArray);
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
                try
                  TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE, lJSONResp, 'JSONResponse');
                  Render(lJSONResp);
                except
                  try
                    lJSONResp.Free;
                  except
                    // do nothing
                  end;
                end;
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
        lJSON.Free;
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
        lJSON := CreateError(lReqID, E.JSONRPCErrorCode, E.Message);
        try
          TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE, lJSON, 'JSONResponse');
          Render(lJSON, False);
        finally
          lJSON.Free;
        end;
        LogE(Format('[JSON-RPC][CLS %s][ERR %d][MSG "%s"]', [E.ClassName, E.JSONRPCErrorCode,
          E.Message]));
      end;
      on Ex: Exception do // use another name for exception variable, otherwise E is nil!!
      begin
        lJSON := CreateError(lReqID, 0, Ex.Message);
        try
          TryToCallMethod(lRTTIType, JSONRPC_HOOKS_ON_BEFORE_SEND_RESPONSE, lJSON, 'JSONResponse');
          Render(lJSON, False);
        finally
          lJSON.Free;
        end;
        LogE(Format('[JSON-RPC][CLS %s][MSG "%s"]', [Ex.ClassName, Ex.Message]));
      end;
    end;
  finally
    lRTTI.Free;
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
const Parameter: TJDOJsonObject; const ParameterName: string);
var
  lHookMethod: TRTTIMethod;
  lHookParam: TRttiParameter;
  lHookParamParamType: string;
  lHookParamName: string;
begin
  if not Assigned(aRTTIType) then
  begin
    Exit;
  end;
  lHookMethod := aRTTIType.GetMethod(MethodName);
  if Assigned(lHookMethod) then
  begin
    if (Length(lHookMethod.GetParameters) <> 1) then
      raise EMVCJSONRPCException.CreateFmt(
        'Invalid signature for %s Hook method [HINT: procedure ' +
        '%s.%s(const %s: TJDOJsonObject)',
        [
        MethodName,
        fRPCInstance.ClassName,
        MethodName,
        ParameterName
        ]);
    lHookParam := lHookMethod.GetParameters[0];
    lHookParamParamType := lHookParam.ParamType.ToString.ToLower;
    lHookParamName := lHookParam.Name.ToLower;
    if ((lHookParamParamType <> 'tjdojsonobject') and (lHookParamParamType <> 'tjsonobject')) or
      (lHookParam.Flags * [pfConst, pfAddress] <> [pfConst, pfAddress]) or (lHookParamName <> ParameterName.ToLower)
    then
      raise EMVCJSONRPCException.CreateFmt(
        'Invalid signature for %s Hook method [HINT: procedure ' +
        '%s.%s(const %s: TJDOJsonObject)',
        [
        MethodName,
        fRPCInstance.ClassName,
        MethodName,
        ParameterName
        ]);
    lHookMethod.Invoke(fRPCInstance, [Parameter])
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
  inherited CreateFmt('Method "%s" not found. The method does not exist or is not available.',
    [MethodName]);
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

constructor EMVCJSONRPCServerError.Create(const JSONRPCError: Integer;
const
  Message:
  string);
begin
  inherited Create(message);
  fJSONRPCErrorCode := JSONRPCError;
end;

{ TJSONRPCRequest }

constructor TJSONRPCRequest.Create(const aID: TValue;
const
  aMethod:
  string);
begin
  inherited Create(aMethod);
  SetID(aID);
end;

constructor TJSONRPCRequest.Create(
  const
  aID:
  TValue);
begin
  inherited Create;
  SetID(aID);
end;

constructor TJSONRPCRequest.Create;
begin
  inherited Create;
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

procedure TJSONRPCRequest.SetJSON(
  const
  JSON:
  TJDOJsonObject);
begin
  if JSON.Types[JSONRPC_ID] = jdtString then
    RequestID := JSON.S[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtInt then
    RequestID := JSON.I[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtLong then
    RequestID := JSON.L[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtULong then
    RequestID := JSON.U[JSONRPC_ID]
  else
    RequestID := TValue.Empty;
  Method := JSON.S[JSONRPC_METHOD];
  Params.Clear;
end;

constructor TJSONRPCNotification.Create(
  const
  aMethod:
  string);
begin
  Create;
  Method := aMethod;
end;

destructor TJSONRPCNotification.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TJSONRPCNotification.FillParameters(
  const JSON: TJDOJsonObject;
const RTTIMethod: TRTTIMethod);
var
  lRTTIMethodParams: TArray<TRttiParameter>;
  lRTTIMethodParam: TRttiParameter;
  lJSONParams: TJDOJsonArray;
  lJSONNamedParams: TJDOJsonObject;
  I: Integer;
  lUseNamedParams: Boolean;
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
  else
    if JSON.Types[JSONRPC_PARAMS] <> jdtNone then
  begin
    raise EMVCJSONRPCException.Create('Params must be a JSON array or null');
  end;

  lRTTIMethodParams := RTTIMethod.GetParameters;
  if lUseNamedParams then
  begin
    if (Length(lRTTIMethodParams) > 0) and (not Assigned(lJSONNamedParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected %d got %d.',
        [Length(lRTTIMethodParams), 0]);

    if Assigned(lJSONNamedParams) and (Length(lRTTIMethodParams) <> lJSONNamedParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected %d got %d.',
        [Length(lRTTIMethodParams), lJSONNamedParams.Count]);
  end
  else
  begin
    if (Length(lRTTIMethodParams) > 0) and (not Assigned(lJSONParams)) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected %d got %d.',
        [Length(lRTTIMethodParams), 0]);

    if Assigned(lJSONParams) and (Length(lRTTIMethodParams) <> lJSONParams.Count) then
      raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected %d got %d.',
        [Length(lRTTIMethodParams), lJSONParams.Count]);
  end;

  for lRTTIMethodParam in lRTTIMethodParams do
  begin
    if lRTTIMethodParam.Flags * [pfVar, pfOut, pfArray, pfReference] <> [] then
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
      JSONDataValueToTValueParam(lJSONNamedParams.Values[lRTTIMethodParams[I].Name.ToLower], lRTTIMethodParams[I], Params);
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
        AppendTValueToJsonObject(
          FParams.fParamValues[I],
          FParams.fParamNames[I],
          FParams.fParamTypes[I],
          Result.O[JSONRPC_PARAMS]);
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

procedure TJSONRPCNotification.SetMethod(
  const
  Value:
  string);
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
begin
  Result := inherited;
  // Must generate something like the following:
  // {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}

  if FID.IsEmpty then
  begin
    Result.Values[JSONRPC_ID] := jdtNone;
  end
  else
    if FID.IsType<string> then
  begin
    Result.S[JSONRPC_ID] := FID.AsString;
  end
  else
    if FID.IsType<Int32> then
  begin
    Result.I[JSONRPC_ID] := FID.AsInteger;
  end
  else
    if FID.IsType<Int64> then
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
    end
    else
    begin
      TValueToJsonElement(Self.FResult, Result, JSONRPC_RESULT);
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

procedure TJSONRPCResponse.SetError(
  const
  Value:
  TJSONRPCResponseError);
begin
  FError := Value;
end;

procedure TJSONRPCResponse.SetID(
  const
  Value:
  TValue);
begin
  FID := Value;
end;

procedure TJSONRPCResponse.SetJSON(const JSON: TJDOJsonObject);
begin
  if JSON.Types[JSONRPC_ID] = jdtString then
    RequestID := JSON.S[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtInt then
    RequestID := JSON.I[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtLong then
    RequestID := JSON.L[JSONRPC_ID]
  else
    if JSON.Types[JSONRPC_ID] = jdtULong then
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
    end
    else
    begin
      raise EMVCJSONRPCException.Create('Response message must have ''result'' or ''error''.' + sLineBreak +
        'Raw message is: ' + sLineBreak +
        JSON.ToJSON());
    end;
  end;
end;

procedure TJSONRPCResponse.SetResult(
  const
  Value:
  TValue);
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

procedure TJSONRPCRequest.SetID(
  const
  Value:
  TValue);
begin
  FID := Value;
end;

procedure TJSONRPCObject.SetJSON(
  const
  Value:
  TJDOJsonObject);
begin
  // not implemented
  raise Exception.Create('This method must be overwritten by child');
end;

procedure TJSONRPCObject.SetJsonString(
  const
  Value:
  string);
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

function TJSONRPCObject.ToString(
  const
  Compact:
  Boolean): string;
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

procedure TJSONRPCResponseError.SetCode(
  const
  Value:
  Integer);
begin
  FCode := Value;
end;

procedure TJSONRPCResponseError.SetMessage(
  const
  Value:
  string);
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
  if not FID.IsEmpty then
  begin
    if FID.IsType<string> then
    begin
      Result.S[JSONRPC_ID] := FID.AsString;
    end
    else
      if FID.IsType<Int32> then
    begin
      Result.I[JSONRPC_ID] := FID.AsInteger;
    end
    else
      if FID.IsType<Int64> then
    begin
      Result.I[JSONRPC_ID] := FID.AsInt64;
    end
    else
      raise EMVCJSONRPCException.Create('ID can be only Int32, Int64 or String');
  end;

end;

{ TJSONRPCProxyGenerator }

constructor TJSONRPCProxyGenerator.Create;
begin
  inherited;
end;

procedure RegisterJSONRPCProxyGenerator(
  const
  aLanguage:
  string;
const
  aClass:
  TJSONRPCProxyGeneratorClass);
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

function TJSONRPCRequestParams.GetItem(
  const
  Index:
  Integer): TValue;
begin
  Result := fParamValues[index];
end;

function TJSONRPCRequestParams.GetItemDataType(
  const
  Index:
  Integer): TJSONRPCParamDataType;
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

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: Boolean);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtBoolean);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TJDOJsonArray);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtJSONArray);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TJDOJsonObject);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdTJDOJsonObject);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: Integer);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtInteger);
end;

procedure TJSONRPCRequestParams.AddByName(const Name, Value: string);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtString);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TValue; const ParamType: TJSONRPCParamDataType);
begin
  CheckBalancedParams;
  fParamNames.Add(LowerCase(name));
  fParamValues.Add(Value);
  fParamTypes.Add(ParamType);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: Double);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtFloat);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TDateTime);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtDateTime);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TTime);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtTime);
end;

procedure TJSONRPCRequestParams.AddByName(const Name: string;
const Value: TDate);
begin
  AddByName(name, Value, TJSONRPCParamDataType.pdtDate);
end;

{ EMVCJSONRPCException }

constructor EMVCJSONRPCError.Create(const ErrCode: Integer;
const Msg: string);
begin
  inherited Create(Msg);
  fJSONRPCErrorCode := ErrCode;
end;

initialization

finalization

FreeAndNil(GProxyGeneratorsRegister);

end.
