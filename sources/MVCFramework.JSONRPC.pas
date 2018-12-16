// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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

type
  IMVCJSONRPCMessage = interface
    ['{73B8D463-75E1-404B-8437-EF4B3C950D2F}']
    function AsJSONRPCMessage: string;
  end;

  TMVCJSONRPCMessage = class abstract(TInterfacedObject, IMVCJSONRPCMessage)
  private
    fJSON: TJsonObject;
  protected
    class procedure CheckVersion(const aJSON: TJsonObject);
    class procedure CheckMethod(const aJSON: TJsonObject);
    class procedure CheckID(const aJSON: TJsonObject; out aIsNotification: Boolean);
    constructor Create; overload;
    procedure Build(const aJSON: TJsonObject); virtual; abstract;
    { IMVCJSONRPCMessage }
    function AsJSONRPCMessage: string;
    function AsJSON: TJsonObject; virtual;
  end;

  IJSONRPCObject = interface
    ['{98E161EE-B106-4023-8722-3C2CB1B4CE87}']
    procedure SetJsonString(const Value: string);
    function GetJSONString: string;
    function GetJSON: TJsonObject;
    procedure SetJSON(const Value: TJsonObject);
    property AsJSON: TJsonObject read GetJSON write SetJSON;
    property AsJSONString: string read GetJSONString write SetJsonString;
  end;

  TJSONRPCObject = class(TInterfacedObject, IJSONRPCObject)
  protected
    procedure SetJsonString(const Value: string); virtual;
    function GetJSONString: string; virtual;
    function GetJSON: TJsonObject; virtual;
    procedure SetJSON(const Value: TJsonObject); virtual;
    property AsJSON: TJsonObject read GetJSON write SetJSON;
    property AsJSONString: string read GetJSONString write SetJsonString;
  public
    constructor Create; virtual;
  end;

  TJSONRPCRequestParams = TList<TValue>;

  IJSONRPCNotification = interface(IJSONRPCObject)
    ['{FAA65A29-3305-4303-833E-825BDBD3FF7F}']
    procedure SetMethod(const Value: string);
    function GetMethod: string;
    function GetParams: TJSONRPCRequestParams;
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONRPCRequestParams read GetParams;
  end;

  TJSONRPCNotification = class(TJSONRPCObject, IJSONRPCObject, IJSONRPCNotification)
  protected
    FMethod: string;
    FParams: TJSONRPCRequestParams;
    procedure SetMethod(const Value: string);
    function GetMethod: string;
    function GetParams: TJSONRPCRequestParams;
    function GetJSON: TJsonObject; override;
    property Method: string read GetMethod write SetMethod;
    property Params: TJSONRPCRequestParams read GetParams;
  public
    constructor Create; overload; override;
    constructor Create(const aMethod: String); reintroduce; overload;
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
    procedure SetJSON(const JSON: TJsonObject); override;
    function GetJSON: TJsonObject; override;
    procedure SetID(const Value: TValue);
    property RequestType: TJSONRPCRequestType read GetRequestType;
    property RequestID: TValue read GetID write SetID;
  public
    constructor Create(const aID: TValue; const aMethod: String); overload; virtual;
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
    function ResultAsJSONObject: TJsonObject;
    function ResultAsJSONArray: TJsonArray;
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
    function GetJSON: TJsonObject; override;
    procedure SetJSON(const JSON: TJsonObject); override;
    procedure SetID(const Value: TValue);
    procedure SetResult(const Value: TValue);
    procedure SetError(const Value: TJSONRPCResponseError);
    function GetError: TJSONRPCResponseError;
    function GetID: TValue;
    function ResultAsJSONObject: TJsonObject;
    function ResultAsJSONArray: TJsonArray;
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
  private
    FJSONRPCErrorCode: Integer;
  public
    property JSONRPCErrorCode: Integer read FJSONRPCErrorCode;
  end;

  EMVCJSONRPCParseError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create;
  end;

  EMVCJSONRPCInvalidRequest = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create;
  end;

  EMVCJSONRPCMethodNotFound = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const MethodName: string);
  end;

  EMVCJSONRPCInvalidParams = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const message: string);
  end;

  EMVCJSONRPCInternalError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create;
  end;

  { -32000 to -32099	Server error	Reserved for implementation-defined server-errors. }
  EMVCJSONRPCServerError = class(EMVCJSONRPCErrorResponse)
  public
    constructor Create(const JSONRPCError: Integer; const message: string);
  end;

  TMVCJSONObject = TJsonObject;
  TMVCJSONArray = TJDOJsonArray;

  TMVCJSONRPCController = class(TMVCController)
  private
    fSerializer: TMVCJsonDataObjectsSerializer;
    fRPCInstance: TObject;
    fOwsRPCInstance: Boolean;
    function GetSerializer: TMVCJsonDataObjectsSerializer;
    procedure InjectParams(lJSONRPCReq: TJSONRPCRequest; lRTTIMethod: TRttiMethod);
  protected
    function CreateError(const RequestID: TValue; const ErrorCode: Integer; const message: string): TJsonObject;
    function CreateResponse(const RequestID: TValue; const Value: TValue): TJSONRPCResponse;
    function CreateRequest(const JSON: TJsonObject): TJSONRPCRequest;
    function JSONObjectAs<T: class, constructor>(const JSON: TJsonObject): T;
  public
    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure Index; virtual;

    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]
    procedure GetProxyCode; virtual;
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TMVCJSONRPCPublisher = class(TMVCJSONRPCController)
  public
    constructor Create(const RPCInstance: TObject; const Owns: Boolean = True); reintroduce; overload;
  end;

  TJSONRPCProxyGenerator = class abstract
  public
    constructor Create; virtual;
    procedure StartGeneration; virtual;
    procedure EndGeneration; virtual;
    procedure VisitMethod(const aRTTIMethod: TRttiMethod); virtual; abstract;
    function GetCode: String; virtual; abstract;
  end;

  TJSONRPCProxyGeneratorClass = class of TJSONRPCProxyGenerator;

procedure RegisterJSONRPCProxyGenerator(const aLanguage: String; const aClass: TJSONRPCProxyGeneratorClass);

implementation

uses
  MVCFramework.Serializer.Intf, MVCFramework.Logger,
  System.TypInfo, MVCFramework.DuckTyping,
  MVCFramework.Serializer.jsondataobjects.CustomTypes;

const
  CALL_TYPE: array [mkProcedure .. mkFunction] of string = ('PROCEDURE', 'FUNCTION');

var
  GProxyGeneratorsRegister: TDictionary<String, TJSONRPCProxyGeneratorClass>;

function JSONDataValueToTValue(const JSONDataValue: TJsonDataValueHelper): TValue;
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
        Result := TJsonArray.Parse(JSONDataValue.ArrayValue.ToJSON) as TJsonArray;
      end;
    jdtObject:
      begin
        { TODO -oDanieleT -cGeneral : Can be deserialized in a PODO? }
        Result := TJsonObject.Parse(JSONDataValue.ObjectValue.ToJSON) as TJsonObject;
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

{ TMVCJSONRPCMessage }

function TMVCJSONRPCMessage.AsJSON: TJsonObject;
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

constructor TMVCJSONRPCPublisher.Create(const RPCInstance: TObject; const Owns: Boolean);
begin
  inherited Create;
  fRPCInstance := RPCInstance;
  fOwsRPCInstance := Owns;
end;

constructor TMVCJSONRPCController.Create;
begin
  inherited Create;
  fRPCInstance := Self;
  fOwsRPCInstance := False;
end;

function TMVCJSONRPCController.CreateError(const RequestID: TValue; const ErrorCode: Integer; const message: string)
  : TJsonObject;
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

function TMVCJSONRPCController.CreateRequest(const JSON: TJsonObject): TJSONRPCRequest;
var
  I: Integer;
  lParams: TJsonArray;
  lReqID: TValue;
  lMethodName: String;
begin
  try
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
  finally
    JSON.Free;
  end;
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

procedure TMVCJSONRPCController.InjectParams(lJSONRPCReq: TJSONRPCRequest; lRTTIMethod: TRttiMethod);
var
  lRTTIMethodParams: TArray<TRttiParameter>;
  lRTTIMethodParam: TRttiParameter;
begin
  lRTTIMethodParams := lRTTIMethod.GetParameters;
  if (Length(lRTTIMethodParams) <> lJSONRPCReq.Params.Count) then
    raise EMVCJSONRPCInvalidParams.CreateFmt('Wrong parameters count. Expected %d got %d.',
      [Length(lRTTIMethodParams), lJSONRPCReq.Params.Count]);
  for lRTTIMethodParam in lRTTIMethodParams do
  begin
    if lRTTIMethodParam.Flags * [pfVar, pfOut, pfArray, pfReference] <> [] then
      raise EMVCJSONRPCInvalidParams.CreateFmt
        ('Parameter modifier not supported for formal parameter [%s]. Only const and value modifiers are allowed.',
        [lRTTIMethodParam.Name]);
  end;
end;

procedure TMVCJSONRPCController.GetProxyCode;
var
  lLanguage: string;
  lClass: TJSONRPCProxyGeneratorClass;
  lGenerator: TJSONRPCProxyGenerator;
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lMethod: TRttiMethod;
begin
  if not Context.Request.QueryStringParamExists('language') then
  begin
    raise EMVCJSONRPCException.Create('Query string parameter "language" is required');
  end;

  lLanguage := Context.Request.Params['language'].ToLower;

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
      lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);
      lGenerator.StartGeneration();
      for lMethod in lRTTIType.GetMethods do
      begin
        lGenerator.VisitMethod(lMethod);
      end;
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

function TMVCJSONRPCController.GetSerializer: TMVCJsonDataObjectsSerializer;
begin
  if not Assigned(fSerializer) then
    fSerializer := TMVCJsonDataObjectsSerializer.Create;
  Result := fSerializer;
end;

procedure TMVCJSONRPCController.Index;
var
  lJSONRPCReq: TJSONRPCRequest;
  lMethod: string;
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIMethod: TRttiMethod;
  lRes: TValue;
  lJSONRPCResponse: TJSONRPCResponse;
  lParamsToInject: TArray<TValue>;
  lReqID: TValue;
  lJSON: TJsonObject;
begin
  lReqID := TValue.Empty;
  SetLength(lParamsToInject, 0);
  try
    lJSON := StringToJSON(Context.Request.Body);
    if not Assigned(lJSON) then
      raise EMVCJSONRPCParseError.Create;
    lJSONRPCReq := CreateRequest(lJSON);
    try
      lMethod := lJSONRPCReq.Method;

      if lJSONRPCReq.RequestType = TJSONRPCRequestType.Request then
      begin
        if lJSONRPCReq.RequestID.IsEmpty then
          raise EMVCJSONRPCInvalidRequest.Create;
        lReqID := lJSONRPCReq.RequestID;
      end;

      lRTTI := TRTTIContext.Create;
      try
        lRTTIType := lRTTI.GetType(fRPCInstance.ClassType);
        lRTTIMethod := lRTTIType.GetMethod(lMethod);
        if Assigned(lRTTIMethod) then
        begin
          if (lRTTIMethod.Visibility <> mvPublic) or (not(lRTTIMethod.MethodKind in [mkProcedure, mkFunction])) then
          begin
            LogW(Format('Method "%s" cannot be called. Only public functions or procedures can be called. ',
              [lMethod]));
            raise EMVCJSONRPCMethodNotFound.Create(lMethod);
          end;

          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Request) and (lRTTIMethod.MethodKind <> mkFunction) then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a procedure using a JSON-RPC request. [HINT] Use requests for functions and notifications for procedures');
          end;

          if (lJSONRPCReq.RequestType = TJSONRPCRequestType.Notification) and (lRTTIMethod.MethodKind <> mkProcedure)
          then
          begin
            raise EMVCJSONRPCInvalidParams.Create
              ('Cannot call a function using a JSON-RPC notification. [HINT] Use requests for functions and notifications for procedures');
          end;

          InjectParams(lJSONRPCReq, lRTTIMethod);
          try
            LogD('[JSON-RPC][CALL][' + CALL_TYPE[lRTTIMethod.MethodKind] + '] ' + lRTTIMethod.Name);
            lRes := lRTTIMethod.Invoke(fRPCInstance, lJSONRPCReq.Params.ToArray);
          except
            on E: EInvalidCast do
            begin
              raise EMVCJSONRPCInvalidParams.Create('Check your input parameters types');
            end;
          end;

          case lJSONRPCReq.RequestType of
            TJSONRPCRequestType.Notification:
              begin
                if lRes.IsObject then
                  lRes.AsObject.Free;
                ResponseStatus(HTTP_STATUS.NoContent);
              end;
            TJSONRPCRequestType.Request:
              begin
                lJSONRPCResponse := CreateResponse(lJSONRPCReq.RequestID, lRes);
                try
                  ResponseStatus(200);
                  Render(lJSONRPCResponse.AsJSON);
                finally
                  lJSONRPCResponse.Free;
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
        lRTTI.Free;
      end;
    finally
      lJSONRPCReq.Free;
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
        - 32700:
          ResponseStatus(500);
        -32600:
          ResponseStatus(400);
        -32601:
          ResponseStatus(404);
        -32602:
          ResponseStatus(500);
        -32603:
          ResponseStatus(500);
        -32099 .. -32000:
          ResponseStatus(500);
      end;
      Render(CreateError(lReqID, E.JSONRPCErrorCode, E.message), True);
      LogE(Format('[JSON-RPC][CLS %s][ERR %d][MSG "%s"]', [E.ClassName, E.JSONRPCErrorCode, E.message]));
    end;
    on E: Exception do
    begin
      Render(CreateError(lReqID, 0, E.message), True);
      LogE(Format('[JSON-RPC][CLS %s][MSG "%s"]', [E.ClassName, E.message]));
    end;
  end;
end;

function TMVCJSONRPCController.JSONObjectAs<T>(const JSON: TJsonObject): T;
begin
  Result := T.Create;
  try
    GetSerializer.JsonObjectToObject(JSON, Result, TMVCSerializationType.stProperties, []);
  except
    Result.Free;
    raise;
  end;
end;

{ EMVCJSONRPCParseError }

constructor EMVCJSONRPCParseError.Create;
begin
  inherited Create
    ('Parse error. Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text');
  FJSONRPCErrorCode := -32700;
end;

{ EMVCJSONRPCInvalidRequest }

constructor EMVCJSONRPCInvalidRequest.Create;
begin
  inherited Create('Invalid Request. The JSON sent is not a valid Request object.');
  FJSONRPCErrorCode := -32600;
end;

{ EMVCJSONRPCMethodNotFound }

constructor EMVCJSONRPCMethodNotFound.Create(const MethodName: string);
begin
  inherited CreateFmt('Method "%s" not found. The method does not exist or is not available.', [MethodName]);
  FJSONRPCErrorCode := -32601;
end;

{ EMVCJSONRPCInvalidParams }

constructor EMVCJSONRPCInvalidParams.Create(const message: string);
begin
  inherited Create('Invalid params. [hint: ' + message + ']');
  FJSONRPCErrorCode := -32602;
end;

{ EMVCJSONRPCInternalError }

constructor EMVCJSONRPCInternalError.Create;
begin
  inherited Create('Internal JSON-RPC error');
  FJSONRPCErrorCode := -32603;
end;

{ EMVCJSONRPCServerError }

constructor EMVCJSONRPCServerError.Create(const JSONRPCError: Integer; const message: string);
begin
  inherited Create(message);
  FJSONRPCErrorCode := JSONRPCError;

end;

{ TJSONRPCRequest }

constructor TJSONRPCRequest.Create(const aID: TValue; const aMethod: String);
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

procedure TJSONRPCRequest.SetJSON(const JSON: TJsonObject);
var
  I: Integer;
  lParams: TJsonArray;
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
  if JSON.Types[JSONRPC_PARAMS] = jdtArray then
  begin
    lParams := JSON.A[JSONRPC_PARAMS];
    for I := 0 to lParams.Count - 1 do
    begin
      Params.Add(JSONDataValueToTValue(lParams[I]));
    end;
  end
  else if JSON.Types[JSONRPC_PARAMS] <> jdtNone then
  begin
    raise EMVCJSONRPCException.Create('Params must be a JSON array or null');
  end;
end;

constructor TJSONRPCNotification.Create(const aMethod: String);
begin
  Create;
  Method := aMethod;
end;

destructor TJSONRPCNotification.Destroy;
var
  lValue: TValue;
begin
  for lValue in FParams do
  begin
    if lValue.IsObject then
      lValue.AsObject.Free;
  end;
  FParams.Free;
  inherited;
end;

function TJSONRPCNotification.GetJSON: TJsonObject;
var
  I: Integer;
begin
  if FMethod.IsEmpty then
    raise EMVCJSONRPCException.Create('JSON-RPC "Method" cannot be empty');
  Result := inherited;
  Result.S[JSONRPC_METHOD] := FMethod;
  if FParams.Count > 0 then
  begin
    for I := 0 to FParams.Count - 1 do
    begin
      AppendTValueToJsonArray(FParams[I], Result.A[JSONRPC_PARAMS]);
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

function TJSONRPCResponse.GetJSON: TJsonObject;
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

function TJSONRPCResponse.ResultAsJSONArray: TJsonArray;
begin
  Result := Self.Result.AsObject as TJsonArray;
end;

function TJSONRPCResponse.ResultAsJSONObject: TJsonObject;
begin
  Result := Self.Result.AsObject as TJsonObject;
end;

procedure TJSONRPCResponse.SetError(const Value: TJSONRPCResponseError);
begin
  FError := Value;
end;

procedure TJSONRPCResponse.SetID(const Value: TValue);
begin
  FID := Value;
end;

procedure TJSONRPCResponse.SetJSON(const JSON: TJsonObject);
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
    end
    else
      raise EMVCJSONRPCException.Create('Response message must have ''result'' or ''error''');
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

function TJSONRPCObject.GetJSON: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S[JSONRPC_HEADER] := JSONRPC_VERSION;
end;

function TJSONRPCObject.GetJSONString: string;
var
  lJSON: TJsonObject;
begin
  lJSON := GetJSON;
  try
    Result := lJSON.ToJSON;
  finally
    lJSON.Free;
  end;
end;

procedure TJSONRPCRequest.SetID(const Value: TValue);
begin
  FID := Value;
end;

procedure TJSONRPCObject.SetJSON(const Value: TJsonObject);
begin
  // not implemented
  raise Exception.Create('This method must be overwritten by child');
end;

procedure TJSONRPCObject.SetJsonString(const Value: string);
var
  lJSON: TJsonObject;
begin
  try
    lJSON := TJsonObject.Parse(Value) as TJsonObject;
  except
    raise EMVCJSONRPCParseError.Create;
  end;
  try
    AsJSON := lJSON;
  finally
    lJSON.Free;
  end;
end;

{ TJSONRPCResponseError }

procedure TJSONRPCResponseError.SetCode(const Value: Integer);
begin
  FCode := Value;
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

function TJSONRPCRequest.GetJSON: TJsonObject;
begin
  Result := inherited GetJSON;
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
  end;

end;

{ TJSONRPCProxyGenerator }

constructor TJSONRPCProxyGenerator.Create;
begin
  inherited;
end;

procedure RegisterJSONRPCProxyGenerator(const aLanguage: String; const aClass: TJSONRPCProxyGeneratorClass);
begin
  if not Assigned(GProxyGeneratorsRegister) then
  begin
    GProxyGeneratorsRegister := TDictionary<String, TJSONRPCProxyGeneratorClass>.Create();
  end;
  GProxyGeneratorsRegister.AddOrSetValue(aLanguage.ToLower, aClass);
end;

procedure TJSONRPCProxyGenerator.EndGeneration;
begin
  // do nothing
end;

procedure TJSONRPCProxyGenerator.StartGeneration;
begin
  // do nothing
end;

initialization

finalization

FreeAndNil(GProxyGeneratorsRegister);

end.
