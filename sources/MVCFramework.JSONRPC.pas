unit MVCFramework.JSONRPC;

interface

uses
  Classes,
  SysUtils,
  jsondataobjects, MVCFramework, MVCFramework.Commons, System.Rtti;

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
  TMVCJSONRPCMessage = class abstract(TInterfacedObject)
  private
    fJSON: TJsonObject;
  protected
    class procedure CheckVersion(const aJSON: TJDOJsonObject);
    class procedure CheckMethod(const aJSON: TJDOJsonObject);
    class procedure CheckID(const aJSON: TJDOJsonObject; out aIsNotification: Boolean);
    constructor Create(const aJSON: TJDOJsonObject); overload;
  public
    function AsJSON: TJDOJsonObject; virtual;
  end;

  TMVCJSONRPCRequest = class(TMVCJSONRPCMessage)
  private
    fIsNotification: Boolean;
    function GetID: Integer;
    function GetMethod: string;
    function GetParams: TJDOJsonArray;
  protected
    constructor Create(const aJSON: TJDOJsonObject; const aIsNotification: Boolean);
  public
    class function LoadFromString(const aValue: string): TMVCJSONRPCRequest;
    class function LoadFromJSON(const aJSON: TJDOJsonObject): TMVCJSONRPCRequest;
    destructor Destroy; override;
    property IsNotification: Boolean read fIsNotification;
    property ID: Integer read GetID;
    property Method: string read GetMethod;
    property Params: TJDOJsonArray read GetParams;
  end;

  EMVCJSONRPCInvalidVersion = class(Exception)

  end;

  EMVCJSONRPCException = class(Exception)
  private
    FJSONRPCErrorCode: Integer;
  public
    property JSONRPCErrorCode: Integer read FJSONRPCErrorCode;
  end;

  EMVCJSONRPCParseError = class(EMVCJSONRPCException)
  public
    constructor Create;
  end;

  EMVCJSONRPCInvalidRequest = class(EMVCJSONRPCException)
  public
    constructor Create;
  end;

  EMVCJSONRPCMethodNotFound = class(EMVCJSONRPCException)
  public
    constructor Create;
  end;

  EMVCJSONRPCInvalidParams = class(EMVCJSONRPCException)
  public
    constructor Create(const Message: string);
  end;

  EMVCJSONRPCInternalError = class(EMVCJSONRPCException)
  public
    constructor Create;
  end;

  { -32000 to -32099	Server error	Reserved for implementation-defined server-errors. }
  EMVCJSONRPCServerError = class(EMVCJSONRPCException)
  public
    constructor Create(const JSONRPCError: Integer; const Message: string);
  end;

  TMVCJSONObject = TJDOJsonObject;
  TMVCJSONArray = TJDOJsonArray;

  TMVCJSONRPCController = class(TMVCController)
  protected
    function PrepareResponse(const aValue: TValue): TMVCJSONObject;
  public
    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure Index; virtual;
  end;

implementation

{ TMVCJSONRPCMessage }

function TMVCJSONRPCMessage.AsJSON: TMVCJSONObject;
begin
  Result := TMVCJSONObject.Create;
  Result.S[JSONRPC_HEADER] := JSONRPC_VERSION;
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
      raise EMVCJSONRPCException.Create('MEssage is not a notification but its ''id'' property is not valid');
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

constructor TMVCJSONRPCMessage.Create(const aJSON: TMVCJSONObject);
begin
  inherited Create;
  fJSON := aJSON;
end;

{ TMVCJSONRPCRequest }

constructor TMVCJSONRPCRequest.Create(const aJSON: TMVCJSONObject;
  const aIsNotification: Boolean);
begin
  inherited Create(aJSON);
  fIsNotification := aIsNotification;
end;

destructor TMVCJSONRPCRequest.Destroy;
begin
  fJSON.Free;
  inherited;
end;

function TMVCJSONRPCRequest.GetID: Integer;
begin
  Result := fJSON.I[JSONRPC_ID];
end;

function TMVCJSONRPCRequest.GetMethod: string;
begin
  Result := fJSON.S[JSONRPC_METHOD];
end;

function TMVCJSONRPCRequest.GetParams: TMVCJSONArray;
begin
  if fJSON.Types[JSONRPC_PARAMS] = jdtArray then
    Result := fJSON.A[JSONRPC_PARAMS]
  else
    Result := nil;
end;

class function TMVCJSONRPCRequest.LoadFromJSON(
  const aJSON: TJDOJsonObject): TMVCJSONRPCRequest;
var
  lIsNotification: Boolean;
begin
  CheckVersion(aJSON);
  CheckMethod(aJSON);
  CheckID(aJSON, lIsNotification);
  Result := TMVCJSONRPCRequest.Create(aJSON, lIsNotification);
end;

class function TMVCJSONRPCRequest.LoadFromString(
  const aValue: string): TMVCJSONRPCRequest;
begin
  Result := LoadFromJSON(TJDOJsonObject.Parse(aValue) as TMVCJSONObject);
end;

{ TMVCJSONRPCController }

procedure TMVCJSONRPCController.Index;
var
  lJSONRPCReq: TMVCJSONRPCRequest;
  lMethod: string;
  lRTTI: TRTTIContext;
  lRTTIType: TRttiType;
  lRTTIMethod: TRttiMethod;
  lRTTIMethodParams: TArray<TRttiParameter>;
  lRes: TValue;
  lJSONRPCResponse: TMVCJSONObject;
  lIsThereResultValue: Boolean;
  lParamsToInject: TArray<TValue>;
begin
  SetLength(lParamsToInject, 0);
  lJSONRPCReq := TMVCJSONRPCRequest.LoadFromString(Context.Request.Body);
  try
    lMethod := lJSONRPCReq.Method;
    lRTTI := TRTTIContext.Create;
    try
      lRTTIType := lRTTI.GetType(ClassType);
      lRTTIMethod := lRTTIType.GetMethod(lMethod);
      if Assigned(lRTTIMethod) then
      begin
        lRTTIMethodParams := lRTTIMethod.GetParameters;
        if (Length(lRTTIMethodParams) = 0) and (lJSONRPCReq.Params = nil) then
        begin
          SetLength(lParamsToInject, 0);
        end
        else
        begin
          if (Length(lRTTIMethodParams) = 0) and (lJSONRPCReq.Params <> nil) then
            raise EMVCJSONRPCInvalidParams.Create('Expected zero parameters');
          if (Length(lRTTIMethodParams) > 1) then
            raise EMVCJSONRPCInvalidParams.Create('Expected just 1 paramater of type JsonDataObjects.TJsonObject or JsonDataObjects.TJsonArray');
          if (lRTTIMethodParams[0].ParamType.QualifiedName <> 'JsonDataObjects.TJsonObject') and
            (lRTTIMethodParams[0].ParamType.QualifiedName <> 'JsonDataObjects.TJsonArray') then
            raise EMVCJSONRPCServerError.Create(-32000, 'Server method must accept JsonDataObjects.TJsonObject or JsonDataObjects.TJsonArray, but it requests ' + lRTTIMethodParams[0].ParamType.QualifiedName);
          SetLength(lParamsToInject, 1);
          lParamsToInject[0] := lJSONRPCReq.Params;
        end;
        lRes := lRTTIMethod.Invoke(Self, []);
        try
          lIsThereResultValue := not lRes.IsEmpty;
          if lJSONRPCReq.IsNotification then
          begin
            ResponseStatus(HTTP_STATUS.NoContent);
          end
          else
          begin
            lJSONRPCResponse := PrepareResponse(lRes);
            try
              lJSONRPCResponse.I[JSONRPC_ID] := lJSONRPCReq.ID;
              lJSONRPCResponse.S[JSONRPC_HEADER] := JSONRPC_VERSION;
              ResponseStatus(200);
              Render(lJSONRPCResponse, False);
            finally
              lJSONRPCResponse.Free;
            end;
          end;
        finally
          if lRes.IsObject or lres.IsObjectInstance then
            lRes.AsObject.Free;
        end;
      end
      else
        raise EMVCJSONRPCMethodNotFound.Create;
    finally
      lRTTI.Free;
    end;
  finally
    lJSONRPCReq.Free;
  end;
end;

function TMVCJSONRPCController.PrepareResponse(
  const aValue: TValue): TJDOJsonObject;
begin
  Result := TJDOJsonObject.Create;
  case aValue.Kind of
    tkInteger:
      begin
        Result.I[JSONRPC_RESULT] := aValue.AsInteger;
      end;
    tkFloat:
      begin
        Result.D[JSONRPC_RESULT] := aValue.AsExtended;
      end;
    tkString, tkUString, tkWChar, tkLString, tkWString:
      begin
        Result.S[JSONRPC_RESULT] := aValue.AsString;
      end;
    tkInt64:
      begin
        Result.I[JSONRPC_RESULT] := aValue.AsInt64;
      end;
    tkClass:
      begin
        if aValue.AsObject is TJsonObject then
          Result.O[JSONRPC_RESULT] := TJsonObject(aValue.AsObject)
        else if aValue.AsObject is TJsonArray then
          Result.A[JSONRPC_RESULT] := TJsonArray(aValue.AsObject)
        else
          Result.O[JSONRPC_RESULT].FromSimpleObject(aValue.AsObject, True);
      end;
  else
    raise EMVCJSONRPCException.Create('Invalid type in JSON-RPC response');
  end;
end;

{ EMVCJSONRPCParseError }

constructor EMVCJSONRPCParseError.Create;
begin
  inherited Create('Parse error	Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text');
  FJSONRPCErrorCode := -32700;
end;

{ EMVCJSONRPCInvalidRequest }

constructor EMVCJSONRPCInvalidRequest.Create;
begin
  inherited Create('Invalid Request	The JSON sent is not a valid Request object');
  FJSONRPCErrorCode := -32600;
end;

{ EMVCJSONRPCMethodNotFound }

constructor EMVCJSONRPCMethodNotFound.Create;
begin
  inherited Create('Method not found	The method does not exist / is not available');
  FJSONRPCErrorCode := -32601;
end;

{ EMVCJSONRPCInvalidParams }

constructor EMVCJSONRPCInvalidParams.Create(const Message: string);
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

constructor EMVCJSONRPCServerError.Create(const JSONRPCError: Integer; const Message: string);
begin
  inherited Create(message);
  FJSONRPCErrorCode := JSONRPCError;

end;

end.
