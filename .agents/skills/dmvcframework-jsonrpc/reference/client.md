# JSON-RPC — the Delphi client (`IMVCJSONRPCExecutor`)

Unit `MVCFramework.JSONRPC.Client`. Concrete class `TMVCJSONRPCExecutor`, interfaces `IMVCJSONRPCExecutor`
(sync) and `IMVCJSONRPCExecutorAsync` (async) — the same object implements both; assign it to whichever
interface variable you need.

```delphi
constructor TMVCJSONRPCExecutor.Create(const aURL: string;
  const aRaiseExceptionOnError: Boolean = True;
  const aDefaultHTTPVerb: TJSONRPCHTTPVerb = jrpcDefault); virtual;
```

`aURL` is the **base** URL (`'http://localhost:8080'`); the endpoint segment is passed per call.
`TJSONRPCHTTPVerb = (jrpcDefault, jrpcGET, jrpcPOST)` — `jrpcDefault` means POST.

### Sync calls

```delphi
function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest;
  const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
function ExecuteRequest(const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
  const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification;
  const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
function ExecuteNotification(const aLastEndPointSegment: string;
  const aJSONRPCNotification: IJSONRPCNotification;
  const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
```

### Building a request

Two equivalent ways — the executor factory, or the class directly:

```delphi
var lExecutor: IMVCJSONRPCExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080');

var lReq := lExecutor.CreateRequest('subtract', Random(1000));    // RequestID: UInt64 or String
lReq.Params.Add(10);
lReq.Params.Add(3);
var lResp := lExecutor.ExecuteRequest('/jsonrpc', lReq);
ShowMessage(lResp.Result.AsInteger.ToString);
```

```delphi
var lReq: IJSONRPCRequest := TJSONRPCRequest.Create;
lReq.Method := 'getuser';
lReq.RequestID := Random(1000);      // TValue — string or integer
lReq.Params.Add(edtUserName.Text);
```

Named params, and the GET verb (the server method needs `[MVCJSONRPCAllowGET]`):

```delphi
lReq.Params.AddByName('Value1', 10);
lReq.Params.AddByName('Value2', 3);
lResp := lExecutor.ExecuteRequest('/jsonrpc', lReq, jrpcGET);
```

`TJSONRPCRequestParams` has `Add` / `AddByName` overloads for `string`, `Integer`, `Boolean`, `Double`,
`TDate`, `TTime`, `TDateTime`, `TJDOJsonObject`, `TJDOJsonArray`, `TObject`, and a
`(Value: TValue; ParamType: TJSONRPCParamDataType)` overload used for explicit typing:

```delphi
var lPerson := TPerson.Create;
lReq.Params.AddByName('Person', lPerson, pdtObject);   // do NOT free lPerson
```

**Client-side ownership:** `TJSONRPCRequestParams.Destroy` frees every object added to it. Once you hand an
object to `Params.Add`/`AddByName`, the request owns it — do not free it, do not reuse it.

### Notifications

```delphi
var lNotification := lExecutor.CreateNotification('dosomething');
lExecutor.ExecuteNotification('/jsonrpc', lNotification);
```

`ExecuteNotification` returns a `TJSONRPCNullResponse` (the server sent 204). Reading `.Result` on it raises
`EMVCJSONRPCException` — do not touch it.

### Reading the response

```delphi
IJSONRPCResponse = interface(IJSONRPCObject)
  function IsError: Boolean;
  function ResultAsJSONObject: TJDOJsonObject;
  function ResultAsJSONArray: TJDOJsonArray;
  procedure ResultAs(const Obj: TObject;
    Serialization: TMVCSerializationType = TMVCSerializationType.stDefault);
  property Result: TValue read GetResult write SetResult;
  property Error: TJSONRPCResponseError read GetError write SetError;
  property RequestID: TValue read GetID write SetID;
end;
```

`ResultAs` deserializes the result into an object **you** own:

```delphi
var lPerson := TPerson.Create;
try
  lResp.ResultAs(lPerson);
  ShowMessage(lPerson.FirstName);
finally
  lPerson.Free;      // your object, your Free
end;
```

`TJSONRPCResponseError` exposes `Code: Integer`, `ErrMessage: string`, `Data: TValue` — the message property
is **`ErrMessage`**, not `Message`.

### Error handling

With the default `aRaiseExceptionOnError = True`, an error response raises
`EMVCJSONRPCRemoteException` (`ErrCode`, `ErrMessage`, `Data`) — you do not have to test `.Error`. Pass
`False` to the constructor if you would rather inspect `lResp.Error` yourself. A non-JSON reply raises
`EMVCJSONRPCException` with the HTTP status and body.

### Headers, TLS, tracing

```delphi
lExecutor.AddHTTPHeader(TNetHeader.Create('Authorization', 'Bearer ' + lJWT));
lExecutor.ClearHTTPHeaders;
lExecutor.HTTPHeadersCount;
lExecutor.HTTPResponse;                              // IHTTPResponse of the last call
lExecutor.ConfigureHTTPClient(procedure(C: THTTPClient) begin C.ConnectionTimeout := 5000; end);
lExecutor.SetOnSendCommand(procedure(Obj: IJSONRPCObject) begin ... end);
lExecutor.SetOnReceiveResponse(procedure(Req, Resp: IJSONRPCObject) begin ... end);
lExecutor.SetOnReceiveHTTPResponse(procedure(R: IHTTPResponse) begin ... end);
lExecutor.SetOnReceiveData(...);
lExecutor.SetOnValidateServerCertificate(...);
lExecutor.SetOnNeedClientCertificate(...);
```

### Async

Declare the variable as `IMVCJSONRPCExecutorAsync`; the handler runs on the main thread.

```delphi
var lExec: IMVCJSONRPCExecutorAsync := TMVCJSONRPCExecutor.Create('http://localhost:8080');
var lReq := lExec.CreateRequest('subtract', Random(1000));
lReq.Params.Add(10);
lReq.Params.Add(3);
lExec.ExecuteRequestAsync('/jsonrpc', lReq,
  procedure(JSONRPCResp: IJSONRPCResponse)          // TJSONRPCResponseHandlerProc
  begin
    edtResult.Text := JSONRPCResp.Result.AsInteger.ToString;
  end,
  procedure(Exc: Exception)                          // TJSONRPCErrorHandlerProc, optional
  begin
    ShowMessage(Exc.Message);
  end);
```

On the async path the response handler and the error handler are both dispatched with `TThread.Queue(nil, …)`,
so they run on the main thread — safe to touch the UI. An error response always reaches the error handler
here (`aRaiseExceptionOnError` governs the sync path); without one, the framework's default task error
handler runs.

Also `ExecuteNotificationAsync`, `SetOnBeginAsyncRequest` / `SetOnEndAsyncRequest`,
`SetOnSendCommandAsync`, `SetOnReceiveResponseAsync`, `SetOnReceiveHTTPResponseAsync`,
`SetConfigureHTTPClientAsync`. Keep the executor alive (a field, not a local) for the duration of the call.
