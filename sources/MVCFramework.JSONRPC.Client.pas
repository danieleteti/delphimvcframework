unit MVCFramework.JSONRPC.Client;

interface

uses
  MVCFramework.JSONRPC,
  System.Net.HttpClient;

type
  IMVCJSONRPCExecutor = interface
    ['{55415094-9D28-4707-AEC5-5FCF925E82BC}']
    function ExecuteRequest(const aJSONRPCRequest: TJSONRPCRequest): TJSONRPCResponse;
    procedure ExecuteNotification(const aJSONRPCNotification: TJSONRPCNotification);
  end;

  TMVCJSONRPCExecutor = class(TInterfacedObject, IMVCJSONRPCExecutor)
  private
    FURL: string;
    FHTTP: THTTPClient;
    FRaiseExceptionOnError: Boolean;
  protected
    function InternalExecute(const aJSONRPCObject: TJSONRPCObject): TJSONRPCResponse;
  public
    constructor Create(const aURL: String; const aRaiseExceptionOnError: Boolean = True); virtual;
    destructor Destroy; override;
    function ExecuteRequest(const aJSONRPCRequest: TJSONRPCRequest): TJSONRPCResponse;
    procedure ExecuteNotification(const aJSONRPCNotification: TJSONRPCNotification);
  end;

implementation

uses
  System.Classes,
  System.Net.URLClient,
  System.SysUtils;

procedure JSONRPCExec(const aJSONRPCURL: string; const aJSONRPCRequest: TJSONRPCRequest;
  out aJSONRPCResponse: TJSONRPCResponse);
var
  lSS: TStringStream;
  lHttpResp: IHTTPResponse;
  lHTTP: THTTPClient;
begin
  lSS := TStringStream.Create(aJSONRPCRequest.AsJSONString);
  try
    lSS.Position := 0;
    lHTTP := THTTPClient.Create;
    try
      lHttpResp := lHTTP.Post('http://localhost:8080/jsonrpc', lSS, nil,
        [TNetHeader.Create('content-type', 'application/json'), TNetHeader.Create('accept', 'application/json')]);
      if (lHttpResp.StatusCode <> 204) then
      begin
        aJSONRPCResponse := TJSONRPCResponse.Create;
        try
          aJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
          if Assigned(aJSONRPCResponse.Error) then
            raise Exception.CreateFmt('Error [%d]: %s', [aJSONRPCResponse.Error.Code,
              aJSONRPCResponse.Error.ErrMessage]);
        except
          aJSONRPCResponse.Free;
          raise;
        end;
      end;
    finally
      lHTTP.Free;
    end;
  finally
    lSS.Free;
  end;
end;

{ TMVCJSONRPCExecutor }

constructor TMVCJSONRPCExecutor.Create(const aURL: String; const aRaiseExceptionOnError: Boolean = True);
begin
  inherited Create;
  FRaiseExceptionOnError := aRaiseExceptionOnError;
  FURL := aURL;
  FHTTP := THTTPClient.Create;
end;

destructor TMVCJSONRPCExecutor.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

procedure TMVCJSONRPCExecutor.ExecuteNotification(const aJSONRPCNotification: TJSONRPCNotification);
begin
  if InternalExecute(aJSONRPCNotification as TJSONRPCObject) <> nil then
    raise EMVCJSONRPCException.Create('A "notification" cannot returns a response. Use ExecuteRequest instead.');
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aJSONRPCRequest: TJSONRPCRequest): TJSONRPCResponse;
begin
  Result := InternalExecute(aJSONRPCRequest);
end;

function TMVCJSONRPCExecutor.InternalExecute(const aJSONRPCObject: TJSONRPCObject): TJSONRPCResponse;
var
  lSS: TStringStream;
  lHttpResp: IHTTPResponse;
  lJSONRPCResponse: TJSONRPCResponse;
begin
  Result := nil;
  lSS := TStringStream.Create(aJSONRPCObject.AsJSONString);
  try
    lSS.Position := 0;
    lHttpResp := FHTTP.Post(FURL, lSS, nil, [TNetHeader.Create('content-type', 'application/json'),
      TNetHeader.Create('accept', 'application/json')]);
    if (lHttpResp.StatusCode <> 204) then
    begin
      lJSONRPCResponse := TJSONRPCResponse.Create;
      try
        lJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
        if Assigned(lJSONRPCResponse.Error) and FRaiseExceptionOnError then
          raise Exception.CreateFmt('Error [%d]: %s', [lJSONRPCResponse.Error.Code, lJSONRPCResponse.Error.ErrMessage]);
        Result := lJSONRPCResponse;
      except
        lJSONRPCResponse.Free;
        raise;
      end;
    end;
  finally
    lSS.Free;
  end;
end;

end.
