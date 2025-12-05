unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections;

type
  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld;

    [MVCPath('/data')]
    [MVCHTTPMethod([httpGET])]
    procedure GetData;

    [MVCPath('/submit')]
    [MVCHTTPMethod([httpPOST])]
    procedure SubmitData;
  end;

  [MVCPath('/health')]
  THealthController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure HealthCheck;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Serializer.Commons,
  System.DateUtils,
  JsonDataObjects;

{ TMainController }

procedure TMainController.HelloWorld;
begin
  Render(StrDict()
    .Add('message', 'Hello from Redis Rate Limited API!')
    .Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
    .Add('storage', 'Redis')
    .Add('rate_limit_info', 'Check response headers: X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset')
  );
end;

procedure TMainController.GetData;
var
  LResponse: TJSONObject;
  LData: TJSONArray;
  J: Integer;
begin
  LResponse := TJSONObject.Create;
  try
    LData := LResponse.A['data'];
    for J := 1 to 10 do
    begin
      with LData.AddObject do
      begin
        S['id'] := J.ToString;
        S['name'] := 'Item ' + J.ToString;
        S['timestamp'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      end;
    end;

    LResponse.I['count'] := LData.Count;
    LResponse.S['note'] := 'Rate limited by Redis';

    Render(LResponse, False);
  finally
    LResponse.Free;
  end;
end;

procedure TMainController.SubmitData;
var
  LBody: TJSONObject;
  LResponse: TJSONObject;
begin
  LBody := StrToJSONObject(Context.Request.Body);
  try
    LResponse := TJSONObject.Create;
    try
      LResponse.S['message'] := 'Data received and processed';
      LResponse.S['received_at'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      LResponse.O['data'] := LBody.Clone as TJSONObject;

      Render(201, LResponse, False);
    finally
      LResponse.Free;
    end;
  finally
    LBody.Free;
  end;
end;

{ THealthController }

procedure THealthController.HealthCheck;
begin
  Render(StrDict()
    .Add('status', 'healthy')
    .Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
    .Add('redis', 'connected')
    .Add('note', 'This endpoint is excluded from rate limiting')
  );
end;

end.
