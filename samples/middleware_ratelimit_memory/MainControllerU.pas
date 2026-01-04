unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.Generics.Collections;

type
  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld;

    [MVCPath('/protected')]
    [MVCHTTPMethod([httpGET])]
    procedure ProtectedEndpoint;

    [MVCPath('/user/($username)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUser(const username: string);
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
  System.DateUtils,
  JsonDataObjects;

{ TMainController }

procedure TMainController.HelloWorld;
begin
  Render(StrDict()
    .Add('message', 'Hello World!')
    .Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
    .Add('rate_limit_info', 'Check response headers: X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset')
  );
end;

procedure TMainController.ProtectedEndpoint;
var
  LResponse: TJSONObject;
  LData: TJSONArray;
begin
  LResponse := TJSONObject.Create;
  try
    LResponse.S['message'] := 'This is a protected endpoint with rate limiting';
    LResponse.S['timestamp'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    LData := LResponse.A['data'];
    LData.Add('item1');
    LData.Add('item2');
    LData.Add('item3');

    Render(LResponse, False);
  finally
    LResponse.Free;
  end;
end;

procedure TMainController.GetUser(const username: string);
begin
  Render(StrDict()
    .Add('username', username)
    .Add('email', username + '@example.com')
    .Add('created_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', IncDay(Now, -100)))
  );
end;

{ THealthController }

procedure THealthController.HealthCheck;
begin
  Render(StrDict()
    .Add('status', 'healthy')
    .Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
    .Add('note', 'This endpoint is excluded from rate limiting')
  );
end;

end.
