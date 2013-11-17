unit TestServerControllerU;

interface

uses MVCFramework;

type

  [MVCPath('/')]
  TTestServerController = class(TMVCController)
  public
    [MVCPath('/req/with/params/($par1)/($par2)/($par3)')]
    [MVCHTTPMethod([httpGET, httpDELETE])]
    procedure ReqWithParams(ctx: TWebContext);

    [MVCPath('/echo/($par1)/($par2)/($par3)')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    procedure EchoBody(ctx: TWebContext);

    [MVCPath('/session/($value)')]
    [MVCHTTPMethod([httpPOST])]
    procedure SessionSet(ctx: TWebContext);

    [MVCPath('/session')]
    [MVCHTTPMethod([httpGET])]
    procedure SessionGet(ctx: TWebContext);

    [MVCPath('/headers')]
    procedure EchoHeaders(ctx: TWebContext);

    [MVCPath('/lotofcookies')]
    procedure GenerateCookies(ctx: TWebContext);

    [MVCPath('/dataset/($datasetname)')]
    procedure DataSetHandling(ctx: TWebContext);

    [MVCPath('/login/($username)')]
    // this is only for test!!!!
    procedure Login(ctx: TWebContext);

    [MVCPath('/logout')]
    // this is only for test!!!!
    procedure Logout(ctx: TWebContext);

    [MVCPath('/encoding')]
    [MVCHTTPMethod([httpGET])]
    // this is only for test!!!!
    procedure TestEncoding(ctx: TWebContext);

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('application/json')]
    [MVCProduces('application/json')]
    procedure TestConsumesProduces(ctx: TWebContext);

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('text/plain')]
    [MVCProduces('text/plain')]
    procedure TestConsumesProducesText(ctx: TWebContext);



  end;

implementation

uses
  Data.DBXJSON,
  MVCFramework.Commons,
  Web.HTTPApp;

{ TTestServerController }

procedure TTestServerController.DataSetHandling(ctx: TWebContext);
begin
  case ctx.Request.HTTPMethod of
    httpGET:
      begin

      end;
    httpPOST:
      begin
      end;
    httpPUT:
      begin
      end;
    httpDELETE:
      begin
      end;
    httpHEAD:
      begin
      end;
    httpOPTIONS:
      begin
      end;
  end;
end;

procedure TTestServerController.EchoBody(ctx: TWebContext);
var
  json: TJSONObject;
begin
  json := ctx.Request.BodyAsJSONObject.Clone as TJSONObject;
  json.AddPair('echo', 'from server');
  Render(json);
end;

procedure TTestServerController.EchoHeaders(ctx: TWebContext);
begin
  ctx.Response.ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(ctx.Request.Headers['ACCEPT']);
end;

procedure TTestServerController.GenerateCookies(ctx: TWebContext);
var
  c: TCookie;
  v: string;
begin
  v := ctx.Request.Cookie('usersettings');

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings';
  c.Value := '01234-5678-90';
  c.Path := '/';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings1';
  c.Value := '11234-5678-90';
  c.Path := '/';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings2';
  c.Value := '21234-5678-90';
  c.Path := '/';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings3';
  c.Value := '31234-5678-90';
  c.Path := '/';
  c.Expires := 0;

end;

procedure TTestServerController.Login(ctx: TWebContext);
begin
  Session['username'] := ctx.Request.Params['username'];
end;

procedure TTestServerController.Logout(ctx: TWebContext);
begin
  SessionStop(false);
end;

procedure TTestServerController.ReqWithParams(ctx: TWebContext);
begin
  Render(TJSONObject.Create.AddPair('par1', ctx.Request.Params['par1'])
    .AddPair('par2', ctx.Request.Params['par2']).AddPair('par3',
    ctx.Request.Params['par3']).AddPair('method',
    ctx.Request.HTTPMethodAsString));
end;

procedure TTestServerController.SessionGet(ctx: TWebContext);
var
  s: string;
begin
  ContentType := ctx.Request.Accept;
  s := Session['value'];
  Render(s); // ['value']);
end;

procedure TTestServerController.SessionSet(ctx: TWebContext);
begin
  Session['value'] := ctx.Request.Params['value'];
end;

procedure TTestServerController.TestConsumesProduces(ctx: TWebContext);
begin
  Render('Hello World');
end;

procedure TTestServerController.TestConsumesProducesText(ctx: TWebContext);
begin
  Render('Hello World');
end;

procedure TTestServerController.TestEncoding(ctx: TWebContext);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  Obj.AddPair('name1', 'jørn');
  Obj.AddPair('name2', 'Što je Unicode?');
  Obj.AddPair('name3', 'àèéìòù');
  Render(Obj);
end;

end.
