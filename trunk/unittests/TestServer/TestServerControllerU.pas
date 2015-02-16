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
    [MVCHTTPMethod([httpPOST, httpPUT, httpPATCH])]
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
    [MVCProduces('application/json', 'utf-8')]
    procedure TestConsumesProduces(ctx: TWebContext);

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('text/plain')]
    [MVCProduces('text/plain')]
    procedure TestConsumesProducesText(ctx: TWebContext);

    [MVCPath('/testconsumejson')]
    [MVCHTTPMethod([httpGET])]
    [MVCConsumes('application/json')]
    [MVCProduces('application/json', 'utf-8')]
    procedure TestConsumeJSON(ctx: TWebContext);

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetPersonByID(ctx: TWebContext);

    [MVCPath('/people/($id)/asfields')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetPersonByIDAsFields(ctx: TWebContext);

    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    procedure TestGetPersons(ctx: TWebContext);

    [MVCPath('/objects')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCProduces('application/json')]
    procedure TestPOSTObject(ctx: TWebContext);

    [MVCPath('/path1/($id)')]
    [MVCPath('/path2/($id)/2/($par)')]
    [MVCPath('/path3/($id)/2/($par)/3')]
    [MVCPath('/path4/($id)/2/($par)/3/4')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    procedure TestMultiplePaths(ctx: TWebContext);

  end;

implementation

uses
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$IFEND}
  MVCFramework.Commons, Web.HTTPApp, BusinessObjectsU, Generics.Collections,
  SysUtils;

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
  JSON: TJSONObject;
begin
  JSON := ctx.Request.BodyAsJSONObject.Clone as TJSONObject;
  JSON.AddPair('echo', 'from server');
  Render(JSON);
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
  Render(TJSONObject.Create.AddPair('par1', ctx.Request.Params['par1']).AddPair('par2', ctx.Request.Params['par2']).AddPair('par3',
    ctx.Request.Params['par3']).AddPair('method', ctx.Request.HTTPMethodAsString));
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

procedure TTestServerController.TestConsumeJSON(ctx: TWebContext);
begin
  Render(TJSONObject.ParseJSONValue('{"key":"Hello World"}'));
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
  ContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  Obj := TJSONObject.Create;
  Obj.AddPair('name1', 'jørn');
  Obj.AddPair('name2', 'Što je Unicode?');
  Obj.AddPair('name3', 'àèéìòù');
  Render(Obj);
end;

procedure TTestServerController.TestGetPersonByID(ctx: TWebContext);
var
  PersonList: TObjectList<TPerson>;
  ID: integer;
begin
  ID := ctx.Request.Params['id'].ToInteger;
  PersonList := TPerson.GetList;
  try
    Render(PersonList[ID - 1], false);
  finally
    PersonList.Free;
  end;
end;

procedure TTestServerController.TestGetPersonByIDAsFields(ctx: TWebContext);
var
  PersonList: TObjectList<TPerson>;
  ID: integer;
begin
  ID := ctx.Request.Params['id'].ToInteger;
  PersonList := TPerson.GetList;
  try
    Render(PersonList[ID - 1], false, TDMVCSerializationType.Fields);
  finally
    PersonList.Free;
  end;
end;

procedure TTestServerController.TestGetPersons(ctx: TWebContext);
var
  Person: TPerson;
begin
  case ctx.Request.HTTPMethod of
    httpGET:
      Render<TPerson>(TPerson.GetList);
    httpPOST:
      begin
        Person := ctx.Request.BodyAs<TPerson>();
        Render(Person);
      end;
    httpPUT:
      ;
  end;

end;

procedure TTestServerController.TestMultiplePaths(ctx: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(ctx.Request.Params['id']);
end;

procedure TTestServerController.TestPOSTObject(ctx: TWebContext);
var
  Person: TPerson;
begin
  Person := ctx.Request.BodyAs<TPerson>();
  Render(Person);
end;

end.
