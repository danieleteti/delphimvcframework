// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit TestServerControllerU;

interface

uses MVCFramework, System.SysUtils, MVCFramework.Commons;

type

  [MVCPath('/')]
  TTestServerController = class(TMVCController)
  private
    FFormatSettings: TFormatSettings;
  protected
    procedure MVCControllerAfterCreate; override;
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
    procedure SessionGet;

    [MVCPath('/headers')]
    procedure EchoHeaders(ctx: TWebContext);

    [MVCPath('/lotofcookies')]
    procedure GenerateCookies(ctx: TWebContext);

    [MVCPath('/dataset/($datasetname)')]
    procedure DataSetHandling(ctx: TWebContext);

    [MVCPath('/login/($username)')]
    // this is only for test!!!!
    procedure Login;

    [MVCPath('/logout')]
    // this is only for test!!!!
    procedure Logout;

    [MVCPath('/encoding')]
    [MVCHTTPMethod([httpGET])]
    // this is only for test!!!!
    procedure TestEncoding(ctx: TWebContext);

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('application/json')]
    [MVCProduces('application/json', 'utf-8')]
    procedure TestConsumesProduces(ctx: TWebContext);

    [MVCPath('/testconsumes/textiso8859_1')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCConsumes(TMVCMediaType.TEXT_PLAIN)]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN, 'iso8859-1')]
    procedure TestConsumesProducesTextISO8859_1;

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

    [MVCPath('/customers/list')]
    [MVCHTTPMethod([httpPOST])]
    procedure TestJSONArrayAsObjectList;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    procedure TestGetPersons(ctx: TWebContext);

    [MVCPath('/wrappedpeople')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetWrappedPersons(ctx: TWebContext);

    [MVCPath('/objects')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCProduces('application/json')]
    procedure TestPOSTObject(ctx: TWebContext);

    [MVCPath('/speed')]
    [MVCHTTPMethod([httpGET])]
    procedure TestHelloWorld(ctx: TWebContext);

    [MVCPath('/path1/($id)')]
    [MVCPath('/path2/($id)/2/($par)')]
    [MVCPath('/path3/($id)/2/($par)/3')]
    [MVCPath('/path4/($id)/2/($par)/3/4')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    procedure TestMultiplePaths(ctx: TWebContext);

    { Strongly typed actions }
    [MVCPath('/typed/string1/($value)')]
    procedure TestTypedActionString1(value: string);

    [MVCPath('/typed/integer1/($value)')]
    procedure TestTypedActionInteger1(value: Integer);

    [MVCPath('/typed/int641/($value)')]
    procedure TestTypedActionInt641(value: Int64);

    [MVCPath('/typed/single1/($value)')]
    procedure TestTypedActionSingle1(value: Single);

    [MVCPath('/typed/double1/($value)')]
    procedure TestTypedActionDouble1(value: Double);

    [MVCPath('/typed/extended1/($value)')]
    procedure TestTypedActionExtended1(value: Extended);

    [MVCPath('/typed/all/($ParString)/($ParInteger)/($ParInt64)/($ParSingle)/($ParDouble)/($ParExtended)')]
    procedure TestTypedActionAllTypes(ParString: string; ParInteger: Integer;
      ParInt64: Int64; ParSingle: Single; ParDouble: Double;
      ParExtended: Extended);

    [MVCPath('/typed/tdatetime1/($value)')]
    procedure TestTypedActionTDateTime1(value: TDateTime);

    [MVCPath('/typed/tdate1/($value)')]
    procedure TestTypedActionTDate1(value: TDate);

    [MVCPath('/typed/ttime1/($value)')]
    procedure TestTypedActionTTime1(value: TTime);

    [MVCPath('/typed/booleans/($bool1)/($bool2)/($bool3)/($bool4)')]
    procedure TestTypedActionBooleans(bool1, bool2, bool3, bool4: Boolean);

    [MVCPath('/renderstreamandfreewithownerfalse')]
    procedure TestRenderStreamAndFreeWithOwnerFalse;

    [MVCPath('/renderstreamandfreewithownertrue')]
    procedure TestRenderStreamAndFreeWithOwnerTrue;

  end;

  [MVCPath('/private')]
  TTestPrivateServerController = class(TMVCController)
  public
    [MVCPath('/role1')]
    procedure OnlyRole1(ctx: TWebContext);
    [MVCPath('/role1session')]
    [MVCHTTPMethods([httpGET])]
    procedure OnlyRole1Session(ctx: TWebContext);
    [MVCPath('/role2')]
    procedure OnlyRole2(ctx: TWebContext);
  end;

implementation

uses
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$IFEND}
  Web.HTTPApp, BusinessObjectsU, Generics.Collections,
  ObjectsMappers, MVCFramework.DuckTyping, System.Classes;

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
  JSON := TJSONObject.ParseJSONValue(ctx.Request.Body) as TJSONObject;
  JSON.AddPair('echo', 'from server');
  Render(JSON, True);
end;

procedure TTestServerController.EchoHeaders(ctx: TWebContext);
begin
  ctx.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(ctx.Request.Headers['ACCEPT']);
end;

procedure TTestServerController.GenerateCookies(ctx: TWebContext);
var
  c: TCookie;
  v: string;
begin
  v := ctx.Request.Cookie('usersettings');

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings1';
  c.value := 'usersettings1-value';
  c.Path := '/usersettings1';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings2';
  c.value := 'usersettings2-value';
  c.Path := '/usersettings2';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings3';
  c.value := 'usersettings3-value';
  c.Path := '/usersettings3';
  c.Expires := 0;

  c := ctx.Response.Cookies.Add;
  c.Name := 'usersettings4';
  c.value := 'usersettings4-value';
  c.Path := '/usersettings4';
  c.Expires := 0;

end;

procedure TTestServerController.Login;
begin
  if Context.SessionStarted then
    raise EMVCException.Create('Session already started');
  Session['username'] := Context.Request.Params['username'];
  if not Context.SessionStarted then
    raise EMVCException.Create('Session still not started');
end;

procedure TTestServerController.Logout;
begin
  if not Context.SessionStarted then
    raise EMVCException.Create('Session not available');
  Context.SessionStop(false);
  if Context.SessionStarted then
    raise EMVCException.Create('Session still available');
end;

procedure TTestServerController.MVCControllerAfterCreate;
begin
  FFormatSettings.DecimalSeparator := '.';
end;

procedure TTestServerController.ReqWithParams(ctx: TWebContext);
begin
  Render(TJSONObject.Create.AddPair('par1', ctx.Request.Params['par1'])
    .AddPair('par2', ctx.Request.Params['par2']).AddPair('par3',
    ctx.Request.Params['par3']).AddPair('method',
    ctx.Request.HTTPMethodAsString));
end;

procedure TTestServerController.SessionGet;
var
  s: string;
begin
  ContentType := Context.Request.Accept;
  s := Session['value'];
  Render(s);
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

procedure TTestServerController.TestConsumesProducesTextISO8859_1;
begin
  Render(Context.Request.Body);
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
  ID: Integer;
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
  ID: Integer;
begin
  raise Exception.Create('Not implemented');
  ID := ctx.Request.Params['id'].ToInteger;
  PersonList := TPerson.GetList;
  try
//    Render(PersonList[ID - 1], false, TDMVCSerializationType.Fields);
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

procedure TTestServerController.TestGetWrappedPersons(ctx: TWebContext);
var
  LWrappedList: IWrappedList;
  lObj: TObject;
begin
  lObj := TPerson.GetList;
  try
    LWrappedList := WrapAsList(lObj);
    Render(LWrappedList);
  finally
    lObj.Free;
  end;
end;

procedure TTestServerController.TestHelloWorld(ctx: TWebContext);
begin
  ContentType := 'text/plain';
  Render('hello world');
end;

procedure TTestServerController.TestJSONArrayAsObjectList;
var
  vUsers: TObjectList<TCustomer>;
begin
  vUsers := Context.Request.BodyAsListOf<TCustomer>();
  try
    vUsers.OwnsObjects := True;
    if (vUsers.Count = 3000) then
      Render('Success!')
    else
      Render('Error!');
  finally
    FreeAndNil(vUsers);
  end;
end;

procedure TTestServerController.TestMultiplePaths(ctx: TWebContext);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(ctx.Request.Params['id']);
end;

procedure TTestServerController.TestPOSTObject(ctx: TWebContext);
var
  Person: TPerson;
begin
  Person := ctx.Request.BodyAs<TPerson>();
  Render(Person);
end;

procedure TTestServerController.TestRenderStreamAndFreeWithOwnerFalse;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    Render(LStream, false);
  finally
    LStream.Free;
  end;
end;

procedure TTestServerController.TestRenderStreamAndFreeWithOwnerTrue;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  Render(LStream, True);
end;

procedure TTestServerController.TestTypedActionAllTypes(ParString: string;
  ParInteger: Integer; ParInt64: Int64; ParSingle: Single; ParDouble: Double;
  ParExtended: Extended);
var
  lJObj: TJSONObject;
begin
  lJObj := TJSONObject.Create;
  lJObj.AddPair('ParString', ParString);
  lJObj.AddPair('ParInteger', TJSONNumber.Create(ParInteger));
  lJObj.AddPair('ParInt64', TJSONNumber.Create(ParInt64));
  lJObj.AddPair('ParSingle', TJSONNumber.Create(ParSingle));
  lJObj.AddPair('ParDouble', TJSONNumber.Create(ParDouble));
  lJObj.AddPair('ParExtended', TJSONNumber.Create(ParExtended));
  Render(lJObj);
end;

procedure TTestServerController.TestTypedActionDouble1(value: Double);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(FloatToStr(value, FFormatSettings) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionExtended1(value: Extended);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(FloatToStr(value, FFormatSettings) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionSingle1(value: Single);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(FloatToStr(value, FFormatSettings) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionInt641(value: Int64);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(value.ToString + ' modified from server');
end;

procedure TTestServerController.TestTypedActionInteger1(value: Integer);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(value.ToString + ' modified from server');
end;

procedure TTestServerController.TestTypedActionString1(value: string);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(value + ' modified from server');
end;

procedure TTestServerController.TestTypedActionTDate1(value: TDate);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(ISODateToString(value) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionTDateTime1(value: TDateTime);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(ISODateTimeToString(value) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionBooleans(bool1, bool2, bool3,
  bool4: Boolean);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(Format('%s.%s.%s.%s', [BoolToStr(bool1, True), BoolToStr(bool2, True),
    BoolToStr(bool3, True), BoolToStr(bool4, True)]));
end;

procedure TTestServerController.TestTypedActionTTime1(value: TTime);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(ISOTimeToString(value) + ' modified from server');
end;

{ TTestPrivateServerController }

procedure TTestPrivateServerController.OnlyRole1(ctx: TWebContext);
begin
  Render(ctx.LoggedUser.UserName);
end;

procedure TTestPrivateServerController.OnlyRole1Session(ctx: TWebContext);
begin
  if ctx.Request.QueryStringParamExists('value') then
  begin
    Session['value'] := ctx.Request.Params['value'];
  end
  else
  begin
    Render(Session['value']);
  end;
end;

procedure TTestPrivateServerController.OnlyRole2(ctx: TWebContext);
begin
  Render(ctx.LoggedUser.UserName);
end;

end.
