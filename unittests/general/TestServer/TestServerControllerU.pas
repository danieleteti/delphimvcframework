// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

{$HINTS OFF}

interface

uses
  MVCFramework,
  System.SysUtils,
  MVCFramework.Commons;

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
    procedure ReqWithParams;

    [MVCPath('/echo/($par1)/($par2)/($par3)')]
    [MVCHTTPMethod([httpPOST, httpPUT, httpPATCH])]
    procedure EchoBody;

    [MVCPath('/session/($value)')]
    [MVCHTTPMethod([httpPOST])]
    procedure SessionSet;

    [MVCPath('/session')]
    [MVCHTTPMethod([httpGET])]
    procedure SessionGet;

    [MVCPath('/headers')]
    procedure EchoHeaders;

    [MVCPath('/lotofcookies')]
    procedure GenerateCookies;

    [MVCPath('/dataset/($datasetname)')]
    procedure DataSetHandling;

    [MVCPath('/login/($username)')]
    // this is only for test!!!!
    procedure Login;

    [MVCPath('/logout')]
    // this is only for test!!!!
    procedure Logout;

    [MVCPath('/encoding')]
    [MVCHTTPMethod([httpGET])]
    // this is only for test!!!!
    procedure TestCharset;

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('application/json')]
    [MVCProduces('application/json', 'utf-8')]
    procedure TestConsumesProduces;

    [MVCPath('/testconsumes/textiso8859_1')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCConsumes(TMVCMediaType.TEXT_PLAIN)]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN, TMVCCharset.ISO88591)]
    procedure TestConsumesProducesTextISO8859_1;

    [MVCPath('/testconsumes')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    [MVCConsumes('text/plain')]
    [MVCProduces('text/plain', 'utf-8')]
    procedure TestConsumesProducesText;

    [MVCPath('/testconsumejson')]
    [MVCHTTPMethod([httpGET])]
    [MVCConsumes('application/json')]
    [MVCProduces('application/json', 'utf-8')]
    procedure TestConsumeJSON;

    [MVCPath('/people/renderaction')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetPersonsHateos;

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetPersonByID;

    [MVCPath('/people/($id)/asfields')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetPersonByIDAsFields;

    [MVCPath('/customers/list')]
    [MVCHTTPMethod([httpPOST])]
    procedure TestJSONArrayAsObjectList;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT])]
    procedure TestGetPersons;

    [MVCPath('/wrappedpeople')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetWrappedPeople;

    [MVCPath('/objects')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCProduces('application/json')]
    procedure TestPOSTObject;

    [MVCPath('/customerecho')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('application/json')]
    procedure TestCustomerEcho;

    [MVCPath('/speed')]
    [MVCHTTPMethod([httpGET])]
    procedure TestHelloWorld;

    [MVCPath('/path1/($id)')]
    [MVCPath('/path2/($id)/2/($par)')]
    [MVCPath('/path3/($id)/2/($par)/3')]
    [MVCPath('/path4/($id)/2/($par)/3/4')]
    [MVCHTTPMethod([httpPOST, httpPUT])]
    procedure TestMultiplePaths;

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
    procedure TestTypedActionAllTypes(ParString: string; ParInteger: Integer; ParInt64: Int64; ParSingle: Single;
      ParDouble: Double; ParExtended: Extended);

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

    [MVCPath('/stringdictionary')]
    procedure TestStringDictionary;

    [MVCPath('/image/png')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetImagePng;

    // Response Objects Tests
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/responses/created')]
    procedure TestResponseCreated;

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/responses/accepted')]
    procedure TestResponseAccepted;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/responses/nocontent')]
    procedure TestResponseNoContent;

  end;

  [MVCPath('/private')]
  TTestPrivateServerController = class(TMVCController)
  public
    [MVCPath('/role1')]
    procedure OnlyRole1;
    [MVCPath('/role1session')]
    [MVCHTTPMethods([httpGET])]
    procedure OnlyRole1Session;
    [MVCPath('/role2')]
    procedure OnlyRole2;
  end;

  [MVCPath('/fault')]
  TTestFaultController = class(TMVCController)
  public
    [MVCPath]
    procedure NeverExecuted;
    constructor Create; override;
  end;

  [MVCPath('/fault2')]
  TTestFault2Controller = class(TTestFaultController)
  public
    [MVCPath]
    procedure NeverExecuted;
    constructor Create; override;
  end;

implementation

uses
  JsonDataObjects,
  System.JSON,
  Web.HTTPApp,
  BusinessObjectsU,
  Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.DuckTyping,
  System.IOUtils,
  System.Classes;

{ TTestServerController }

procedure TTestServerController.DataSetHandling;
begin
  case Context.Request.HTTPMethod of
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

procedure TTestServerController.EchoBody;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
  JSON.AddPair('echo', 'from server');
  Render(JSON, True);
end;

procedure TTestServerController.EchoHeaders;
begin
  Context.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(Context.Request.Headers['ACCEPT']);
end;

procedure TTestServerController.GenerateCookies;
var
  c: TCookie;
  v: string;
begin
  v := Context.Request.Cookie('usersettings');

  c := Context.Response.Cookies.Add;
  c.Name := 'usersettings1';
  c.value := 'usersettings1-value';
  c.Path := '/usersettings1';
  c.Expires := 0;

  c := Context.Response.Cookies.Add;
  c.Name := 'usersettings2';
  c.value := 'usersettings2-value';
  c.Path := '/usersettings2';
  c.Expires := 0;

  c := Context.Response.Cookies.Add;
  c.Name := 'usersettings3';
  c.value := 'usersettings3-value';
  c.Path := '/usersettings3';
  c.Expires := 0;

  c := Context.Response.Cookies.Add;
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

procedure TTestServerController.ReqWithParams;
begin
  Render(TJSONObject.Create.AddPair('par1', Context.Request.Params['par1']).AddPair('par2',
    Context.Request.Params['par2']).AddPair('par3', Context.Request.Params['par3']).AddPair('method',
    Context.Request.HTTPMethodAsString));
end;

procedure TTestServerController.SessionGet;
var
  s: string;
begin
  ContentType := Context.Request.Accept;
  s := Session['value'];
  Render(s);
end;

procedure TTestServerController.SessionSet;
begin
  Session['value'] := Context.Request.Params['value'];
end;

procedure TTestServerController.TestConsumeJSON;
begin
  Render(TJSONObject.ParseJSONValue('{"key":"Hello World"}'));
end;

procedure TTestServerController.TestConsumesProduces;
begin
  Render('Hello World');
end;

procedure TTestServerController.TestConsumesProducesText;
begin
  Render('Hello World');
end;

procedure TTestServerController.TestConsumesProducesTextISO8859_1;
begin
  Render(Context.Request.Body);
end;

procedure TTestServerController.TestCustomerEcho;
var
  lCustomer: TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>();
  // lCustomer.Logo.SaveToFile('pippo_server_before.bmp');
  lCustomer.Name := lCustomer.Name + ' changed';
  lCustomer.Logo.Canvas.TextOut(10, 10, 'Changed');
  // lCustomer.Logo.SaveToFile('pippo_server_after.bmp');
  Render(lCustomer, True);
end;

procedure TTestServerController.TestCharset;
var
  Obj: TJDOJSONObject;
begin
  ContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharset.UTF_8);
  Obj := TJDOJSONObject.Create;
  try
    Obj.s['name1'] := 'jørn';
    Obj.s['name2'] := 'Što je Unicode?';
    Obj.s['name3'] := 'àèéìòù';
    Render(Obj, false);
  finally
    Obj.Free;
  end;
end;

procedure TTestServerController.TestGetImagePng;
begin
  ContentType := TMVCMediaType.IMAGE_PNG;
  Render(TFile.OpenRead('..\..\sample.png'));
end;

procedure TTestServerController.TestGetPersonByID;
var
  PersonList: TObjectList<TPerson>;
  ID: Integer;
begin
  ID := Context.Request.Params['id'].ToInteger;
  PersonList := TPerson.GetList;
  try
    Render(PersonList[ID - 1], false);
  finally
    PersonList.Free;
  end;
end;

procedure TTestServerController.TestGetPersonByIDAsFields;
var
  PersonList: TObjectList<TPerson>;
  ID: Integer;
begin
  raise Exception.Create('Not implemented');
  ID := Context.Request.Params['id'].ToInteger;
  PersonList := TPerson.GetList;
  try
    // Render(PersonList[ID - 1], false, TDMVCSerializationType.Fields);
  finally
    PersonList.Free;
  end;
end;

procedure TTestServerController.TestGetPersons;
var
  Person: TPerson;
begin
  case Context.Request.HTTPMethod of
    httpGET:
      Render<TPerson>(TPerson.GetList, True);
    httpPOST:
      begin
        Person := Context.Request.BodyAs<TPerson>();
        Render(Person);
      end;
    httpPUT:
      ;
  end;

end;

procedure TTestServerController.TestGetPersonsHateos;
begin
  Render<TPerson>(TPerson.GetList, True,
    procedure(const Person: TPerson; const Links: IMVCLinks)
    begin
      Links.AddRefLink
        .Add(HATEOAS.HREF, '/api/people/' + Person.ID.ToString)
        .Add(HATEOAS.REL, 'test0')
        .Add(HATEOAS._TYPE, 'application/json');
      Links.AddRefLink
        .Add(HATEOAS.HREF, '/api/test/' + Person.ID.ToString)
        .Add(HATEOAS.REL, 'test1')
        .Add(HATEOAS._TYPE, 'application/json')
    end);
end;

procedure TTestServerController.TestGetWrappedPeople;
var
  LWrappedList: IWrappedList;
  lObj: TObject;
begin
  if not Context.Request.QueryStringParamExists('count') then
  begin
    lObj := TPerson.GetList;
  end
  else
  begin
    lObj := TPerson.GetList(Context.Request.ParamsAsInt64['count']);
  end;
  try
    LWrappedList := WrapAsList(lObj);
    Render(LWrappedList);
  finally
    lObj.Free;
  end;
end;

procedure TTestServerController.TestHelloWorld;
begin
  ContentType := 'text/plain';
  Render('hello world');
end;

procedure TTestServerController.TestJSONArrayAsObjectList;
var
  lUsers: TObjectList<TCustomer>;
begin
  lUsers := Context.Request.BodyAsListOf<TCustomer>();
  try
    lUsers.OwnsObjects := True;
    if (lUsers.Count = 3000) then
      Render('Success!')
    else
      Render('Error!');
  finally
    FreeAndNil(lUsers);
  end;
end;

procedure TTestServerController.TestMultiplePaths;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(Context.Request.Params['id']);
end;

procedure TTestServerController.TestPOSTObject;
var
  Person: TPerson;
begin
  Person := Context.Request.BodyAs<TPerson>();
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

procedure TTestServerController.TestResponseAccepted;
begin
  ResponseAccepted('http://pippo.it/1234','1234','thisisthereason');
end;

procedure TTestServerController.TestResponseCreated;
begin
  ResponseCreated('thisisthelocation','thisisthereason');
end;

procedure TTestServerController.TestResponseNoContent;
begin
  ResponseNoContent('thisisthereason');
end;

procedure TTestServerController.TestStringDictionary;
var
  lDict: TMVCStringDictionary;
begin
  lDict := Context.Request.BodyAs<TMVCStringDictionary>;
  try
    lDict['fromserver'] := 'changed';
    Render(lDict, false);
  finally
    lDict.Free;
  end;
end;

procedure TTestServerController.TestTypedActionAllTypes(ParString: string; ParInteger: Integer; ParInt64: Int64;
ParSingle: Single; ParDouble: Double; ParExtended: Extended);
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
  Render(DateToISODate(value) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionTDateTime1(value: TDateTime);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(DateTimeToISOTimeStamp(value) + ' modified from server');
end;

procedure TTestServerController.TestTypedActionBooleans(bool1, bool2, bool3, bool4: Boolean);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(Format('%s.%s.%s.%s', [BoolToStr(bool1, True), BoolToStr(bool2, True), BoolToStr(bool3, True),
    BoolToStr(bool4, True)]));
end;

procedure TTestServerController.TestTypedActionTTime1(value: TTime);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(TimeToISOTime(value) + ' modified from server');
end;

{ TTestPrivateServerController }

procedure TTestPrivateServerController.OnlyRole1;
begin
  Render(Context.LoggedUser.UserName);
end;

procedure TTestPrivateServerController.OnlyRole1Session;
begin
  if Context.Request.QueryStringParamExists('value') then
  begin
    Session['value'] := Context.Request.Params['value'];
  end
  else
  begin
    Render(Session['value']);
  end;
end;

procedure TTestPrivateServerController.OnlyRole2;
begin
  Render(Context.LoggedUser.UserName);
end;

{ TTestFaultController }

constructor TTestFaultController.Create;
begin
  inherited;
  raise Exception.Create('BOOOM!!!');
end;

procedure TTestFaultController.NeverExecuted;
begin
  // do nothing
end;

{ TTestFault2Controller }

constructor TTestFault2Controller.Create;
begin
  inherited;
end;

procedure TTestFault2Controller.NeverExecuted;
begin
  // do nothing
end;

end.
