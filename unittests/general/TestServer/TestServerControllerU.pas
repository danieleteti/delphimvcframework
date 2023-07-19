// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Commons,
  FireDAC.Comp.Client,
  System.Generics.Collections,
  Data.DB,
  BusinessObjectsU, MVCFramework.Serializer.Commons, System.Classes,
  System.UITypes;

type

  [MVCPath]
  [MVCPath('/donotusethis')]
  TTestServerController = class(TMVCController)
  private
    FFormatSettings: TFormatSettings;
    fDataSet: TFDMemTable;
  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;
  public
    class function GetDataSet: TDataSet;

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

    [MVCPath('/adapter/testconsumejson')]
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

    [MVCPath('/customerecho2')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('application/json')]
    procedure TestCustomerEchoWithRootNode;

    [MVCPath('/customerechobodyfor')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('application/json')]
    procedure TestCustomerEchoBodyFor;

    [MVCPath('/echowithallverbs')]
    [MVCHTTPMethod([httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpTRACE])]
    [MVCProduces('application/json')]
    procedure TestWithAllVerbs;

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

    [MVCPath('/typed/tguid1/($value)')]
    procedure TestTypedActionTGuid1(value: TGUID);

    [MVCPath('/typed/booleans/($bool1)/($bool2)/($bool3)/($bool4)')]
    procedure TestTypedActionBooleans(bool1, bool2, bool3, bool4: Boolean);

    [MVCPath('/renderstreamandfreewithownerfalse')]
    procedure TestRenderStreamAndFreeWithOwnerFalse;

    [MVCPath('/renderstreamandfreewithownertrue')]
    procedure TestRenderStreamAndFreeWithOwnerTrue;

    [MVCPath('/stringdictionary')]
    procedure TestStringDictionary;

    [MVCPath('/entitywitharrays')]
    procedure TestEntityWithArrays;

    [MVCPath('/image/png')]
    [MVCHTTPMethod([httpGET])]
    procedure TestGetImagePng;

    [MVCPath('/objectdict')]
    procedure TestObjectDict;

    // exception rendering
    [MVCPath('/exception/emvcexception1')]
    procedure TestEMVCException1;

    [MVCPath('/exception/emvcexception2')]
    procedure TestEMVCException2;

    [MVCPath('/exception/emvcexception3')]
    procedure TestEMVCException3;

    [MVCPath('/exception/emvcexception4')]
    procedure TestEMVCException4;

    // Nullables Tests
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/nullables/pingpong')]
    procedure TestDeserializeAndSerializeNullables;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/nullables/getsingle')]
    procedure TestSerializeNullables;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/nullables/getsinglewithnulls')]
    procedure TestSerializeNullablesWithNulls;

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

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issue338/($projectid)')]
    procedure GetProject;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issue338/($projectid)/pictures/($imageuuid)')]
    procedure GetImage;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issue492/($stringvalue)')]
    procedure GetIssue492;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issue542/($stringvalue)')]
    procedure GetIssue542;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issue552')]
    procedure TestIssue552GUIDSupport;

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/guidserializationecho')]
    procedure TestGUIDSerializationEcho;

    { injectable parameters }
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/injectable10')]
    procedure GetInject10(
      const [MVCFromQueryString('parstring')]     ParString: String;
      const [MVCFromQueryString('parinteger')]    ParInteger: Integer;
      const [MVCFromQueryString('parint64')]      ParInt64: Int64;
      const [MVCFromQueryString('partdate')]      ParTDate: TDate;
      const [MVCFromQueryString('parttime')]      ParTTime: TTime;
      const [MVCFromQueryString('partdatetime')]  ParTDateTime: TDateTime;
      const [MVCFromQueryString('parbool')]       ParBool: Boolean
      );

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/injectable20')]
    procedure GetInject20(
      const [MVCFromHeader('parstring')]     ParString: String;
      const [MVCFromHeader('parinteger')]    ParInteger: Integer;
      const [MVCFromHeader('parint64')]      ParInt64: Int64;
      const [MVCFromHeader('partdate')]      ParTDate: TDate;
      const [MVCFromHeader('parttime')]      ParTTime: TTime;
      const [MVCFromHeader('partdatetime')]  ParTDateTime: TDateTime;
      const [MVCFromHeader('parbool')]       ParBool: Boolean
      );

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/injectable30')]
    procedure PostInject30(const [MVCFromBody] Person: TPerson);

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/injectable40/married/($Married)/id/($ID)')]
    procedure PostInject40(
      const Married: Boolean;
      const ID: Int64;
      const [MVCFromBody] Person: TPerson;
      const [MVCFromQueryString('FirstName')] FirstName: String;
      const [MVCFromHeader('LastName')] LastName: String;
      const [MVCFromCookie('DOB')] DOB: TDate
      );

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/injectable50')]
    procedure PostInject50(const [MVCFromBody] People: TObjectList<TPerson>);


    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/programmerex')]
    procedure CreateProgrammerEx(const [MVCFromBody] ProgrammerEx: TProgrammerEx);

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/programmerex2')]
    procedure CreateProgrammerEx2(const [MVCFromBody] ProgrammerEx2: TProgrammerEx2);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/ignoredfieldstest')]
    procedure RenderProgrammerWithIgnoredFields(
      const [MVCFromQueryString('ignoredfieldscsv','')] IgnoredFieldsCSV: String);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/ignoredfieldstestdataset')]
    procedure RenderDataSetWithIgnoredFields(
      const [MVCFromQueryString('ignoredfieldscsv','')] IgnoredFieldsCSV: String);

    { templates }
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/website/list')]
    procedure Tmpl_ListOfDataUsingDatasets;

    { issues }
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issues/406')]
    procedure TestIssue406;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issues/526')]
    procedure TestIssue526;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/issues/542')]
    procedure TestIssue542;
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

  [MVCPath('/exception/fault')]
  TTestFaultController = class(TMVCController)
  public
    [MVCPath]
    procedure NeverExecuted;
    constructor Create; override;
  end;

  [MVCPath('/exception/fault2')]
  TTestFault2Controller = class(TTestFaultController)
  public
    [MVCPath]
    procedure NeverExecuted;
    constructor Create; override;
  end;

  [MVCPath]
  [MVCPath('/api/v1')]
  [MVCPath('/api/v2')]
  TTestMultiPathController = class(TMVCController)
  public
    [MVCPath]
    [MVCPath('/action1')]
    [MVCPath('/action2')]
    procedure Action1or2;
  end;


// action result types
  [MVCNameCase(ncLowerCase)]
  TSum = class
  private
    fValue: Integer;
  public
    property Value: Integer read fValue write fValue;
  end;

  [MVCNameCase(ncLowerCase)]
  TComplexObject = class
  private
    fValue: Integer;
    FPeople: TPeople;
    FPerson: TPerson;
    procedure SetPeople(const Value: TPeople);
    procedure SetPerson(const Value: TPerson);
  public
    destructor Destroy; override;
    property Value: Integer read fValue write fValue;
    property Person: TPerson read FPerson write SetPerson;
    property People: TPeople read FPeople write SetPeople;
  end;

  [MVCNameCase(ncCamelCase)]
  TPersonRec = record
    FirstName, LastName: String;
    Age: Integer;
    class function Create: TPersonRec; static;
  end;
// action result types - end

  [MVCPath('/api/v1/actionresult')]
  TTestActionResultController = class(TMVCController)
  public
    { actions returning records }
    [MVCPath('/sums/($a)/($b)')]
    [MVCHTTPMethod([httpGET])]
    function GetObject(a,b: Integer): TSum;

    [MVCPath('/records/single')]
    function GetSingleRecord: TPersonRec;

    [MVCPath('/records/multiple')]
    function GetMultipleRecords: TArray<TPersonRec>;

    [MVCPath('/complex')]
    [MVCHTTPMethod([httpGET])]
    function GetComplexObject: TComplexObject;

    [MVCPath('/people')]
    [MVCHTTPMethod([httpGET])]
    function GetPeople: TObjectList<TPerson>;

    [MVCPath('/people/($id)')]
    [MVCHTTPMethod([httpGET])]
    function GetPerson(id: Integer): IPerson;

    [MVCPath('/photo')]
    [MVCHTTPMethod([httpGET])]
    function GetPhoto: TStream;

    [MVCPath('/string')]
    [MVCHTTPMethod([httpGET])]
    function GetString: String;

    [MVCPath('/enum')]
    [MVCHTTPMethod([httpGET])]
    function GetEnum: TFontStyle;

    [MVCPath('/bool')]
    [MVCHTTPMethod([httpGET])]
    function GetBool: Boolean;

    [MVCPath('/float')]
    [MVCHTTPMethod([httpGET])]
    function GetFloat: Double;

    [MVCPath('/strdict')]
    [MVCHTTPMethod([httpGET])]
    function GetStrDict: TMVCStringDictionary;

    [MVCPath('/TSimpleRecord')]
    [MVCHTTPMethod([httpGET])]
    function GetTSimpleRecord: TSimpleRecord;

    [MVCPath('/ArrayOf/TSimpleRecord')]
    [MVCHTTPMethod([httpGET])]
    function GetArrayOfTSimpleRecord: TArray<TSimpleRecord>;

    [MVCPath('/TComplexRecord')]
    [MVCHTTPMethod([httpGET])]
    function GetTComplexRecord: TComplexRecord;

    [MVCPath('/ArrayOf/TComplexRecord')]
    [MVCHTTPMethod([httpGET])]
    function GetArrayOfTComplexRecord: TComplexRecordArray;

    [MVCPath('/dataset/single')]
    [MVCHTTPMethod([httpGET])]
    function GetDataSetSingle: TDataSet;

    [MVCPath('/dataset/multiple')]
    [MVCHTTPMethod([httpGET])]
    function GetDataSetMultiple: IMVCObjectDictionary;

  end;



implementation

uses
  JsonDataObjects,
  System.JSON,
  Web.HTTPApp,
  Generics.Collections,
  MVCFramework.Serializer.Defaults,
  MVCFramework.DuckTyping,
  System.IOUtils, MVCFramework.Tests.Serializer.Entities, System.DateUtils;

{ TTestServerController }

procedure TTestServerController.CreateProgrammerEx(
  const ProgrammerEx: TProgrammerEx);
begin
  Render(ProgrammerEx, False);
end;

procedure TTestServerController.CreateProgrammerEx2(
  const ProgrammerEx2: TProgrammerEx2);
begin
  Render(ProgrammerEx2, False);
end;

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

class function TTestServerController.GetDataSet: TDataSet;
begin
  Result := TFDMemTable.Create(nil);
  TFDMemTable(Result).LoadFromFile(TPath.Combine(AppPath, 'customers.json'));
end;

procedure TTestServerController.GetImage;
begin
  // do nothing
end;


procedure TTestServerController.GetInject10(const ParString: String;
  const ParInteger: Integer; const ParInt64: Int64; const ParTDate: TDate;
  const ParTTime: TTime; const ParTDateTime: TDateTime; const ParBool: Boolean);
var
  lJObj: TJDOJsonObject;
begin
  lJObj := TJDOJsonObject.Create;
  lJObj.S['ParString'] := ParString;
  lJObj.I['ParInteger'] := ParInteger;
  lJObj.L['ParInt64'] := ParInt64;
  lJObj.S['ParTDate'] :=  DateToISODate(ParTDate);
  lJObj.S['ParTTime'] := TimeToISOTime(ParTTime);
  lJObj.S['ParTDateTime'] := DateTimeToISOTimeStamp(ParTDateTime);
  lJObj.B['ParBool'] := ParBool;
  Render(lJObj);
end;

procedure TTestServerController.GetInject20(const ParString: String;
  const ParInteger: Integer; const ParInt64: Int64; const ParTDate: TDate;
  const ParTTime: TTime; const ParTDateTime: TDateTime; const ParBool: Boolean);
var
  lJObj: TJDOJsonObject;
begin
  lJObj := TJDOJsonObject.Create;
  lJObj.S['ParString'] := ParString;
  lJObj.I['ParInteger'] := ParInteger;
  lJObj.L['ParInt64'] := ParInt64;
  lJObj.S['ParTDate'] :=  DateToISODate(ParTDate);
  lJObj.S['ParTTime'] := TimeToISOTime(ParTTime);
  lJObj.S['ParTDateTime'] := DateTimeToISOTimeStamp(ParTDateTime);
  lJObj.B['ParBool'] := ParBool;
  Render(lJObj);
end;

procedure TTestServerController.GetIssue492;
begin
  // do nothing
end;

procedure TTestServerController.GetIssue542;
begin
  // do nothing
end;

procedure TTestServerController.GetProject;
begin
  // do nothing
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
  Context.SessionStop;
  if Context.SessionStarted then
    raise EMVCException.Create('Session still available');
end;

procedure TTestServerController.MVCControllerAfterCreate;
begin
  FFormatSettings.DecimalSeparator := '.';
end;

procedure TTestServerController.MVCControllerBeforeDestroy;
begin
  inherited;

end;

procedure TTestServerController.PostInject30(const Person: TPerson);
begin
  Render(Person, False);
end;

procedure TTestServerController.PostInject40(
      const Married: Boolean;
      const ID: Int64;
      const [MVCFromBody] Person: TPerson;
      const [MVCFromQueryString('FirstName')] FirstName: String;
      const [MVCFromHeader('LastName')] LastName: String;
      const [MVCFromCookie('DOB')] DOB: TDate
      );
begin
  Person.FirstName := FirstName;
  Person.LastName := LastName;
  Person.DOB := DOB;
  Person.Married := Married;
  Person.ID := ID;
  Render(Person, False);
end;

procedure TTestServerController.PostInject50(
  const People: TObjectList<TPerson>);
begin
  Render<TPerson>(People, False);
end;

procedure TTestServerController.RenderDataSetWithIgnoredFields(
  const IgnoredFieldsCSV: String);
var
  lDict: IMVCObjectDictionary;
  lIgnoredFields: TMVCIgnoredList;
begin
  lIgnoredFields := TMVCIgnoredList(IgnoredFieldsCSV.Split([';',',']));
  lDict := ObjectDict(True)
    .Add('ncUpperCase_List', GetDataSet, nil, dstAllRecords, ncUpperCase, lIgnoredFields)
    .Add('ncLowerCase_List', GetDataSet, nil, dstAllRecords, ncLowerCase, lIgnoredFields)
    .Add('ncCamelCase_List', GetDataSet, nil, dstAllRecords, ncCamelCase, lIgnoredFields)
    .Add('ncPascalCase_List', GetDataSet, nil, dstAllRecords, ncPascalCase, lIgnoredFields)
    .Add('ncUpperCase_Single', GetDataSet, nil, dstSingleRecord, ncUpperCase, lIgnoredFields)
    .Add('ncLowerCase_Single', GetDataSet, nil, dstSingleRecord, ncLowerCase, lIgnoredFields)
    .Add('ncCamelCase_Single', GetDataSet, nil, dstSingleRecord, ncCamelCase, lIgnoredFields)
    .Add('ncPascalCase_Single', GetDataSet, nil, dstSingleRecord, ncPascalCase, lIgnoredFields)
    .Add('meta', StrDict(['page'], ['1']));
  Render(lDict);
end;

procedure TTestServerController.RenderProgrammerWithIgnoredFields(
  const IgnoredFieldsCSV: String);
begin
  Render(ObjectDict().Add(
    'data',
    TProgrammerEx.GetNew('Daniele','Teti', EncodeDate(1979,11,4),True),
    nil,
    TMVCIgnoredList(IgnoredFieldsCSV.Split([';',',']))))
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
  ContentType := TMVCMediaType.TEXT_PLAIN;
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
{$IFNDEF LINUX}
  //lCustomer.Logo.Canvas.TextOut(10, 10, 'Changed');
{$ENDIF}
  // lCustomer.Logo.SaveToFile('pippo_server_after.bmp');
  Render(lCustomer, True);
end;

procedure TTestServerController.TestCustomerEchoBodyFor;
var
  lCustomer: TCustomer;
begin
  lCustomer := TCustomer.Create;
  try
    Context.Request.BodyFor<TCustomer>(lCustomer);
    lCustomer.Name := lCustomer.Name + ' changed';
    Render(lCustomer, False);
  finally
    lCustomer.Free;
  end;
end;

  procedure TTestServerController.TestCustomerEchoWithRootNode;
  var
    lCustomer1, lCustomer2: TCustomer;
  begin
    lCustomer1 := Context.Request.BodyAs<TCustomer>('customer1');
    try
      lCustomer2 := Context.Request.BodyAs<TCustomer>('customer2');
      try
        Render(ObjectDict(False)
          .Add('customer1', lCustomer1)
          .Add('customer2', lCustomer2));
      finally
        lCustomer2.Free;
      end;
    finally
      lCustomer1.Free;
    end;
  end;

procedure TTestServerController.TestDeserializeAndSerializeNullables;
var
  lNullablesTest: TNullablesTest;
begin
  lNullablesTest := Context.Request.BodyAs<TNullablesTest>;
  Render(lNullablesTest);
end;

procedure TTestServerController.TestEMVCException1;
begin
  raise EMVCException.Create('message');
end;

procedure TTestServerController.TestEMVCException2;
begin
  raise EMVCException.Create(HTTP_STATUS.BadRequest, 'message');
end;

procedure TTestServerController.TestEMVCException3;
begin
  raise EMVCException.Create(HTTP_STATUS.Created, 999, 'message');
end;

procedure TTestServerController.TestEMVCException4;
begin
  raise EMVCException.Create('message', 'detailedmessage', 999, HTTP_STATUS.Created, ['erritem1', 'erritem2']);
end;

procedure TTestServerController.TestEntityWithArrays;
var
  lObj: TEntityWithArray;
begin
  lObj := Context.Request.BodyAs<TEntityWithArray>;
  try
    lObj.Names := lObj.Names + ['added'];
    lObj.Values := lObj.Values + [99];
    lObj.Booleans := lObj.Booleans + [true];
    Render(lObj, False);
  finally
    lObj.Free;
  end;
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
var
  lFName: string;
begin
  ContentType := TMVCMediaType.IMAGE_PNG;
  lFName := TPath.Combine(AppPath, 'sample.png');
  Render(TFile.OpenRead(lFName));
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
      Links.AddRefLink.Add(HATEOAS.HREF, '/api/people/' + Person.ID.ToString).Add(HATEOAS.REL, 'test0')
        .Add(HATEOAS._TYPE, 'application/json');
      Links.AddRefLink.Add(HATEOAS.HREF, '/api/test/' + Person.ID.ToString).Add(HATEOAS.REL, 'test1').Add(HATEOAS._TYPE,
        'application/json')
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

procedure TTestServerController.TestGUIDSerializationEcho;
var
  lEnt: TEntityWithGUIDs;
begin
  lEnt := Context.Request.BodyAs<TEntityWithGUIDs>;
  try
    Render(lEnt, False);
  finally
    lEnt.Free;
  end;
end;

procedure TTestServerController.TestHelloWorld;
begin
  ContentType := 'text/plain';
  Render('hello world');
end;

procedure TTestServerController.TestIssue406;
begin
  Render(HTTP_STATUS.UnprocessableEntity, TMVCErrorResponseItem.Create('The Message'));
end;

procedure TTestServerController.TestIssue526;
begin
  ContentType := 'application/fhir+xml; fhirVersion=4.0';
  ResponseStream.Append('OK');
  RenderResponseStream;
end;

procedure TTestServerController.TestIssue542;
var
  lJSON: TJDOJSONObject;
begin
  lJSON := TJDOJSONObject.Create;
  try
    lJSON.S['QueryStringParams_DelimitedText'] := Context.Request.QueryStringParams.DelimitedText;
    lJSON.S['QueryStringParam_par1'] := Context.Request.QueryStringParam('par1');
    lJSON.S['QueryStringParam_par2'] := Context.Request.QueryStringParam('par2');
    lJSON.I['QueryParams_Count'] := Context.Request.QueryParams.Count;
    lJSON.S['QueryParams_par1'] := Context.Request.QueryParams['par1'];
    lJSON.S['QueryParams_par2'] := Context.Request.QueryParams['par2'];
    Render(lJSON, False);
  finally
    lJSON.Free;
  end;
end;

procedure TTestServerController.TestIssue552GUIDSupport;
var
  lObj: TEntityWithGUIDs;
begin
  lObj := TEntityWithGUIDs.Create(False);
  lObj.GUID := StringToGUID('{75ADE43E-F8C1-4F66-B714-D04726FD2C21}');
  lObj.NullableGUID := StringToGUID('{7B17F2DD-6ED5-40A4-A334-8ED877A6803E}');
  lObj.NullableGUID2.Clear;
  Render(lObj);
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

procedure TTestServerController.TestObjectDict;
var
  lDict: IMVCObjectDictionary;
begin
  lDict := ObjectDict(True).Add('ncUpperCase_List', GetDataSet, nil, dstAllRecords, ncUpperCase)
    .Add('ncLowerCase_List', GetDataSet, nil, dstAllRecords, ncLowerCase).Add('ncCamelCase_List', GetDataSet, nil,
    dstAllRecords, ncCamelCase).Add('ncPascalCase_List', GetDataSet, nil, dstAllRecords, ncPascalCase)
    .Add('ncUpperCase_Single', GetDataSet, nil, dstSingleRecord, ncUpperCase).Add('ncLowerCase_Single', GetDataSet, nil,
    dstSingleRecord, ncLowerCase).Add('ncCamelCase_Single', GetDataSet, nil, dstSingleRecord, ncCamelCase)
    .Add('ncPascalCase_Single', GetDataSet, nil, dstSingleRecord, ncPascalCase).Add('meta', StrDict(['page'], ['1']));
  Render(lDict);
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
  Render202Accepted('http://pippo.it/1234', '1234', 'thisisthereason');
end;

procedure TTestServerController.TestResponseCreated;
begin
  Render201Created('thisisthelocation', 'thisisthereason');
end;

procedure TTestServerController.TestResponseNoContent;
begin
  Render204NoContent('thisisthereason');
end;

procedure TTestServerController.TestSerializeNullables;
var
  lObj: TNullablesTest;
begin
  lObj := TNullablesTest.Create();
  lObj.LoadSomeData;
  Render(lObj);
end;

procedure TTestServerController.TestSerializeNullablesWithNulls;
var
  lObj: TNullablesTest;
begin
  lObj := TNullablesTest.Create();
  Render(lObj);
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
  Render('*' + value + '*');
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

procedure TTestServerController.TestTypedActionTGuid1(value: TGUID);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render(GuidToString(value) + ' modified from server');
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

procedure TTestServerController.TestWithAllVerbs;
var
  lPerson: TPerson;
begin
  lPerson := Context.Request.BodyAs<TPerson>();
  Render(lPerson, True);
end;

procedure TTestServerController.Tmpl_ListOfDataUsingDatasets;
var
  lDS: TFDMemTable;
begin
  lDS := TFDMemTable.Create(nil);
  try
    var lFName: string := TPath.Combine(AppPath, 'customers.json');
    lDS.LoadFromFile(lFName);
    ViewDataset['customers'] := lDS;
    ViewData['customers2'] := lDS;
    LoadView(['dataset_list']);
    RenderResponseStream;
  finally
    lDS.Free;
  end;
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

{ TTestMultiPathController }

procedure TTestMultiPathController.Action1or2;
begin
  Render(HTTP_STATUS.OK);
end;

{ TTestActionResultController }

function TTestActionResultController.GetArrayOfTComplexRecord: TComplexRecordArray;
begin
  SetLength(Result,3);
  Result[0] := TComplexRecord.Create;
  Result[1] := TComplexRecord.Create;
  Result[2] := TComplexRecord.Create;

  Result[0].StringProperty := 'item 0';
  Result[1].StringProperty := 'item 1';
  Result[2].StringProperty := 'item 2';
end;

function TTestActionResultController.GetArrayOfTSimpleRecord: TArray<TSimpleRecord>;
begin
  SetLength(Result, 3);
  Result[0] := TSimpleRecord.Create;
  Result[1] := TSimpleRecord.Create;
  Result[2] := TSimpleRecord.Create;
end;

function TTestActionResultController.GetBool: Boolean;
begin
  Result := True;
end;

function TTestActionResultController.GetComplexObject: TComplexObject;
begin
  Result := TComplexObject.Create;
  Result.Value := 1234;
  Result.Person := TPerson.GetNew('Danielem', 'Teti', EncodeDate(1920,12,23), True);
  Result.People := TPerson.GetList();
end;

function TTestActionResultController.GetDataSetMultiple: IMVCObjectDictionary;
begin
  Result :=
    ObjectDict()
      .Add('ds1', TTestServerController.GetDataSet)
      .Add('ds2', TTestServerController.GetDataSet);
end;

function TTestActionResultController.GetDataSetSingle: TDataSet;
begin
  Result := TTestServerController.GetDataSet;
end;

function TTestActionResultController.GetEnum: TFontStyle;
begin
  Result := TFontStyle.fsBold;
end;

function TTestActionResultController.GetFloat: Double;
begin
  Result := 3.1415;
end;

function TTestActionResultController.GetMultipleRecords: TArray<TPersonRec>;
begin
  SetLength(Result, 3);
  Result[0] := TPersonRec.Create;
  Result[1] := TPersonRec.Create;
  Result[2] := TPersonRec.Create;
  Result[0].Age := 20;
  Result[1].Age := 30;
  Result[2].Age := 40;
end;

function TTestActionResultController.GetPeople: TObjectList<TPerson>;
begin
  Result := TPerson.GetList();
end;

function TTestActionResultController.GetPerson(id: Integer): IPerson;
begin
  Result := TInterfacedPerson.Create('Daniele Teti', 20, 2010);
end;

function TTestActionResultController.GetPhoto: TStream;
begin
  Context.Response.ContentType := TMVCMediaType.IMAGE_X_PNG;
  Result := TFileStream.Create('sample.png', fmOpenRead or fmShareDenyNone);
end;

function TTestActionResultController.GetSingleRecord: TPersonRec;
begin
  Result := TPersonRec.Create;
end;

function TTestActionResultController.GetStrDict: TMVCStringDictionary;
begin
  Result := StrDict.Add('first_name','Daniele').Add('last_name','Teti');
end;

function TTestActionResultController.GetString: String;
begin
  Result := 'Hello World';
end;

function TTestActionResultController.GetTComplexRecord: TComplexRecord;
begin
  Result := TComplexRecord.Create;
end;

function TTestActionResultController.GetTSimpleRecord: TSimpleRecord;
begin
  Result := TSimpleRecord.Create;
end;

function TTestActionResultController.GetObject(a, b: Integer): TSum;
begin
  StatusCode := 201;
  Context.Response.SetCustomHeader('X-CUSTOM-HEADER','CARBONARA');
  Result := TSum.Create;
  Result.Value := a + b;
end;

{ TComplexObject }

destructor TComplexObject.Destroy;
begin
  FPerson.Free;
  FPeople.Free;
  inherited;
end;

procedure TComplexObject.SetPeople(const Value: TPeople);
begin
  FPeople := Value;
end;

procedure TComplexObject.SetPerson(const Value: TPerson);
begin
  FPerson := Value;
end;

{ TPersonRec }

class function TPersonRec.Create: TPersonRec;
begin
  Result.FirstName := 'Daniele';
  Result.LastName := 'Teti';
  Result.Age := 99;
end;

end.
