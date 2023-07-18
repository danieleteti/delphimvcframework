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

unit FrameworkTestsU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  BOs,
  MVCFramework, Data.DB, System.SysUtils, MVCFramework.JWT,
  MVCFramework.Serializer.Intf, MVCFramework.Serializer.Defaults,
  MVCFramework.MultiMap, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  MVCFramework.Crypt.Utils;

type

  [TestFixture]
  TTestRouting = class(TObject)
  private
    FRouter: TMVCRouter;
    FControllers: TObjectList<TMVCControllerDelegate>;
    FMVCActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem>;
    FConfig: TMVCConfig;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestWithNoParameters;
    [Test]
    procedure TestWithNoPath;
    [Test]
    procedure TestPathButNoParameters;
    [Test]
    procedure TestPathWithParameters;
    [Test]
    procedure TestWithMethodTypes;
    [Test]
    procedure TestComplexRoutings;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_338;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_A;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_B;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_513_C;
    [Test]
    [Category('issues')]
    procedure Test_ISSUE_492;
    [Test]
    procedure TestProduceRoutings;
    [Test]
    procedure TestProduceRoutingsWithExplicitCharset;
    [Test]
    procedure TestPathPrefix;
    [Test]
    procedure TestReservedIPs;
    // procedure TestRoutingSpeed;

    // objects mappers
  end;

  [TestFixture]
  TTestJWT = class(TObject)
  private
    FJWT: TJWT;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestHMAC;
    [Test]
    procedure TestStorage;
    [Test]
    procedure TestCreateAndValidateToken;
    [Test]
    procedure TestLoadToken;
    [Test]
    procedure TestNotBefore;
    [Test]
    procedure TestExpirationTime;
    [Test]
    procedure TestIssuedAt;
    [Test]
    procedure TestDefaults;
  end;

  { This is the base test case for all the serunser testcases,
    check 'SerializationFrameworkTestU.pas' }
  [TestFixture]
  TMVCSerUnSerTestCase = class abstract(TObject)
  private
    FSerializer: IMVCSerializer;
  protected
    procedure SetSerializer(const ASerializer: IMVCSerializer);
    [SetUp]
    procedure SetUp;
    function GetObjectsList: TObjectList<TMyObject>;
    function GetObjectsWithStreamsList: TObjectList<TMyStreamObject>;
    function GetObjectsWithTValueList: TObjectList<TMyObjectWithTValue>;
    property Serializer: IMVCSerializer read FSerializer;
    [Test]
    procedure TestSerUnSerObject; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectList; virtual; abstract;
    [Test]
    procedure TestSerUnSerNestedObjects; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectWithStream; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectListWithStream; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectWithTValue; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectListWithTValue; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectStrict; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectBuiltInCustomTypes; virtual; abstract;
    [Test]
    procedure TestSerUnSerObjectBuiltInCustomTypesFullObject; virtual; abstract;
  end;

  [TestFixture]
  TTestMultiMap = class(TObject)
  protected
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestObjectMultiMapAdd;
    [Test]
    procedure TestObjectMultiMapRemove;
    [Test]
    procedure TestInterfaceMultiMapAdd;
    [Test]
    procedure TestInterfaceMultiMapRemove;
  end;

  [TestFixture]
  TTestNameCase = class(TObject)
  private
    fOutDATA: array [1 .. 5] of array [ncAsIs .. ncSnakeCase] of string;
    fOrigDATA: array [1 .. 5] of string;
  public
    [SetupFixture]
    procedure SetupFixture;
    [Test]
    procedure TestNameCase;

    [Test]
    [TestCase('LowerCase', 'onetwo,onetwo')]
    [TestCase('LowerCaseWithUnderline', 'one_two,one_two')]
    [TestCase('UpperCase', 'ONETWO,onetwo')]
    [TestCase('UpperCaseWithUnderline', 'ONE_TWO,one_two')]
    [TestCase('PascalCase', 'OneTwo,one_two')]
    [TestCase('CamelCase', 'oneTwo,one_two')]
    [TestCase('UPPERLower1', 'ONETwo,one_two')]
    [TestCase('UPPERLower2', 'OneTWOThree,one_two_three')]
    [TestCase('UPPERLower3', 'DATEOf,date_of')]
    [TestCase('UPPERLower4', 'RESTClient,rest_client')]
    [TestCase('UPPERLowerWithNumber1', 'OneTWO01,one_two_01')]
    [TestCase('UPPERLowerWithNumber2', 'ONE02Three,one_02_three')]
    [TestCase('WithSpaces1', 'One two three,one_two_three')]
    [TestCase('WithSpaces2', 'One  two  three,one_two_three')]
    [TestCase('WithSpaces3', 'One   two   three,one_two_three')]
    [TestCase('WithDots1', 'One.two.three,one_two_three')]
    [TestCase('WithDots2', 'ONE.TWO.THREE,one_two_three')]
    [TestCase('WithDots3', 'ONE.02.THREE,one_02_three')]
    [TestCase('WithUnderlines', 'One_two_three,one_two_three')]
    [TestCase('MultipleUnderlines', 'One___two______three,one_two_three')]
    [TestCase('WithNumber1', 'OneTwo1,one_two_1')]
    [TestCase('WithNumber02', 'OneTwo02,one_two_02')]
    [TestCase('WithNumberInTheMiddle1', 'OneTwo1Two,one_two_1_two')]
    [TestCase('WithNumberInTheMiddle2', 'OneTwo_2two,one_two_2_two')]
    [TestCase('WithNumberInTheMiddle3', 'OneTwo3_Two3,one_two_3_two_3')]
    [TestCase('WithNumberInTheMiddle4', 'OneTwo_4_Two,one_two_4_two')]
    [TestCase('WithNumberInTheMiddle5', 'OneTwo05Three,one_two_05_three')]
    [TestCase('WithNumberInTheMiddle6', 'OneTwo_06Three,one_two_06_three')]
    [TestCase('WithNumberInTheMiddle7', 'OneTwo07_Three,one_two_07_three')]
    [TestCase('WithNumberInTheMiddle8', 'OneTwo_08_Three,one_two_08_three')]
    procedure TestSnakeCase(const AValue1: string; const AValue2: string);

  end;

  [TestFixture]
  TTestCryptUtils = class(TObject)
  public
    [SetupFixture]
    procedure SetupFixture;
    [Test]
    procedure TestPBKDF2_SHA1;
    [Test]
    procedure TestPBKDF2_SHA256;
  end;

  [TestFixture]
  TTestUTC = class(TObject)
  public
    [Test]
    procedure TestStringToDateTime_Local;
    [Test]
    procedure TestStringToDateTime_in_DST_period;
    [Test]
    procedure TestStringToDateTime_in_no_DST_period;
    [Test]
    procedure TestStringToDateTime_NewYork;
    [Test]
    procedure TestStringToDateTime_Mumbai;
  end;

  [TestFixture]
  TTestLRUCache = class(TObject)
  public
    [Test]
    [Category('lru')]
    procedure TestPutGet;
    [Test]
    [Category('lru')]
    procedure TestPutGet_Check_No_AV;
  end;


  [TestFixture]
  TTestDotEnv = class(TObject)
  public
    [Test]
    procedure TestWithoutProfiles;
    [Test]
    procedure TestWithDevProfile;
    [Test]
    procedure TestWithDevAndTestProfile;
  end;

  [TestFixture]
  TTestDotEnvParser = class(TObject)
  public
    [Test]
    procedure TestKeyValue;
    [Test]
    procedure TestKeyValueWithQuotedValues;
    [Test]
    procedure TestValueWithMultiline;
    [Test]
    procedure TestVarPlaceHolders;
    [Test]
    procedure TestInLineComments;
  end;


implementation

{$WARN SYMBOL_DEPRECATED OFF}

uses
  System.DateUtils, System.TimeSpan, System.Math,
  TestControllersU, DBClient,
  Web.HTTPApp, Soap.EncdDecd,
  IdHashMessageDigest, idHash,
  System.Threading,
  MVCFramework.HMAC, System.Diagnostics,
  MVCFramework.LRUCache,

{$IF CompilerVersion < 27}
  Data.DBXJSON,

{$ELSE}
  System.JSON,

{$ENDIF}
  TestServerControllerU, System.Classes,
  MVCFramework.DuckTyping, System.IOUtils, MVCFramework.SystemJSONUtils,
  IdGlobal, System.TypInfo, System.Types, Winapi.Windows, MVCFramework.DotEnv,
  MVCFramework.DotEnv.Parser;

var
  JWT_SECRET_KEY_TEST: string = 'myk3y';
  HMAC_ALG_AND_RESULTS: array [0 .. 4] of array [0 .. 1] of string = (
    (
      'md5',
      '5256311089fa9c80f735fb8cc28bf4fe'
    ),
    (
      'sha1',
      '323ff5f4e53c43f2d9342952299a9d35f9ee5dc2'
    ),
    (
      'sha224',
      '2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35'
    ),
    (
      'sha256',
      '1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b'
    ),
    (
      'sha512',
      '22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9'
    )
  );

function MD5(const aStream: TStream): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  aStream.Position := 0;
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashBytesAsHex(idmd5.HashStream(aStream));
  finally
    idmd5.Free;
  end;
end;

procedure TTestRouting.SetUp;
begin
  FControllers := TObjectList<TMVCControllerDelegate>.Create;
  FControllers.Add(TMVCControllerDelegate.Create(TSimpleController, nil));
  FControllers.Add(TMVCControllerDelegate.Create(TNotSoSimpleController, nil));
  FControllers.Add(TMVCControllerDelegate.Create(TTestServerController, nil));
  FMVCActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
  FConfig := TMVCConfig.Create;
  FConfig.Value[TMVCConfigKey.PathPrefix] := '';
  FRouter := TMVCRouter.Create(FConfig, FMVCActionParamsCache);
end;

procedure TTestRouting.TearDown;
begin
  FRouter.Free;
  FControllers.Free;
  FMVCActionParamsCache.Free;
  FConfig.Free;
end;

procedure TTestRouting.TestComplexRoutings;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/path1/1', httpPOST, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.isNull(FRouter.MethodToCall);
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_338;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/338
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/issue338/projectid/pictures/imageuuid', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetImage', FRouter.MethodToCall.Name);
    Assert.areEqual(2, Params.Count);
    Assert.areEqual('projectid', Params['projectid']);
    Assert.areEqual('imageuuid', Params['imageuuid']);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/issue338/projectid', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetProject', FRouter.MethodToCall.Name);
    Assert.areEqual(1, Params.Count);
    Assert.areEqual('projectid', Params['projectid']);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_492;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/492
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/issue492/delphi$mvc$framework', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetIssue492', FRouter.MethodToCall.Name);
    Assert.areEqual(1, Params.Count);
    Assert.areEqual('delphi$mvc$framework', Params['stringvalue']);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.Test_ISSUE_513_A;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/patient/$match', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetOrderIssue513', FRouter.MethodToCall.Name);
    Assert.areEqual(0, Params.Count);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.Test_ISSUE_513_B;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/patient/$match/daniele/teti', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetOrderIssue513WithPars', FRouter.MethodToCall.Name);
    Assert.areEqual(2, Params.Count);
    Assert.areEqual('daniele', Params['par1']);
    Assert.areEqual('teti', Params['par2']);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.Test_ISSUE_513_C;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  // https://github.com/danieleteti/delphimvcframework/issues/513
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/patient/$match/da$niele/te$ti', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('GetOrderIssue513WithPars', FRouter.MethodToCall.Name);
    Assert.areEqual(2, Params.Count);
    Assert.areEqual('da$niele', Params['par1']);
    Assert.areEqual('te$ti', Params['par2']);
  finally
    Params.Free;
  end;

end;


// procedure TTestMappers.TestDataSetToJSONArray;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// JArr: TJSONArray;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// ds.First;
// // JArr := TJSONArray.Create;
// JArr := ds.AsJSONArray;
// try
// // Mapper.DataSetToJSONArray(ds, JArr, false);
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds.First;
// while not ds.Eof do
// begin
// ds2.Insert;
// JObj := JArr.Get(ds.RecNo - 1) as TJSONObject;
// ds2.LoadFromJSONObject(JObj);
// // Mapper.JSONObjectToDataSet(JObj, ds2, false);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// ds.Next;
// end;
// finally
// JArr.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObject;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject;
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyAsIsCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpAsIs);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpAsIs);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyLowerCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpLowerCase);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpLowerCase);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;
//
// procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyUpperCase;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// ds2: TClientDataSet;
// begin
// ds := TClientDataSet.Create(nil);
// ds2 := TClientDataSet.Create(nil);
// try
// ds.LoadFromFile('..\..\fishes.xml');
// JObj := ds.AsJSONObject(false, fpUpperCase);
// try
// ds2.LoadFromFile('..\..\fishes.xml');
// ds2.EmptyDataSet;
// ds2.Insert;
// ds2.LoadFromJSONObject(JObj, fpUpperCase);
// ds2.Post;
// SameFishesDataSet(ds, ds2);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// ds2.Free;
// end;
// end;

// procedure TTestMappers.TestDataSetToJSONObjectWithNulls;
// var
// ds: TClientDataSet;
// JObj: TJSONObject;
// begin
// ds := TClientDataSet.Create(nil);
// try
// ds.FieldDefs.Add('string_value', ftString, 50);
// ds.FieldDefs.Add('integer_value', ftInteger);
// ds.FieldDefs.Add('float_value', ftFloat);
// ds.FieldDefs.Add('null_value', ftString, 50);
// ds.FieldDefs.Add('boolean_value', ftBoolean);
// ds.CreateDataSet;
// ds.Insert;
// ds.FieldByName('string_value').AsString := 'myStringValue';
// ds.FieldByName('integer_value').AsInteger := 123;
// ds.FieldByName('float_value').AsFloat := 123.456;
// ds.FieldByName('null_value').Clear;
// ds.FieldByName('boolean_value').AsBoolean := true;
// ds.Post;
// JObj := ds.AsJSONObject;
// try
// Assert.areEqual('myStringValue', JObj.Values['string_value'].Value);
// Assert.areEqual(123, JObj.Values['integer_value'].GetValue<TJSONNumber>().AsInt);
// Assert.areEqual(123.456, JObj.Values['float_value'].GetValue<TJSONNumber>().AsDouble, 0.0009);
// Assert.isTrue(JObj.Values['null_value'].GetValue<TJSONNull>().Null);
// Assert.areEqual(true, JObj.Values['boolean_value'].GetValue<TJSONBool>().AsBoolean);
// Assert.isTrue(JObj.ToJSON.Replace(' ', '').Contains('"null_value":null'));
// ds.Insert;
// ds.LoadFromJSONObject(JObj);
// ds.Post;
// Assert.isTrue(ds.FieldByName('null_value').IsNull);
// finally
// JObj.Free;
// end;
// finally
// ds.Free;
// end;
// end;

// procedure TTestMappers.TestJSONArrayToObjectListNoGenerics;
// var
// ListObj, RetList: TObjectList<TMyObject>;
// JSONArr: TJSONArray;
// I: Integer;
// begin
// ListObj := TObjectList<TMyObject>.Create;
// try
// ListObj.Add(GetMyObject);
// ListObj.Add(GetMyObject);
// JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
// try
// RetList := TObjectList<TMyObject>(Mapper.JSONArrayToObjectList(TMyObject,
// JSONArr, false));
// try
// Assert.areEqual(2, RetList.Count);
// for I := 0 to ListObj.Count - 1 do
// Assert.isTrue(ListObj[I].Equals(RetList[I]));
// finally
// RetList.Free;
// end;
// finally
// JSONArr.Free;
// end;
// finally
// ListObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONArrayToObjectListNoGenericsWrappedList;
// var
// ListObj, RetList: TObjectList<TMyObject>;
// JSONArr: TJSONArray;
// I: Integer;
// begin
// ListObj := TObjectList<TMyObject>.Create;
// try
// ListObj.Add(GetMyObject);
// ListObj.Add(GetMyObject);
// JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
// try
// RetList := TObjectList<TMyObject>.Create;
// try
// Mapper.JSONArrayToObjectList(WrapAsList(RetList), TMyObject,
// JSONArr, false);
// Assert.areEqual(2, RetList.Count);
// for I := 0 to ListObj.Count - 1 do
// Assert.isTrue(ListObj[I].Equals(RetList[I]));
// finally
// RetList.Free;
// end;
// finally
// JSONArr.Free;
// end;
// finally
// ListObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectStringToObject;
// const
// MYOBJECTJSON =
// '{"PropString":"Some text \u00E0\u00E8\u00E9\u00EC\u00F2\u00F9",' +
// '"PropAnsiString":"This is an ANSI text","PropInteger":-1234,' +
// '"PropUInt32":1234,"PropInt64":-1234567890,"PropUInt64":1234567890,' +
// '"PropUInt16":12345,"PropInt16":-12345,"PropBoolean":true,' +
// '"PropDate":"2010-10-20","PropTime":"10:20:30",' +
// '"PropDateTime":"2010-10-20 10:20:30",' +
// '"PropTimeStamp":63423339630040,"PropCurrency":1234.5678}';
// var
// lMyObject: TMyObject;
// lMyObject2: TMyObject;
// begin
// lMyObject := Mapper.JSONObjectStringToObject<TMyObject>(MYOBJECTJSON);
// try
// lMyObject2 := GetMyObject;
// try
// Assert.isTrue(lMyObject.Equals(lMyObject2));
// finally
// lMyObject2.Free;
// end;
// finally
// lMyObject.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectStringToObjectWithWrongJSON;
// begin
// ExpectedException := EMapperException;
// Mapper.JSONObjectStringToObject<TObject>('{wrongjson}');
// end;
//
// procedure TTestMappers.TestJSONObjectToObjectAndBack;
// var
// Obj: TMyObject;
// JObj: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyObject>(JObj);
// try
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JObj.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestJSONObjectToObjectWithNullInJSONString;
// var
// LJSONObject: string;
// Obj: TMyStreamObject;
// begin
// LJSONObject := '{"ImageStream":null}';
// Obj := Mapper.JSONObjectStringToObject<TMyStreamObject>(LJSONObject);
// Assert.isNull(Obj.ImageStream);
// Obj.Free;
// end;
//
// procedure TTestMappers.TestLoadJSONObjectToObjectAndBack;
// var
// Obj: TMyObject;
// JObj: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := TMyObject.Create;
// try
// Mapper.LoadJSONObjectToObject<TMyObject>(JObj, Obj2);
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JObj.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectListToJSONArray;
// var
// Obj: TMyObject;
// ObjList, Obj2List: TObjectList<TMyObject>;
// JSON: TJSONArray;
// I: Integer;
// begin
// ObjList := TObjectList<TMyObject>.Create(true);
// try
// for I := 1 to 10 do
// begin
// Obj := GetMyObject;
// Obj.PropInteger := I;
// ObjList.Add(Obj);
// end;
// JSON := Mapper.ObjectListToJSONArray<TMyObject>(ObjList);
//
// Obj2List := Mapper.JSONArrayToObjectList<TMyObject>(JSON);
// try
// Assert.areEqual(ObjList.Count, Obj2List.Count);
// for I := 0 to 9 do
// begin
// Assert.isTrue(Obj2List[I].Equals(ObjList[I]));
// end;
// finally
// Obj2List.Free;
// end;
// finally
// ObjList.Free;
// end;
// end;
//
// procedure TTestMappers.TestWrappedListToJSONArray;
// var
// Obj: TMyObject;
// ObjList: TObjectList<TMyObject>;
// WrapList: IWrappedList;
// JSON: TJSONArray;
// I: Integer;
// LJSONObj: TJSONObject;
// LMyItem: TMyObject;
// begin
// ObjList := TObjectList<TMyObject>.Create(true);
// try
// for I := 1 to 10 do
// begin
// Obj := GetMyObject;
// Obj.PropInteger := I;
// ObjList.Add(Obj);
// end;
// WrapList := WrapAsList(ObjList);
// JSON := Mapper.ObjectListToJSONArray(WrapList);
// try
// Assert.areEqual(WrapList.Count, JSON.Count);
// for I := 0 to 9 do
// begin
// LJSONObj := JSON.Items[I] as TJSONObject;
// LMyItem := WrapList.GetItem(I) as TMyObject;
// Assert.areEqual(LMyItem.PropInteger, LJSONObj.GetValue<Integer>('PropInteger'));
// end;
// finally
// JSON.Free;
// end;
// finally
// ObjList.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObject;
// var
// Obj: TMyObject;
// JSON: TJSONObject;
// Obj2: TMyObject;
// begin
// Obj := GetMyObject;
// try
// JSON := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyObject>(JSON);
// try
// Assert.isTrue(Obj.Equals(Obj2));
// finally
// Obj2.Free;
// end;
// finally
// JSON.Free;
// end;
// finally
// Obj.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStream;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// begin
// // ARRANGE
// SO := TMyStreamObject.Create;
// try
// // ACT
// TMemoryStream(SO.ImageStream)
// .LoadFromFile('..\..\..\..\..\samples\_\customer.png');
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// // ASSERT
// Assert.areEqual(SO.ImageStream.Size, ResultSO.ImageStream.Size);
// Assert.areEqual(MD5(SO.ImageStream), MD5(ResultSO.ImageStream));
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF16;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// ResultStr, str: UnicodeString;
// begin
// // ARRANGE
// str := 'This is a UTF16 String (什么是)';
// SO := TMyStreamObject.Create;
// try
// // ACT
// SO.PropStream := TStringStream.Create(str, TEncoding.Unicode);
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// ResultStr := TStringStream(ResultSO.PropStream).DataString;
// // ASSERT
// Assert.areEqual(str, ResultStr);
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF8;
// var
// SO: TMyStreamObject;
// JSONObj: TJSONObject;
// ResultSO: TMyStreamObject;
// ResultStr, str: UTF8String;
// begin
// // ARRANGE
// str := 'This is a UTF8 String (什么是)';
// SO := TMyStreamObject.Create;
// try
// // ACT
// SO.Prop8Stream := TStringStream.Create(string(str), TEncoding.UTF8);
// JSONObj := Mapper.ObjectToJSONObject(SO);
// try
// ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// ResultStr := UTF8String(TStringStream(ResultSO.Prop8Stream).DataString);
// // ASSERT
// Assert.areEqual(str, ResultStr);
// finally
// ResultSO.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// SO.Free;
// end;
// end;
//
// procedure TTestMappers.TestObjectToJSONObject_Generics;
// var
// lObjList: TObjectList<TMyClass>;
// lResponse: TResponseWrapper<TMyClass>;
// LJSONObj: TJSONObject;
// begin
// lObjList := TObjectList<TMyClass>.Create();
// lObjList.Add(TMyClass.Create(1, 'pippo'));
// lObjList.Add(TMyClass.Create(2, 'pluto'));
// lResponse := TResponseWrapper<TMyClass>.Create(lObjList.Count, lObjList);
// try
// LJSONObj := Mapper.ObjectToJSONObject(lResponse);
// try
// CheckNotNull(LJSONObj.GetValue('Items'));
// Assert.areEqual(2, TJSONArray(LJSONObj.GetValue('Items')).Count);
// finally
// LJSONObj.Free;
// end;
// finally
// lResponse.Free;
// end;
// end;

procedure TTestRouting.TestPathButNoParameters;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentCharset: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, ResponseContentType, ResponseContentCharset));
    Assert.areEqual(0, Params.Count);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('Orders', FRouter.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestPathPrefix;
var
  lControllers: TObjectList<TMVCControllerDelegate>;
  lMVCActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem>;
  lConfig: TMVCConfig;
  lRouter: TMVCRouter;
  lParams: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  lControllers := TObjectList<TMVCControllerDelegate>.Create;
  lControllers.Add(TMVCControllerDelegate.Create(TSimpleController, nil));
  lControllers.Add(TMVCControllerDelegate.Create(TNotSoSimpleController, nil));
  lControllers.Add(TMVCControllerDelegate.Create(TTestServerController, nil));
  lMVCActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
  lConfig := TMVCConfig.Create;
  lConfig.Value[TMVCConfigKey.PathPrefix] := '';
  lRouter := TMVCRouter.Create(lConfig, FMVCActionParamsCache);
  try
    lParams := TMVCRequestParamsTable.Create;
    try
      Assert.isFalse(lRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain', 'text/plain', FControllers,
        'text/plain', TMVCConstants.DEFAULT_CONTENT_CHARSET, '', lParams, ResponseContentType,
        ResponseContentEncoding));

      Assert.isTrue(lRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain', 'text/plain', FControllers,
        'text/plain', TMVCConstants.DEFAULT_CONTENT_CHARSET, '/api', lParams, ResponseContentType,
        ResponseContentEncoding));
      Assert.areEqual(0, lParams.Count);
      Assert.areEqual('TSimpleController', lRouter.ControllerClazz.ClassName);
      Assert.areEqual('Orders', lRouter.MethodToCall.Name);
      Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentEncoding);
    finally
      lParams.Free;
    end;

  finally
    lRouter.Free;
    lControllers.Free;
    lMVCActionParamsCache.Free;
    lConfig.Free;
  end;
end;

procedure TTestRouting.TestPathWithParameters;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual(1, Params.Count);
    Assert.areEqual('789', Params['ordernumber']);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;

  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/orders/àèéìòù .-_\', httpGET, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual(1, Params.Count);
    Assert.areEqual('àèéìòù .-_\', Params['ordernumber']);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;

end;

procedure TTestRouting.TestProduceRoutings;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentCharset: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    // a GET request with a ACCEPT: application/json
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, '', 'application/json', FControllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, ResponseContentType,
      ResponseContentCharset));
    Assert.areEqual(0, Params.Count);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('OrdersProduceJSON', FRouter.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestProduceRoutingsWithExplicitCharset;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentCharset: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    // a GET request with a ACCEPT: application/json
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, '', 'application/json; charset=UTF-8', FControllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, '', Params, ResponseContentType,
      ResponseContentCharset));
    Assert.areEqual(0, Params.Count);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('OrdersProduceJSON', FRouter.MethodToCall.Name);
    Assert.areEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestReservedIPs;
var
  I: Integer;
begin
  // this test just tests the IP2Long implementation
  for I := low(RESERVED_IPv4) to high(RESERVED_IPv4) do
  begin
    Assert.areEqual(IPv4ToUInt32(RESERVED_IPv4[I][1]), IP2Long(RESERVED_IPv4[I][1]));
    Assert.areEqual(IPv4ToUInt32(RESERVED_IPv4[I][2]), IP2Long(RESERVED_IPv4[I][2]));
  end;
end;

{ Use this test only if you want to test the speed of the router }
// procedure TTestRouting.TestRoutingSpeed;
// var
// Params: TMVCRequestParamsTable;
// ResponseContentType: string;
// ResponseContentEncoding: string;
// I: Integer;
// lSW: TStopwatch;
// begin
// // procedure TestTypedActionBooleans(bool1, bool2, bool3, bool4: Boolean);
// Params := TMVCRequestParamsTable.Create;
// try
// lSW := TStopWatch.Create;
// lSW.Start;
// for I := 1 to 1000 do
// begin
// Params.Clear;
// Router.ExecuteRouting(
// '/typed/booleans/true/false/true/false',
// httpGET,
// TMVCMediaType.APPLICATION_JSON,
// TMVCMediaType.APPLICATION_JSON,
// Controllers,
// TMVCMediaType.APPLICATION_JSON,
// TMVCMediaType.APPLICATION_JSON,
// Params,
// ResponseContentType, ResponseContentEncoding);
// end;
// Assert.isTrue(false, lSW.ElapsedMilliseconds.ToString);
// finally
// Params.Free;
// end;
//
// end;

// procedure TTestMappers.TestSerializeUsingFields;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TObject;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(4, lJObj.Count); // 3 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FFirstName'));
// CheckNotNull(lJObj.Get('FLastName'));
// CheckNotNull(lJObj.Get('FAge'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyObjectWithLogic,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingFieldsComplexObject;
// var
// lJObj: TJSONObject;
// lObj2: TObject;
// lObj: TMyComplexObject;
// begin
// lObj := GetMyComplexObject;
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 4 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FProp1'));
// CheckNotNull(lJObj.Get('FChildObjectList'));
// CheckNotNull(lJObj.Get('FChildObject'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyComplexObject,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingFieldsComplexObject2;
// var
// lJObj: TJSONObject;
// lObj2: TObject;
// lObj: TMyComplexObject;
// begin
// lObj := GetMyComplexObjectWithNotInitializedChilds;
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 4 properties + $dmvc.classname
// CheckNotNull(lJObj.Get('FProp1'));
// CheckNotNull(lJObj.Get('FChildObjectList'));
// CheckNotNull(lJObj.Get('FChildObject'));
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
// try
// CheckIs(lObj2, TMyComplexObject,
// 'wrong classtype for deserialized object');
// Assert.isTrue(lObj.Equals(lObj2),
// 'restored object is different from the original');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.
// TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TMyObjectWithLogic;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
// try
// lJObj.RemovePair('FFirstName').Free;
// lObj2 := Mapper.JSONObjectFieldsToObject(lJObj) as TMyObjectWithLogic;
// try
// Assert.areEqual('', lObj2.FirstName);
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;
//
// procedure TTestMappers.TestSerializeUsingProperties;
// var
// lObj: TMyObjectWithLogic;
// lJObj: TJSONObject;
// lObj2: TMyObjectWithLogic;
// begin
// lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
// try
// lJObj := Mapper.ObjectToJSONObject(lObj, []);
// try
// Assert.areEqual(5, lJObj.Count); // 5 properties
// CheckNotNull(lJObj.Get('FirstName'));
// CheckNotNull(lJObj.Get('LastName'));
// CheckNotNull(lJObj.Get('Age'));
// CheckNotNull(lJObj.Get('FullName'));
// CheckNotNull(lJObj.Get('IsAdult'));
// lObj2 := Mapper.JSONObjectToObject<TMyObjectWithLogic>(lJObj);
// try
// Assert.isTrue(lObj2.Equals(lObj),
// 'deserialized object is not equals to the original object');
// finally
// lObj2.Free;
// end;
// finally
// lJObj.Free;
// end;
// finally
// lObj.Free;
// end;
// end;

procedure TTestRouting.TestWithMethodTypes;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPOST, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('UpdateOrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPUT, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('UpdateOrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPATCH, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('PatchOrder', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpDELETE, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.isNull(FRouter.MethodToCall);
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpHEAD, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding),
      'Resolved as HEAD');
    Assert.isNull(FRouter.MethodToCall, 'Resolved as HEAD');
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain', 'text/plain', FControllers,
      'text/plain', TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding),
      'Resolved as OPTIONS');
    Assert.isNull(FRouter.MethodToCall, 'Resolved as OPTIONS');
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestWithNoParameters;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual(0, Params.Count);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('Index', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestWithNoPath;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('', httpGET, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, '', Params, ResponseContentType, ResponseContentEncoding));
    Assert.areEqual(0, Params.Count);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('Index', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

{ TTestJWT }

procedure TTestJWT.SetUp;
begin
  inherited;
  FJWT := TJWT.Create(JWT_SECRET_KEY_TEST);
end;

procedure TTestJWT.TearDown;
begin
  FJWT.Free;
  inherited;
end;

procedure TTestJWT.TestCreateAndValidateToken;
var
  lToken: string;
  lError: string;
begin
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.JWT_ID := TGUID.NewGuid.ToString;
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';
  FJWT.Claims.ExpirationTime := Tomorrow;
  FJWT.Claims.IssuedAt := Yesterday;
  FJWT.Claims.NotBefore := Yesterday;
  lToken := FJWT.GetToken;
  // TFile.WriteAllText('jwt_token.dat', lToken);

  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Generated token is not valid');
end;

procedure TTestJWT.TestDefaults;
begin
  Assert.areEqual('HS512', FJWT.HMACAlgorithm, 'Default algorithm should be HS512');
  Assert.areEqual(300, FJWT.LeewaySeconds, 'Default leeway should be 5 minutes');
  if FJWT.RegClaimsToChecks * [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
    TJWTCheckableClaim.IssuedAt] <> [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
    TJWTCheckableClaim.IssuedAt] then
    Assert.Fail('Default RegClaimsToCheck not correct');
end;

procedure TTestJWT.TestExpirationTime;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.ExpirationTime];
  FJWT.Claims.ExpirationTime := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Expired token is considered valid');

  FJWT.Claims.ExpirationTime := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Now - (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Expired token is considered valid');
end;

procedure TTestJWT.TestHMAC;
var
  lAlg: string;
  lValue: string;
  I: Integer;
begin
  for I := low(HMAC_ALG_AND_RESULTS) to high(HMAC_ALG_AND_RESULTS) do
  begin
    lAlg := HMAC_ALG_AND_RESULTS[I][0];
    lValue := HMAC_ALG_AND_RESULTS[I][1];
    Assert.areEqual(lValue, BytesToHex(HMAC(lAlg, 'Daniele Teti', 'daniele')), 'HMAC ' + lAlg + ' fails');
  end;
end;

procedure TTestJWT.TestIssuedAt;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.IssuedAt];
  FJWT.Claims.IssuedAt := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');

  FJWT.Claims.IssuedAt := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestLoadToken;
var
  lToken: string;
  lJWT: TJWT;
  lError: string;
  lExp: TDateTime;
begin
  lExp := Now + OneHour * 2;
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.Audience := 'DelphiDevelopers';
  FJWT.Claims.IssuedAt := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.ExpirationTime := lExp;
  FJWT.Claims.NotBefore := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.JWT_ID := '123456';
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';

  lToken := FJWT.GetToken;
  // TFile.WriteAllText('jwt_token_full.dat', lToken);

  lJWT := TJWT.Create(JWT_SECRET_KEY_TEST);
  try
    lJWT.LoadToken(lToken, lError);
    Assert.areEqual('bit Time Professionals', lJWT.Claims.Issuer);
    Assert.areEqual('DelphiMVCFramework', lJWT.Claims.Subject);
    Assert.areEqual('DelphiDevelopers', lJWT.Claims.Audience);
    Assert.areEqual('123456', lJWT.Claims.JWT_ID);
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), lJWT.Claims.IssuedAt);
    Assert.areEqual(Roundto(lExp, 4), Roundto(lJWT.Claims.ExpirationTime, 4));
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), lJWT.Claims.NotBefore);
    Assert.areEqual('dteti', lJWT.CustomClaims['username']);
    Assert.areEqual('admin', lJWT.CustomClaims['userrole']);
  finally
    lJWT.Free;
  end;

end;

procedure TTestJWT.TestNotBefore;
var
  lToken: string;
  lError: string;
begin
  FJWT.RegClaimsToChecks := [TJWTCheckableClaim.NotBefore];
  FJWT.Claims.NotBefore := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid (near midnight is ok... fix this test) ');

  FJWT.Claims.NotBefore := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError), 'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError), 'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestStorage;
begin
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.Audience := 'DelphiDevelopers';
  FJWT.Claims.IssuedAt := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.ExpirationTime := FJWT.Claims.IssuedAt + OneHour * 2;
  FJWT.Claims.NotBefore := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.JWT_ID := '123456';
  FJWT.CustomClaims['username'] := 'dteti';
  FJWT.CustomClaims['userrole'] := 'admin';

  Assert.areEqual('bit Time Professionals', FJWT.Claims.Issuer);
  Assert.areEqual('DelphiMVCFramework', FJWT.Claims.Subject);
  Assert.areEqual('DelphiDevelopers', FJWT.Claims.Audience);
  Assert.areEqual('123456', FJWT.Claims.JWT_ID);
  Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), FJWT.Claims.IssuedAt);
  Assert.areEqual(Roundto(FJWT.Claims.IssuedAt + OneHour * 2, 4), Roundto(FJWT.Claims.ExpirationTime, 4));
  Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), FJWT.Claims.NotBefore);

  Assert.areEqual('dteti', FJWT.CustomClaims['username']);
  Assert.areEqual('admin', FJWT.CustomClaims['userrole']);

end;

{ TMVCSerUnSerTestCase }

function TMVCSerUnSerTestCase.GetObjectsList: TObjectList<TMyObject>;
var
  I: Integer;
begin
  Result := TObjectList<TMyObject>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObject);
    Result.Last.PropInteger := I;
  end;
end;

function TMVCSerUnSerTestCase.GetObjectsWithTValueList: TObjectList<TMyObjectWithTValue>;
var
  I: Integer;
begin
  Result := TObjectList<TMyObjectWithTValue>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObjectWithTValue);
  end;
end;

function TMVCSerUnSerTestCase.GetObjectsWithStreamsList: TObjectList<TMyStreamObject>;
var
  I: Integer;
begin
  Result := TObjectList<TMyStreamObject>.Create(true);
  for I := 1 to 10 do
  begin
    Result.Add(GetMyObjectWithStream);
  end;
end;

procedure TMVCSerUnSerTestCase.SetSerializer(const ASerializer: IMVCSerializer);
begin
  FSerializer := ASerializer;
end;

procedure TMVCSerUnSerTestCase.SetUp;
begin
  raise Exception.Create('You should override this to use a specific MVCSerUnSer');
end;

{ TTestMultiMap }

procedure TTestMultiMap.SetUp;
begin
  inherited;

end;

procedure TTestMultiMap.TearDown;
begin
  inherited;

end;

procedure TTestMultiMap.TestInterfaceMultiMapAdd;
var
  lMultiMap: IMVCInterfaceMultiMap<IMyInterface>;
begin
  lMultiMap := TMVCInterfaceMultiMap<IMyInterface>.Create;
  Assert.AreEqual<Integer>(0, Length(lMultiMap.Keys));
  lMultiMap.Clear;
  Assert.isFalse(lMultiMap.Contains('key1'));
  lMultiMap.Add('key1', TMyIntfObject.Create(1, 'value1'));
  Assert.isTrue(lMultiMap.Contains('key1'));
  Assert.areEqual(1, lMultiMap.GetItems('key1').Count);
  lMultiMap.Add('key1', TMyIntfObject.Create(2, 'value2'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual('value1', lMultiMap.GetItems('key1')[0].GetDescription);
  Assert.areEqual('value2', lMultiMap.GetItems('key1')[1].GetDescription);
  lMultiMap.Add('key2', TMyIntfObject.Create(1, 'value3'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual(1, lMultiMap.GetItems('key2').Count);
end;

procedure TTestMultiMap.TestInterfaceMultiMapRemove;
var
  lMultiMap: IMVCInterfaceMultiMap<IMyInterface>;
begin
  lMultiMap := TMVCInterfaceMultiMap<IMyInterface>.Create;
  lMultiMap.Remove('not valid');
  lMultiMap.Add('key1', TMyIntfObject.Create(1, 'value1'));
  lMultiMap.Add('key1', TMyIntfObject.Create(2, 'value2'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.isTrue(lMultiMap.Contains('key1'));
  lMultiMap.Remove('key1');
  Assert.isFalse(lMultiMap.Contains('key1'));
end;

procedure TTestMultiMap.TestObjectMultiMapAdd;
var
  lMultiMap: IMVCObjectMultiMap<TMyClass>;
begin
  lMultiMap := TMVCObjectMultiMap<TMyClass>.Create;
  Assert.AreEqual<Integer>(0, Length(lMultiMap.Keys));
  lMultiMap.Clear;
  Assert.isFalse(lMultiMap.Contains('key1'));
  lMultiMap.Add('key1', TMyClass.Create(1, 'value1'));
  Assert.isTrue(lMultiMap.Contains('key1'));
  Assert.areEqual(1, lMultiMap.GetItems('key1').Count);
  lMultiMap.Add('key1', TMyClass.Create(2, 'value2'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual('value1', lMultiMap.GetItems('key1')[0].Description);
  Assert.areEqual('value2', lMultiMap.GetItems('key1')[1].Description);
  lMultiMap.Add('key2', TMyClass.Create(1, 'value3'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.areEqual(1, lMultiMap.GetItems('key2').Count);
end;

procedure TTestMultiMap.TestObjectMultiMapRemove;
var
  lMultiMap: IMVCObjectMultiMap<TMyClass>;
begin
  lMultiMap := TMVCObjectMultiMap<TMyClass>.Create;
  lMultiMap.Remove('not valid');
  lMultiMap.Add('key1', TMyClass.Create(1, 'value1'));
  lMultiMap.Add('key1', TMyClass.Create(2, 'value2'));
  Assert.areEqual(2, lMultiMap.GetItems('key1').Count);
  Assert.isTrue(lMultiMap.Contains('key1'));
  lMultiMap.Remove('key1');
  Assert.isFalse(lMultiMap.Contains('key1'));
end;

{ TTestNameCase }

procedure TTestNameCase.SetupFixture;
begin
  fOrigDATA[1] := 'one_two_3or4';
  fOrigDATA[2] := 'ONE_TWO_THREE';
  fOrigDATA[3] := 'JustOne';
  fOrigDATA[4] := '_with__underscores_';
  fOrigDATA[5] := 'oneTwo___three04';

  fOutDATA[1][ncAsIs] := fOrigDATA[1];
  fOutDATA[2][ncAsIs] := fOrigDATA[2];
  fOutDATA[3][ncAsIs] := fOrigDATA[3];
  fOutDATA[4][ncAsIs] := fOrigDATA[4];
  fOutDATA[5][ncAsIs] := fOrigDATA[5];

  fOutDATA[1][ncUpperCase] := 'ONE_TWO_3OR4';
  fOutDATA[2][ncUpperCase] := 'ONE_TWO_THREE';
  fOutDATA[3][ncUpperCase] := 'JUSTONE';
  fOutDATA[4][ncUpperCase] := '_WITH__UNDERSCORES_';
  fOutDATA[5][ncUpperCase] := 'ONETWO___THREE04';

  fOutDATA[1][ncLowerCase] := 'one_two_3or4';
  fOutDATA[2][ncLowerCase] := 'one_two_three';
  fOutDATA[3][ncLowerCase] := 'justone';
  fOutDATA[4][ncLowerCase] := '_with__underscores_';
  fOutDATA[5][ncLowerCase] := 'onetwo___three04';

  fOutDATA[1][ncCamelCase] := 'oneTwo3Or4';
  fOutDATA[2][ncCamelCase] := 'oneTwoThree';
  fOutDATA[3][ncCamelCase] := 'justOne';
  fOutDATA[4][ncCamelCase] := 'WithUnderscores';
  fOutDATA[5][ncCamelCase] := 'oneTwoThree04';

  fOutDATA[1][ncPascalCase] := 'OneTwo3Or4';
  fOutDATA[2][ncPascalCase] := 'OneTwoThree';
  fOutDATA[3][ncPascalCase] := 'JustOne';
  fOutDATA[4][ncPascalCase] := 'WithUnderscores';
  fOutDATA[5][ncPascalCase] := 'OneTwoThree04';

  fOutDATA[1][ncSnakeCase] := 'one_two_3_or_4';
  fOutDATA[2][ncSnakeCase] := 'one_two_three';
  fOutDATA[3][ncSnakeCase] := 'just_one';
  fOutDATA[4][ncSnakeCase] := '_with_underscores_';
  fOutDATA[5][ncSnakeCase] := 'one_two_three_04';

end;

procedure TTestNameCase.TestNameCase;
var
  I: Integer;
  lNameCaseIdx: TMVCNameCase;
  lOrig: string;
  lOutData: string;
  lActualOutData: string;
begin
  for lNameCaseIdx := ncAsIs to ncSnakeCase do
  begin
    for I := 1 to 5 do
    begin
      lOrig := fOrigDATA[I];
      lOutData := fOutDATA[I][lNameCaseIdx];
      lActualOutData := TMVCSerializerHelper.ApplyNameCase(lNameCaseIdx, lOrig);
      Assert.areEqual(lOutData, lActualOutData, False, lOrig + ' for ' + GetEnumName(TypeInfo(TMVCNameCase),
        Ord(lNameCaseIdx)));
    end;
  end;
end;

procedure TTestNameCase.TestSnakeCase(const AValue1, AValue2: string);
begin
  Assert.areEqual(AValue2, SnakeCase(AValue1));
end;

{ TTestCryptUtils }

procedure TTestCryptUtils.SetupFixture;
begin
  MVCCryptInit;
end;

procedure TTestCryptUtils.TestPBKDF2_SHA1;
var
  P: TBytes;
  S: TBytes;
  K: TBytes;
begin
  // https://www.freecodeformat.com/pbkdf2.php
  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($78, $57, $8E, $5A, $5D, $63, $CB, $06);

  K := PBKDF2(P, S, 2048, 24);
  Assert.areEqual('BFDE6BE94DF7E11DD409BCE20A0255EC327CB936FFE93643', BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74);

  K := PBKDF2(P, S, 1, 20);
  Assert.areEqual('0C60C80F961F0E71F3A9B524AF6012062FE037A6', BytesToHexString(K));
  K := PBKDF2(P, S, 2, 20);
  Assert.areEqual('EA6C014DC72D6F8CCD1ED92ACE1D41F0D8DE8957', BytesToHexString(K));
  K := PBKDF2(P, S, 4096, 20);
  Assert.areEqual('4B007901B765489ABEAD49D926F721D065A429C1', BytesToHexString(K));
  // K := PBKDF2(P, S, 16777216, 20);
  // Assert.AreEqual('EEFE3D61CD4DA4E4E9945B3D6BA2158C2634E984', BytesToHexString(K));

  P := TBytes.Create($70, $61, $73, $73, $77, $6F, $72, $64, $50, $41, $53, $53, $57, $4F, $52, $44, $70, $61, $73, $73,
    $77, $6F, $72, $64);
  S := TBytes.Create($73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74,
    $53, $41, $4C, $54, $73, $61, $6C, $74, $53, $41, $4C, $54, $73, $61, $6C, $74);

  K := PBKDF2(P, S, 4096, 25);
  Assert.areEqual('3D2EEC4FE41C849B80C8D83662C0E44A8B291A964CF2F07038', BytesToHexString(K));
end;

procedure TTestCryptUtils.TestPBKDF2_SHA256;
var
  lPassword: string;
  lSalt: string;
  lOut: TBytes;
begin
  lPassword := 'daniele.teti';
  lSalt := 'thisissomesalt';
  lOut := PBKDF2(TEncoding.ASCII.GetBytes(lPassword), TEncoding.ASCII.GetBytes(lSalt), 50, 512 div 8);
  // https://www.freecodeformat.com/pbkdf2.php
  Assert.areEqual
    ('caca227458fe66cf8c19f2d943190feca54fd403b966189d6c7befc3bc856e2d5218d825e91912058fdbdb488dbe4ae3e7be5f59318b03d805857440017ee440',
    BytesToHexString(lOut));
end;

{ TTestUTC }

procedure TTestUTC.TestStringToDateTime_Local;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // Local time
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12');
  Assert.areEqual<TDateTime>(EncodeDateTime(2020, 11, 4, 12, 12, 12, 0), lDate);

  // UTC with no time zone (in a DST period)
  lDate := ISOTimeStampToDateTime('2020-08-15T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 8, 15, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in DST period)');


  // UTC with no time zone (in no DST period)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 11, 04, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in no DST period)');
end;

procedure TTestUTC.TestStringToDateTime_in_DST_period;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // UTC with no time zone (in a DST period)
  lDate := ISOTimeStampToDateTime('2020-08-15T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 8, 15, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in DST period)');
end;

procedure TTestUTC.TestStringToDateTime_in_no_DST_period;
var
  lDate, lDateToCompare: TDateTime;
  s1,s2: string;
begin
  // UTC with no time zone (in no DST period)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12Z');
  lDateToCompare := TTimeZone.Local.ToLocalTime(EncodeDateTime(2020, 11, 04, 12, 12, 12, 0));
  s1 := DateTimeToStr(lDate);
  s2 := DateTimeToStr(lDateToCompare);
  Assert.areEqual(s1,s2, 'UTC with no time zone (in no DST period)');
end;


procedure TTestUTC.TestStringToDateTime_Mumbai;
var
  lDate: TDateTime;
begin
  // UTC "+05:30" for Mumbai (UTC+05:30)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12+05:30');
  Assert.areEqual(DateTimeToStr(EncodeDateTime(2020, 11, 4, 7, 42, 12, 0)), DateTimeToStr(lDate));
end;

procedure TTestUTC.TestStringToDateTime_NewYork;
var
  lDate: TDateTime;
begin
  // UTC "−05:00" for New York on standard time (UTC-05:00)
  lDate := ISOTimeStampToDateTime('2020-11-04T12:12:12-05:00');
  Assert.areEqual(DateTimeToStr(EncodeDateTime(2020, 11, 4, 18, 12, 12, 0)), DateTimeToStr(lDate));
end;

{ TTestLRUCache }

procedure TTestLRUCache.TestPutGet;
var
  lCache: TMVCLRUCache<TMyObject>;
  I: Integer;
  lMyObj: TMyObject;
  lItemIndex: UInt64;
begin
  lCache := TMVCLRUCache<TMyObject>.Create(5);
  try
    lCache.Lock;
    try
      for I := 1 to 100 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;

      for I := 1 to 100 do
      begin
        lMyObj := TMyObject.Create;
        lMyObj.PropString := I.ToString;
        lCache.Put(I.ToString, lMyObj);
        Assert.isTrue(lCache.Contains(I.ToString, lItemIndex));
        Assert.isTrue(lCache.TryGet(I.ToString, lMyObj));
        Assert.AreEqual(I.ToString, lMyObj.PropString);
      end;
      Assert.areEqual(5, lCache.Size);
    finally
      lCache.UnLock;
    end;
    lCache.Lock;
    try
      Assert.isTrue(lCache.Contains('100', lItemIndex));
      Assert.isTrue(lCache.Contains('99', lItemIndex));
      Assert.isTrue(lCache.Contains('98', lItemIndex));
      Assert.isTrue(lCache.Contains('97', lItemIndex));
      Assert.isTrue(lCache.Contains('96', lItemIndex));
      for I := 95 downto -10 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;
      for I := 101 to 105 do
      begin
        Assert.isFalse(lCache.Contains(I.ToString, lItemIndex));
      end;
    finally
      lCache.UnLock;
    end;
  finally
    lCache.Free;
  end;
end;

procedure TTestLRUCache.TestPutGet_Check_No_AV;
var
  lCache: TMVCLRUCache<TMyObject>;
  lMyObj: TMyObject;
  lProducer1, lProducer2, lProducer3, lProducer4: ITask;
  lProcProducer, lProcConsumer: TProc;
  lConsumer1, lConsumer2, lConsumer3, lConsumer4: ITask;
begin
  lCache := TMVCLRUCache<TMyObject>.Create(10);
  try
    lProcProducer := procedure
      var
        T, J: Integer;
        lKey: string;
      begin
        for T := 1 to 50 do
        begin
          Sleep(T * 5);
          lCache.Lock;
          try
            for J := 1 to 100 do
            begin
              lMyObj := TMyObject.Create;
              lKey := T.ToString + '|' + J.ToString;
              lMyObj.PropString := lKey;
              lCache.Put(lKey, lMyObj);
            end;
          finally
            lCache.UnLock;
          end;
        end;
      end;

    lProcConsumer := procedure
      var
        T, J: Integer;
        lKey: string;
      begin
        for T := 1 to 20 do
        begin
          Sleep(T * 10);
          lCache.Lock;
          try
            for J := T to T + 100 do
            begin
              lKey := T.ToString + '|' + J.ToString;
              lCache.TryGet(lKey, lMyObj);
            end;
          finally
            lCache.UnLock;
          end;
        end;
      end;

    lProducer1 := TTask.Run(lProcProducer).Start; // Thread
    lProducer2 := TTask.Run(lProcProducer).Start; // Thread
    lProducer3 := TTask.Run(lProcProducer).Start; // Thread
    lProducer4 := TTask.Run(lProcProducer).Start; // Thread

    lConsumer1 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer2 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer3 := TTask.Run(lProcConsumer).Start; // Thread
    lConsumer4 := TTask.Run(lProcConsumer).Start; // Thread
    TTask.WaitForAll([lProducer1, lProducer2, lProducer3, lProducer4, lConsumer1, lConsumer2, lConsumer3, lConsumer4]);
    Assert.Pass('No Exception raised');
  finally
    lCache.Free;
  end;
end;

{ TTestDotEnv }

function Are2FilesEqual(const File1, File2: TFileName): Boolean;
var
  ms1, ms2: TMemoryStream;
begin
  Result := False;
  ms1 := TMemoryStream.Create;
  try
    ms1.LoadFromFile(File1);
    ms2 := TMemoryStream.Create;
    try
      ms2.LoadFromFile(File2);
      if ms1.Size = ms2.Size then
      begin
        Result := CompareMem(ms1.Memory, ms2.memory, ms1.Size);
      end;
    finally
      ms2.Free;
    end;
  finally
    ms1.Free;
  end
end;

procedure TTestDotEnv.TestWithDevAndTestProfile;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('dev').UseProfile('test').Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev-and-test.test.txt')),
    'Files are different');
end;

procedure TTestDotEnv.TestWithDevProfile;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.UseProfile('dev').Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-profile-dev.test.txt')),
    'Files are different');
end;

procedure TTestDotEnv.TestWithoutProfiles;
var
  lDotEnv: IMVCDotEnv;
begin
  lDotEnv := NewDotEnv.Build('..\dotEnv');
  lDotEnv.SaveToFile(TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.test.txt'));
  Assert.IsTrue(Are2FilesEqual(
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.correct.txt'),
    TPath.Combine(AppPath, '..\dotEnv\dotEnvDump-noprofile.test.txt')),
    'Files are different');
end;

{ TTestDotEnvParser }

procedure TTestDotEnvParser.TestInLineComments;
const
  DOTENVCODE =
    '#comment1' + sLineBreak +
    '#comment2' + sLineBreak +
    'key1= "value1" #inline comment' + sLineBreak +
    ';comment3' + sLineBreak +
    'key2 = ''value2'' #inline comment' + sLineBreak +
    ';comment' + sLineBreak +
    'key3 = value3 #inline comment' + sLineBreak +
    'key4 = " value4 " #inline comment' + sLineBreak +
    ';commentX';

begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('value3', lDict['key3']);
      Assert.AreEqual(' value4 ', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestKeyValue;
const
  DOTENVCODE = 'key1=value1' + sLineBreak + 'key2 = value2 with another value' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2 with another value', lDict['key2']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestKeyValueWithQuotedValues;
const
  DOTENVCODE =
    'key1= "value1"' + sLineBreak +
    'key2 = ''value2''' + sLineBreak +
    'key3 = "uno''due''"' + sLineBreak +
    'key4 = ''uno"due"''' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('uno''due''', lDict['key3']);
      Assert.AreEqual('uno"due"', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestValueWithMultiline;
const
  DOTENVCODE =
    'key1= "value1' + sLineBreak +
    'value2' + sLineBreak +
    'value3" # comment' + sLineBreak +
    'key2 = value2' + sLineBreak;
begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1' + slinebreak + 'value2' + sLineBreak + 'value3', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

procedure TTestDotEnvParser.TestVarPlaceHolders;
const
  DOTENVCODE =
    '#comment1' + sLineBreak +
    '#comment2' + sLineBreak +
    'key1= "value1"' + sLineBreak +
    ';comment3' + sLineBreak +
    'key2 = ''value2''' + sLineBreak +
    ';comment' + sLineBreak +
    'key3 = |${key1}|${key2}|' + sLineBreak +
    'key4 = value4' + sLineBreak +
    ';commentX';

begin
  var lParser := TMVCDotEnvParser.Create;
  try
    var lDict := TMVCDotEnvDictionary.Create();
    try
      lParser.Parse(lDict, DOTENVCODE);
      Assert.AreEqual('value1', lDict['key1']);
      Assert.AreEqual('value2', lDict['key2']);
      Assert.AreEqual('|${key1}|${key2}|', lDict['key3']);
      Assert.AreEqual('value4', lDict['key4']);
    finally
      lDict.Free;
    end;
  finally
    lParser.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestRouting);
// TDUnitX.RegisterTestFixture(TTestMappers);
TDUnitX.RegisterTestFixture(TTestJWT);
TDUnitX.RegisterTestFixture(TTestMultiMap);
TDUnitX.RegisterTestFixture(TTestNameCase);
TDUnitX.RegisterTestFixture(TTestCryptUtils);
TDUnitX.RegisterTestFixture(TTestLRUCache);
TDUnitX.RegisterTestFixture(TTestDotEnv);
TDUnitX.RegisterTestFixture(TTestDotEnvParser);

finalization

end.
