﻿// ***************************************************************************
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

unit FrameworkTestsU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  BOs,
  MVCFramework, Data.DB, System.SysUtils, MVCFramework.JWT,
  MVCFramework.Serializer.Intf, MVCFramework.Serializer.Defaults,
  MVCFramework.MultiMap, MVCFramework.Commons;

type

  [TestFixture]
  TTestMappers = class(TObject)
  protected
    [Test]
    procedure SameFishesDataSet(ds, ds2: TDataSet);

  public
    // procedure TestObjectToJSONObject;
    // procedure TestObjectListToJSONArray;
    // procedure TestObjectToJSONObject_Generics;
    // procedure TestWrappedListToJSONArray;
    // procedure TestJSONObjectToObjectAndBack;
    // procedure TestLoadJSONObjectToObjectAndBack;
    // procedure TestSerializeUsingProperties;
    // procedure TestSerializeUsingFields;
    // procedure TestSerializeUsingFieldsComplexObject;
    // procedure TestSerializeUsingFieldsComplexObject2;
    // procedure TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
    // procedure TestComplexObjectToJSONObjectAndBack;
    // procedure TestComplexObjectToJSONObjectAndBackWithNilReference;
    // procedure TestDataSetToJSONObject;
    // procedure TestDataSetToJSONObjectWithNulls;
    // procedure TestDataSetToJSONObjectFieldPolicyLowerCase;
    // procedure TestDataSetToJSONObjectFieldPolicyUpperCase;
    // procedure TestDataSetToJSONObjectFieldPolicyAsIsCase;
    // procedure TestDataSetToJSONArray;
    // procedure TestObjectToJSONObjectAndBackWithStringStreamUTF16;
    // procedure TestObjectToJSONObjectAndBackWithStringStreamUTF8;
    // procedure TestObjectToJSONObjectAndBackWithStream;
    // procedure TestJSONArrayToObjectListNoGenerics;
    // procedure TestJSONArrayToObjectListNoGenericsWrappedList;
    // procedure TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
    // procedure TestJSONObjectToObjectWithNullInJSONString;
    // procedure TestJSONObjectStringToObject;
    // procedure TestJSONObjectStringToObjectWithWrongJSON;
  end;

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

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses System.DateUtils, System.Math,
  TestControllersU, DBClient,
  Web.HTTPApp, Soap.EncdDecd,
  IdHashMessageDigest, idHash,
  MVCFramework.Serializer.Commons,
  MVCFramework.HMAC, System.Diagnostics,

  {$IF CompilerVersion < 27}

  Data.DBXJSON,

  {$ELSE}

  System.JSON,

  {$ENDIF}

  TestServerControllerU, System.Classes,
  MVCFramework.DuckTyping, System.IOUtils, MVCFramework.SystemJSONUtils,
  IdGlobal;

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

procedure TTestMappers.SameFishesDataSet(ds, ds2: TDataSet);
begin
  Assert.areEqual(ds.FieldByName('Species No').AsInteger,
    ds2.FieldByName('Species No').AsInteger);
  Assert.areEqual(ds.FieldByName('Category').AsString, ds2.FieldByName('Category')
    .AsString);
  Assert.areEqual(ds.FieldByName('Common_Name').AsString,
    ds2.FieldByName('Common_Name').AsString);
  Assert.areEqual(ds.FieldByName('Species Name').AsString,
    ds2.FieldByName('Species Name').AsString);
  Assert.areEqual(ds.FieldByName('Length (cm)').AsString,
    ds2.FieldByName('Length (cm)').AsString);
  Assert.areEqual(ds.FieldByName('Length_In').AsInteger,
    ds2.FieldByName('Length_In').AsInteger);
  Assert.areEqual(ds.FieldByName('Notes').AsString, ds2.FieldByName('Notes')
    .AsString);
  Assert.areEqual(ds.FieldByName('Graphic').AsString, ds2.FieldByName('Graphic')
    .AsString);
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

// procedure TTestRouting.TestClassNameMethodNameRouting;
// var
// Params: TMVCRequestParamsTable;
// ResponseContentType: string;
// ResponseContentEncoding: string;
// begin
// Params := TMVCRequestParamsTable.Create;
// try
// Assert.isTrue(Router.ExecuteRouting('/TNotSoSimpleController/Method1', httpGET, 'text/plain', Controllers,
// Params, ResponseContentType, ResponseContentEncoding));
// Assert.areEqual(0, Params.Count);
// Assert.areEqual('TSimpleController', Router.MVCControllerClass.ClassName);
// Assert.areEqual('Index', Router.MethodToCall.Name);
// finally
// Params.Free;
// end;
// end;

// procedure TTestMappers.TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
// var
// Obj: TMyStreamObject;
// JSONObj: TJSONObject;
// DesObj: TMyStreamObject;
// begin
// // ARRANGE
// Obj := TMyStreamObject.Create;
// try
// Obj.PropStream := nil;
// Obj.Prop8Stream := nil;
// // ACT
// JSONObj := TSystemJSON.ObjectToJSONObject(Obj);
// try
// GetDefaultSerializer.de
// DesObj := TSystemJSON.JSONObjectToObject<TMyStreamObject>(JSONObj);
// try
// // ASSERT
// Assert.isTrue(TStringStream(DesObj.PropStream).DataString.IsEmpty);
// Assert.isTrue(TStringStream(DesObj.Prop8Stream).DataString.IsEmpty);
// finally
// DesObj.Free;
// end;
// finally
// JSONObj.Free;
// end;
// finally
// Obj.Free;
// end;
// end;

// procedure TTestMappers.TestComplexObjectToJSONObjectAndBack;
// var
// Obj: TMyComplexObject;
// JObj: TJSONObject;
// Obj2: TMyComplexObject;
// begin
// Obj := GetMyComplexObject;
// try
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyComplexObject>(JObj);
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
// procedure TTestMappers.TestComplexObjectToJSONObjectAndBackWithNilReference;
// var
// Obj: TMyComplexObject;
// JObj: TJSONObject;
// Obj2: TMyComplexObject;
// begin
// Obj := GetMyComplexObject;
// try
// Obj.ChildObject.Free;
// Obj.ChildObject := nil;
// JObj := Mapper.ObjectToJSONObject(Obj);
// try
// Obj2 := Mapper.JSONObjectToObject<TMyComplexObject>(JObj);
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

procedure TTestRouting.TestComplexRoutings;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/path1/1', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST,
      'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.areEqual('TestMultiplePaths', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter',
      httpPOST, 'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.isNull(FRouter.MethodToCall);
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

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
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, 'text/plain',
      'text/plain', FControllers, 'text/plain',
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentCharset));
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
      Assert.isFalse(lRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain',
        'text/plain', FControllers, 'text/plain',
        TMVCConstants.DEFAULT_CONTENT_CHARSET, lParams, ResponseContentType,
        ResponseContentEncoding));

      lConfig.Value[TMVCConfigKey.PathPrefix] := '/api';
      Assert.isTrue(lRouter.ExecuteRouting('/api/orders', httpGET, 'text/plain',
        'text/plain', FControllers, 'text/plain',
        TMVCConstants.DEFAULT_CONTENT_CHARSET, lParams, ResponseContentType,
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
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual(1, Params.Count);
    Assert.areEqual('789', Params['ordernumber']);
    Assert.areEqual('TSimpleController', FRouter.ControllerClazz.ClassName);
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);
  finally
    Params.Free;
  end;

  Params := TMVCRequestParamsTable.Create;
  try
    Assert.isTrue(FRouter.ExecuteRouting('/orders/àèéìòù .-_\', httpGET,
      'text/plain', 'text/plain', FControllers, 'text/plain',
      TMVCMediaType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
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
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, '', 'application/json',
      FControllers, TMVCConstants.DEFAULT_CONTENT_TYPE,
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
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
    Assert.isTrue(FRouter.ExecuteRouting('/orders', httpGET, '',
      'application/json; charset=UTF-8', FControllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET,
      Params, ResponseContentType, ResponseContentCharset));
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
  for I := low(RESERVED_IPS) to high(RESERVED_IPS) do
  begin
    Assert.AreEqual(IPv4ToUInt32(RESERVED_IPS[I][1]), IP2Long(RESERVED_IPS[I][1]));
    Assert.AreEqual(IPv4ToUInt32(RESERVED_IPS[I][2]), IP2Long(RESERVED_IPS[I][2]));
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
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPOST, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('UpdateOrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPUT, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('UpdateOrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpPATCH, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('PatchOrder', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpDELETE, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.isNull(FRouter.MethodToCall);
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpHEAD, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as HEAD');
    Assert.isNull(FRouter.MethodToCall, 'Resolved as HEAD');
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isFalse(FRouter.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as OPTIONS');
    Assert.isNull(FRouter.MethodToCall, 'Resolved as OPTIONS');
    Assert.isFalse(Assigned(FRouter.ControllerClazz));

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.areEqual('OrderNumber', FRouter.MethodToCall.Name);

    Params.Clear;
    Assert.isTrue(FRouter.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
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
    Assert.isTrue(FRouter.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
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
    Assert.isTrue(FRouter.ExecuteRouting('', httpGET, 'text/plain', 'text/plain',
      FControllers, 'text/plain', TMVCMediaType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
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
  if FJWT.RegClaimsToChecks * [TJWTCheckableClaim.ExpirationTime,
    TJWTCheckableClaim.NotBefore, TJWTCheckableClaim.IssuedAt] <>
    [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.NotBefore,
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
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Yesterday;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Expired token is considered valid');

  FJWT.Claims.ExpirationTime := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Now - (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Expired token is considered valid');
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
    Assert.areEqual(lValue, BytesToHex(HMAC(lAlg, 'Daniele Teti', 'daniele')),
      'HMAC ' + lAlg + ' fails');
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
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid');

  FJWT.Claims.IssuedAt := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestLoadToken;
var
  lToken: string;
  lJWT: TJWT;
  lError: string;
begin
  FJWT.Claims.Issuer := 'bit Time Professionals';
  FJWT.Claims.Subject := 'DelphiMVCFramework';
  FJWT.Claims.Audience := 'DelphiDevelopers';
  FJWT.Claims.IssuedAt := EncodeDateTime(2011, 11, 17, 17, 30, 0, 0);
  FJWT.Claims.ExpirationTime := Now + OneHour * 2;
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
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
      lJWT.Claims.IssuedAt);
    Assert.areEqual(Roundto(lJWT.Claims.IssuedAt + OneHour * 2, 4),
      Roundto(lJWT.Claims.ExpirationTime, 4));
    Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
      lJWT.Claims.NotBefore);
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
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid (near midnight is ok... fix this test) ');

  FJWT.Claims.NotBefore := Now;
  lToken := FJWT.GetToken;
  Assert.isTrue(FJWT.LoadToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.isFalse(FJWT.LoadToken(lToken, lError),
    'Still-not-valid token is considered valid');
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
  Assert.areEqual(Roundto(FJWT.Claims.IssuedAt + OneHour * 2, 4),
    Roundto(FJWT.Claims.ExpirationTime, 4));
  Assert.areEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
    FJWT.Claims.NotBefore);

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

procedure TMVCSerUnSerTestCase.SetSerializer(
  const
  ASerializer: IMVCSerializer);
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
  Assert.areEqual(0, Length(lMultiMap.Keys));
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
  Assert.areEqual(0, Length(lMultiMap.Keys));
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

initialization

TDUnitX.RegisterTestFixture(TTestRouting);
// TDUnitX.RegisterTestFixture(TTestMappers);
TDUnitX.RegisterTestFixture(TTestJWT);
TDUnitX.RegisterTestFixture(TTestMultiMap);

finalization

end.
