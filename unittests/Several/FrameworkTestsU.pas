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
  MVCFramework, Data.DB, System.SysUtils, MVCFramework.JWT;

type
  [TestFixture]
  TTestMappers = class(TObject)
  protected
    procedure SameFishesDataSet(ds, ds2: TDataSet);

  published
    [Test]
	procedure TestObjectToJSONObject;
    [Test]
    procedure TestObjectListToJSONArray;
    [Test]
    procedure TestObjectToJSONObject_Generics;
    [Test]
    procedure TestWrappedListToJSONArray;
    [Test]
    procedure TestJSONObjectToObjectAndBack;
    [Test]
    procedure TestLoadJSONObjectToObjectAndBack;
    [Test]
    procedure TestSerializeUsingProperties;
    [Test]
    procedure TestSerializeUsingFields;
    [Test]
    procedure TestSerializeUsingFieldsComplexObject;
    [Test]
    procedure TestSerializeUsingFieldsComplexObject2;
    [Test]
    procedure TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
    [Test]
    procedure TestComplexObjectToJSONObjectAndBack;
    [Test]
    procedure TestComplexObjectToJSONObjectAndBackWithNilReference;
    [Test]
    procedure TestDataSetToJSONObject;
    [Test]
    procedure TestDataSetToJSONObjectWithNulls;
    [Test]
    procedure TestDataSetToJSONObjectFieldPolicyLowerCase;
    [Test]
    procedure TestDataSetToJSONObjectFieldPolicyUpperCase;
    [Test]
    procedure TestDataSetToJSONObjectFieldPolicyAsIsCase;
    [Test]
    procedure TestDataSetToJSONArray;
    [Test]
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF16;
    [Test]
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF8;
    [Test]
    procedure TestObjectToJSONObjectAndBackWithStream;
    [Test]
    procedure TestJSONArrayToObjectListNoGenerics;
    [Test]
    procedure TestJSONArrayToObjectListNoGenericsWrappedList;
    [Test]
    procedure TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
    [Test]
    procedure TestJSONObjectToObjectWithNullInJSONString;
    [Test]
    procedure TestJSONObjectStringToObject;
    [Test]
    procedure TestJSONObjectStringToObjectWithWrongJSON;
  end;

  [TestFixture]
  TTestRouting = class(TObject)
  private
    Router: TMVCRouter;
    Controllers: TObjectList<TMVCControllerRoutable>;

  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  published
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

    // objects mappers
  end;

  [TestFixture]
  TTestJWT = class(TObject)
  private
    FJWT: TJWT;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  published
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

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses System.DateUtils, System.Math, MVCFramework.Commons,
  TestControllersU, DBClient,
  Web.HTTPApp, Soap.EncdDecd,
  IdHashMessageDigest, idHash,
  ObjectsMappers,
  BOs,
  MVCFramework.HMAC,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  TestServerControllerU, System.Classes,
  MVCFramework.DuckTyping, System.IOUtils;

var
  JWT_SECRET_KEY_TEST: String = 'myk3y';
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
    result := idmd5.HashBytesAsHex(idmd5.HashStream(aStream));
  finally
    idmd5.Free;
  end;
end;

procedure TTestMappers.SameFishesDataSet(ds, ds2: TDataSet);
begin
  Assert.AreEqual(ds.FieldByName('Species No').AsInteger,
    ds2.FieldByName('Species No').AsInteger);
  Assert.AreEqual(ds.FieldByName('Category').AsString, ds2.FieldByName('Category')
    .AsString);
  Assert.AreEqual(ds.FieldByName('Common_Name').AsString,
    ds2.FieldByName('Common_Name').AsString);
  Assert.AreEqual(ds.FieldByName('Species Name').AsString,
    ds2.FieldByName('Species Name').AsString);
  Assert.AreEqual(ds.FieldByName('Length (cm)').AsString,
    ds2.FieldByName('Length (cm)').AsString);
  Assert.AreEqual(ds.FieldByName('Length_In').AsInteger,
    ds2.FieldByName('Length_In').AsInteger);
  Assert.AreEqual(ds.FieldByName('Notes').AsString, ds2.FieldByName('Notes')
    .AsString);
  Assert.AreEqual(ds.FieldByName('Graphic').AsString, ds2.FieldByName('Graphic')
    .AsString);
end;

procedure TTestRouting.SetUp;
begin
  Controllers := TObjectList<TMVCControllerRoutable>.Create;
  Controllers.Add(TMVCControllerRoutable.Create(TSimpleController, nil));
  Controllers.Add(TMVCControllerRoutable.Create(TNotSoSimpleController, nil));
  Controllers.Add(TMVCControllerRoutable.Create(TTestServerController, nil));
  Router := TMVCRouter.Create(nil);
end;

procedure TTestRouting.TearDown;
begin
  Router.Free;
  Controllers.Free;
end;

// procedure TTestRouting.TestClassNameMethodNameRouting;
// var
// Params: TMVCRequestParamsTable;
// ResponseContentType: string;
// ResponseContentEncoding: string;
// begin
// Params := TMVCRequestParamsTable.Create;
// try
// CheckTrue(Router.ExecuteRouting('/TNotSoSimpleController/Method1', httpGET, 'text/plain', Controllers,
// Params, ResponseContentType, ResponseContentEncoding));
// CheckEquals(0, Params.Count);
// CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
// CheckEquals('Index', Router.MethodToCall.Name);
// finally
// Params.Free;
// end;
// end;

procedure TTestMappers.TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
var
  Obj: TMyStreamObject;
  JSONObj: TJSONObject;
  DesObj: TMyStreamObject;
begin
  // ARRANGE
  Obj := TMyStreamObject.Create;
  try
    Obj.PropStream := nil;
    Obj.Prop8Stream := nil;
    // ACT
    JSONObj := Mapper.ObjectToJSONObject(Obj);
    try
      DesObj := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
      try
        // ASSERT
        Assert.IsTrue(TStringStream(DesObj.PropStream).DataString.IsEmpty);
        Assert.IsTrue(TStringStream(DesObj.Prop8Stream).DataString.IsEmpty);
      finally
        DesObj.Free;
      end;
    finally
      JSONObj.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestMappers.TestComplexObjectToJSONObjectAndBack;
var
  Obj: TMyComplexObject;
  JObj: TJSONObject;
  Obj2: TMyComplexObject;
begin
  Obj := GetMyComplexObject;
  try
    JObj := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyComplexObject>(JObj);
      try
        Assert.IsTrue(Obj.Equals(Obj2));
      finally
        Obj2.Free;
      end;
    finally
      JObj.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestMappers.TestComplexObjectToJSONObjectAndBackWithNilReference;
var
  Obj: TMyComplexObject;
  JObj: TJSONObject;
  Obj2: TMyComplexObject;
begin
  Obj := GetMyComplexObject;
  try
    Obj.ChildObject.Free;
    Obj.ChildObject := nil;
    JObj := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyComplexObject>(JObj);
      try
        Assert.IsTrue(Obj.Equals(Obj2));
      finally
        Obj2.Free;
      end;
    finally
      JObj.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRouting.TestComplexRoutings;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.IsTrue(Router.ExecuteRouting('/path1/1', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST,
      'text/plain', 'text/plain', Controllers, 'text/plain',
      TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.AreEqual('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsFalse(Router.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter',
      httpPOST, 'text/plain', 'text/plain', Controllers, 'text/plain',
      TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.IsNull(Router.MethodToCall);
    Assert.IsFalse(Assigned(Router.MVCControllerClass));

  finally
    Params.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONArray;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
  JArr: TJSONArray;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    ds.First;
    // JArr := TJSONArray.Create;
    JArr := ds.AsJSONArray;
    try
      // Mapper.DataSetToJSONArray(ds, JArr, false);
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds.First;
      while not ds.Eof do
      begin
        ds2.Insert;
        JObj := JArr.Get(ds.RecNo - 1) as TJSONObject;
        ds2.LoadFromJSONObject(JObj);
        // Mapper.JSONObjectToDataSet(JObj, ds2, false);
        ds2.Post;
        SameFishesDataSet(ds, ds2);
        ds.Next;
      end;
    finally
      JArr.Free;
    end;
  finally
    ds.Free;
    ds2.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONObject;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    JObj := ds.AsJSONObject;
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.Free;
    end;
  finally
    ds.Free;
    ds2.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyAsIsCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    JObj := ds.AsJSONObject(false, fpAsIs);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpAsIs);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.Free;
    end;
  finally
    ds.Free;
    ds2.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyLowerCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    JObj := ds.AsJSONObject(false, fpLowerCase);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpLowerCase);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.Free;
    end;
  finally
    ds.Free;
    ds2.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONObjectFieldPolicyUpperCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    JObj := ds.AsJSONObject(false, fpUpperCase);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpUpperCase);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.Free;
    end;
  finally
    ds.Free;
    ds2.Free;
  end;
end;

procedure TTestMappers.TestDataSetToJSONObjectWithNulls;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
begin
  ds := TClientDataSet.Create(nil);
  try
    ds.FieldDefs.Add('string_value', ftString, 50);
    ds.FieldDefs.Add('integer_value', ftInteger);
    ds.FieldDefs.Add('float_value', ftFloat);
    ds.FieldDefs.Add('null_value', ftString, 50);
    ds.FieldDefs.Add('boolean_value', ftBoolean);
    ds.CreateDataSet;
    ds.Insert;
    ds.FieldByName('string_value').AsString := 'myStringValue';
    ds.FieldByName('integer_value').AsInteger := 123;
    ds.FieldByName('float_value').AsFloat := 123.456;
    ds.FieldByName('null_value').Clear;
    ds.FieldByName('boolean_value').AsBoolean := true;
    ds.Post;
    JObj := ds.AsJSONObject;
    try
      Assert.AreEqual('myStringValue', JObj.Values['string_value'].Value);
      Assert.AreEqual(123, JObj.Values['integer_value'].GetValue<TJSONNumber>().AsInt);
      Assert.AreEqual(123.456, JObj.Values['float_value'].GetValue<TJSONNumber>().AsDouble, 0.0009);
      Assert.IsTrue(JObj.Values['null_value'].GetValue<TJSONNull>().Null);
      Assert.AreEqual(true, JObj.Values['boolean_value'].GetValue<TJSONBool>().AsBoolean);
      Assert.IsTrue(JObj.ToJSON.Replace(' ', '').Contains('"null_value":null'));
      ds.Insert;
      ds.LoadFromJSONObject(JObj);
      ds.Post;
      Assert.IsTrue(ds.FieldByName('null_value').IsNull);
    finally
      JObj.Free;
    end;
  finally
    ds.Free;
  end;
end;

procedure TTestMappers.TestJSONArrayToObjectListNoGenerics;
var
  ListObj, RetList: TObjectList<TMyObject>;
  JSONArr: TJSONArray;
  I: Integer;
begin
  ListObj := TObjectList<TMyObject>.Create;
  try
    ListObj.Add(GetMyObject);
    ListObj.Add(GetMyObject);
    JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
    try
      RetList := TObjectList<TMyObject>(Mapper.JSONArrayToObjectList(TMyObject,
        JSONArr, false));
      try
        Assert.AreEqual(2, RetList.Count);
        for I := 0 to ListObj.Count - 1 do
          Assert.IsTrue(ListObj[I].Equals(RetList[I]));
      finally
        RetList.Free;
      end;
    finally
      JSONArr.Free;
    end;
  finally
    ListObj.Free;
  end;
end;

procedure TTestMappers.TestJSONArrayToObjectListNoGenericsWrappedList;
var
  ListObj, RetList: TObjectList<TMyObject>;
  JSONArr: TJSONArray;
  I: Integer;
begin
  ListObj := TObjectList<TMyObject>.Create;
  try
    ListObj.Add(GetMyObject);
    ListObj.Add(GetMyObject);
    JSONArr := Mapper.ObjectListToJSONArray<TMyObject>(ListObj);
    try
      RetList := TObjectList<TMyObject>.Create;
      try
        Mapper.JSONArrayToObjectList(WrapAsList(RetList), TMyObject,
          JSONArr, false);
        Assert.AreEqual(2, RetList.Count);
        for I := 0 to ListObj.Count - 1 do
          Assert.IsTrue(ListObj[I].Equals(RetList[I]));
      finally
        RetList.Free;
      end;
    finally
      JSONArr.Free;
    end;
  finally
    ListObj.Free;
  end;
end;

procedure TTestMappers.TestJSONObjectStringToObject;
const
  MYOBJECTJSON =
    '{"PropString":"Some text \u00E0\u00E8\u00E9\u00EC\u00F2\u00F9",' +
    '"PropAnsiString":"This is an ANSI text","PropInteger":-1234,' +
    '"PropUInt32":1234,"PropInt64":-1234567890,"PropUInt64":1234567890,' +
    '"PropUInt16":12345,"PropInt16":-12345,"PropBoolean":true,' +
    '"PropDate":"2010-10-20","PropTime":"10:20:30",' +
    '"PropDateTime":"2010-10-20 10:20:30",' +
    '"PropTimeStamp":63423339630040,"PropCurrency":1234.5678}';
var
  lMyObject: TMyObject;
  lMyObject2: TMyObject;
begin
  lMyObject := Mapper.JSONObjectStringToObject<TMyObject>(MYOBJECTJSON);
  try
    lMyObject2 := GetMyObject;
    try
      Assert.IsTrue(lMyObject.Equals(lMyObject2));
    finally
      lMyObject2.Free;
    end;
  finally
    lMyObject.Free;
  end;
end;

procedure TTestMappers.TestJSONObjectStringToObjectWithWrongJSON;
begin
  //ExpectedException := EMapperException;
  Mapper.JSONObjectStringToObject<TObject>('{wrongjson}');
end;

procedure TTestMappers.TestJSONObjectToObjectAndBack;
var
  Obj: TMyObject;
  JObj: TJSONObject;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    JObj := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyObject>(JObj);
      try
        Assert.IsTrue(Obj.Equals(Obj2));
      finally
        Obj2.Free;
      end;
    finally
      JObj.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestMappers.TestJSONObjectToObjectWithNullInJSONString;
var
  LJSONObject: string;
  Obj: TMyStreamObject;
begin
  LJSONObject := '{"ImageStream":null}';
  Obj := Mapper.JSONObjectStringToObject<TMyStreamObject>(LJSONObject);
  Assert.IsNull(Obj.ImageStream);
  Obj.Free;
end;

procedure TTestMappers.TestLoadJSONObjectToObjectAndBack;
var
  Obj: TMyObject;
  JObj: TJSONObject;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    JObj := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := TMyObject.Create;
      try
        Mapper.LoadJSONObjectToObject<TMyObject>(JObj, Obj2);
        Assert.IsTrue(Obj.Equals(Obj2));
      finally
        Obj2.Free;
      end;
    finally
      JObj.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestMappers.TestObjectListToJSONArray;
var
  Obj: TMyObject;
  ObjList, Obj2List: TObjectList<TMyObject>;
  JSON: TJSONArray;
  I: Integer;
begin
  ObjList := TObjectList<TMyObject>.Create(true);
  try
    for I := 1 to 10 do
    begin
      Obj := GetMyObject;
      Obj.PropInteger := I;
      ObjList.Add(Obj);
    end;
    JSON := Mapper.ObjectListToJSONArray<TMyObject>(ObjList);

    Obj2List := Mapper.JSONArrayToObjectList<TMyObject>(JSON);
    try
      Assert.AreEqual(ObjList.Count, Obj2List.Count);
      for I := 0 to 9 do
      begin
        Assert.IsTrue(Obj2List[I].Equals(ObjList[I]));
      end;
    finally
      Obj2List.Free;
    end;
  finally
    ObjList.Free;
  end;
end;

procedure TTestMappers.TestWrappedListToJSONArray;
var
  Obj: TMyObject;
  ObjList: TObjectList<TMyObject>;
  WrapList: IWrappedList;
  JSON: TJSONArray;
  I: Integer;
  LJSONObj: TJSONObject;
  LMyItem: TMyObject;
begin
  ObjList := TObjectList<TMyObject>.Create(true);
  try
    for I := 1 to 10 do
    begin
      Obj := GetMyObject;
      Obj.PropInteger := I;
      ObjList.Add(Obj);
    end;
    WrapList := WrapAsList(ObjList);
    JSON := Mapper.ObjectListToJSONArray(WrapList);
    try
      Assert.AreEqual(WrapList.Count, JSON.Count);
      for I := 0 to 9 do
      begin
        LJSONObj := JSON.Items[I] as TJSONObject;
        LMyItem := WrapList.GetItem(I) as TMyObject;
        Assert.AreEqual(LMyItem.PropInteger, LJSONObj.GetValue<Integer>('PropInteger'));
      end;
    finally
      JSON.Free;
    end;
  finally
    ObjList.Free;
  end;
end;

procedure TTestMappers.TestObjectToJSONObject;
var
  Obj: TMyObject;
  JSON: TJSONObject;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    JSON := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyObject>(JSON);
      try
        Assert.IsTrue(Obj.Equals(Obj2));
      finally
        Obj2.Free;
      end;
    finally
      JSON.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestMappers.TestObjectToJSONObjectAndBackWithStream;
var
  SO: TMyStreamObject;
  JSONObj: TJSONObject;
  ResultSO: TMyStreamObject;
begin
  // ARRANGE
  SO := TMyStreamObject.Create;
  try
    // ACT
    TMemoryStream(SO.ImageStream)
      .LoadFromFile('..\..\..\..\samples\_\customer.png');
    JSONObj := Mapper.ObjectToJSONObject(SO);
    try
      ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
      try
        // ASSERT
        Assert.AreEqual(SO.ImageStream.Size, ResultSO.ImageStream.Size);
        Assert.AreEqual(MD5(SO.ImageStream), MD5(ResultSO.ImageStream));
      finally
        ResultSO.Free;
      end;
    finally
      JSONObj.Free;
    end;
  finally
    SO.Free;
  end;
end;

procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF16;
var
  SO: TMyStreamObject;
  JSONObj: TJSONObject;
  ResultSO: TMyStreamObject;
  ResultStr, str: UnicodeString;
begin
  // ARRANGE
  str := 'This is a UTF16 String (什么是)';
  SO := TMyStreamObject.Create;
  try
    // ACT
    SO.PropStream := TStringStream.Create(str, TEncoding.Unicode);
    JSONObj := Mapper.ObjectToJSONObject(SO);
    try
      ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
      try
        ResultStr := TStringStream(ResultSO.PropStream).DataString;
        // ASSERT
        Assert.AreEqual(str, ResultStr);
      finally
        ResultSO.Free;
      end;
    finally
      JSONObj.Free;
    end;
  finally
    SO.Free;
  end;
end;

procedure TTestMappers.TestObjectToJSONObjectAndBackWithStringStreamUTF8;
var
  SO: TMyStreamObject;
  JSONObj: TJSONObject;
  ResultSO: TMyStreamObject;
  ResultStr, str: UTF8String;
begin
  // ARRANGE
  str := 'This is a UTF8 String (什么是)';
  SO := TMyStreamObject.Create;
  try
    // ACT
    SO.Prop8Stream := TStringStream.Create(string(str), TEncoding.UTF8);
    JSONObj := Mapper.ObjectToJSONObject(SO);
    try
      ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
      try
        ResultStr := UTF8String(TStringStream(ResultSO.Prop8Stream).DataString);
        // ASSERT
        Assert.AreEqual(str, ResultStr);
      finally
        ResultSO.Free;
      end;
    finally
      JSONObj.Free;
    end;
  finally
    SO.Free;
  end;
end;

procedure TTestMappers.TestObjectToJSONObject_Generics;
var
  lObjList: TObjectList<TMyClass>;
  lResponse: TResponseWrapper<TMyClass>;
  LJSONObj: TJSONObject;
begin
  lObjList := TObjectList<TMyClass>.Create();
  lObjList.Add(TMyClass.Create(1, 'pippo'));
  lObjList.Add(TMyClass.Create(2, 'pluto'));
  lResponse := TResponseWrapper<TMyClass>.Create(lObjList.Count, lObjList);
  try
    LJSONObj := Mapper.ObjectToJSONObject(lResponse);
    try
      Assert.IsNotNull(LJSONObj.GetValue('Items'));
      Assert.AreEqual(2, TJSONArray(LJSONObj.GetValue('Items')).Count);
    finally
      LJSONObj.Free;
    end;
  finally
    lResponse.Free;
  end;
end;

procedure TTestRouting.TestPathButNoParameters;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.IsTrue(Router.ExecuteRouting('/orders', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain',
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.AreEqual(0, Params.Count);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('Orders', Router.MethodToCall.Name);
    Assert.AreEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentEncoding);
  finally
    Params.Free;
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
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual(1, Params.Count);
    Assert.AreEqual('789', Params['ordernumber']);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('OrderNumber', Router.MethodToCall.Name);
  finally
    Params.Free;
  end;

  Params := TMVCRequestParamsTable.Create;
  try
    Assert.IsTrue(Router.ExecuteRouting('/orders/àèéìòù .-_\', httpGET,
      'text/plain', 'text/plain', Controllers, 'text/plain',
      TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    Assert.AreEqual(1, Params.Count);
    Assert.AreEqual('àèéìòù .-_\', Params['ordernumber']);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('OrderNumber', Router.MethodToCall.Name);
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
    Assert.IsTrue(Router.ExecuteRouting('/orders', httpGET, '', 'application/json',
      Controllers, TMVCConstants.DEFAULT_CONTENT_TYPE,
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentCharset));
    Assert.AreEqual(0, Params.Count);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('OrdersProduceJSON', Router.MethodToCall.Name);
    Assert.AreEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
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
    Assert.IsTrue(Router.ExecuteRouting('/orders', httpGET, '',
      'application/json; charset=UTF-8', Controllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET,
      Params, ResponseContentType, ResponseContentCharset));
    Assert.AreEqual(0, Params.Count);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('OrdersProduceJSON', Router.MethodToCall.Name);
    Assert.AreEqual(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.Free;
  end;
end;

procedure TTestMappers.TestSerializeUsingFields;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TObject;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      Assert.AreEqual(4, lJObj.Count); // 3 properties + $dmvc.classname
      Assert.IsNotNull(lJObj.Get('FFirstName'));
      Assert.IsNotNull(lJObj.Get('FLastName'));
      Assert.IsNotNull(lJObj.Get('FAge'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        //Assert.AreSame(lObj2, TMyObjectWithLogic,
        //  'wrong classtype for deserialized object');
        Assert.IsTrue(lObj.Equals(lObj2),
          'restored object is different from the original');
      finally
        lObj2.Free;
      end;
    finally
      lJObj.Free;
    end;
  finally
    lObj.Free;
  end;
end;

procedure TTestMappers.TestSerializeUsingFieldsComplexObject;
var
  lJObj: TJSONObject;
  lObj2: TObject;
  lObj: TMyComplexObject;
begin
  lObj := GetMyComplexObject;
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      Assert.AreEqual(4, lJObj.Count); // 3 properties + $dmvc.classname
      Assert.IsNotNull(lJObj.Get('FProp1'));
      Assert.IsNotNull(lJObj.Get('FChildObjectList'));
      Assert.IsNotNull(lJObj.Get('FChildObject'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        Assert.isTrue(lObj2 is TMyComplexObject,
          'wrong classtype for deserialized object');
        Assert.IsTrue(lObj.Equals(lObj2),
          'restored object is different from the original');
      finally
        lObj2.Free;
      end;
    finally
      lJObj.Free;
    end;
  finally
    lObj.Free;
  end;
end;

procedure TTestMappers.TestSerializeUsingFieldsComplexObject2;
var
  lJObj: TJSONObject;
  lObj2: TObject;
  lObj: TMyComplexObject;
begin
  lObj := GetMyComplexObjectWithNotInitializedChilds;
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      Assert.AreEqual(4, lJObj.Count); // 3 properties + $dmvc.classname
      Assert.IsNotNull(lJObj.Get('FProp1'));
      Assert.IsNotNull(lJObj.Get('FChildObjectList'));
      Assert.IsNotNull(lJObj.Get('FChildObject'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        Assert.isTrue(lObj2 is TMyComplexObject,
          'wrong classtype for deserialized object');
        Assert.IsTrue(lObj.Equals(lObj2),
          'restored object is different from the original');
      finally
        lObj2.Free;
      end;
    finally
      lJObj.Free;
    end;
  finally
    lObj.Free;
  end;
end;

procedure TTestMappers.
  TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TMyObjectWithLogic;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      lJObj.RemovePair('FFirstName').Free;
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj) as TMyObjectWithLogic;
      try
        Assert.AreEqual('', lObj2.FirstName);
      finally
        lObj2.Free;
      end;
    finally
      lJObj.Free;
    end;
  finally
    lObj.Free;
  end;
end;

procedure TTestMappers.TestSerializeUsingProperties;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TMyObjectWithLogic;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObject(lObj, []);
    try
      Assert.AreEqual(5, lJObj.Count); // 5 properties
      Assert.IsNotNull(lJObj.Get('FirstName'));
      Assert.IsNotNull(lJObj.Get('LastName'));
      Assert.IsNotNull(lJObj.Get('Age'));
      Assert.IsNotNull(lJObj.Get('FullName'));
      Assert.IsNotNull(lJObj.Get('IsAdult'));
      lObj2 := Mapper.JSONObjectToObject<TMyObjectWithLogic>(lJObj);
      try
        Assert.IsTrue(lObj2.Equals(lObj),
          'deserialized object is not equals to the original object');
      finally
        lObj2.Free;
      end;
    finally
      lJObj.Free;
    end;
  finally
    lObj.Free;
  end;
end;

procedure TTestRouting.TestWithMethodTypes;
var
  Params: TMVCRequestParamsTable;
  ResponseContentType: string;
  ResponseContentEncoding: string;
begin
  Params := TMVCRequestParamsTable.Create;
  try
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpPUT, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpPATCH, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('PatchOrder', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsFalse(Router.ExecuteRouting('/orders/789', httpDELETE, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.IsNull(Router.MethodToCall);
    Assert.IsFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    Assert.IsFalse(Router.ExecuteRouting('/orders/789', httpHEAD, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as HEAD');
    Assert.IsNull(Router.MethodToCall, 'Resolved as HEAD');
    Assert.IsFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    Assert.IsFalse(Router.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as OPTIONS');
    Assert.IsNull(Router.MethodToCall, 'Resolved as OPTIONS');
    Assert.IsFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('OrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    Assert.IsTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual('OrderNumber', Router.MethodToCall.Name);
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
    Assert.IsTrue(Router.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual(0, Params.Count);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('Index', Router.MethodToCall.Name);
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
    Assert.IsTrue(Router.ExecuteRouting('', httpGET, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    Assert.AreEqual(0, Params.Count);
    Assert.AreEqual('TSimpleController', Router.MVCControllerClass.ClassName);
    Assert.AreEqual('Index', Router.MethodToCall.Name);
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
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError), 'Generated token is not valid');
end;

procedure TTestJWT.TestDefaults;
begin
  Assert.AreEqual('HS256', FJWT.HMACAlgorithm, 'Default algorithm should be HS256');
  Assert.AreEqual(Int64(300), FJWT.LeewaySeconds, 'Default leeway should be 5 minutes');
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
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Yesterday;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
    'Expired token is considered valid');

  FJWT.Claims.ExpirationTime := Now;
  lToken := FJWT.GetToken;
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered expired');

  FJWT.Claims.ExpirationTime := Now - (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
    'Expired token is considered valid');
end;

procedure TTestJWT.TestHMAC;
var
  lAlg: String;
  lValue: String;
  I: Integer;
begin
  for I := Low(HMAC_ALG_AND_RESULTS) to High(HMAC_ALG_AND_RESULTS) do
  begin
    lAlg := HMAC_ALG_AND_RESULTS[I][0];
    lValue := HMAC_ALG_AND_RESULTS[I][1];
    Assert.AreEqual(lValue, BytesToHex(HMAC(lAlg, 'Daniele Teti', 'daniele')),
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
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
    'Still-not-valid token is considered valid');

  FJWT.Claims.IssuedAt := Now;
  lToken := FJWT.GetToken;
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.IssuedAt := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
    'Still-not-valid token is considered valid');
end;

procedure TTestJWT.TestLoadToken;
var
  lToken: string;
  lJWT: TJWT;
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
    lJWT.LoadToken(lToken);
    Assert.AreEqual('bit Time Professionals', lJWT.Claims.Issuer);
    Assert.AreEqual('DelphiMVCFramework', lJWT.Claims.Subject);
    Assert.AreEqual('DelphiDevelopers', lJWT.Claims.Audience);
    Assert.AreEqual('123456', lJWT.Claims.JWT_ID);
    Assert.AreEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
      lJWT.Claims.IssuedAt);
    Assert.AreEqual(Roundto(lJWT.Claims.IssuedAt + OneHour * 2, 4),
      Roundto(lJWT.Claims.ExpirationTime, 4));
    Assert.AreEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
      lJWT.Claims.NotBefore);
    Assert.AreEqual('dteti', lJWT.CustomClaims['username']);
    Assert.AreEqual('admin', lJWT.CustomClaims['userrole']);
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
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Tomorrow;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
    'Still-not-valid token is considered valid');

  FJWT.Claims.NotBefore := Now;
  lToken := FJWT.GetToken;
  Assert.IsTrue(FJWT.IsValidToken(lToken, lError),
    'Valid token is considered not valid');

  FJWT.Claims.NotBefore := Now + (FJWT.LeewaySeconds + 1) * OneSecond;
  lToken := FJWT.GetToken;
  Assert.IsFalse(FJWT.IsValidToken(lToken, lError),
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

  Assert.AreEqual('bit Time Professionals', FJWT.Claims.Issuer);
  Assert.AreEqual('DelphiMVCFramework', FJWT.Claims.Subject);
  Assert.AreEqual('DelphiDevelopers', FJWT.Claims.Audience);
  Assert.AreEqual('123456', FJWT.Claims.JWT_ID);
  Assert.AreEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0), FJWT.Claims.IssuedAt);
  Assert.AreEqual(Roundto(FJWT.Claims.IssuedAt + OneHour * 2, 4),
    Roundto(FJWT.Claims.ExpirationTime, 4));
  Assert.AreEqual(EncodeDateTime(2011, 11, 17, 17, 30, 0, 0),
    FJWT.Claims.NotBefore);

  Assert.AreEqual('dteti', FJWT.CustomClaims['username']);
  Assert.AreEqual('admin', FJWT.CustomClaims['userrole']);

end;

initialization

TDUnitX.RegisterTestFixture(TTestRouting);
TDUnitX.RegisterTestFixture(TTestMappers);
TDUnitX.RegisterTestFixture(TTestJWT);

finalization

end.
