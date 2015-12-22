unit FrameworkTestsU;

interface

uses
  TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  MVCFramework, Data.DB, System.SysUtils;

type
  TTestRouting = class(TTestCase)
  private
    Router: TMVCRouter;
    Controllers: TList<TMVCControllerClass>;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    procedure SameFishesDataSet(ds, ds2: TDataSet);
  published
    procedure TestWithNoParameters;
    procedure TestWithNoPath;
    procedure TestPathButNoParameters;
    procedure TestPathWithParameters;
    procedure TestWithMethodTypes;
    procedure TestComplexRoutings;
    procedure TestProduceRoutings;
    procedure TestProduceRoutingsWithExplicitCharset;

    procedure TestObjectToJSONObject;
    procedure TestObjectListToJSONArray;
    // objects mappers
    procedure TestJSONObjectToObjectAndBack;
    procedure TestSerializeUsingProperties;
    procedure TestSerializeUsingFields;
    procedure TestSerializeUsingFieldsComplexObject;
    procedure TestSerializeUsingFieldsComplexObject2;
    procedure TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
    procedure TestComplexObjectToJSONObjectAndBack;
    procedure TestComplexObjectToJSONObjectAndBackWithNilReference;
    procedure TestDataSetToJSONObject;
    procedure TestDataSetToJSONObjectFieldPolicyLowerCase;
    procedure TestDataSetToJSONObjectFieldPolicyUpperCase;
    procedure TestDataSetToJSONObjectFieldPolicyAsIsCase;
    procedure TestDataSetToJSONArray;
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF16;
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF8;
    procedure TestJSONArrayToObjectListNoGenerics;
    procedure TestJSONArrayToObjectListNoGenericsWrappedList;
    procedure TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
  end;

implementation

{$WARN SYMBOL_DEPRECATED OFF}

uses MVCFramework.Commons,
  TestControllersU, DBClient,
  Web.HTTPApp, Soap.EncdDecd,
  ObjectsMappers,
  BOs,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  TestServerControllerU, System.Classes, DuckListU;

procedure TTestRouting.SameFishesDataSet(ds, ds2: TDataSet);
begin
  CheckEquals(ds.FieldByName('Species No').AsInteger, ds2.FieldByName('Species No').AsInteger);
  CheckEquals(ds.FieldByName('Category').AsString, ds2.FieldByName('Category').AsString);
  CheckEquals(ds.FieldByName('Common_Name').AsString, ds2.FieldByName('Common_Name').AsString);
  CheckEquals(ds.FieldByName('Species Name').AsString, ds2.FieldByName('Species Name').AsString);
  CheckEquals(ds.FieldByName('Length (cm)').AsString, ds2.FieldByName('Length (cm)').AsString);
  CheckEquals(ds.FieldByName('Length_In').AsInteger, ds2.FieldByName('Length_In').AsInteger);
  CheckEquals(ds.FieldByName('Notes').AsString, ds2.FieldByName('Notes').AsString);
  CheckEquals(ds.FieldByName('Graphic').AsString, ds2.FieldByName('Graphic').AsString);
end;

procedure TTestRouting.SetUp;
begin
  Controllers := TList<TMVCControllerClass>.Create;
  Controllers.Add(TSimpleController);
  Controllers.Add(TNotSoSimpleController);
  Controllers.Add(TTestServerController);
  Router := TMVCRouter.Create(nil);
end;

procedure TTestRouting.TearDown;
begin
  Router.free;
  Controllers.free;
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

procedure TTestRouting.TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
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
        CheckTrue(TStringStream(DesObj.PropStream).DataString.IsEmpty);
        CheckTrue(TStringStream(DesObj.Prop8Stream).DataString.IsEmpty);
      finally
        DesObj.free;
      end;
    finally
      JSONObj.free;
    end;
  finally
    Obj.free;
  end;
end;

procedure TTestRouting.TestComplexObjectToJSONObjectAndBack;
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
        CheckTrue(Obj.Equals(Obj2));
      finally
        Obj2.free;
      end;
    finally
      JObj.free;
    end;
  finally
    Obj.free;
  end;
end;

procedure TTestRouting.TestComplexObjectToJSONObjectAndBackWithNilReference;
var
  Obj: TMyComplexObject;
  JObj: TJSONObject;
  Obj2: TMyComplexObject;
begin
  Obj := GetMyComplexObject;
  try
    Obj.ChildObject.free;
    Obj.ChildObject := nil;
    JObj := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyComplexObject>(JObj);
      try
        CheckTrue(Obj.Equals(Obj2));
      finally
        Obj2.free;
      end;
    finally
      JObj.free;
    end;
  finally
    Obj.free;
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
    CheckTrue(Router.ExecuteRouting('/path1/1', httpPOST, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter', httpPOST,
      'text/plain', 'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckNull(Router.MethodToCall);
    CheckFalse(Assigned(Router.MVCControllerClass));

  finally
    Params.free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONArray;
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
      JArr.free;
    end;
  finally
    ds.free;
    ds2.free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONObject;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    // JObj := TJSONObject.Create;
    JObj := ds.AsJSONObject;
    try
      // Mapper.DataSetToJSONObject(ds, JObj, false);
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj);
      // Mapper.JSONObjectToDataSet(JObj, ds2, false);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.free;
    end;
  finally
    ds.free;
    ds2.free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONObjectFieldPolicyAsIsCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    // JObj := TJSONObject.Create;
    JObj := ds.AsJSONObject(false, fpAsIs);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpAsIs);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.free;
    end;
  finally
    ds.free;
    ds2.free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONObjectFieldPolicyLowerCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    // JObj := TJSONObject.Create;
    JObj := ds.AsJSONObject(false, fpLowerCase);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpLowerCase);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.free;
    end;
  finally
    ds.free;
    ds2.free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONObjectFieldPolicyUpperCase;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    // JObj := TJSONObject.Create;
    JObj := ds.AsJSONObject(false, fpUpperCase);
    try
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj, fpUpperCase);
      ds2.Post;
      SameFishesDataSet(ds, ds2);
    finally
      JObj.free;
    end;
  finally
    ds.free;
    ds2.free;
  end;
end;

procedure TTestRouting.TestJSONArrayToObjectListNoGenerics;
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
      RetList := TObjectList<TMyObject>(Mapper.JSONArrayToObjectList(TMyObject, JSONArr, false));
      try
        CheckEquals(2, RetList.Count);
        for I := 0 to ListObj.Count - 1 do
          CheckTrue(ListObj[I].Equals(RetList[I]));
      finally
        RetList.free;
      end;
    finally
      JSONArr.free;
    end;
  finally
    ListObj.free;
  end;
end;

procedure TTestRouting.TestJSONArrayToObjectListNoGenericsWrappedList;
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
        Mapper.JSONArrayToObjectList(WrapAsList(RetList), TMyObject, JSONArr, false);
        CheckEquals(2, RetList.Count);
        for I := 0 to ListObj.Count - 1 do
          CheckTrue(ListObj[I].Equals(RetList[I]));
      finally
        RetList.free;
      end;
    finally
      JSONArr.free;
    end;
  finally
    ListObj.free;
  end;
end;

procedure TTestRouting.TestJSONObjectToObjectAndBack;
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
        CheckTrue(Obj.Equals(Obj2));
      finally
        Obj2.free;
      end;
    finally
      JObj.free;
    end;
  finally
    Obj.free;
  end;
end;

procedure TTestRouting.TestObjectListToJSONArray;
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
      CheckEquals(ObjList.Count, Obj2List.Count);
      for I := 0 to 9 do
      begin
        CheckTrue(Obj2List[I].Equals(ObjList[I]));
      end;
    finally
      Obj2List.free;
    end;
  finally
    ObjList.free;
  end;
end;

procedure TTestRouting.TestObjectToJSONObject;
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
        CheckTrue(Obj.Equals(Obj2));
      finally
        Obj2.free;
      end;
    finally
      JSON.free;
    end;
  finally
    Obj.free;
  end;
end;

procedure TTestRouting.TestObjectToJSONObjectAndBackWithStringStreamUTF16;
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
        CheckEquals(str, ResultStr);
      finally
        ResultSO.free;
      end;
    finally
      JSONObj.free;
    end;
  finally
    SO.free;
  end;
end;

procedure TTestRouting.TestObjectToJSONObjectAndBackWithStringStreamUTF8;
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
        CheckEquals(str, ResultStr);
      finally
        ResultSO.free;
      end;
    finally
      JSONObj.free;
    end;
  finally
    SO.free;
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Orders', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentEncoding);
  finally
    Params.free;
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
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals(1, Params.Count);
    CheckEquals('789', Params['ordernumber']);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrderNumber', Router.MethodToCall.Name);
  finally
    Params.free;
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, '', 'application/json', Controllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET, Params,
      ResponseContentType, ResponseContentCharset));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrdersProduceJSON', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.free;
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, '', 'application/json; charset=UTF-8',
      Controllers, TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET,
      Params, ResponseContentType, ResponseContentCharset));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrdersProduceJSON', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.free;
  end;
end;

procedure TTestRouting.TestSerializeUsingFields;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TObject;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      CheckEquals(4, lJObj.Count); // 3 properties + $dmvc.classname
      CheckNotNull(lJObj.Get('FFirstName'));
      CheckNotNull(lJObj.Get('FLastName'));
      CheckNotNull(lJObj.Get('FAge'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        CheckIs(lObj2, TMyObjectWithLogic, 'wrong classtype for deserialized object');
        CheckTrue(lObj.Equals(lObj2), 'restored object is different from the original');
      finally
        lObj2.free;
      end;
    finally
      lJObj.free;
    end;
  finally
    lObj.free;
  end;
end;

procedure TTestRouting.TestSerializeUsingFieldsComplexObject;
var
  lJObj: TJSONObject;
  lObj2: TObject;
  lObj: TMyComplexObject;
begin
  lObj := GetMyComplexObject;
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      CheckEquals(4, lJObj.Count); // 3 properties + $dmvc.classname
      CheckNotNull(lJObj.Get('FProp1'));
      CheckNotNull(lJObj.Get('FChildObjectList'));
      CheckNotNull(lJObj.Get('FChildObject'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        CheckIs(lObj2, TMyComplexObject, 'wrong classtype for deserialized object');
        CheckTrue(lObj.Equals(lObj2), 'restored object is different from the original');
      finally
        lObj2.free;
      end;
    finally
      lJObj.free;
    end;
  finally
    lObj.free;
  end;
end;

procedure TTestRouting.TestSerializeUsingFieldsComplexObject2;
var
  lJObj: TJSONObject;
  lObj2: TObject;
  lObj: TMyComplexObject;
begin
  lObj := GetMyComplexObjectWithNotInitializedChilds;
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      CheckEquals(4, lJObj.Count); // 3 properties + $dmvc.classname
      CheckNotNull(lJObj.Get('FProp1'));
      CheckNotNull(lJObj.Get('FChildObjectList'));
      CheckNotNull(lJObj.Get('FChildObject'));
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj);
      try
        CheckIs(lObj2, TMyComplexObject, 'wrong classtype for deserialized object');
        CheckTrue(lObj.Equals(lObj2), 'restored object is different from the original');
      finally
        lObj2.free;
      end;
    finally
      lJObj.free;
    end;
  finally
    lObj.free;
  end;
end;

procedure TTestRouting.TestSerializeUsingFieldsWithNotExixtentPropetyInJSONObject;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TMyObjectWithLogic;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObjectFields(lObj, []);
    try
      lJObj.RemovePair('FFirstName').free;
      lObj2 := Mapper.JSONObjectFieldsToObject(lJObj) as TMyObjectWithLogic;
      try
        CheckEquals('', lObj2.FirstName);
      finally
        lObj2.free;
      end;
    finally
      lJObj.free;
    end;
  finally
    lObj.free;
  end;
end;

procedure TTestRouting.TestSerializeUsingProperties;
var
  lObj: TMyObjectWithLogic;
  lJObj: TJSONObject;
  lObj2: TMyObjectWithLogic;
begin
  lObj := TMyObjectWithLogic.Create('Daniele', 'Teti', 35);
  try
    lJObj := Mapper.ObjectToJSONObject(lObj, []);
    try
      CheckEquals(5, lJObj.Count); // 5 properties
      CheckNotNull(lJObj.Get('FirstName'));
      CheckNotNull(lJObj.Get('LastName'));
      CheckNotNull(lJObj.Get('Age'));
      CheckNotNull(lJObj.Get('FullName'));
      CheckNotNull(lJObj.Get('IsAdult'));
      lObj2 := Mapper.JSONObjectToObject<TMyObjectWithLogic>(lJObj);
      try
        CheckTrue(lObj2.Equals(lObj), 'deserialized object is not equals to the original object');
      finally
        lObj2.free;
      end;
    finally
      lJObj.free;
    end;
  finally
    lObj.free;
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
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPOST, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPUT, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPATCH, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('PatchOrder', Router.MethodToCall.Name);

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpDELETE, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckNull(Router.MethodToCall);
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpHEAD, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding), 'Resolved as HEAD');
    CheckNull(Router.MethodToCall, 'Resolved as HEAD');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding), 'Resolved as OPTIONS');
    CheckNull(Router.MethodToCall, 'Resolved as OPTIONS');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('OrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('OrderNumber', Router.MethodToCall.Name);
  finally
    Params.free;
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
    CheckTrue(Router.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Index', Router.MethodToCall.Name);
  finally
    Params.free;
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
    CheckTrue(Router.ExecuteRouting('', httpGET, 'text/plain', 'text/plain', Controllers,
      'text/plain', TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Index', Router.MethodToCall.Name);
  finally
    Params.free;
  end;
end;

{ TMyObject }

initialization

RegisterTest(TTestRouting.suite);

finalization

end.
