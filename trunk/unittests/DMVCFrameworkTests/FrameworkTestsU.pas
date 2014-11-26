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
    procedure TestComplexObjectToJSONObjectAndBack;
    procedure TestDataSetToJSONObject;
    procedure TestDataSetToJSONArray;
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF16;
    procedure TestObjectToJSONObjectAndBackWithStringStreamUTF8;
    procedure TestCheckMapperSerializeAsStringIsEmptyStrIfObjIsNil;
  end;

implementation

{ TTestRouting }

uses MVCFramework.Commons,
  TestControllersU, DBClient,
  Web.HTTPApp,
  ObjectsMappers,
  BOs,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  TestServerControllerU, System.Classes;

procedure TTestRouting.SameFishesDataSet(ds, ds2: TDataSet);
begin
  CheckEquals(ds.FieldByName('Species No').AsInteger,
    ds2.FieldByName('Species No').AsInteger);
  CheckEquals(ds.FieldByName('Category').AsString, ds2.FieldByName('Category')
    .AsString);
  CheckEquals(ds.FieldByName('Common_Name').AsString,
    ds2.FieldByName('Common_Name').AsString);
  CheckEquals(ds.FieldByName('Species Name').AsString,
    ds2.FieldByName('Species Name').AsString);
  CheckEquals(ds.FieldByName('Length (cm)').AsString,
    ds2.FieldByName('Length (cm)').AsString);
  CheckEquals(ds.FieldByName('Length_In').AsInteger,
    ds2.FieldByName('Length_In').AsInteger);
  CheckEquals(ds.FieldByName('Notes').AsString, ds2.FieldByName('Notes')
    .AsString);
  CheckEquals(ds.FieldByName('Graphic').AsString, ds2.FieldByName('Graphic')
    .AsString);
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
    DesObj := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
    try
      // ASSERT
      CheckTrue(TStringStream(DesObj.PropStream).DataString.IsEmpty);
      CheckTrue(TStringStream(DesObj.Prop8Stream).DataString.IsEmpty);
    finally
      DesObj.Free;
    end;
  finally
    Obj.Free;
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
    CheckTrue(Router.ExecuteRouting('/path1/1', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path2/1/2/3', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path3/1/2/tre/3', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/path4/par1/2/par2/3/4', httpPOST,
      'text/plain', 'text/plain', Controllers, 'text/plain',
      TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals('TestMultiplePaths', Router.MethodToCall.Name);

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/path4/par1/par2/3/4/notvalidparameter',
      httpPOST, 'text/plain', 'text/plain', Controllers, 'text/plain',
      TMVCMimeType.TEXT_PLAIN, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckNull(Router.MethodToCall);
    CheckFalse(Assigned(Router.MVCControllerClass));

  finally
    Params.Free;
  end;
end;

procedure TTestRouting.TestDataSetToJSONArray;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  S: string;
  ds2: TClientDataSet;
  JArr: TJSONArray;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    ds.First;
    JArr := TJSONArray.Create;
    try
      JArr := ds.AsJSONArray;
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

procedure TTestRouting.TestDataSetToJSONObject;
var
  ds: TClientDataSet;
  JObj: TJSONObject;
  S: string;
  ds2: TClientDataSet;
begin
  ds := TClientDataSet.Create(nil);
  ds2 := TClientDataSet.Create(nil);
  try
    ds.LoadFromFile('..\..\fishes.xml');
    JObj := TJSONObject.Create;
    try
      JObj := ds.AsJSONObject;
      // Mapper.DataSetToJSONObject(ds, JObj, false);
      ds2.LoadFromFile('..\..\fishes.xml');
      ds2.EmptyDataSet;
      ds2.Insert;
      ds2.LoadFromJSONObject(JObj);
      // Mapper.JSONObjectToDataSet(JObj, ds2, false);
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
        Obj2.Free;
      end;
    finally
      JObj.Free;
    end;
  finally
    Obj.Free;
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
      Obj2List.Free;
    end;
  finally
    ObjList.Free;
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
      CheckTrue(Obj.Equals(Obj2));
    finally
      JSON.Free;
    end;
  finally
    Obj.Free;
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
    ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
    try
      ResultStr := TStringStream(ResultSO.PropStream).DataString;
      // ASSERT
      CheckEquals(str, ResultStr);
    finally
      ResultSO.Free;
    end;
  finally
    SO.Free;
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
    SO.Prop8Stream := TStringStream.Create(str, TEncoding.UTF8);
    JSONObj := Mapper.ObjectToJSONObject(SO);
    ResultSO := Mapper.JSONObjectToObject<TMyStreamObject>(JSONObj);
    try
      ResultStr := TStringStream(ResultSO.Prop8Stream).DataString;
      // ASSERT
      CheckEquals(str, ResultStr);
    finally
      ResultSO.Free;
    end;
  finally
    SO.Free;
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain',
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Orders', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentEncoding);
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
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals(1, Params.Count);
    CheckEquals('789', Params['ordernumber']);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrderNumber', Router.MethodToCall.Name);
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, '', 'application/json',
      Controllers, TMVCConstants.DEFAULT_CONTENT_TYPE,
      TMVCConstants.DEFAULT_CONTENT_CHARSET, Params, ResponseContentType,
      ResponseContentCharset));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrdersProduceJSON', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
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
    CheckTrue(Router.ExecuteRouting('/orders', httpGET, '',
      'application/json; charset=UTF-8', Controllers,
      TMVCConstants.DEFAULT_CONTENT_TYPE, TMVCConstants.DEFAULT_CONTENT_CHARSET,
      Params, ResponseContentType, ResponseContentCharset));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrdersProduceJSON', Router.MethodToCall.Name);
    CheckEquals(TMVCConstants.DEFAULT_CONTENT_CHARSET, ResponseContentCharset);
  finally
    Params.Free;
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
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPOST, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPUT, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPATCH, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('PatchOrder', Router.MethodToCall.Name);

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpDELETE, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckNull(Router.MethodToCall);
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpHEAD, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as HEAD');
    CheckNull(Router.MethodToCall, 'Resolved as HEAD');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding), 'Resolved as OPTIONS');
    CheckNull(Router.MethodToCall, 'Resolved as OPTIONS');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('OrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      'text/plain', Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals('OrderNumber', Router.MethodToCall.Name);
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
    CheckTrue(Router.ExecuteRouting('/', httpGET, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Index', Router.MethodToCall.Name);
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
    CheckTrue(Router.ExecuteRouting('', httpGET, 'text/plain', 'text/plain',
      Controllers, 'text/plain', TMVCMimeType.TEXT_PLAIN, Params,
      ResponseContentType, ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Index', Router.MethodToCall.Name);
  finally
    Params.Free;
  end;
end;

{ TMyObject }

initialization

RegisterTest(TTestRouting.suite);

finalization

end.
