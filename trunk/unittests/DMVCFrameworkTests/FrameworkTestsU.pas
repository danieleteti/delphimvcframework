unit FrameworkTestsU;

interface

uses
  TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  MVCFramework;

type
  TTestRouting = class(TTestCase)
  private
    Router: TMVCRouter;
    Controllers: TList<TMVCControllerClass>;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestWithNoParameters;
    procedure TestWithNoPath;
    procedure TestPathButNoParameters;
    procedure TestPathWithParameters;
    procedure TestWithMethodTypes;
    procedure TestComplexRoutings;
    // procedure TestClassNameMethodNameRouting;

    procedure TestObjectToJSONObject;
    procedure TestObjectListToJSONArray;
  end;

implementation

{ TTestRouting }

uses MVCFramework.Commons,
  TestControllersU,
  Web.HTTPApp, ObjectsMappers, BOs, Data.DBXJSON;

procedure TTestRouting.SetUp;
begin
  Controllers := TList<TMVCControllerClass>.Create;
  Controllers.Add(TSimpleController);
  Controllers.Add(TNotSoSimpleController);
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

procedure TTestRouting.TestComplexRoutings;
begin

end;

procedure TTestRouting.TestObjectListToJSONArray;
var
  Obj, Obj2: TMyObject;
  ObjList, Obj2List: TObjectList<TMyObject>;
  json: TJSONArray;
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
    json := Mapper.ObjectListToJSONArray<TMyObject>(ObjList);

    Obj2List := Mapper.JSONArrayToObjectList<TMyObject>(json);
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
  json: TJSONObject;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    json := Mapper.ObjectToJSONObject(Obj);
    try
      Obj2 := Mapper.JSONObjectToObject<TMyObject>(json);
      CheckTrue(Obj.Equals(Obj2));
    finally
      json.Free;
    end;
  finally
    Obj.Free;
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
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals(0, Params.Count);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('Orders', Router.MethodToCall.Name);
    CheckEquals('UTF-8', ResponseContentEncoding);
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
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals(1, Params.Count);
    CheckEquals('789', Params['ordernumber']);
    CheckEquals('TSimpleController', Router.MVCControllerClass.ClassName);
    CheckEquals('OrderNumber', Router.MethodToCall.Name);
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
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpPUT, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('UpdateOrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpDELETE, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckNull(Router.MethodToCall);
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpHEAD, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding),
      'Resolved as HEAD');
    CheckNull(Router.MethodToCall, 'Resolved as HEAD');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckFalse(Router.ExecuteRouting('/orders/789', httpOPTIONS, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding),
      'Resolved as OPTIONS');
    CheckNull(Router.MethodToCall, 'Resolved as OPTIONS');
    CheckFalse(Assigned(Router.MVCControllerClass));

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
    CheckEquals('OrderNumber', Router.MethodToCall.Name);

    Params.Clear;
    CheckTrue(Router.ExecuteRouting('/orders/789', httpGET, 'text/plain',
      Controllers, Params, ResponseContentType, ResponseContentEncoding));
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
    CheckTrue(Router.ExecuteRouting('/', httpGET, 'text/plain', Controllers,
      Params, ResponseContentType, ResponseContentEncoding));
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
    CheckTrue(Router.ExecuteRouting('', httpGET, 'text/plain', Controllers,
      Params, ResponseContentType, ResponseContentEncoding));
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
