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

unit MyObjectU;

interface

uses
  JsonDataObjects,
  System.Generics.Collections,
  Data.DB,
  BusinessObjectsU,
  FireDAC.Comp.Client,
  MVCFramework.Serializer.Commons,
  MVCFramework.Commons, MVCFramework, MVCFramework.JSONRPC, CommonTypesU;

type

  TMyObject = class
  private
    function GetCustomersDataset: TFDMemTable;
    procedure FillCustomersDataset(const DataSet: TDataSet);
    // function GetPeopleDataset: TFDMemTable;
    procedure FillPeopleDataset(const DataSet: TDataSet);
  public
    procedure OnBeforeCallHook(const Context: TWebContext; const JSONRequest: TJDOJsonObject);
    procedure OnBeforeRoutingHook(const Context: TWebContext; const JSON: TJDOJsonObject);
    procedure OnAfterCallHook(const Context: TWebContext; const JSONResponse: TJDOJsonObject);
  public
    [MVCDoc('You know, returns aValue1 - aValue2')]
    function Subtract(Value1, Value2: Integer): Integer;
    [MVCDoc('Returns the revers of the string passed as input')]
    function ReverseString(const aString: string; const aUpperCase: Boolean): string;
    [MVCDoc('Returns the next monday starting from aDate')]
    function GetNextMonday(const aDate: TDate): TDate;
    function PlayWithDatesAndTimes(const aJustAFloat: Double; const aTime: TTime;
      const aDate: TDate; const aDateAndTime: TDateTime): TDateTime;
    [MVCJSONRPCAllowGET]
    function GetCustomers(FilterString: string): TDataSet;
    [MVCJSONRPCAllowGET]
    function GetMulti: TMultiDataset;
    [MVCJSONRPCAllowGET]
    function GetStringDictionary: TMVCStringDictionary;
    function GetUser(aUserName: string): TPerson;
    function SavePerson(const Person: TJsonObject): Integer;
    function FloatsTest(const aDouble: Double; const aExtended: Extended): Extended;
    procedure DoSomething;
    procedure RaiseCustomException;
    function RaiseGenericException(const ExceptionType: Integer): Integer;
    function SaveObjectWithJSON(const WithJSON: TJsonObject): TJsonObject;
    //enums and sets support
    function PassingEnums(Value1: TEnumTest; Value2: TEnumTest): TEnumTest;
    function GetSetBySet(Value: TSetTest): TSetTest;

    //records support
    function SavePersonRec(PersonRec: TTestRec): TTestRec;
    function GetPeopleRecDynArray: TTestRecDynArray;
    function GetPeopleRecStaticArray: TTestRecArray;
    function GetPersonRec: TTestRec;
    function GetComplex1: TNestedArraysRec;
    function EchoComplexArrayOfRecords(PeopleList: TTestRecDynArray): TTestRecDynArray;
    function EchoComplexArrayOfRecords2(VendorProxiesAndLinks: TNestedArraysRec): TNestedArraysRec;

    // invalid parameters modifiers
    procedure InvalidMethod1(var MyVarParam: Integer);
    procedure InvalidMethod2(out MyOutParam: Integer);

  end;

  TUtils = class sealed
    class function JSONObjectAs<T: constructor, class>(const JSON: TJsonObject): T;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils,
  System.DateUtils, MVCFramework.Serializer.JsonDataObjects;

class function TUtils.JSONObjectAs<T>(const JSON: TJsonObject): T;
var
  lObj: TObject;
  lSerializer: TMVCJsonDataObjectsSerializer;
begin
  lObj := T.Create;
  try
    lSerializer := TMVCJsonDataObjectsSerializer.Create;
    try
      lSerializer.JsonObjectToObject(JSON, lObj, TMVCSerializationType.stProperties, []);
    finally
      lSerializer.Free;
    end;
  except
    lObj.Free;
    raise;
  end;
  Result := T(lObj);
end;

{ TMyDerivedController }

procedure TMyObject.DoSomething;
begin

end;

function TMyObject.PassingEnums(Value1, Value2: TEnumTest): TEnumTest;
begin
  if Value1 = Value2 then
  begin
    Result := TEnumTest.ptEnumValue4;
  end
  else
  begin
    Result := TEnumTest.ptEnumValue3;
  end;
end;

function TMyObject.EchoComplexArrayOfRecords(
  PeopleList: TTestRecDynArray): TTestRecDynArray;
begin
  Result := PeopleList;
end;

function TMyObject.EchoComplexArrayOfRecords2(
  VendorProxiesAndLinks: TNestedArraysRec): TNestedArraysRec;
begin
  Result := VendorProxiesAndLinks;
  Result.TestRecProp.StringProp := VendorProxiesAndLinks.TestRecProp.StringProp + ' (changed from server)';
end;

procedure TMyObject.FillCustomersDataset(const DataSet: TDataSet);
begin
  DataSet.AppendRecord([1, 'Ford']);
  DataSet.AppendRecord([2, 'Ferrari']);
  DataSet.AppendRecord([3, 'Lotus']);
  DataSet.AppendRecord([4, 'FCA']);
  DataSet.AppendRecord([5, 'Hyundai']);
  DataSet.AppendRecord([6, 'De Tomaso']);
  DataSet.AppendRecord([7, 'Dodge']);
  DataSet.AppendRecord([8, 'Tesla']);
  DataSet.AppendRecord([9, 'Kia']);
  DataSet.AppendRecord([10, 'Tata']);
  DataSet.AppendRecord([11, 'Volkswagen']);
  DataSet.AppendRecord([12, 'Audi']);
  DataSet.AppendRecord([13, 'Skoda']);
  DataSet.First;
end;

procedure TMyObject.FillPeopleDataset(const DataSet: TDataSet);
begin
  DataSet.AppendRecord(['Daniele', 'Teti']);
  DataSet.AppendRecord(['Peter', 'Parker']);
  DataSet.AppendRecord(['Bruce', 'Banner']);
  DataSet.AppendRecord(['Scott', 'Summers']);
  DataSet.AppendRecord(['Sue', 'Storm']);
  DataSet.First;
end;

function TMyObject.FloatsTest(const aDouble: Double; const aExtended: Extended): Extended;
begin
  Result := aDouble + aExtended;
end;

function TMyObject.GetComplex1: TNestedArraysRec;
begin
  SetLength(Result.ArrayProp1, 2);
  SetLength(Result.ArrayProp2, 2);

  Result.ArrayProp1[0] := TTestRec.Create(1234);
  Result.ArrayProp1[1] := TTestRec.Create(2345);

  Result.ArrayProp2[0] := TTestRec.Create(3456);
  Result.ArrayProp2[1] := TTestRec.Create(4567);

end;

function TMyObject.GetCustomers(FilterString: string): TDataSet;
var
  lMT: TFDMemTable;
begin
  lMT := GetCustomersDataset;
  try
    if not FilterString.IsEmpty then
    begin
      lMT.Filter := FilterString;
      lMT.Filtered := True;
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function TMyObject.GetCustomersDataset: TFDMemTable;
var
  lMT: TFDMemTable;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('Code', ftInteger);
    lMT.FieldDefs.Add('Name', ftString, 20);
    lMT.Active := True;
    lMT.AppendRecord([1, 'Ford']);
    lMT.AppendRecord([2, 'Ferrari']);
    lMT.AppendRecord([3, 'Lotus']);
    lMT.AppendRecord([4, 'FCA']);
    lMT.AppendRecord([5, 'Hyundai']);
    lMT.AppendRecord([6, 'De Tomaso']);
    lMT.AppendRecord([7, 'Dodge']);
    lMT.AppendRecord([8, 'Tesla']);
    lMT.AppendRecord([9, 'Kia']);
    lMT.AppendRecord([10, 'Tata']);
    lMT.AppendRecord([11, 'Volkswagen']);
    lMT.AppendRecord([12, 'Audi']);
    lMT.AppendRecord([13, 'Skoda']);
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function TMyObject.GetMulti: TMultiDataset;
begin
  Result := TMultiDataset.Create;
  FillCustomersDataset(Result.Customers);
  FillPeopleDataset(Result.People);
end;

function TMyObject.GetNextMonday(const aDate: TDate): TDate;
var
  lDate: TDate;
begin
  lDate := aDate + 1;
  while DayOfTheWeek(lDate) <> 1 do
  begin
    lDate := lDate + 1;
  end;
  Result := lDate;
end;

function TMyObject.GetPeopleRecDynArray: TTestRecDynArray;
begin
  SetLength(Result, 2);
  Result[0] := TTestRec.Create(1);
  Result[1] := TTestRec.Create(2);
end;

function TMyObject.GetPeopleRecStaticArray: TTestRecArray;
begin
  Result[0] := TTestRec.Create(7);
  Result[1] := TTestRec.Create(8);
end;

function TMyObject.GetPersonRec: TTestRec;
begin
  Result := TTestRec.Create(99);
end;

function TMyObject.GetSetBySet(Value: TSetTest): TSetTest;
begin
  Result := [];
  for var lItem := ptEnumValue1 to ptEnumValue4 do
  begin
    if lItem in Value then
    begin
      Result := Result - [lItem];
    end
    else
    begin
      Result := Result + [lItem];
    end;
  end;
end;

function TMyObject.GetStringDictionary: TMVCStringDictionary;
begin
  Result := TMVCStringDictionary.Create;
  Result.Add('key1', 'value1');
  Result.Add('key2', 'value2');
  Result.Add('key3', 'value3');
  Result.Add('key4', 'value4');
end;

function TMyObject.GetUser(aUserName: string): TPerson;
begin
  Result := TPerson.Create;
  Result.FirstName := 'Daniele (a.k.a. ' + aUserName + ')';
  Result.LastName := 'Teti';
  Result.DOB := EncodeDate(1932, 11, 4); // hey, it is a joke :-)
  Result.Married := True;
end;

procedure TMyObject.InvalidMethod1(var MyVarParam: Integer);
begin
  // do nothing
end;

procedure TMyObject.InvalidMethod2(out MyOutParam: Integer);
begin
  // do nothing
end;

function TMyObject.PlayWithDatesAndTimes(const aJustAFloat: Double; const aTime: TTime;
  const aDate: TDate; const aDateAndTime: TDateTime): TDateTime;
begin
  Result := aDateAndTime + aDate + aTime + TDateTime(aJustAFloat);
end;

procedure TMyObject.RaiseCustomException;
begin
  raise EMVCJSONRPCError.Create(JSONRPC_USER_ERROR + 1, 'This is an exception message');
end;

function TMyObject.RaiseGenericException(const ExceptionType: Integer): Integer;
var
  l: Integer;
begin
  case ExceptionType of
    1:
      begin
        l := 0;
        Result := 10 div l;
      end;
    2:
      begin
        raise EInvalidPointer.Create('Fake Invalid Pointer Operation');
      end;
    else
    begin
      raise Exception.Create('BOOOOM!');
    end;
  end;
end;

function TMyObject.ReverseString(const aString: string; const aUpperCase: Boolean): string;
begin
  Result := System.StrUtils.ReverseString(aString);
  if aUpperCase then
    Result := Result.ToUpper;
end;

function TMyObject.SaveObjectWithJSON(const WithJSON: TJsonObject): TJsonObject;
var
  lObj: TObjectWithJSONObject;
begin
  lObj := TUtils.JSONObjectAs<TObjectWithJSONObject>(WithJSON);
  try
    LogD(lObj);
    Result := WithJSON.Clone as TJsonObject;
  finally
    lObj.Free;
  end;
end;

function TMyObject.SavePerson(const Person: TJsonObject): Integer;
// var
// lPerson: TPerson;
begin
  // lPerson := JSONObjectAs<TPerson>(aPerson);
  // try
  // // do something with lPerson
  // finally
  // lPerson.Free;
  // end;

  // this maybe the id of the newly created person
  Result := Random(1000);
end;

function TMyObject.SavePersonRec(PersonRec: TTestRec): TTestRec;
begin
  Result := PersonRec;
end;

function TMyObject.Subtract(Value1, Value2: Integer): Integer;
begin
  Result := Value1 - Value2;
end;

{ TMyObjectWithHooks }

procedure TMyObject.OnBeforeCallHook(const Context: TWebContext; const JSONRequest: TJDOJsonObject);
begin
  Log.Info('TMyObjectWithHooks.OnBeforeCallHook >> ', 'jsonrpc');
  Log.Info(JSONRequest.ToJSON(False), 'jsonrpc');
  Log.Info('TMyObjectWithHooks.OnBeforeCallHook << ', 'jsonrpc');
end;

procedure TMyObject.OnBeforeRoutingHook(const Context: TWebContext; const JSON: TJDOJsonObject);
begin
  Log.Info('TMyObjectWithHooks.OnBeforeRoutingHook >> ', 'jsonrpc');
  Log.Info(JSON.ToJSON(False), 'jsonrpc');
  Log.Info('TMyObjectWithHooks.OnBeforeRoutingHook << ', 'jsonrpc');
end;

procedure TMyObject.OnAfterCallHook(const Context: TWebContext; const JSONResponse: TJDOJsonObject);
begin
  Log.Info('TMyObjectWithHooks.OnAfterCallHook >> ', 'jsonrpc');
  Log.Info(JSONResponse.ToJSON(False), 'jsonrpc');
  Log.Info('TMyObjectWithHooks.OnAfterCallHook << ', 'jsonrpc');
end;

end.
