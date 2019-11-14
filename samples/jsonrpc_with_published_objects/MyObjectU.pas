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

unit MyObjectU;

interface

uses
  JsonDataObjects,
  Data.DB,
  BusinessObjectsU,
  FireDAC.Comp.Client,
  MVCFramework.Serializer.Commons,
  MVCFramework.Commons, MVCFramework;

type

  [MVCNameCase(ncLowerCase)]
  TMultiDataset = class
  private
    fCustomers2: TDataset;
    fCustomers1: TDataset;
  public
    property Customers: TDataset read fCustomers1 write fCustomers1;
    property People: TDataset read fCustomers2 write fCustomers2;
    destructor Destroy; override;
  end;

  TMyObject = class
  private
    function GetCustomersDataset: TFDMemTable;
    function GetPeopleDataset: TFDMemTable;
  public
    [MVCDoc('You know, returns aValue1 - aValue2')]
    function Subtract(aValue1, aValue2: Integer): Integer;
    [MVCDoc('Returns the revers of the string passed as input')]
    function ReverseString(const aString: string; const aUpperCase: Boolean): string;
    [MVCDoc('Returns the next monday starting from aDate')]
    function GetNextMonday(const aDate: TDate): TDate;
    function PlayWithDatesAndTimes(const aJustAFloat: Double; const aTime: TTime; const aDate: TDate;
      const aDateAndTime: TDateTime): TDateTime;
    function GetCustomers(aString: string): TDataset;
    function GetMulti: TMultiDataset;
    function GetStringDictionary: TMVCStringDictionary;
    function GetUser(aUserName: string): TPerson;
    function SavePerson(const aPerson: TJsonObject): Integer;
    function FloatsTest(const aDouble: Double; const aExtended: Extended): Extended;
    procedure DoSomething;
    function SaveObjectWithJSON(const WithJSON: TJsonObject): TJsonObject;
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

function TMyObject.FloatsTest(const aDouble: Double; const aExtended: Extended): Extended;
begin
  Result := aDouble + aExtended;
end;

function TMyObject.GetCustomers(aString: string): TDataset;
var
  lMT: TFDMemTable;
begin
  lMT := GetCustomersDataset;
  try
    if not aString.IsEmpty then
    begin
      lMT.Filter := aString;
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
  Result.Customers := GetCustomersDataset;
  Result.People := GetPeopleDataset;
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

function TMyObject.GetPeopleDataset: TFDMemTable;
var
  lMT: TFDMemTable;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('FirstName', ftString, 20);
    lMT.FieldDefs.Add('LastName', ftString, 20);
    lMT.Active := True;
    lMT.AppendRecord(['Daniele', 'Teti']);
    lMT.AppendRecord(['Peter', 'Parker']);
    lMT.AppendRecord(['Bruce', 'Banner']);
    lMT.AppendRecord(['Scott', 'Summers']);
    lMT.AppendRecord(['Sue', 'Storm']);
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
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

function TMyObject.PlayWithDatesAndTimes(const aJustAFloat: Double; const aTime: TTime; const aDate: TDate;
  const aDateAndTime: TDateTime): TDateTime;
begin
  Result := aDateAndTime + aDate + aTime + TDateTime(aJustAFloat);
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

function TMyObject.SavePerson(const aPerson: TJsonObject): Integer;
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

function TMyObject.Subtract(aValue1, aValue2: Integer): Integer;
begin
  Result := aValue1 - aValue2;
end;

{ TData }

destructor TMultiDataset.Destroy;
begin
  fCustomers1.Free;
  fCustomers2.Free;
  inherited;
end;

end.
