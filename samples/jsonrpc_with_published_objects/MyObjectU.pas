// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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
  BusinessObjectsU;

type
  TMyObject = class
  public
    function Subtract(aValue1, aValue2: Integer): Integer;
    function ReverseString(aString: string): string;
    function GetNextMonday(const aDate: TDate): TDate;
    function GetCustomers(aString: string): TDataSet;
    function GetUser(aUserName: string): TPerson;
    function SavePerson(const aPerson: TJsonObject): Integer;
    procedure DoSomething;
    // invalid parameters modifiers
    procedure InvalidMethod1(var MyVarParam: Integer);
    procedure InvalidMethod2(out MyOutParam: Integer);

  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils,
  FireDAC.Comp.Client,
  System.DateUtils;

{ TMyDerivedController }

procedure TMyObject.DoSomething;
begin

end;

function TMyObject.GetCustomers(aString: string): TDataSet;
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

function TMyObject.ReverseString(aString: string): string;
begin
  Result := System.StrUtils.ReverseString(aString);
end;

function TMyObject.SavePerson(const aPerson: TJsonObject): Integer;
var
  lPerson: TPerson;
begin
//  lPerson := JSONObjectAs<TPerson>(aPerson);
//  try
//    // do something with lPerson
//  finally
//    lPerson.Free;
//  end;

  // this maybe the id of the newly created person
  Result := Random(1000);
end;

function TMyObject.Subtract(aValue1, aValue2: Integer): Integer;
begin
  Result := aValue1 - aValue2;
end;

end.
