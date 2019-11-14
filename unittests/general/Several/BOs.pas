// ***************************************************************************
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

unit BOs;

interface

uses
  system.TimeSpan, system.SysUtils, generics.collections, system.Classes,
  system.Rtti, MVCFramework.Serializer.Commons, JsonDataObjects;

type
  TMyObject = class
  private
    FPropString: string;
    FPropAnsiString: AnsiString;
    FPropInt64: Int64;
    FPropUInt32: cardinal;
    FPropUInt64: UInt64;
    FPropUInt16: word;
    FPropInt16: smallint;
    FPropBoolean: boolean;
    FPropDateTime: TDateTime;
    FPropDate: TDate;
    FPropInteger: Integer;
    FPropTimeStamp: TTimeStamp;
    FPropTime: TTime;
    FPropCurrency: Currency;
    fPropJSONObject: TJSONObject;
    procedure SetPropAnsiString(const Value: AnsiString);
    procedure SetPropString(const Value: string);
    procedure SetPropInt64(const Value: Int64);
    procedure SetPropUInt32(const Value: cardinal);
    procedure SetPropUInt64(const Value: UInt64);
    procedure SetPropInt16(const Value: smallint);
    procedure SetPropUInt16(const Value: word);
    procedure SetPropBoolean(const Value: boolean);
    procedure SetPropDate(const Value: TDate);
    procedure SetPropDateTime(const Value: TDateTime);
    procedure SetPropInteger(const Value: Integer);
    procedure SetPropTimeStamp(const Value: TTimeStamp);
    procedure SetPropTime(const Value: TTime);
    procedure SetPropCurrency(const Value: Currency);
  public
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TMyObject): boolean; reintroduce;
    property PropString: string read FPropString write SetPropString;
    property PropAnsiString: AnsiString read FPropAnsiString
      write SetPropAnsiString;
    property PropInteger: Integer read FPropInteger write SetPropInteger;
    property PropUInt32: cardinal read FPropUInt32 write SetPropUInt32;
    property PropInt64: Int64 read FPropInt64 write SetPropInt64;
    property PropUInt64: UInt64 read FPropUInt64 write SetPropUInt64;
    property PropUInt16: word read FPropUInt16 write SetPropUInt16;
    property PropInt16: smallint read FPropInt16 write SetPropInt16;
    property PropBoolean: boolean read FPropBoolean write SetPropBoolean;
    property PropDate: TDate read FPropDate write SetPropDate;
    property PropTime: TTime read FPropTime write SetPropTime;
    property PropDateTime: TDateTime read FPropDateTime write SetPropDateTime;
    property PropTimeStamp: TTimeStamp read FPropTimeStamp
      write SetPropTimeStamp;
    property PropCurrency: Currency read FPropCurrency write SetPropCurrency;
    property PropJSONObject: TJSONObject read fPropJSONObject;
  end;

  TMyChildObject = class
  private
    FMyChildProperty1: string;
    procedure SetMyChildProperty1(const Value: string);
  public
    constructor Create;
    property MyChildProperty1: string read FMyChildProperty1
      write SetMyChildProperty1;
  end;

  TMyObjectWithTValue = class
  private
    FValueAsString: TValue;
    FValueAsInteger: TValue;
    FValue1: TValue;
    FValue2: TValue;
    FValue3: TValue;
    FValue4: TValue;
    FValue5: TValue;
    procedure SetValueAsInteger(const Value: TValue);
    procedure SetValueAsString(const Value: TValue);
  public
    function Equals(Obj: TObject): boolean; reintroduce;
    // [TValueAsType(TypeInfo(String))]
    property ValueAsString: TValue read FValueAsString write SetValueAsString;
    // [TValueAsType(TypeInfo(Integer))]
    property ValueAsInteger: TValue read FValueAsInteger write SetValueAsInteger;
    property Value1: TValue read FValue1 write FValue1;
    property Value2: TValue read FValue2 write FValue2;
    property Value3: TValue read FValue3 write FValue3;
    property Value4: TValue read FValue4 write FValue4;
    property Value5: TValue read FValue5 write FValue5;
  end;

  [MVCListOf(TMyChildObject)]
  TMyChildObjectList = class(TObjectList<TMyChildObject>)
  end;

  TMyComplexObject = class
  private
    FProp1: string;
    FChildObjectList: TMyChildObjectList;
    FChildObject: TMyChildObject;
    FPropStringList: TStringList;
    procedure SetChildObject(const Value: TMyChildObject);
    procedure SetChildObjectList(const Value: TMyChildObjectList);
    procedure SetProp1(const Value: string);
    procedure SetPropStringList(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;

    property Prop1: string read FProp1 write SetProp1;
    property PropStringList: TStringList read FPropStringList write SetPropStringList;
    property ChildObject: TMyChildObject read FChildObject write SetChildObject;
    property ChildObjectList: TMyChildObjectList read FChildObjectList
      write SetChildObjectList;
  end;

  TMyStreamObject = class(TObject)
  private
    FPropStream: TStream;
    FProp8Stream: TStream;
    FImageStream: TStream;
    procedure SetPropStream(const Value: TStream);
    procedure SetProp8Stream(const Value: TStream);
    procedure SetImageStream(const Value: TStream);
  public
    function Equals(Obj: TMyStreamObject): boolean; reintroduce;
    constructor Create;
    destructor Destroy; override;
    [MVCSerializeAsString]
    property PropStream: TStream read FPropStream write SetPropStream;
    [MVCSerializeAsString]
    // utf-8 is default
    property Prop8Stream: TStream read FProp8Stream write SetProp8Stream;
    property ImageStream: TStream read FImageStream write SetImageStream;
  end;

  TMyObjectWithLogic = class
  private
    FLastName: string;
    FFirstName: string;
    FAge: Integer;
    function GetFullName: string;
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function GetIsAdult: boolean;
    procedure SetAge(const Value: Integer);
  public
    constructor Create(aFirstName, aLastName: string; aAge: Integer);
    function Equals(Obj: TObject): boolean; override;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property IsAdult: boolean read GetIsAdult;
    property FullName: string read GetFullName;
  end;

  TMyClass = class
  private
    FID: Integer;
    FDescription: string;
    procedure SetId(ID: Integer);
    procedure SetDescription(Description: string);
  public
    function Equals(Obj: TObject): boolean; override;
    property ID: Integer read FID write SetId;
    property Description: string read FDescription write SetDescription;
    constructor Create(ID: Integer; Description: string); overload;
  end;

  IMyInterface = interface
    ['{B36E786B-5871-4211-88AD-365B453DC408}']
    function GetID: Integer;
    function GetDescription: string;
  end;

  TMyIntfObject = class(TInterfacedObject, IMyInterface)
  private
    FID: Integer;
    FValue: string;
  public
    constructor Create(const ID: Integer; const Value: string);
    function GetDescription: string;
    function GetID: Integer;
  end;

  TResponseWrapper<T: class> = class
  private
    FTotalItems: Integer;
    FItems: TObjectList<T>;
    procedure SetTotalItems(TotalItems: Integer);
    procedure SetItems(aItems: TObjectList<T>);
  public
    property TotalItems: Integer read FTotalItems write SetTotalItems;
    property Items: TObjectList<T> read FItems write SetItems;
    constructor Create(TotalItems: Integer; aItems: TObjectList<T>); overload;
    destructor Destroy; override;
  end;

  TObjectWithCustomType = class
  private
    FPropStringList: TStringList;
    FPropStringArray: TArray<string>;
    FPropStreamAsBASE64: TStringStream;
    FPropStreamAsString: TStringStream;
    procedure SetPropStringList(const Value: TStringList);
    procedure SetPropStringArray(const Value: TArray<string>);
    procedure SetPropStreamAsBASE64(const Value: TStringStream);
    procedure SetPropStreamAsString(const Value: TStringStream);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    property PropStringList: TStringList read FPropStringList write SetPropStringList;
    property PropStringArray: TArray<string> read FPropStringArray write SetPropStringArray;
    [MVCSerializeAsString]
    property PropStreamAsString: TStringStream read FPropStreamAsString write SetPropStreamAsString;
    property PropStreamAsBASE64: TStringStream read FPropStreamAsBASE64 write SetPropStreamAsBASE64;
  end;

function GetMyObject: TMyObject;
function GetMyObjectWithTValue: TMyObjectWithTValue;
function GetMyObjectWithStream: TMyStreamObject;
function GetMyComplexObject: TMyComplexObject;
function GetMyComplexObjectWithNotInitializedChilds: TMyComplexObject;
function GetMyObjectWithCustomType: TObjectWithCustomType;

const
  BASE64_STRING: AnsiString = 'This is serialized as BASE64 String';

implementation

uses
  system.DateUtils, Winapi.Windows;

function GetMyObjectWithCustomType: TObjectWithCustomType;
begin
  Result := TObjectWithCustomType.Create;
  Result.PropStringList.Add('item 1');
  Result.PropStringList.Add('item 2');
  Result.PropStringList.Add('item 3');
  Result.PropStringList.Add('item 4');
  Result.PropStringArray := ['item 1', 'item 2', 'item 3', 'item 4'];
  Result.PropStreamAsString.WriteString('This is a string');
  Result.PropStreamAsBASE64.WriteString('This is a BASE64 string');
end;

function GetMyComplexObjectWithNotInitializedChilds: TMyComplexObject;
begin
  Result := TMyComplexObject.Create;
  Result.Prop1 := 'property1';
end;

function GetMyObjectWithTValue: TMyObjectWithTValue;
begin
  Result := TMyObjectWithTValue.Create;
  Result.ValueAsString := 'Value As String';
  Result.ValueAsInteger := 1234;
  Result.Value1 := 'Hello World';
  Result.Value2 := 1234;
  Result.Value3 := true;
  Result.Value4 := 1234.5678;
  // VALUE5 must be a floting number bacause of equals check
  Result.Value5 := EncodeDateTime(2004, 6, 7, 11, 30, 0, 0);
end;

function GetMyComplexObject: TMyComplexObject;
var
  co: TMyChildObject;
begin
  Result := TMyComplexObject.Create;
  Result.Prop1 := 'property1';
  Result.PropStringList := TStringList.Create;
  Result.PropStringList.Add('item 1');
  Result.PropStringList.Add('item 2');
  Result.PropStringList.Add('item 3');
  Result.PropStringList.Add('item 4');
  Result.ChildObject.MyChildProperty1 := 'MySingleChildProperty1';
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty1';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty2';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty3';
  Result.ChildObjectList.Add(co);
  co := TMyChildObject.Create;
  co.MyChildProperty1 := 'MyChildProperty4';
  Result.ChildObjectList.Add(co);
end;

function GetMyObjectWithStream: TMyStreamObject;
var
  Buff: TBytes;
begin
  Result := TMyStreamObject.Create;
  TStringStream(Result.PropStream).WriteString('This is an UTF16 String');
  TStringStream(Result.Prop8Stream).WriteString('This is an UTF8 String');
  SetLength(Buff, Length(BASE64_STRING));
  MoveMemory(Buff, PAnsiChar(AnsiString(BASE64_STRING)), Length(BASE64_STRING));
  TMemoryStream(Result.ImageStream).Write(Buff, Length(Buff));
end;

function GetMyObject: TMyObject;
begin
  Result := TMyObject.Create;
  Result.PropString := 'Some text אטילעש';
  Result.PropAnsiString := 'This is an ANSI text';
  Result.PropInteger := -1234;
  Result.PropUInt32 := 1234;
  Result.PropInt64 := -1234567890;
  Result.PropUInt64 := 1234567890;
  Result.PropUInt16 := 12345;
  Result.PropInt16 := -12345;
  Result.PropCurrency := 1234.5678;
  Result.PropBoolean := true;
  Result.PropDate := EncodeDate(2010, 10, 20);
  Result.PropTime := EncodeTime(10, 20, 30, 40);
  Result.PropDateTime := Result.PropDate + Result.PropTime;
  Result.PropTimeStamp := DateTimeToTimeStamp(Result.PropDateTime + 1);
  Result.PropJSONObject.S['stringprop1'] := 'This is a string prop';
  Result.PropJSONObject.I['intprop1'] := 1234;
  Result.PropJSONObject.A['arrprop'].Add(1234);
  Result.PropJSONObject.A['arrprop'].Add('Hello World');
  Result.PropJSONObject.O['objprop'].S['innerprop1'] := 'value1';
  Result.PropJSONObject.O['objprop'].S['innerprop2'] := 'value2';
  Result.PropJSONObject.O['objprop'].S['innerprop3'] := 'value3';
end;

constructor TMyObject.Create;
begin
  inherited;
  fPropJSONObject := TJsonObject.Create;
end;

destructor TMyObject.Destroy;
begin
  fPropJSONObject.Free;
  inherited;
end;

function TMyObject.Equals(Obj: TMyObject): boolean;
begin
  Result := true;
  Result := Result and (Self.PropString = Obj.PropString);
  Result := Result and (Self.PropAnsiString = Obj.PropAnsiString);
  Result := Result and (Self.PropInteger = Obj.PropInteger);
  Result := Result and (Self.PropUInt32 = Obj.PropUInt32);
  Result := Result and (Self.PropInt64 = Obj.PropInt64);
  Result := Result and (Self.PropUInt64 = Obj.PropUInt64);
  Result := Result and (Self.PropUInt16 = Obj.PropUInt16);
  Result := Result and (Self.PropInt16 = Obj.PropInt16);
  Result := Result and (Self.PropBoolean = Obj.PropBoolean);
  Result := Result and (Self.PropDate = Obj.PropDate);
  Result := Result and (Self.PropCurrency = Obj.PropCurrency);
  Result := Result and (SecondsBetween(Self.PropTime, Obj.PropTime) = 0);
  Result := Result and (SecondsBetween(Self.PropDateTime,
    Obj.PropDateTime) = 0);
  Result := Result and (Self.PropTimeStamp.Date = Obj.PropTimeStamp.Date) and
    (Self.PropTimeStamp.Time = Obj.PropTimeStamp.Time);
  Result := Result and (Self.fPropJSONObject.ToJSON() = Obj.PropJSONObject.ToJSON());
end;

procedure TMyObject.SetPropAnsiString(const Value: AnsiString);
begin
  FPropAnsiString := Value;
end;

procedure TMyObject.SetPropBoolean(const Value: boolean);
begin
  FPropBoolean := Value;
end;

procedure TMyObject.SetPropCurrency(const Value: Currency);
begin
  FPropCurrency := Value;
end;

procedure TMyObject.SetPropDate(const Value: TDate);
begin
  FPropDate := Value;
end;

procedure TMyObject.SetPropDateTime(const Value: TDateTime);
begin
  FPropDateTime := Value;
end;

procedure TMyObject.SetPropInt16(const Value: smallint);
begin
  FPropInt16 := Value;
end;

procedure TMyObject.SetPropInt64(const Value: Int64);
begin
  FPropInt64 := Value;
end;

procedure TMyObject.SetPropInteger(const Value: Integer);
begin
  FPropInteger := Value;
end;

procedure TMyObject.SetPropString(const Value: string);
begin
  FPropString := Value;
end;

procedure TMyObject.SetPropTime(const Value: TTime);
begin
  FPropTime := Value;
end;

procedure TMyObject.SetPropTimeStamp(const Value: TTimeStamp);
begin
  FPropTimeStamp := Value;
end;

procedure TMyObject.SetPropUInt16(const Value: word);
begin
  FPropUInt16 := Value;
end;

procedure TMyObject.SetPropUInt32(const Value: cardinal);
begin
  FPropUInt32 := Value;
end;

procedure TMyObject.SetPropUInt64(const Value: UInt64);
begin
  FPropUInt64 := Value;
end;

{ TMyComplexObject }

constructor TMyComplexObject.Create;
begin
  inherited;
  FChildObjectList := TMyChildObjectList.Create(true);
  FChildObject := TMyChildObject.Create;
end;

destructor TMyComplexObject.Destroy;
begin
  FChildObjectList.Free;
  FChildObject.Free;
  FPropStringList.Free;
  inherited;
end;

function TMyComplexObject.Equals(Obj: TObject): boolean;
var
  co: TMyComplexObject;
begin
  co := Obj as TMyComplexObject;

  Result := co.Prop1 = Self.Prop1;
  if Assigned(co.ChildObject) and Assigned(Self.ChildObject) then
    Result := Result and
      (co.ChildObject.MyChildProperty1 = Self.ChildObject.MyChildProperty1)
  else
    Result := Result and (not Assigned(co.ChildObject)) and
      (not Assigned(Self.ChildObject));
  Result := Result and (co.ChildObjectList.Count = Self.ChildObjectList.Count);
  if co.ChildObjectList.Count = 0 then
    Exit;

  Result := Result and
    (co.ChildObjectList[0].MyChildProperty1 = Self.ChildObjectList[0]
    .MyChildProperty1);
  Result := Result and
    (co.ChildObjectList[1].MyChildProperty1 = Self.ChildObjectList[1]
    .MyChildProperty1);
  Result := Result and
    (co.ChildObjectList[2].MyChildProperty1 = Self.ChildObjectList[2]
    .MyChildProperty1);
  Result := Result and
    (co.ChildObjectList[3].MyChildProperty1 = Self.ChildObjectList[3]
    .MyChildProperty1);
end;

procedure TMyComplexObject.SetChildObject(const Value: TMyChildObject);
begin
  FChildObject := Value;
end;

procedure TMyComplexObject.SetChildObjectList(const Value: TMyChildObjectList);
begin
  FChildObjectList := Value;
end;

procedure TMyComplexObject.SetProp1(const Value: string);
begin
  FProp1 := Value;
end;

procedure TMyComplexObject.SetPropStringList(const Value: TStringList);
begin
  FPropStringList := Value;
end;

{ TMyChildObject }

constructor TMyChildObject.Create;
begin
  inherited;
  Self.MyChildProperty1 := 'my default value';
end;

procedure TMyChildObject.SetMyChildProperty1(const Value: string);
begin
  FMyChildProperty1 := Value;
end;

{ TMyStreamObject }

constructor TMyStreamObject.Create;
begin
  inherited Create;
  FPropStream := TStringStream.Create('', TEncoding.Unicode);
  FProp8Stream := TStringStream.Create('', TEncoding.UTF8);
  FImageStream := TMemoryStream.Create;
end;

destructor TMyStreamObject.Destroy;
begin
  FPropStream.Free;
  FProp8Stream.Free;
  FImageStream.Free;
  inherited;
end;

function TMyStreamObject.Equals(Obj: TMyStreamObject): boolean;
var
  lPMemSelf: PByte;
  lPMemOther: PByte;
  I: Integer;
begin
  Result := true;
  Result := Result and (TStringStream(Self.PropStream).DataString = TStringStream(Obj.PropStream).DataString);
  Result := Result and (TStringStream(Self.Prop8Stream).DataString = TStringStream(Obj.Prop8Stream).DataString);
  Result := Result and (Self.ImageStream.Size = Obj.ImageStream.Size);

  if Result then
  begin
    lPMemSelf := TMemoryStream(Self.ImageStream).Memory;
    lPMemOther := TMemoryStream(Obj.ImageStream).Memory;
    for I := 0 to Self.ImageStream.Size - 1 do
    begin
      Result := Result and (lPMemSelf^ = lPMemOther^);
      Inc(lPMemSelf);
      Inc(lPMemOther);
    end;
  end;
end;

procedure TMyStreamObject.SetImageStream(const Value: TStream);
begin
  FImageStream := Value;
end;

procedure TMyStreamObject.SetProp8Stream(const Value: TStream);
begin
  if Assigned(FProp8Stream) then
    FreeAndNil(FProp8Stream);
  FProp8Stream := Value;
end;

procedure TMyStreamObject.SetPropStream(const Value: TStream);
begin
  if Assigned(FPropStream) then
    FPropStream.Free;
  FPropStream := Value;
end;

{ TCliente }

{ TMyObjectWithLogic }

constructor TMyObjectWithLogic.Create(aFirstName, aLastName: string;
  aAge: Integer);
begin
  inherited Create;
  FFirstName := aFirstName;
  FLastName := aLastName;
  FAge := aAge;
end;

function TMyObjectWithLogic.Equals(Obj: TObject): boolean;
var
  lOther: TMyObjectWithLogic;
begin
  Result := (Obj is TMyObjectWithLogic);
  if Result then
  begin
    lOther := TMyObjectWithLogic(Obj);
    Result := Result and (Self.FirstName = lOther.FirstName);
    Result := Result and (Self.LastName = lOther.LastName);
    Result := Result and (Self.Age = lOther.Age);
  end;
end;

function TMyObjectWithLogic.GetFullName: string;
begin
  Result := FirstName + ' ' + LastName; // logic
end;

function TMyObjectWithLogic.GetIsAdult: boolean;
begin
  Result := Age >= 18; // logic
end;

procedure TMyObjectWithLogic.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TMyObjectWithLogic.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TMyObjectWithLogic.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TResponseGrid<T> }

constructor TResponseWrapper<T>.Create(TotalItems: Integer; aItems: TObjectList<T>);
begin
  inherited Create;
  Self.SetTotalItems(TotalItems);
  Self.SetItems(aItems);
  aItems.OwnsObjects := true;
end;

destructor TResponseWrapper<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TResponseWrapper<T>.SetItems(aItems: TObjectList<T>);
begin
  Self.FItems := aItems;
end;

procedure TResponseWrapper<T>.SetTotalItems(TotalItems: Integer);
begin
  Self.FTotalItems := TotalItems;
end;

{ TMyClass }

constructor TMyClass.Create(ID: Integer; Description: string);
begin
  inherited Create;
  Self.SetId(ID);
  Self.SetDescription(Description);
end;

function TMyClass.Equals(Obj: TObject): boolean;
begin
  Result := Obj is TMyClass;
  if Result then
  begin
    Result := Result and (TMyClass(Obj).ID = ID);
    Result := Result and (TMyClass(Obj).Description = Description);
  end;
end;

procedure TMyClass.SetDescription(Description: string);
begin
  Self.FDescription := Description;
end;

procedure TMyClass.SetId(ID: Integer);
begin
  Self.FID := ID;
end;

{ TMyObjectWithTValue }

function TMyObjectWithTValue.Equals(Obj: TObject): boolean;
var
  lOther: TMyObjectWithTValue;
begin
  Result := Obj is TMyObjectWithTValue;
  if Result then
  begin
    lOther := TMyObjectWithTValue(Obj);
    Result := Result and (Self.ValueAsString.AsString = lOther.ValueAsString.AsString);
    Result := Result and (Self.ValueAsInteger.AsInteger = lOther.ValueAsInteger.AsInteger);
    Result := Result and (Self.Value1.AsVariant = lOther.Value1.AsVariant);
    Result := Result and (Self.Value2.AsVariant = lOther.Value2.AsVariant);
    Result := Result and (Self.Value3.AsVariant = lOther.Value3.AsVariant);
    Result := Result and (Self.Value4.AsVariant = lOther.Value4.AsVariant);
    Result := Result and (Trunc(Self.Value5.AsVariant * 100000) = Trunc(lOther.Value5.AsVariant * 100000));
  end;
end;

procedure TMyObjectWithTValue.SetValueAsInteger(const Value: TValue);
begin
  FValueAsInteger := Value;
end;

procedure TMyObjectWithTValue.SetValueAsString(const Value: TValue);
begin
  FValueAsString := Value;
end;

{ TMyIntfObject }

constructor TMyIntfObject.Create(const ID: Integer; const Value: string);
begin
  inherited Create;
  FID := ID;
  FValue := Value;
end;

function TMyIntfObject.GetDescription: string;
begin
  Result := FValue;
end;

function TMyIntfObject.GetID: Integer;
begin
  Result := FID;
end;

{ TObjectWithCustomType }

constructor TObjectWithCustomType.Create;
begin
  inherited;
  FPropStringList := TStringList.Create;
  FPropStreamAsString := TStringStream.Create;
  FPropStreamAsBASE64 := TStringStream.Create;
end;

destructor TObjectWithCustomType.Destroy;
begin
  FPropStringList.Free;
  FPropStreamAsString.Free;
  FPropStreamAsBASE64.Free;
  inherited;
end;

function TObjectWithCustomType.Equals(Obj: TObject): boolean;
var
  lOther: TObjectWithCustomType;
begin
  if not(Obj is TObjectWithCustomType) then
    Exit(false);
  lOther := TObjectWithCustomType(Obj);
  Result := true;
  Result := Result and (lOther.PropStringList.Text = Self.PropStringList.Text);
  Result := Result and (string.Join(',', lOther.PropStringArray) = string.Join(',', Self.PropStringArray));
  Result := Result and (lOther.PropStreamAsString.DataString = PropStreamAsString.DataString);
  Result := Result and (lOther.PropStreamAsBASE64.DataString = PropStreamAsBASE64.DataString);
end;

procedure TObjectWithCustomType.SetPropStreamAsBASE64(
  const Value: TStringStream);
begin
  FPropStreamAsBASE64 := Value;
end;

procedure TObjectWithCustomType.SetPropStreamAsString(
  const Value: TStringStream);
begin
  FPropStreamAsString := Value;
end;

procedure TObjectWithCustomType.SetPropStringArray(const Value: TArray<string>);
begin
  FPropStringArray := Value;
end;

procedure TObjectWithCustomType.SetPropStringList(const Value: TStringList);
begin
  FPropStringList := Value;
end;

end.
