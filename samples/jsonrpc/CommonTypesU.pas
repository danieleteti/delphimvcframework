unit CommonTypesU;

interface

uses
  MVCFramework.Commons, MVCFramework.Serializer.Commons;

type
  TEnumTest = (ptEnumValue1, ptEnumValue2, ptEnumValue3, ptEnumValue4);
  TSetTest = set of TEnumTest;

  [MVCNameCase(ncCamelCase)]
  TNestedRec = record
    StringProp: String;
    IntegerProp: Integer;
    BooleanProp: Boolean;
    EnumProp: TEnumTest;
    SetProp: TSetTest;
    ArrOfStringsProp: TArray<String>;
    ArrOfIntegersProp: TArray<Integer>;
    ArrOfBooleansProp: TArray<Boolean>;
    ArrOfDateProp: TArray<TDate>;
    ArrOfTimeProp: TArray<TTime>;
    ArrOfDateTimeProp: TArray<TDateTime>;
    constructor Create(Value: Integer);
    function ToString: String;
  end;

  [MVCNameCase(ncCamelCase)]
  TTestRec = record
    StringProp: String;
    IntegerProp: Integer;
    BooleanProp: Boolean;
    EnumProp: TEnumTest;
    SetProp: TSetTest;
    ArrOfStringsProp: TArray<String>;
    ArrOfIntegersProp: TArray<Integer>;
    ArrOfBooleansProp: TArray<Boolean>;
    ArrOfDateProp: TArray<TDate>;
    ArrOfTimeProp: TArray<TTime>;
    ArrOfDateTimeProp: TArray<TDateTime>;
    NestedRecProp: TNestedRec;
    function ToString: String;
    constructor Create(Value: Integer);
  end;

  TTestRecDynArray = TArray<TTestRec>;

  TTestRecArray = array [0 .. 1] of TTestRec;

  TNestedArraysRec = record
    TestRecProp: TTestRec;
    ArrayProp1: TArray<TTestRec>;
    ArrayProp2: TArray<TTestRec>;
    function ToString: String;
  end;

implementation

uses
  System.SysUtils, System.TypInfo;

{ TPersonRec }

constructor TTestRec.Create(Value: Integer) ;
begin
  StringProp := 'StringProp' + Value.ToString;
  IntegerProp := Value;
  BooleanProp := True;
  EnumProp := TEnumTest(Value mod 3);
  SetProp := [TEnumTest(Value mod 3), TEnumTest((Value+1) mod 3)];
  ArrOfStringsProp := ['ArrOfStringsProp' + Value.ToString, 'ArrOfStringsProp' + Value.ToString];
  ArrOfIntegersProp := [Value mod 3, (Value + 1 ) mod 3,  (Value + 2 ) mod 3];
  ArrOfBooleansProp := [((Value mod 3) = 1), ((Value + 1) mod 3 = 1), ((Value + 2) mod 3 = 1)];
  ArrOfDateProp := [
    EncodeDate(2022,(Value mod 11)+1, Value mod 28),
    EncodeDate(2022,((Value+1) mod 11)+1, (Value+1) mod 28),
    EncodeDate(2022,((Value+2) mod 11)+1, (Value+2) mod 28)
  ];
  ArrOfTimeProp := [
    EncodeTime(Value mod 24, Value mod 60, Value mod 60, 0),
    EncodeTime((Value + 1) mod 24, (Value + 1) mod 60, (Value + 1) mod 60, 0),
    EncodeTime((Value + 2) mod 24, (Value + 2) mod 60, (Value + 2) mod 60, 0)
  ];
  ArrOfDateTimeProp := [
    ArrOfDateProp[0] + ArrOfTimeProp[0],
    ArrOfDateProp[1] + ArrOfTimeProp[1],
    ArrOfDateProp[2] + ArrOfTimeProp[2]
  ];
  NestedRecProp := TNestedRec.Create(Value + 1);
end;

function TTestRec.ToString: String;
  function SetPropAsString: String;
  var
    lEl: TEnumTest;
  begin
    for lEl in SetProp do
    begin
      Result := Result + GetEnumName(TypeInfo(TEnumTest), Ord(lEl)) + ',';
    end;
    Result := Result.Remove(Result.Length - 1);
  end;
var
  I: Integer;
begin
  Result :=
    'StringProp = ' + self.StringProp + sLineBreak +
    'IntegerProp = ' + self.IntegerProp.ToString + sLineBreak +
    'BooleanProp = ' + self.BooleanProp.ToString(TUseBoolStrs.True) + sLineBreak +
    'EnumProp = ' + GetEnumName(TypeInfo(TEnumTest), Ord(EnumProp)) + sLineBreak +
    'SetProp = ' + SetPropAsString + sLineBreak;

  Result := Result + 'ArrOfStringsProp = ';
  for I := Low(ArrOfStringsProp) to High(ArrOfStringsProp) do
  begin
    Result := Result + ArrOfStringsProp[I] + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfIntegersProp = ';
  for I := Low(ArrOfIntegersProp) to High(ArrOfIntegersProp) do
  begin
    Result := Result + ArrOfIntegersProp[I].ToString() + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfBooleansProp = ';
  for I := Low(ArrOfBooleansProp) to High(ArrOfBooleansProp) do
  begin
    Result := Result + ArrOfBooleansProp[I].ToString(TUseBoolStrs.True) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfDateProp = ';
  for I := Low(ArrOfDateProp) to High(ArrOfDateProp) do
  begin
    Result := Result + DateToStr(ArrOfDateProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfTimeProp = ';
  for I := Low(ArrOfTimeProp) to High(ArrOfTimeProp) do
  begin
    Result := Result + TimeToStr(ArrOfTimeProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfDateTimeProp = ';
  for I := Low(ArrOfDateTimeProp) to High(ArrOfDateTimeProp) do
  begin
    Result := Result + DateTimeToStr(ArrOfDateTimeProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'NestedRecProp **> ' + sLineBreak;
  Result := Result + NestedRecProp.ToString();
end;

{ TChildRec }

constructor TNestedRec.Create(Value: Integer);
begin
  StringProp := 'StringProp' + Value.ToString;
  IntegerProp := Value;
  BooleanProp := True;
  EnumProp := TEnumTest(Value mod 3);
  SetProp := [TEnumTest(Value mod 3), TEnumTest((Value+1) mod 3)];
  ArrOfStringsProp := ['ArrOfStringsProp' + Value.ToString, 'ArrOfStringsProp' + Value.ToString];
  ArrOfIntegersProp := [Value mod 3, (Value + 1 ) mod 3,  (Value + 2 ) mod 3];
  ArrOfBooleansProp := [((Value mod 3) = 1), ((Value + 1) mod 3 = 1), ((Value + 2) mod 3 = 1)];
  ArrOfDateProp := [
    EncodeDate(2022,(Value mod 11)+1, Value mod 28),
    EncodeDate(2022,((Value+1) mod 11)+1, (Value+1) mod 28),
    EncodeDate(2022,((Value+2) mod 11)+1, (Value+2) mod 28)
  ];
  ArrOfTimeProp := [
    EncodeTime(Value mod 24, Value mod 60, Value mod 60, 0),
    EncodeTime((Value + 1) mod 24, (Value + 1) mod 60, (Value + 1) mod 60, 0),
    EncodeTime((Value + 2) mod 24, (Value + 2) mod 60, (Value + 2) mod 60, 0)
  ];
  ArrOfDateTimeProp := [
    ArrOfDateProp[0] + ArrOfTimeProp[0],
    ArrOfDateProp[1] + ArrOfTimeProp[1],
    ArrOfDateProp[2] + ArrOfTimeProp[2]
  ];
end;

function TNestedRec.ToString: String;
  function SetPropAsString: String;
  var
    lEl: TEnumTest;
  begin
    for lEl in SetProp do
    begin
      Result := Result + GetEnumName(TypeInfo(TEnumTest), Ord(lEl)) + ',';
    end;
    Result := Result.Remove(Result.Length - 1);
  end;
var
  I: Integer;
begin
  Result :=
    'StringProp = ' + self.StringProp + sLineBreak +
    'IntegerProp = ' + self.IntegerProp.ToString + sLineBreak +
    'BooleanProp = ' + self.BooleanProp.ToString(TUseBoolStrs.True) + sLineBreak +
    'EnumProp = ' + GetEnumName(TypeInfo(TEnumTest), Ord(EnumProp)) + sLineBreak +
    'SetProp = ' + SetPropAsString + sLineBreak;

  Result := Result + 'ArrOfStringsProp = ';
  for I := Low(ArrOfStringsProp) to High(ArrOfStringsProp) do
  begin
    Result := Result + ArrOfStringsProp[I] + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfIntegersProp = ';
  for I := Low(ArrOfIntegersProp) to High(ArrOfIntegersProp) do
  begin
    Result := Result + ArrOfIntegersProp[I].ToString() + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfBooleansProp = ';
  for I := Low(ArrOfBooleansProp) to High(ArrOfBooleansProp) do
  begin
    Result := Result + ArrOfBooleansProp[I].ToString(TUseBoolStrs.True) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfDateProp = ';
  for I := Low(ArrOfDateProp) to High(ArrOfDateProp) do
  begin
    Result := Result + DateToStr(ArrOfDateProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfTimeProp = ';
  for I := Low(ArrOfTimeProp) to High(ArrOfTimeProp) do
  begin
    Result := Result + TimeToStr(ArrOfTimeProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;

  Result := Result + 'ArrOfDateTimeProp = ';
  for I := Low(ArrOfDateTimeProp) to High(ArrOfDateTimeProp) do
  begin
    Result := Result + DateTimeToStr(ArrOfDateTimeProp[I]) + ',';
  end;
  Result := Result.Remove(Result.Length - 1) + sLineBreak;
end;

{ TNestedArraysRec }

function TNestedArraysRec.ToString: String;
var
  I: Integer;
begin
  Result := '-- TestRecProp -- ' + sLineBreak + TestRecProp.ToString + sLineBreak;
  Result := Result + sLineBreak + '-- ArrayProp1 -- ' + sLineBreak;
  for I := Low(ArrayProp1) to High(ArrayProp1) do
  begin
    Result := Result + 'ITEM ' + I.ToString + sLineBreak + ArrayProp1[I].ToString + sLineBreak;
  end;
  Result := Result + sLineBreak + '-- ArrayProp2 -- ' + sLineBreak;
  for I := Low(ArrayProp2) to High(ArrayProp2) do
  begin
    Result := Result + 'ITEM ' + I.ToString + sLineBreak + ArrayProp2[I].ToString + sLineBreak;
  end;
end;

end.
