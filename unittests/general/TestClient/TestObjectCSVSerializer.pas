// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit TestObjectCSVSerializer;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.Serializer.CSV;

type
  TPriority = (pLow, pMedium, pHigh);

  TPlainObject = class
  private
    FNumbers: Integer;
    FText: string;
    FAmount: Double;
    FActive: Boolean;
  public
    property Numbers: Integer read FNumbers write FNumbers;
    property Text: string read FText write FText;
    property Amount: Double read FAmount write FAmount;
    property Active: Boolean read FActive write FActive;
  end;

  TRichObject = class
  private
    FFirstName: string;
    FLastName: string;
    FBirthDate: TDate;
    FSalary: Currency;
    FPriority: TPriority;
    FNotes: string;
  public
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property BirthDate: TDate read FBirthDate write FBirthDate;
    property Salary: Currency read FSalary write FSalary;
    property Priority: TPriority read FPriority write FPriority;
    property Notes: string read FNotes write FNotes;
  end;

  [TestFixture]
  TTestObjectCSVSerializer = class
  private
    FSerializer: TMVCCSVSerializer;
  public
    [Setup]
    procedure SetUp;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDeserialize_IssueExample;

    [Test]
    procedure TestDeserialize_PreservesQuotedDelimiterAndEscapedQuotes;

    [Test]
    procedure TestDeserialize_BoolVariants;

    [Test]
    procedure TestDeserialize_NoHeader_WithCustomColumns;

    [Test]
    procedure TestDeserialize_SemicolonDelimiter;

    [Test]
    procedure TestDeserialize_MultilineQuotedValue;

    [Test]
    procedure TestDeserialize_DateCurrencyEnum;

    [Test]
    procedure TestDeserialize_EmptyString;

    [Test]
    procedure TestDeserialize_HeaderOnly;

    [Test]
    procedure TestDeserialize_TrailingNewline;

    [Test]
    procedure TestDeserialize_LFOnly;

    [Test]
    procedure TestDeserialize_CROnly;

    [Test]
    procedure TestDeserialize_UnknownHeaderColumnIgnored;

    [Test]
    procedure TestDeserialize_RaisesOnInvalidInteger;

    [Test]
    procedure TestSerialize_BasicCollection;

    [Test]
    procedure TestSerialize_QuotesValuesContainingDelimiter;

    [Test]
    procedure TestSerialize_HonorsIgnoredAttributes;

    [Test]
    procedure TestRoundTrip_Equality;

    [Test]
    procedure TestLocaleIndependence_HostileFormatSettings;

    [Test]
    procedure TestSettings_ExcelEUPreset;

    [Test]
    procedure TestDeserialize_NilListRaises;

    [Test]
    procedure TestSerialize_NilListRaises;

    [Test]
    procedure TestBuildColumns_AllReadableProperties;

    [Test]
    procedure TestBuildColumns_HonorsIgnoredAttributes;

    [Test]
    procedure TestBuildHeaderLine_QuotesNamesContainingDelimiter;

    [Test]
    procedure TestBuildDataLine_ProducesSingleRow;

    [Test]
    procedure TestBuildDataLine_NilObjectRaises;
  end;

implementation

uses
  System.DateUtils,
  System.StrUtils,
  System.TypInfo,
  MVCFramework.Serializer.Commons;

{ TTestObjectCSVSerializer }

procedure TTestObjectCSVSerializer.SetUp;
begin
  FSerializer := TMVCCSVSerializer.Create;
end;

procedure TTestObjectCSVSerializer.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_IssueExample;
const
  CSV =
    'Numbers,Text'#13#10 +
    '99,"some, text"'#13#10 +
    '-1,plain text';
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(CSV, List, TPlainObject);
    Assert.AreEqual<Integer>(2, List.Count);
    Assert.AreEqual<Integer>(99, List[0].Numbers);
    Assert.AreEqual('some, text', List[0].Text);
    Assert.AreEqual<Integer>(-1, List[1].Numbers);
    Assert.AreEqual('plain text', List[1].Text);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_PreservesQuotedDelimiterAndEscapedQuotes;
const
  CSV =
    'Numbers,Text,Amount,Active'#13#10 +
    '42,"text with ""quotes"" inside",0,1';
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(CSV, List, TPlainObject);
    Assert.AreEqual<Integer>(1, List.Count);
    Assert.AreEqual('text with "quotes" inside', List[0].Text);
    Assert.AreEqual(True, List[0].Active);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_BoolVariants;
const
  CSV =
    'Numbers,Text,Amount,Active'#13#10 +
    '1,a,0,true'#13#10 +
    '2,b,0,false'#13#10 +
    '3,c,0,1'#13#10 +
    '4,d,0,0'#13#10 +
    '5,e,0,TRUE'#13#10 +
    '6,f,0,FaLsE';
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(CSV, List, TPlainObject);
    Assert.AreEqual<Integer>(6, List.Count);
    Assert.IsTrue (List[0].Active, 'true');
    Assert.IsFalse(List[1].Active, 'false');
    Assert.IsTrue (List[2].Active, '1');
    Assert.IsFalse(List[3].Active, '0');
    Assert.IsTrue (List[4].Active, 'TRUE');
    Assert.IsFalse(List[5].Active, 'FaLsE');
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_NoHeader_WithCustomColumns;
const
  CSV =
    '1,foo,1.0,true'#13#10 +
    '2,bar,2.0,false';
var
  List: TObjectList<TPlainObject>;
  Settings: TMVCCSVSerializerSettings;
  Ser: TMVCCSVSerializer;
begin
  Settings := TMVCCSVSerializerSettings.Default;
  Settings.HasHeader := False;
  Ser := TMVCCSVSerializer.Create(Settings);
  List := TObjectList<TPlainObject>.Create(True);
  try
    Ser.DeserializeCollectionOfClass(CSV, List, TPlainObject,
      ['Numbers', 'Text', 'Amount', 'Active']);
    Assert.AreEqual<Integer>(2, List.Count);
    Assert.AreEqual<Integer>(1, List[0].Numbers);
    Assert.AreEqual('foo', List[0].Text);
    Assert.IsTrue(List[0].Active);
    Assert.AreEqual<Integer>(2, List[1].Numbers);
    Assert.AreEqual('bar', List[1].Text);
    Assert.IsFalse(List[1].Active);
  finally
    Ser.Free;
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_SemicolonDelimiter;
const
  CSV =
    'Numbers;Text;Amount;Active'#13#10 +
    '7;"a;b";1.5;true'#13#10 +
    '8;"x"";""y";2.5;false';
var
  List: TObjectList<TPlainObject>;
  Settings: TMVCCSVSerializerSettings;
  Ser: TMVCCSVSerializer;
begin
  Settings := TMVCCSVSerializerSettings.Default;
  Settings.Delimiter := ';';
  Ser := TMVCCSVSerializer.Create(Settings);
  List := TObjectList<TPlainObject>.Create(True);
  try
    Ser.DeserializeCollection(CSV, List, TPlainObject);
    Assert.AreEqual<Integer>(2, List.Count);
    Assert.AreEqual('a;b', List[0].Text);
    Assert.AreEqual('x";"y', List[1].Text);
  finally
    Ser.Free;
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_MultilineQuotedValue;
const
  CSV =
    'Numbers,Text,Amount,Active'#13#10 +
    '1,"line1'#13#10'line2'#13#10'line3",9.99,true'#13#10 +
    '2,single,0,false';
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(CSV, List, TPlainObject);
    Assert.AreEqual<Integer>(2, List.Count);
    Assert.AreEqual('line1'#13#10'line2'#13#10'line3', List[0].Text);
    Assert.AreEqual('single', List[1].Text);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_DateCurrencyEnum;
const
  CSV =
    'FirstName,LastName,BirthDate,Salary,Priority,Notes'#13#10 +
    'Mario,Rossi,1985-03-12,1500.50,pHigh,"VIP, since 2010"'#13#10 +
    'Luigi,Verdi,1990-12-01,2200,pMedium,'#13#10 +
    'Anna,Bianchi,1978-07-25,3100.99,pLow,"multi-word note"';
var
  List: TObjectList<TRichObject>;
begin
  List := TObjectList<TRichObject>.Create(True);
  try
    FSerializer.DeserializeCollection(CSV, List, TRichObject);
    Assert.AreEqual<Integer>(3, List.Count);

    Assert.AreEqual('Mario', List[0].FirstName);
    Assert.AreEqual(EncodeDate(1985, 3, 12), List[0].BirthDate);
    Assert.AreEqual(Currency(1500.50), List[0].Salary);
    Assert.AreEqual<Integer>(Ord(pHigh), Ord(List[0].Priority));
    Assert.AreEqual('VIP, since 2010', List[0].Notes);

    Assert.AreEqual(Currency(2200), List[1].Salary);
    Assert.AreEqual<Integer>(Ord(pMedium), Ord(List[1].Priority));
    Assert.AreEqual('', List[1].Notes);

    Assert.AreEqual<Integer>(Ord(pLow), Ord(List[2].Priority));
    Assert.AreEqual('multi-word note', List[2].Notes);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_EmptyString;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection('', List, TPlainObject);
    Assert.AreEqual<Integer>(0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_HeaderOnly;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection('Numbers,Text,Amount,Active',
      List, TPlainObject);
    Assert.AreEqual<Integer>(0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_TrailingNewline;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(
      'Numbers,Text,Amount,Active'#13#10'1,foo,1,true'#13#10,
      List, TPlainObject);
    Assert.AreEqual<Integer>(1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_LFOnly;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(
      'Numbers,Text,Amount,Active'#10'1,foo,1,true'#10'2,bar,2,false',
      List, TPlainObject);
    Assert.AreEqual<Integer>(2, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_CROnly;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(
      'Numbers,Text,Amount,Active'#13'1,foo,1,true'#13'2,bar,2,false',
      List, TPlainObject);
    Assert.AreEqual<Integer>(2, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_UnknownHeaderColumnIgnored;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    FSerializer.DeserializeCollection(
      'Numbers,Bogus,Active'#13#10'7,xxx,true', List, TPlainObject);
    Assert.AreEqual<Integer>(1, List.Count);
    Assert.AreEqual<Integer>(7, List[0].Numbers);
    Assert.IsTrue(List[0].Active);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_RaisesOnInvalidInteger;
var
  List: TObjectList<TPlainObject>;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    Assert.WillRaise(
      procedure
      begin
        FSerializer.DeserializeCollection(
          'Numbers,Text,Amount,Active'#13#10'notanumber,foo,1,true',
          List, TPlainObject);
      end,
      EConvertError,
      'invalid integer must raise EConvertError');
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestSerialize_BasicCollection;
var
  List: TObjectList<TPlainObject>;
  Obj: TPlainObject;
  CSV: string;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    Obj := TPlainObject.Create;
    Obj.Numbers := 99; Obj.Text := 'hello'; Obj.Amount := 10.5; Obj.Active := True;
    List.Add(Obj);

    Obj := TPlainObject.Create;
    Obj.Numbers := -1; Obj.Text := 'world'; Obj.Amount := 3.14; Obj.Active := False;
    List.Add(Obj);

    CSV := FSerializer.SerializeCollection(List);
    Assert.IsTrue(Pos('Numbers,Text,Amount,Active', CSV) = 1, 'header missing');
    Assert.IsTrue(Pos('99,hello,10.5,true', CSV) > 0, '99 row');
    Assert.IsTrue(Pos('-1,world,3.14,false', CSV) > 0, '-1 row');
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestSerialize_QuotesValuesContainingDelimiter;
var
  List: TObjectList<TPlainObject>;
  Obj: TPlainObject;
  CSV: string;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    Obj := TPlainObject.Create;
    Obj.Numbers := 1;
    Obj.Text := 'a,b';
    Obj.Amount := 0;
    Obj.Active := False;
    List.Add(Obj);

    Obj := TPlainObject.Create;
    Obj.Numbers := 2;
    Obj.Text := 'has "quotes"';
    Obj.Amount := 0;
    Obj.Active := False;
    List.Add(Obj);

    CSV := FSerializer.SerializeCollection(List);
    Assert.IsTrue(Pos('1,"a,b",0,false', CSV) > 0,
      'value with delimiter must be quoted');
    Assert.IsTrue(Pos('2,"has ""quotes""",0,false', CSV) > 0,
      'embedded quotes must be doubled');
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestSerialize_HonorsIgnoredAttributes;
var
  List: TObjectList<TPlainObject>;
  Obj: TPlainObject;
  CSV: string;
begin
  List := TObjectList<TPlainObject>.Create(True);
  try
    Obj := TPlainObject.Create;
    Obj.Numbers := 99; Obj.Text := 'hello'; Obj.Amount := 10.5; Obj.Active := True;
    List.Add(Obj);

    CSV := FSerializer.SerializeCollection(List, stDefault, ['Amount', 'Active']);
    Assert.IsTrue(Pos('Numbers,Text', CSV) = 1, 'expected only Numbers,Text header');
    Assert.IsFalse(Pos('Amount', CSV) > 0, 'Amount must be ignored');
    Assert.IsFalse(Pos('Active', CSV) > 0, 'Active must be ignored');
    Assert.IsTrue(Pos('99,hello', CSV) > 0);
  finally
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestRoundTrip_Equality;
var
  L1, L2: TObjectList<TPlainObject>;
  Obj: TPlainObject;
  CSV: string;
  i: Integer;
begin
  L1 := TObjectList<TPlainObject>.Create(True);
  L2 := TObjectList<TPlainObject>.Create(True);
  try
    for i := 1 to 5 do
    begin
      Obj := TPlainObject.Create;
      Obj.Numbers := i * 10;
      Obj.Text := Format('Item #%d, "qty"=%d', [i, i]);
      Obj.Amount := i * 1.5;
      Obj.Active := i mod 2 = 1;
      L1.Add(Obj);
    end;

    CSV := FSerializer.SerializeCollection(L1);
    FSerializer.DeserializeCollection(CSV, L2, TPlainObject);

    Assert.AreEqual<Integer>(L1.Count, L2.Count);
    for i := 0 to L1.Count - 1 do
    begin
      Assert.AreEqual<Integer>(L1[i].Numbers, L2[i].Numbers);
      Assert.AreEqual(L1[i].Text,    L2[i].Text);
      Assert.IsTrue(Abs(L1[i].Amount - L2[i].Amount) < 1e-9, 'Amount mismatch');
      Assert.AreEqual(L1[i].Active,  L2[i].Active);
    end;
  finally
    L1.Free;
    L2.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestLocaleIndependence_HostileFormatSettings;
var
  L1, L2: TObjectList<TRichObject>;
  P: TRichObject;
  Saved: TFormatSettings;
  CSV: string;
begin
  Saved := FormatSettings;
  L1 := TObjectList<TRichObject>.Create(True);
  L2 := TObjectList<TRichObject>.Create(True);
  try
    FormatSettings.DecimalSeparator := ':';
    FormatSettings.ThousandSeparator := ':';
    FormatSettings.DateSeparator := '*';
    FormatSettings.TimeSeparator := '#';
    FormatSettings.ShortDateFormat := 'dd*mm*yyyy';
    FormatSettings.LongDateFormat := 'dd*mm*yyyy';
    FormatSettings.ShortTimeFormat := 'hh#nn#ss';
    FormatSettings.LongTimeFormat := 'hh#nn#ss';
    FormatSettings.CurrencyDecimals := 4;

    P := TRichObject.Create;
    P.FirstName := 'Mario';
    P.LastName := 'Rossi';
    P.BirthDate := EncodeDate(1985, 3, 12);
    P.Salary := 1500.50;
    P.Priority := pHigh;
    P.Notes := 'note';
    L1.Add(P);

    CSV := FSerializer.SerializeCollection(L1);

    Assert.IsTrue(Pos('1985-03-12', CSV) > 0,
      'date must use canonical "-" separator regardless of locale');
    Assert.IsTrue(Pos('1500.5', CSV) > 0,
      'decimal must use "." separator regardless of locale');
    Assert.IsTrue(Pos('pHigh', CSV) > 0, 'enum name must be present');

    FSerializer.DeserializeCollection(CSV, L2, TRichObject);
    Assert.AreEqual<Integer>(1, L2.Count);
    Assert.AreEqual(EncodeDate(1985, 3, 12), L2[0].BirthDate);
    Assert.IsTrue(Abs(L2[0].Salary - 1500.50) < 0.001);
    Assert.AreEqual<Integer>(Ord(pHigh), Ord(L2[0].Priority));
  finally
    FormatSettings := Saved;
    L1.Free;
    L2.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestSettings_ExcelEUPreset;
var
  Ser: TMVCCSVSerializer;
  List: TObjectList<TPlainObject>;
  Obj: TPlainObject;
  CSV: string;
begin
  Ser := TMVCCSVSerializer.Create(TMVCCSVSerializerSettings.ExcelEU);
  List := TObjectList<TPlainObject>.Create(True);
  try
    Obj := TPlainObject.Create;
    Obj.Numbers := 1; Obj.Text := 'foo'; Obj.Amount := 10.5; Obj.Active := True;
    List.Add(Obj);

    CSV := Ser.SerializeCollection(List);
    Assert.IsTrue(Pos(';', CSV) > 0, 'must use ; delimiter');
    Assert.IsTrue(Pos('10,5', CSV) > 0, 'must use , decimal');
  finally
    Ser.Free;
    List.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestDeserialize_NilListRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FSerializer.DeserializeCollection('a,b'#13#10'1,2', nil, TPlainObject);
    end,
    EMVCCSVSerializerException,
    'nil list must raise');
end;

procedure TTestObjectCSVSerializer.TestSerialize_NilListRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FSerializer.SerializeCollection(TObject(nil));
    end,
    EMVCCSVSerializerException,
    'nil list must raise');
end;

procedure TTestObjectCSVSerializer.TestBuildColumns_AllReadableProperties;
var
  Cols: TArray<TRttiProperty>;
  Names: TArray<string>;
  i: Integer;
begin
  Cols := FSerializer.BuildColumns(TPlainObject);
  Assert.AreEqual<Integer>(4, Length(Cols));
  SetLength(Names, Length(Cols));
  for i := 0 to High(Cols) do
    Names[i] := Cols[i].Name;
  Assert.IsTrue(MatchText('Numbers', Names));
  Assert.IsTrue(MatchText('Text',    Names));
  Assert.IsTrue(MatchText('Amount',  Names));
  Assert.IsTrue(MatchText('Active',  Names));
end;

procedure TTestObjectCSVSerializer.TestBuildColumns_HonorsIgnoredAttributes;
var
  Cols: TArray<TRttiProperty>;
  Names: TArray<string>;
  i: Integer;
begin
  Cols := FSerializer.BuildColumns(TPlainObject, ['Amount', 'Active']);
  Assert.AreEqual<Integer>(2, Length(Cols));
  SetLength(Names, Length(Cols));
  for i := 0 to High(Cols) do
    Names[i] := Cols[i].Name;
  Assert.IsTrue (MatchText('Numbers', Names));
  Assert.IsTrue (MatchText('Text',    Names));
  Assert.IsFalse(MatchText('Amount',  Names));
  Assert.IsFalse(MatchText('Active',  Names));
end;

procedure TTestObjectCSVSerializer.TestBuildHeaderLine_QuotesNamesContainingDelimiter;
var
  Cols: TArray<TRttiProperty>;
  Header: string;
begin
  Cols := FSerializer.BuildColumns(TPlainObject);
  Header := FSerializer.BuildHeaderLine(Cols);
  Assert.IsTrue(Pos(',', Header) > 0, 'header must contain delimiter');
  Assert.IsTrue(Pos('Numbers', Header) > 0);
  Assert.IsTrue(Pos('Active', Header) > 0);
end;

procedure TTestObjectCSVSerializer.TestBuildDataLine_ProducesSingleRow;
var
  Cols: TArray<TRttiProperty>;
  Obj: TPlainObject;
  Line: string;
begin
  Cols := FSerializer.BuildColumns(TPlainObject);
  Obj := TPlainObject.Create;
  try
    Obj.Numbers := 99;
    Obj.Text := 'value, with comma';
    Obj.Amount := 10.5;
    Obj.Active := True;
    Line := FSerializer.BuildDataLine(Obj, Cols);
    Assert.IsTrue(Pos('99', Line) > 0);
    Assert.IsTrue(Pos('"value, with comma"', Line) > 0,
      'comma value must be quoted');
    Assert.IsTrue(Pos('10.5', Line) > 0);
    Assert.IsTrue(Pos('true', Line) > 0);
    Assert.IsFalse(Line.Contains(#13) or Line.Contains(#10),
      'BuildDataLine must NOT include line terminator');
  finally
    Obj.Free;
  end;
end;

procedure TTestObjectCSVSerializer.TestBuildDataLine_NilObjectRaises;
var
  Cols: TArray<TRttiProperty>;
begin
  Cols := FSerializer.BuildColumns(TPlainObject);
  Assert.WillRaise(
    procedure
    begin
      FSerializer.BuildDataLine(TObject(nil), Cols);
    end,
    EMVCCSVSerializerException,
    'nil AObject must raise');
end;

initialization

TDUnitX.RegisterTestFixture(TTestObjectCSVSerializer);

end.
