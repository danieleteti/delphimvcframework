// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

unit BusinessObjectsU;

interface

uses
  MVCFramework.Serializer.Commons,
  MVCFramework.Nullables,
  MVCFramework.ActiveRecord,
  System.Generics.Collections,
{$IFNDEF LINUX}
  Vcl.Graphics,
{$ENDIF}
  JsonDataObjects, System.Classes, Data.DB;

type

  TEnumTest = (etValue1, etValue2, etValue3);
  TSetOfEnumTest = set of TEnumTest;

  TEnumColorTest = (ctRed, ctGreen, ctBlue);
  TSetOfColors = set of TEnumColorTest;

  [MVCNameCase(ncLowerCase)]
  TPerson = class
  private
    FLastName: string;
    FDOB: TDate;
    FFirstName: string;
    FMarried: boolean;
    fID: Int64;
    procedure SetDOB(const Value: TDate);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetMarried(const Value: boolean);
    function GetFullName: string;
  public
    function Equals(Obj: TObject): boolean; override;
    function ToString: String; override;

    property ID: Int64 read fID write fID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property FullName: string read GetFullName;
    property DOB: TDate read FDOB write SetDOB;
    property Married: boolean read FMarried write SetMarried;
    constructor Create; overload; virtual;
    constructor Create(FirstName, LastName: String; Age: Integer); overload;
    destructor Destroy; override;
    class function GetNew(AFirstName, ALastName: string; ADOB: TDate; AMarried: boolean): TPerson;
    class function GetList(const aCount: Integer = 3): TObjectList<TPerson>;
  end;

  TPeople = TObjectList<TPerson>;

  TClassWithArrays = class
  private
    fArrayOfString: TArray<string>;
    fArrayOfInt: TArray<Integer>;
    fArrayOfInt64: TArray<Int64>;
    fArrayOfDouble: TArray<Double>;
  public
    property ArrayOfString: TArray<string> read fArrayOfString write fArrayOfString;
    property ArrayOfInt: TArray<Integer> read fArrayOfInt write fArrayOfInt;
    property ArrayOfInt64: TArray<Int64> read fArrayOfInt64 write fArrayOfInt64;
    property ArrayOfDouble: TArray<Double> read fArrayOfDouble write fArrayOfDouble;
  end;


  TClassWithEnums = class
  private
    fRGBSet: TSetOfColors;
    fEnumWithName: TEnumColorTest;
    fEnumDefaultSerialization: TEnumColorTest;
    fEnumWithOrdValue: TEnumColorTest;
    fEnumWithMappedValues: TEnumColorTest;
  public
    property RGBSet: TSetOfColors read fRGBSet write fRGBSet;
    property EnumDefaultSerialization: TEnumColorTest read fEnumDefaultSerialization write fEnumDefaultSerialization;
    [MVCEnumSerialization(estEnumName)]
    property EnumWithName: TEnumColorTest read fEnumWithNAme write fEnumWithName;
    [MVCEnumSerialization(estEnumOrd)]
    property EnumWithOrdValue: TEnumColorTest read fEnumWithOrdValue write fEnumWithOrdValue;
    [MVCEnumSerialization(estEnumMappedValues, 'Red,Green,Blue')]
    property EnumWithMappedValues: TEnumColorTest read fEnumWithMappedValues write fEnumWithMappedValues;
  end;

  [MVCNameCase(ncLowerCase)]
  [MVCNameCase(ncLowerCase)]
  [MVCTable('nullables_test')]
  TNullablesTest = class(TMVCActiveRecord)
  private
    [MVCTableField('f_int2')]
    ff_int2: NullableInt16;
    [MVCTableField('f_int4')]
    ff_int4: NullableInt32;
    [MVCTableField('f_int8')]
    ff_int8: NullableInt64;
    [MVCTableField('f_date')]
    ff_date: NullableTDate;
    [MVCTableField('f_time')]
    ff_time: NullableTTime;
    [MVCTableField('f_bool')]
    ff_bool: NullableBoolean;
    [MVCTableField('f_datetime')]
    ff_datetime: NullableTDateTime;
    [MVCTableField('f_float4')]
    ff_float4: NullableSingle;
    [MVCTableField('f_float8')]
    ff_float8: NullableDouble;
    [MVCTableField('f_string')]
    ff_string: NullableString;
    [MVCTableField('f_currency')]
    ff_currency: NullableCurrency;
    [MVCTableField('f_blob')]
    ff_blob: TStream;
    ff_float8_not_null: Double;
    ff_float4_not_null: Single;
  public
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    // f_int2 int2 NULL,
    property f_int2: NullableInt16 read ff_int2 write ff_int2;
    // f_int4 int4 NULL,
    property f_int4: NullableInt32 read ff_int4 write ff_int4;
    // f_int8 int8 NULL,
    property f_int8: NullableInt64 read ff_int8 write ff_int8;
    // f_string varchar NULL,
    property f_string: NullableString read ff_string write ff_string;
    // f_bool bool NULL,
    property f_bool: NullableBoolean read ff_bool write ff_bool;
    // f_date date NULL,
    property f_date: NullableTDate read ff_date write ff_date;
    // f_time time NULL,
    property f_time: NullableTTime read ff_time write ff_time;
    // f_datetime timestamp NULL,
    property f_datetime: NullableTDateTime read ff_datetime write ff_datetime;
    // f_float4 float4 NULL,
    property f_float4: NullableSingle read ff_float4 write ff_float4;
    // f_float8 float8 NULL,
    property f_float8: NullableDouble read ff_float8 write ff_float8;
    // f_currency numeric(18,4) NULL
    property f_currency: NullableCurrency read ff_currency write ff_currency;
    // f_blob bytea NULL
    [MVCSerializeAsString]
    property f_blob: TStream read ff_blob write ff_blob;

    property f_float4_not_null: Single read ff_float4_not_null write ff_float4_not_null;
    // f_float8 float8 NULL,
    property f_float8_not_null: Double read ff_float8_not_null write ff_float8_not_null;

    procedure LoadSomeData;
  end;

  IPerson = interface
    ['{1D00C67A-A6D9-4B31-8291-705B339CDE9B}']
    function GetName: string;
    procedure SetName(const Value: string);
    function GetAge: Integer;
    procedure SetAge(const Value: Integer);
    function GetDOB: TDate;
    procedure SetDOB(const Value: TDate);
    property name: string read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
    property DOB: TDate read GetDOB write SetDOB;
  end;

  TObjectWithJSONObject = class
  private
    fJSONObject: TJSONObject;
    FStringProp: string;
    procedure SetStringProp(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property StringProp: string read FStringProp write SetStringProp;
    property JSONObject: TJSONObject read fJSONObject;
  end;

  [MVCNameCase(ncCamelCase)]
  TInterfacedPerson = class(TInterfacedObject, IPerson)
  private
    fName: string;
    fDOB: TDate;
    fAge: Integer;
  protected
    function GetName: string;
    procedure SetName(const Value: string);
    function GetAge: Integer;
    procedure SetAge(const Value: Integer);
    function GetDOB: TDate;
    procedure SetDOB(const Value: TDate);
  public
    property Name: string read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
    property DOB: TDate read GetDOB write SetDOB;
    constructor Create(Name: String; Age: Integer; DOB: TDate); virtual;
  end;

  //TPeople = class(TObjectList<TPerson>);

  [MVCNameCase(ncLowerCase)]
  TMetadata = class
  private
    FCustomData: string;
    FStopProcessing: TDateTime;
    FStartProcessing: TDateTime;
    procedure SetCustomData(const Value: string);
    procedure SetStartProcessing(const Value: TDateTime);
    procedure SetStopProcessing(const Value: TDateTime);
  public
    property StartProcessing: TDateTime read FStartProcessing write SetStartProcessing;
    property StopProcessing: TDateTime read FStopProcessing write SetStopProcessing;
    property CustomData: string read FCustomData write SetCustomData;
  end;

  [MVCNameCase(ncLowerCase)]
  TPeopleWithMetadata = class(TObject)
  private
    FItems: TPeople;
    FMetadata: TMetadata;
  public
    constructor Create;
    destructor Destroy; override;
    property Items: TPeople read FItems;
    property Metadata: TMetadata read FMetadata;
  end;

  [MVCNameCase(ncLowerCase)]
  TCustomer = class
  private
    fName: string;
    FAddressLine2: string;
    FAddressLine1: string;
    FContactFirst: string;
    FCity: string;
    FContactLast: string;
{$IFNDEF LINUX}
    fLogo: TBitmap;
{$ENDIF}
    procedure SetAddressLine1(const Value: string);
    procedure SetAddressLine2(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetContactFirst(const Value: string);
    procedure SetContactLast(const Value: string);
    procedure SetName(const Value: string);
{$IFNDEF LINUX}
    procedure SetLogo(const Value: TBitmap);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    property name: string read fName write SetName;
    [MVCDoNotSerialize]
    property ContactFirst: string read FContactFirst write SetContactFirst;
    [MVCDoNotSerialize]
    property ContactLast: string read FContactLast write SetContactLast;
    property AddressLine1: string read FAddressLine1 write SetAddressLine1;
    property AddressLine2: string read FAddressLine2 write SetAddressLine2;
    property City: string read FCity write SetCity;
{$IFNDEF LINUX}
    property Logo: TBitmap read fLogo write SetLogo;
{$ENDIF}
    class function GetList(Count: Integer = 1000): TObjectList<TCustomer>;
  end;

  [MVCNameCase(ncLowerCase)]
  TEntityWithGUIDs = class
  private
    fNullableGUID: NullableTGUID;
    fGUID: TGUID;
    fNullableGUID2: NullableTGUID;
  public
    constructor Create(const RandomInitialization: boolean = True);
    property GUID: TGUID read fGUID write fGUID;
    property NullableGUID: NullableTGUID read fNullableGUID write fNullableGUID;
    property NullableGUID2: NullableTGUID read fNullableGUID2 write fNullableGUID2;
  end;

  [MVCNameCase(ncLowerCase)]
  TProgrammer = class(TPerson)
  private
    FSkills: string;
    procedure SetSkills(const Value: string);
  public
    property Skills: string read FSkills write SetSkills;
  end;

  [MVCNameCase(ncLowerCase)]
  TProgrammerEx = class(TProgrammer)
  private
    [MVCOwned] //required only for field serialization
    FMentor: TProgrammerEx;
  public
    destructor Destroy; override;
    [MVCOwned] //required only for property serialization
    property Mentor: TProgrammerEx read FMentor write fMentor;
  end;

  [MVCNameCase(ncLowerCase)]
  TProgrammerEx2 = class(TProgrammer)
  private
    FMentor: TProgrammer;
  public
    destructor Destroy; override;
    [MVCOwned(TProgrammerEx2)]
    property Mentor: TProgrammer read FMentor write fMentor;
  end;


  [MVCNameCase(ncLowerCase)]
  TPhilosopher = class(TPerson)
  private
    FMentors: string;
    procedure SetMentors(const Value: string);
  public
    property Mentors: string read FMentors write SetMentors;
  end;

  [MVCNameCase(ncLowerCase)]
  TMultiDataset = class
  private
    fPeople: TDataset;
    fCustomers: TDataset;
    procedure SetCustomers(const Value: TDataset);
    procedure SetPeople(const Value: TDataset);
  public
    constructor Create;
    property Customers: TDataset read fCustomers write SetCustomers;
    property People: TDataset read fPeople write SetPeople;
    destructor Destroy; override;
  end;

  // Records
  TMyEnum = (EnumItem1, EnumItem2, EnumItem3);
  TMySet = set of TMyEnum;

  [MVCNameCase(ncCamelCase)]
  TSimpleRecord = record
    StringProperty: String;
    IntegerProperty: Integer;
    FloatProperty: Double;
    CurrencyProperty: Currency;
    BooleanProperty: Boolean;
    DateProperty: TDate;
    TimeProperty: TTime;
    DateTimeProperty: TDateTime;
    EnumProperty: TMyEnum;
    SetProperty: TMySet;
    class function Create: TSimpleRecord; overload; static;
    class function Create(Value: Integer): TSimpleRecord; overload; static;
    function ToString: String;
    function Equals(SimpleRecord: TSimpleRecord): Boolean;
  end;

  TSimpleRecordDynArray = TArray<TSimpleRecord>;
  TSimpleRecordStaticArray = array [0..2] of TSimpleRecord;

  TComplexRecord = record
    StringProperty: String;
    IntegerProperty: Integer;
    FloatProperty: Double;
    CurrencyProperty: Currency;
    DateProperty: TDate;
    TimeProperty: TTime;
    DateTimeProperty: TDateTime;
    BooleanProperty: Boolean;
    EnumProperty: TMyEnum;
    SetProperty: TMySet;
    SimpleRecord: TSimpleRecord;
    SimpleRecordDynArray: TSimpleRecordDynArray;
    SimpleRecordStaticArray: TSimpleRecordStaticArray;
    class function Create: TComplexRecord; static;
    function Equals(ComplexRecord: TComplexRecord): Boolean;
  end;

  TComplexRecordArray = TArray<TComplexRecord>;

  TCustomerIssue648 = record
    Id: NullableInt32;
    Added: TDateTime;
    Name: NullableString;
    ExpirationDate: NullableTDate;
    MaxUpdateDate: NullableTDate;
    AppVersion: NullableString;
    Activated: NullableTDateTime;
  end;


implementation

uses
  System.SysUtils,
  System.Math,
  RandomUtilsU, FireDAC.Comp.Client, System.TypInfo;

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  fID := 1000 + Random(1000);
end;

constructor TPerson.Create(FirstName, LastName: String; Age: Integer);
begin
  Create;
  FFirstName := FirstName;
  FLastName := LastName;
  FDOB := EncodeDate(CurrentYear - Age, 1, 1);
  FMarried := False;
end;

destructor TPerson.Destroy;
begin

  inherited;
end;

function TPerson.Equals(Obj: TObject): boolean;
begin
  Result := Obj is TPerson;
  if Result then
  begin
    Result := Result and (TPerson(Obj).LastName = Self.LastName);
    Result := Result and (TPerson(Obj).FirstName = Self.FirstName);
    Result := Result and (TPerson(Obj).Married = Self.Married);
    Result := Result and (TPerson(Obj).DOB = Self.DOB);
  end;
end;

function TPerson.GetFullName: string;
begin
  Result := Format('%s, %s', [FFirstName, FLastName]);
end;

class function TPerson.GetList(const aCount: Integer): TObjectList<TPerson>;
var
  I: Integer;
begin
  if aCount = 3 then
  begin // retrocompatibility
    Result := TObjectList<TPerson>.Create(true);
    Result.Add(TPerson.GetNew('Tony', 'Stark', EncodeDate(1965, 5, 15), true));
    Result.Add(TPerson.GetNew('Steve', 'Rogers', 0, true));
    Result.Add(TPerson.GetNew('Bruce', 'Banner', 0, true));
  end
  else
  begin
    Result := TObjectList<TPerson>.Create(true);
    for I := 1 to aCount do
    begin
      Result.Add(TPerson.GetNew(GetRndFirstName, GetRndLastName, EncodeDate(1900 + Random(100), Random(12) + 1,
        Random(27) + 1), true));
    end;
  end;
end;

class function TPerson.GetNew(AFirstName, ALastName: string; ADOB: TDate; AMarried: boolean): TPerson;
begin
  Result := TPerson.Create;
  Result.FLastName := ALastName;
  Result.FFirstName := AFirstName;
  Result.FDOB := ADOB;
  Result.FMarried := AMarried;
end;

procedure TPerson.SetDOB(const Value: TDate);
begin
  FDOB := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPerson.SetMarried(const Value: boolean);
begin
  FMarried := Value;
end;

function TPerson.ToString: String;
begin
  Result :=
    Format('ID: %d, LAST_NAME: %s, FIRST_NAME: %s, MARRIED: %s, DOB: %s',[
      Self.ID,
      Self.LastName,
      Self.FirstName,
      BoolToStr(Self.Married, True),
      DateToISODate(Self.DOB)
    ]);
end;

{ TCustomer }

constructor TCustomer.Create;
begin
  inherited;
{$IFNDEF LINUX}
  fLogo := TBitmap.Create;
{$ENDIF}
end;

destructor TCustomer.Destroy;
begin
{$IFNDEF LINUX}
  fLogo.Free;
{$ENDIF}
  inherited;
end;

class function TCustomer.GetList(Count: Integer): TObjectList<TCustomer>;
var
  C1: TCustomer;
  I: Integer;
begin
  Result := TObjectList<TCustomer>.Create(true);
  for I := 1 to Count do
  begin
    C1 := TCustomer.Create;
    C1.name := I.ToString + ': bit Time Professionals';
    C1.ContactFirst := 'Daniele';
    C1.ContactLast := 'Teti';
    C1.AddressLine1 := 'Via di Valle Morta 10';
    C1.City := 'Rome, IT';
    Result.Add(C1);

    C1 := TCustomer.Create;
    C1.name := I.ToString + ': Stark Industries';
    C1.ContactFirst := 'Tony';
    C1.ContactLast := 'Stark';
    C1.AddressLine1 := 'Superhero Street 555';
    C1.City := 'Palo Alto, CA';
    Result.Add(C1);

    C1 := TCustomer.Create;
    C1.name := I.ToString + ': Google Inc';
    C1.ContactFirst := 'Larry';
    C1.ContactLast := 'Page';
    C1.AddressLine1 := '';
    C1.City := 'Mountain View, CA';
    Result.Add(C1);
  end;

end;

procedure TCustomer.SetAddressLine1(const Value: string);
begin
  FAddressLine1 := Value;
end;

procedure TCustomer.SetAddressLine2(const Value: string);
begin
  FAddressLine2 := Value;
end;

procedure TCustomer.SetCity(const Value: string);
begin
  FCity := Value;
end;

procedure TCustomer.SetContactFirst(const Value: string);
begin
  FContactFirst := Value;
end;

procedure TCustomer.SetContactLast(const Value: string);
begin
  FContactLast := Value;
end;

{$IFNDEF LINUX}


procedure TCustomer.SetLogo(const Value: TBitmap);
begin
  fLogo := Value;
end;
{$ENDIF}


procedure TCustomer.SetName(const Value: string);
begin
  fName := Value;
end;

{ TProgrammer }

procedure TProgrammer.SetSkills(const Value: string);
begin
  FSkills := Value;
end;

{ TPhilosopher }

procedure TPhilosopher.SetMentors(const Value: string);
begin
  FMentors := Value;
end;

{ TMetadata }

procedure TMetadata.SetCustomData(const Value: string);
begin
  FCustomData := Value;
end;

procedure TMetadata.SetStartProcessing(const Value: TDateTime);
begin
  FStartProcessing := Value;
end;

procedure TMetadata.SetStopProcessing(const Value: TDateTime);
begin
  FStopProcessing := Value;
end;

{ TPeopleWithMetadata }

constructor TPeopleWithMetadata.Create;
begin
  inherited;
  FMetadata := TMetadata.Create;
  FItems := TPeople.Create(true);
end;

destructor TPeopleWithMetadata.Destroy;
begin
  FMetadata.Free;
  FItems.Free;
  inherited;
end;

{ TInterfacedPerson }

constructor TInterfacedPerson.Create(Name: String; Age: Integer; DOB: TDate);
begin
  inherited Create;
  fName := Name;
  fAge := Age;
  fDOB := DOB;
end;

function TInterfacedPerson.GetAge: Integer;
begin
  Result := fAge;
end;

function TInterfacedPerson.GetDOB: TDate;
begin
  Result := FDOB;
end;

function TInterfacedPerson.GetName: string;
begin
  Result := fName;
end;

procedure TInterfacedPerson.SetAge(const Value: Integer);
begin
  fAge := Value;
end;

procedure TInterfacedPerson.SetDOB(const Value: TDate);
begin
  FDOB := Value;
end;

procedure TInterfacedPerson.SetName(const Value: string);
begin
  fName := Value;
end;

{ TObjectWithJSONObject }

constructor TObjectWithJSONObject.Create;
begin
  inherited;
  fJSONObject := TJSONObject.Create;
end;

destructor TObjectWithJSONObject.Destroy;
begin
  fJSONObject.Free;
  inherited;
end;

procedure TObjectWithJSONObject.SetStringProp(const Value: string);
begin
  FStringProp := Value;
end;

{ TPersonWithNulls }

function TNullablesTest.Equals(Obj: TObject): boolean;
var
  lOtherObj: TNullablesTest;
begin
  lOtherObj := Obj as TNullablesTest;
  Result := true;
  Result := Result and (Self.ff_int2 = lOtherObj.ff_int2);
  Result := Result and (Self.ff_int4 = lOtherObj.ff_int4);
  Result := Result and (Self.ff_int8 = lOtherObj.ff_int8);
  Result := Result and (Self.ff_bool = lOtherObj.ff_bool);
  Result := Result and Self.ff_date.Equals(lOtherObj.ff_date);
  Result := Result and Self.ff_time.Equals(lOtherObj.ff_time);
  Result := Result and Self.ff_datetime.Equals(lOtherObj.ff_datetime);
  Result := Result and (Self.ff_float4 = lOtherObj.ff_float4);
  Result := Result and (Self.ff_float8 = lOtherObj.ff_float8);
  Result := Result and (Self.ff_string = lOtherObj.ff_string);
  Result := Result and (Self.ff_currency = lOtherObj.ff_currency);

  { TODO -oDanieleT -cGeneral : Deserialize a stream over a nil pointer... should we create the TMemoryStream? }
  // Result := Result and ((Self.ff_blob as TStringStream).DataString = (lOtherObj.ff_blob as TStringStream).DataString);
end;

procedure TNullablesTest.LoadSomeData;
begin
  ff_int2 := 2;
  ff_int4 := 4;
  ff_int8 := 8;
  ff_date := EncodeDate(2011, 11, 17);
  ff_time := encodetime(12, 24, 36, 48);
  ff_datetime := ff_date.Value + ff_time.Value;
  ff_bool := true;
  ff_float4 := 10 / 4;
  ff_float8 := 10 / 8;
  ff_string := '0123456789';
  ff_currency := 98765.4321;
  ff_blob := TStringStream.Create(ff_string);
end;

destructor TNullablesTest.Destroy;
begin
  ff_blob.Free;
  inherited;
end;

{ TMultiDataset }

constructor TMultiDataset.Create;
begin
  inherited Create;
  fCustomers := TFDMemTable.Create(nil);
  fCustomers.FieldDefs.Clear;
  fCustomers.FieldDefs.Add('Code', ftInteger);
  fCustomers.FieldDefs.Add('Name', ftString, 20);
  fCustomers.Active := true;

  fPeople := TFDMemTable.Create(nil);
  fPeople.FieldDefs.Clear;
  fPeople.FieldDefs.Add('FirstName', ftString, 20);
  fPeople.FieldDefs.Add('LastName', ftString, 20);
  fPeople.Active := true;
end;

destructor TMultiDataset.Destroy;
begin
  fCustomers.Free;
  fPeople.Free;
  inherited;
end;

procedure TMultiDataset.SetCustomers(const Value: TDataset);
begin
  if fCustomers <> nil then
  begin
    raise Exception.Create('DataSet Already Initialized');
  end;
  fCustomers := Value;
end;

procedure TMultiDataset.SetPeople(const Value: TDataset);
begin
  if fPeople <> nil then
  begin
    raise Exception.Create('DataSet Already Initialized');
  end;
  fPeople := Value;
end;

{ TProgrammerEx }

destructor TProgrammerEx.Destroy;
begin
  FMentor.Free;
  inherited;
end;

{ TProgrammerEx2 }

destructor TProgrammerEx2.Destroy;
begin
  FMentor.Free;
  inherited;
end;

{ TEntityWithGUIDs }

constructor TEntityWithGUIDs.Create(const RandomInitialization: boolean);
begin
  inherited Create;
  if RandomInitialization then
  begin
    fGUID := TGUID.NewGuid;
    fNullableGUID := TGUID.NewGuid;
  end;
end;

{ TSimpleRecord }

class function TSimpleRecord.Create: TSimpleRecord;
begin
  Result.StringProperty := 'the string property';
  Result.IntegerProperty:= 1234;
  Result.FloatProperty := 1234.56789;
  Result.CurrencyProperty := 1234.5678;
  Result.DateProperty:= EncodeDate(2022,7,5);
  Result.TimeProperty := EncodeTime(12,13,14,0);
  Result.DateTimeProperty := Result.DateProperty + Result.TimeProperty;
  Result.BooleanProperty := True;
  Result.EnumProperty := EnumItem2;
  Result.SetProperty := [EnumItem1, EnumItem3];
end;

class function TSimpleRecord.Create(Value: Integer): TSimpleRecord;
begin
  Result := TSimpleRecord.Create;
  Result.StringProperty := Value.ToString;
  Result.IntegerProperty := Value;
  Result.CurrencyProperty := Value + Value div 1000;
end;

function TSimpleRecord.Equals(SimpleRecord: TSimpleRecord): Boolean;
begin
  Result := True;
  Result := Result and (StringProperty = SimpleRecord.StringProperty);
  Result := Result and (IntegerProperty = SimpleRecord.IntegerProperty);
  Result := Result and (FloatProperty = SimpleRecord.FloatProperty);
  Result := Result and (CurrencyProperty = SimpleRecord.CurrencyProperty);
  Result := Result and (DateProperty = SimpleRecord.DateProperty);
  Result := Result and (TimeProperty = SimpleRecord.TimeProperty);
  Result := Result and (CompareValue(DateTimeProperty, SimpleRecord.DateTimeProperty, 0.0001) = 0);
  Result := Result and (BooleanProperty = SimpleRecord.BooleanProperty);
  Result := Result and (EnumProperty = SimpleRecord.EnumProperty);
  Result := Result and (SetProperty * SimpleRecord.SetProperty = [EnumItem1, EnumItem3]);
  Result := Result and (SetProperty - SimpleRecord.SetProperty = []);
end;

function TSimpleRecord.ToString: String;
  function SetToString: String;
  var
    lEl: TMyEnum;
  begin
    for lEl in SetProperty do
    begin
      Result := Result + GetEnumName(typeinfo(TMyEnum), Ord(Self.EnumProperty)) + ',';
    end;
    Result := Result.Trim([',']);
  end;
begin
  Result :=
    'StringProperty   = ' + Self.StringProperty + sLineBreak +
    'IntegerProperty  = ' + Self.IntegerProperty.ToString + sLineBreak +
    'FloatProperty    = ' + Self.FloatProperty.ToString + sLineBreak +
    'CurrencyProperty = ' + CurrToStr(Self.CurrencyProperty) + sLineBreak +
    'DateProperty     = ' + DateToStr(Self.DateProperty) + sLineBreak +
    'TimeProperty     = ' + TimeToStr(Self.TimeProperty) + sLineBreak +
    'DateTimeProperty = ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Self.DateTimeProperty) + sLineBreak +
    'BooleanProperty  = ' + BoolToStr(Self.BooleanProperty, True) + sLineBreak +
    'EnumProperty     = ' + GetEnumName(typeinfo(TMyEnum), Ord(Self.EnumProperty)) + sLineBreak +
    'SetProperty      = ' + SetToString + sLineBreak;
end;

{ TComplexRecord }

class function TComplexRecord.Create: TComplexRecord;
begin
  Result.StringProperty := 'the string property';
  Result.IntegerProperty:= 1234;
  Result.FloatProperty := 1234.56789;
  Result.CurrencyProperty := 1234.5678;
  Result.DateProperty:= EncodeDate(2022,7,5);
  Result.TimeProperty := EncodeTime(12,13,14,0);
  Result.DateTimeProperty := Result.DateProperty + Result.TimeProperty;
  Result.BooleanProperty := True;
  Result.EnumProperty := EnumItem2;
  Result.SetProperty := [EnumItem1, EnumItem3];
  Result.SimpleRecord := TSimpleRecord.Create;
  SetLength(Result.SimpleRecordDynArray,2);
  Result.SimpleRecordDynArray[0] := TSimpleRecord.Create(1);
  Result.SimpleRecordDynArray[1] := TSimpleRecord.Create(2);
  Result.SimpleRecordStaticArray[0] := TSimpleRecord.Create(3);
  Result.SimpleRecordStaticArray[1] := TSimpleRecord.Create(4);
  Result.SimpleRecordStaticArray[2] := TSimpleRecord.Create(5);
end;

function TComplexRecord.Equals(ComplexRecord: TComplexRecord): Boolean;
begin
  Result := True;
  Result := Result and (StringProperty = ComplexRecord.StringProperty);
  Result := Result and (IntegerProperty = ComplexRecord.IntegerProperty);
  Result := Result and (FloatProperty = ComplexRecord.FloatProperty);
  Result := Result and (CurrencyProperty = ComplexRecord.CurrencyProperty);
  Result := Result and (DateProperty = ComplexRecord.DateProperty);
  Result := Result and (TimeProperty = ComplexRecord.TimeProperty);
  Result := Result and (CompareValue(DateTimeProperty, ComplexRecord.DateTimeProperty, 0.0001)  = 0);
  Result := Result and (BooleanProperty = ComplexRecord.BooleanProperty);
  Result := Result and (EnumProperty = ComplexRecord.EnumProperty);
  Result := Result and (SetProperty * ComplexRecord.SetProperty = [EnumItem1, EnumItem3]);
  Result := Result and (SetProperty - ComplexRecord.SetProperty = []);

end;

initialization

Randomize;

end.
