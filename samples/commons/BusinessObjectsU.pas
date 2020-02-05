// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
  JsonDataObjects, System.Classes;

type

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
    function GetFullName: String;
  public
    function Equals(Obj: TObject): boolean; override;

    property ID: Int64 read fID write fID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property FullName: String read GetFullName;
    property DOB: TDate read FDOB write SetDOB;
    property Married: boolean read FMarried write SetMarried;
    constructor Create; virtual;
    class function GetNew(AFirstName, ALastName: string; ADOB: TDate; AMarried: boolean): TPerson;
    class function GetList(const aCount: Integer = 3): TObjectList<TPerson>;
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

    procedure LoadSomeData;
  end;

  IPerson = interface
    ['{1D00C67A-A6D9-4B31-8291-705B339CDE9B}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetAge: Integer;
    procedure SetAge(const Value: Integer);
    function GetDOB: TDate;
    procedure SetDOB(const Value: TDate);
    property Name: String read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
    property DOB: TDate read GetDOB write SetDOB;
  end;

  TObjectWithJSONObject = class
  private
    fJSONObject: TJSONObject;
    FStringProp: String;
    procedure SetStringProp(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    property StringProp: String read FStringProp write SetStringProp;
    property JSONObject: TJSONObject read fJSONObject;
  end;

  [MVCNameCase(ncCamelCase)]
  TInterfacedPerson = class(TInterfacedObject, IPerson)
  private
    fName: string;
    FDOB: TDate;
    fAge: Integer;
  protected
    function GetName: String;
    procedure SetName(const Value: String);
    function GetAge: Integer;
    procedure SetAge(const Value: Integer);
    function GetDOB: TDate;
    procedure SetDOB(const Value: TDate);
  public
    property Name: String read GetName write SetName;
    property Age: Integer read GetAge write SetAge;
    property DOB: TDate read GetDOB write SetDOB;
  end;

  TPeople = class(TObjectList<TPerson>);

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
    property Name: string read fName write SetName;
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
    class function GetList: TObjectList<TCustomer>;
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
  TPhilosopher = class(TPerson)
  private
    FMentors: string;
    procedure SetMentors(const Value: string);
  public
    property Mentors: string read FMentors write SetMentors;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  RandomUtilsU;

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  fID := 1000 + Random(1000);
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

function TPerson.GetFullName: String;
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

class function TCustomer.GetList: TObjectList<TCustomer>;
var
  C1: TCustomer;
  I: Integer;
begin
  Result := TObjectList<TCustomer>.Create(true);
  for I := 1 to 1000 do
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

function TInterfacedPerson.GetAge: Integer;
begin
  Result := fAge;
end;

function TInterfacedPerson.GetDOB: TDate;
begin
  Result := FDOB;
end;

function TInterfacedPerson.GetName: String;
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

procedure TInterfacedPerson.SetName(const Value: String);
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

procedure TObjectWithJSONObject.SetStringProp(const Value: String);
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
  Result := Result and Self.ff_int2.Equals(lOtherObj.ff_int2);
  Result := Result and Self.ff_int4.Equals(lOtherObj.ff_int4);
  Result := Result and Self.ff_int8.Equals(lOtherObj.ff_int8);
  Result := Result and Self.ff_bool.Equals(lOtherObj.ff_bool);
  Result := Result and (DateToISODate(Self.ff_date) = DateToISODate(lOtherObj.ff_date));
  Result := Result and (TimeToISOTime(Self.ff_time) = TimeToISOTime(lOtherObj.ff_time));
  Result := Result and (DateTimeToISOTimeStamp(Self.ff_datetime) = DateTimeToISOTimeStamp(lOtherObj.ff_datetime));
  Result := Result and Self.ff_float4.Equals(lOtherObj.ff_float4);
  Result := Result and Self.ff_float8.Equals(lOtherObj.ff_float8);
  Result := Result and Self.ff_string.Equals(lOtherObj.ff_string);
  Result := Result and Self.ff_currency.Equals(lOtherObj.ff_currency);
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

initialization

Randomize;

end.
