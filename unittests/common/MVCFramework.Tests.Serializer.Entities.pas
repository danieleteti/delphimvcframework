// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Tests.Serializer.Entities;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Commons,
  MVCFramework.Nullables;

type

  TRole = (roAdmin, roGuest);

  TNote = class
  private
    FDescription: string;
  public
    constructor Create(const ADescription: string);
    property Description: string read FDescription write FDescription;
  end;

  TDepartment = class
  private
    FId: Int64;
    FName: string;

    // The MVCListOfAttribute attribute can be placed in Field or Property
    //[MVCListOf(TNote)]
    FNotes: TObjectList<TNote>;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Int64 read FId write FId;
    [MVCNameAs('Name')]
    property name: string read FName write FName;

//    [MVCListOf(TNote)]
    property Notes: TObjectList<TNote> read FNotes write FNotes;
  end;

  TEntity = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
    FSalary: Double;
    FBirthday: TDate;
    FAccessDateTime: TDateTime;
    FAccessTime: TTime;
    FActive: Boolean;
    FRole: TRole;
    FTeporization: TTimeStamp;
    FDepartment: TDepartment;
    FDepartmentNull: TDepartment;

//    [MVCListOf(TNote)]
    FNotes: TObjectList<TNote>;

  //  [MVCListOf(TNote)]
    FNotesEmpty: TObjectList<TNote>;

    // The MVCValueAsTypeAttribute attribute can be placed in Field or Property
    [MVCValueAsType(System.TypeInfo(string))]
    FAppreciationAs: TValue;

    FAppreciation: TValue;

    FIgnored: string;

    // The MVCDoNotSerializeAttribute attribute can be placed in Field or Property
    [MVCDoNotSerialize]
    FTransient: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    [MVCNameAs('Name')]
    property name: string read FName write FName;
    property Salary: Double read FSalary write FSalary;
    property Birthday: TDate read FBirthday write FBirthday;
    property AccessDateTime: TDateTime read FAccessDateTime write FAccessDateTime;
    property AccessTime: TTime read FAccessTime write FAccessTime;
    property Active: Boolean read FActive write FActive;
    property Role: TRole read FRole write FRole;
    property Teporization: TTimeStamp read FTeporization write FTeporization;
    property Department: TDepartment read FDepartment write FDepartment;
    property DepartmentNull: TDepartment read FDepartmentNull write FDepartmentNull;

    //[MVCListOf(TNote)]
    property Notes: TObjectList<TNote> read FNotes;

    //[MVCListOf(TNote)]
    property NotesEmpty: TObjectList<TNote> read FNotesEmpty;

    [MVCValueAsType(System.TypeInfo(string))]
    property AppreciationAs: TValue read FAppreciationAs write FAppreciationAs;

    property Appreciation: TValue read FAppreciation write FAppreciation;
    property Ignored: string read FIgnored write FIgnored;

    [MVCDoNotSerialize]
    property Transient: string read FTransient write FTransient;
  end;

  [MVCNameCase(ncUpperCase)]
  TEntityUpperCase = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    property name: string read FName write FName;
  end;

  [MVCNameCase(ncLowerCase)]
  TEntityLowerCase = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    property name: string read FName write FName;
  end;

  // The MVCNameAsAttribute attribute can be placed in Field or Property
  TEntityNameAs = class
  private
    [MVCNameAs('Id_Id')]
    FId: Int64;

    [MVCNameAs('Code_Code')]
    FCode: Integer;

    [MVCNameAs('Name_Name')]
    FName: string;
  public
    [MVCNameAs('Id_Id')]
    property Id: Int64 read FId write FId;

    [MVCNameAs('Code_Code')]
    property Code: Integer read FCode write FCode;

    [MVCNameAs('Name_Name')]
    property name: string read FName write FName;
  end;

  TMVCNullable<T> = record
  private
    fValue: T;
    fHasValue: Boolean;
    function GetHasValue: Boolean; inline;
    procedure SetValue(const Value: T);
  public
    procedure Clear;
    property HasValue: Boolean read GetHasValue;
    property Value: T read fValue write SetValue;
  end;

  TMVCNullableInteger = TMVCNullable<Integer>;
  TMVCNullableString = TMVCNullable<string>;

  TEntityCustom = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    [MVCNameAs('Name')]
    property name: string read FName write FName;

  end;

  TEntityCustomWithNullables = class(TEntityCustom)
  private
    FNullableInteger: TMVCNullable<Integer>;
    FNullableString: TMVCNullable<string>;
  public
    property NullableInteger: TMVCNullable<Integer> read FNullableInteger write FNullableInteger;
    property NullableString: TMVCNullable<string> read FNullableString write FNullableString;
  end;

  TEntityCustomWithGuid = class(TEntityCustom)
  private
    FGuidValue: TGUID;
    FGuidValue2: TGUID;
    FNullableGuid: NullableTGUID;
    FNullableGuid2: NullableTGUID;
  public
    property GuidValue: TGUID read FGuidValue write FGuidValue;
    [MVCSerializeGuidWithoutBraces]
    property GuidValue2: TGUID read FGuidValue2 write FGuidValue2;
    property NullableGuid: NullableTGUID read FNullableGuid write FNullableGuid;
    [MVCSerializeGuidWithoutBraces]
    property NullableGuid2: NullableTGUID read FNullableGuid2 write FNullableGuid2;
  end;

  TColorEnum = (RED, GREEN, BLUE);
  TMonthEnum = (meJanuary, meFebruary, meMarch, meApril);
  TMonths = set of TMonthEnum;
  TColors = set of TColorEnum;

  TEntityWithSets = class
  private
    fMonthsSet: TMonths;
    fColorsSet: TColors;
  public
    property MonthsSet: TMonths read fMonthsSet write fMonthsSet;
    property ColorsSet: TColors read fColorsSet write fColorsSet;
  end;

  TEntityWithEnums = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
    FColor: TColorEnum;
    FMonthName: TMonthEnum;
    FMonthOrder: TMonthEnum;
    FMonthName2: TMonthEnum;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    [MVCNameAs('Name')]
    property name: string read FName write FName;
    property Color: TColorEnum read FColor write FColor;
    [MVCEnumSerialization(estEnumMappedValues, 'January,February,March,April')]
    property MonthName: TMonthEnum read FMonthName write FMonthName;
    [MVCEnumSerialization(estEnumName)]
    property MonthName2: TMonthEnum read FMonthName2 write FMonthName2;
    [MVCEnumSerialization(estEnumOrd)]
    property MonthOrder: TMonthEnum read FMonthOrder write FMonthOrder;
  end;

  [MVCSerialize(stFields)]
  TEntitySerializeFields = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    property name: string read FName write FName;
  end;

  [MVCSerialize(stProperties)]
  TEntitySerializeProperties = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    [MVCNameAs('Name')]
    property name: string read FName write FName;
  end;

  TSale = class
  private
    FEntity: TEntityCustom;
    FNotes: TStringStream;
    FNotesAsString: TStringStream;
  public
    constructor Create;
    destructor Destroy; override;

    property Entity: TEntityCustom read FEntity write FEntity;
    property Notes: TStringStream read FNotes write FNotes;

    [MVCSerializeAsString]
    property NotesAsString: TStringStream read FNotesAsString write FNotesAsString;
  end;

  TEntityWithArray = class
  private
    FId: Int64;
    FNames: TArray<String>;
    FValues: TArray<Integer>;
    FBooleans: TArray<Boolean>;
  public
    property Id: Int64 read FId write FId;
    property Names: TArray<String> read FNames write FNames;
    property Values: TArray<Integer> read FValues write FValues;
    property Booleans: TArray<Boolean> read FBooleans write FBooleans;
  end;

  IChildEntity = interface
  ['{4A4CF508-D64F-4205-9783-E91B888A8987}']
    function GetCode: Integer;
    procedure SetCode(const Value: Integer);
    function GetDescription: string;
    procedure SetDescription(const Value: string);

    property Code: Integer read GetCode write SetCode;
    property Description: string read GetDescription write SetDescription;
  end;

  IEntityWithInterface = interface
  ['{81B1323F-B3FF-4FBD-84F3-9BDBA9997D8E}']
    function GetId: Integer;
    procedure SetId(const Value: Integer);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetChildEntity: IChildEntity;
    procedure SetChildEntity(const Value: IChildEntity);

    property Id: Integer read GetId write SetId;
    property Name: string read GetName write SetName;
    property ChildEntity: IChildEntity read GetChildEntity write SetChildEntity;
  end;

  TEntityWithInterface = class(TInterfacedObject, IEntityWithInterface)
  private
    FId: Integer;
    FName: string;
    FChildEntity: IChildEntity;
    function GetId: Integer;
    procedure SetId(const Value: Integer);
    function GetName: string;
    procedure SetName(const Value: string);
    function GetChildEntity: IChildEntity;
    procedure SetChildEntity(const Value: IChildEntity);
  public
    constructor Create;
    property Id: Integer read GetId write SetId;
    property Name: string read GetName write SetName;
    property ChildEntity: IChildEntity read GetChildEntity write SetChildEntity;
  end;

  TChildEntity = class(TInterfacedObject, IChildEntity)
  private
    FCode: Integer;
    FDescription: string;
    function GetCode: Integer;
    procedure SetCode(const Value: Integer);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Code: Integer read GetCode write SetCode;
    property Description: string read GetDescription write SetDescription;
  end;

  TGenericEntity<T: class> = class
  private
    FCode: Integer;
    FItems: TObjectList<T>;
    FDescription: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Code: Integer read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property Items: TObjectList<T> read FItems write FItems;
  end;

  TMultipleGenericEntity<T1: class; T2: class> = class
  private
    FCode: Integer;
    FItems: TObjectList<T1>;
    FItems2: TObjectList<T2>;
    FDescription: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Code: Integer read FCode write FCode;
    property Description: string read FDescription write FDescription;
    property Items: TObjectList<T1> read FItems write FItems;
    property Items2: TObjectList<T2> read FItems2 write FItems2;
  end;

  TEntityWithStringDictionary = class
  private
    FDict: TMVCStringDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    property Dict: TMVCStringDictionary read FDict write FDict;
  end;


implementation


{ TEntity }

constructor TEntity.Create;
begin
  inherited Create;
  FDepartment := TDepartment.Create;
  FNotes := TObjectList<TNote>.Create(True);
  FNotesEmpty := TObjectList<TNote>.Create(True);
end;

destructor TEntity.Destroy;
begin
  FDepartment.Free;
  FNotes.Free;
  FNotesEmpty.Free;
  inherited Destroy;
end;

{ TDepartment }

constructor TDepartment.Create;
begin
  FNotes := TObjectList<TNote>.Create(True);
end;

destructor TDepartment.Destroy;
begin
  FNotes.Free;
  inherited;
end;

{ TNote }

constructor TNote.Create(const ADescription: string);
begin
  inherited Create;
  FDescription := ADescription;
end;

{ TSale }

constructor TSale.Create;
begin
  FEntity := TEntityCustom.Create;
  FNotes := TStringStream.Create;
  FNotesAsString := TStringStream.Create;
end;

destructor TSale.Destroy;
begin
  FEntity.Free;
  FNotes.Free;
  FNotesAsString.Free;
  inherited;
end;

{ TMVCNullable<T> }

procedure TMVCNullable<T>.Clear;
begin
  fHasValue := False;
  fValue := default (T);
end;

function TMVCNullable<T>.GetHasValue: Boolean;
begin
  Result := fHasValue;
end;

procedure TMVCNullable<T>.SetValue(const Value: T);
begin
  fValue := Value;
  fHasValue := True;
end;

{ TEntityWithInterface }

constructor TEntityWithInterface.Create;
begin
  FChildEntity := TChildEntity.Create;
end;

function TEntityWithInterface.GetChildEntity: IChildEntity;
begin
  Result := FChildEntity;
end;

function TEntityWithInterface.GetId: Integer;
begin
  Result := FId;
end;

function TEntityWithInterface.GetName: string;
begin
  Result := FName;
end;

procedure TEntityWithInterface.SetChildEntity(const Value: IChildEntity);
begin
  FChildEntity := Value;
end;

procedure TEntityWithInterface.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TEntityWithInterface.SetName(const Value: string);
begin
  FName := Value;
end;


{ TChildEntity }

function TChildEntity.GetCode: Integer;
begin
  Result := FCode;
end;

function TChildEntity.GetDescription: string;
begin
  Result := FDescription;
end;

procedure TChildEntity.SetCode(const Value: Integer);
begin
  FCode := Value;
end;

procedure TChildEntity.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

{ TGenericEntity<T> }

constructor TGenericEntity<T>.Create;
begin
  inherited Create;
  FItems := TObjectList<T>.Create;
end;

destructor TGenericEntity<T>.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

constructor TMultipleGenericEntity<T1,T2>.Create;
begin
  inherited Create;
  FItems := TObjectList<T1>.Create;
  FItems2 := TObjectList<T2>.Create;
end;

destructor TMultipleGenericEntity<T1,T2>.Destroy;
begin
  FItems.Free;
  FItems2.Free;
  inherited Destroy;
end;


{ TEntityWithStringDictionary }

constructor TEntityWithStringDictionary.Create;
begin
  FDict := TMVCStringDictionary.Create;
end;

destructor TEntityWithStringDictionary.Destroy;
begin
  FDict.Free;
  inherited;
end;

end.
