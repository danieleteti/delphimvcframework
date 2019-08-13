// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Serializer.Commons;

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
    [MVCListOf(TNote)]
    FNotes: TObjectList<TNote>;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Int64 read FId write FId;
    [MVCNameAs('Name')]
    property name: string read FName write FName;

    [MVCListOf(TNote)]
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

    [MVCListOf(TNote)]
    FNotes: TObjectList<TNote>;

    [MVCListOf(TNote)]
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

    [MVCListOf(TNote)]
    property Notes: TObjectList<TNote> read FNotes;

    [MVCListOf(TNote)]
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
  public
    property GuidValue: TGUID read FGuidValue write FGuidValue;
  end;

  TColorEnum = (RED, GREEN, BLUE);
  TMonthEnum = (meJanuary, meFebruary, meMarch, meApril);

  TEntityWithEnums = class
  private
    FId: Int64;
    FCode: Integer;
    FName: string;
    FColor: TColorEnum;
    FMonthName: TMonthEnum;
    FMonthOrder: TMonthEnum;
  public
    property Id: Int64 read FId write FId;
    property Code: Integer read FCode write FCode;
    [MVCNameAs('Name')]
    property name: string read FName write FName;
    property Color: TColorEnum read FColor write FColor;
    [MVCEnumSerializationTypeAttribute(estEnumName, 'me')]
    property MonthName: TMonthEnum read FMonthName write FMonthName;
    [MVCEnumSerializationTypeAttribute(estEnumOrd)]
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
  public
    property Id: Int64 read FId write FId;
    property Names: TArray<String> read FNames write FNames;
    property Values: TArray<Integer> read FValues write FValues;
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

end.
