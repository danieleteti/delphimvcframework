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

unit Serializers.JsonDataObjectsTestU;

interface

uses
  DUnitX.TestFramework,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Tests.Serializer.Intf,
  MVCFramework.Tests.Serializer.Entities,
  MVCFramework.Tests.Serializer.EntitiesModule,
  JsonDataObjects,
  MVCFramework.DataSet.Utils;

type

  [TestFixture]
  TMVCTestSerializerJsonDataObjects = class(TObject)
  private
    fSerializer: IMVCSerializer;
  public
    [SetupFixture]
    procedure SetupFixture;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { serialize declarations }
    [Test]
    procedure TestSerializeAllTypes;
    [Test]
    procedure TestSerializeDateTimeProperty;
    [Test]
    procedure TestSerializeAllNullableTypes;
    [Test]
    procedure TestSerializeAllTypesInList;
    [Test]
    procedure TestSerializeEntity;
    [Test]
    procedure TestSerializeNil;
    [Test]
    procedure TestSerializeEntityUpperCaseNames;
    [Test]
    procedure TestSerializeEntityWithArray;
    [Test]
    procedure TestSerializeEntityLowerCaseNames;
    [Test]
    procedure TestSerializeEntityNameAs;
    [Test]
    procedure TestSerializeEntityCustomSerializer;
    [Test]
    procedure TestSerializeEntityCustomMemberSerializer;
    [Test]
    procedure TestSerializeEntitySerializationType;
    [Test]
    procedure TestSerializeCollection;
    [Test]
    procedure TestSerializeDataSet;
    [Test]
    [Category('datasets')]
    procedure TestDataSetHelpers;
    { deserialize declarations }
    [Test]
    procedure TestDeserializeEntity;
    [Test]
    procedure TestDeserializeEntityCustomSerializer;
    [Test]
    procedure TestDeserializeEntityCustomValueTypeSerializer;
    [Test]
    procedure TestDeserializeEntityCustomMemberSerializer;
    [Test]
    procedure TestDeserializeEntitySerializationType;
    [Test]
    procedure TestDeserializeCollection;
    [Test]
    procedure TestDeserializeDataSet;
    [Test]
    procedure TestSerializeEmptyDataSet;
    [Test]
    // [Category('this')]
    procedure TestDeserializeEntityWithArray;
    { full cycle }
    [Test]
    procedure TestSerializeDeSerializeEntityWithEnums;
    [Test]
    procedure TestSerializeDeSerializeEntityWithSet;
    [Test]
    procedure TestStringDictionary;
    [Test]
    procedure TestSerializeDeserializeGuid;
    [Test]
    procedure TestSerializeDeserializeEntityWithInterface;

    [Test]
    procedure TestSerializeDeserializeGenericEntity;
    [Test]
    procedure TestSerializeDeserializeMultipleGenericEntity;
    [Test]
    procedure TestDoNotSerializeDoNotDeSerialize;
    [Test]
    [Category('serializers')]
    procedure TestSerializeListOfSomething;

    [Test]
    [Category('serializers')]
    procedure TestSerializeListWithNulls;

    [Test]
    [Category('serializers')]
    procedure TestSerializeListWithNulls2;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyUnassigned_JSONExists;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyAssigned_JSONExists;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyAssigned_JSONNull;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyAssigned_JSONNotExists;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyUnAssigned_JSONNull;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithFieldsUnassigned_JSONExists;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedField_WithFieldsAssigned_JSONNull;

    [Test]
    [Category('serializers')]
    procedure TestDeserializeOwnedProperty_WithPropertyUnassigned_JSONExists_Polimorphic;
  end;

  TMVCEntityCustomSerializerJsonDataObjects = class(TInterfacedObject, IMVCTypeSerializer)
  private
    { private declarations }
  protected
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>);
  public
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);

  end;

  TMVCNullableIntegerSerializerJsonDataObjects = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
  end;

  /// <summary>
  /// When using nested generic types it is necessary to declare explicitly for delphi's RTTI to recognize them.
  /// </summary>
  TNestedGenericEntity = TGenericEntity<TGenericEntity<TNote>>;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Commons, System.TypInfo, BOs, BusinessObjectsU;

const
  LINE_BREAK = #$A;
  TAB_SPACE = #9;

  { TMVCTestSerializerJsonDataObjects }

procedure TMVCTestSerializerJsonDataObjects.Setup;
begin
  inherited;
  fSerializer := TMVCJsonDataObjectsSerializer.Create;
  fSerializer.RegisterTypeSerializer(System.TypeInfo(TStream), TMVCStreamSerializerJsonDataObject.Create);
  fSerializer.RegisterTypeSerializer(System.TypeInfo(TStringStream), TMVCStreamSerializerJsonDataObject.Create);
  fSerializer.RegisterTypeSerializer(System.TypeInfo(TMemoryStream), TMVCStreamSerializerJsonDataObject.Create);
  fSerializer.RegisterTypeSerializer(System.TypeInfo(TEntityCustom), TMVCEntityCustomSerializerJsonDataObjects.Create);
  fSerializer.RegisterTypeSerializer(System.TypeInfo(TMVCNullable<Integer>),
    TMVCNullableIntegerSerializerJsonDataObjects.Create);
end;

procedure TMVCTestSerializerJsonDataObjects.SetupFixture;
begin
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  FormatSettings.DateSeparator:= '/';
  FormatSettings.TimeSeparator:= ':';
end;

procedure TMVCTestSerializerJsonDataObjects.TearDown;
begin
  inherited;
  fSerializer := nil;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDataSetHelpers;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":"1987-10-15",' + '"AccessDateTime":"2017-02-17T16:37:50.000+01:00",' + '"AccessTime":"16:40:50",' +
    '"Active":true,' + '"Amount":100,' + '"BlobFld":"PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' +
    '"Items":[' + '{' + '"Id":1,' + '"Name":"Ezequiel Juliano Müller"' + '},' + '{' + '"Id":2,' + '"Name":"Juliano"' +
    '}' + '],' + '"Departament":{' + '"Name":"Depto1"' + '},' + '"GUID":"{9386C957-5379-4370-8492-8FA464A9CF0C}"' + '}';

  JSON_LOWERCASE = '{' + '"id":1,' + '"name":"Ezequiel Juliano Müller"' + '}';

  JSON_UPPERCASE = '{' + '"ID":1,' + '"NAME":"Ezequiel Juliano Müller"' + '}';

  JSON_ASIS = '{' + '"Id":1,' + '"Name":"Ezequiel Juliano Müller"' + '}';

  JSON_LIST = '[' + '{' + '"Id_Id":1,' + '"Name_Name":"Ezequiel Juliano Müller"' + '},' + '{' + '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' + '}' + ']';

var
  Dm: TEntitiesModule;
  S: string;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    Dm.Entity.Insert;
    Dm.EntityId.AsLargeInt := 1;
    Dm.EntityCode.AsInteger := 2;
    Dm.EntityName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityBirthday.AsDateTime := StrToDate('15/10/1987');
    Dm.EntityAccessDateTime.AsDateTime := StrToDateTime('17/02/2017 16:37:50');
    Dm.EntityAccessTime.AsDateTime := StrToTime('16:40:50');
    Dm.EntityActive.AsBoolean := True;
    Dm.EntitySalary.AsCurrency := 100;
    Dm.EntityAmount.AsFloat := 100;
    Dm.EntityBlobFld.AsString := '<html><body><h1>BLOB</h1></body></html>';
    Dm.EntityGUID.AsGuid := StringToGUID('{9386C957-5379-4370-8492-8FA464A9CF0C}');

    Dm.Item.Insert;
    Dm.ItemId.AsLargeInt := 1;
    Dm.ItemName.AsString := 'Ezequiel Juliano Müller';
    Dm.Item.Post;

    Dm.Item.Insert;
    Dm.ItemId.AsLargeInt := 2;
    Dm.ItemName.AsString := 'Juliano';
    Dm.Item.Post;

    Dm.Departament.Insert;
    Dm.DepartamentName.AsString := 'Depto1';
    Dm.Departament.Post;

    S := Dm.Entity.AsJSONObject(ncAsIs, ['Ignored']);
    Assert.areEqual(JSON, S, False);

    Dm.Item.First;
    S := Dm.Item.AsJSONObject(ncAsIs);
    Assert.areEqual(JSON_ASIS, S, False);

    Dm.Item.First;
    S := Dm.Item.AsJSONObject(ncUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S, False);

    Dm.Item.First;
    S := Dm.Item.AsJSONObject(ncLowerCase);
    Assert.areEqual(JSON_LOWERCASE, S, False);

  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeCollection;

  procedure CheckObjectList(const AList: TObjectList<TNote>);
  begin
    Assert.isTrue(AList.Count = 4);
    Assert.isTrue(AList.Items[0].Description = 'Description 1');
    Assert.isTrue(AList.Items[1].Description = 'Description 2');
    Assert.isTrue(AList.Items[2].Description = 'Description 3');
    Assert.isTrue(AList.Items[3].Description = 'Description 4');
  end;

const
  JSON_PROPERTIES = '[' + '{' + '"Description":"Description 1"' + '},' + '{' + '"Description":"Description 2"' + '},' +
    '{' + '"Description":"Description 3"' + '},' + '{' + '"Description":"Description 4"' + '}' + ']';

  JSON_FIELDS = '[' + '{' + '"FDescription":"Description 1"' + '},' + '{' + '"FDescription":"Description 2"' + '},' +
    '{' + '"FDescription":"Description 3"' + '},' + '{' + '"FDescription":"Description 4"' + '}' + ']';
var
  O: TObjectList<TNote>;
begin
  O := TObjectList<TNote>.Create(True);
  try
    fSerializer.DeserializeCollection(JSON_PROPERTIES, O, TNote);
    CheckObjectList(O);
  finally
    O.Free;
  end;

  O := TObjectList<TNote>.Create(True);
  try
    fSerializer.DeserializeCollection(JSON_FIELDS, O, TNote, stFields);
    CheckObjectList(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeDataSet;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":"1987-10-15",' + '"AccessDateTime":"2017-02-17 16:37:50",' + '"AccessTime":"16:40:50",' +
    '"Active":true,' + '"Amount":100,' + '"BlobFld":"PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' +
    '"Items":[' + '{' + '"Id":1,' + '"Name":"Ezequiel"' + '},' + '{' + '"Id":2,' + '"Name":"Juliano"' + '}' + '],' +
    '"Departament":{' + '"Name":"Depto1"' + '},' + '"GUID":"{9386C957-5379-4370-8492-8FA464A9CF0C}"' + '}';

  JSON_LOWERCASE = '{' + '"id":1,' + '"name":"Ezequiel Juliano Müller"' + '}';

  JSON_UPPERCASE = '{' + '"ID":1,' + '"NAME":"Ezequiel Juliano Müller"' + '}';

  JSON_ASIS = '{' + '"Id_Id":1,' + '"Name_Name":"Ezequiel Juliano Müller"' + '}';

  JSON_LIST = '[' + '{' + '"Id_Id":1,' + '"Name_Name":"Ezequiel Juliano Müller"' + '},' + '{' + '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' + '}' + ']';

  JSON_ITEMS = '{' + '"items":[' + '{' + '"Id_Id":1,' + '"Name_Name":"Pedro Henrique de Oliveira"' + '},' + '{' +
    '"Id_Id":2,' + '"Name_Name":"Rogers Abe"' + '}' + '],' + '"meta":{"count":"2"}}';
var
  Dm: TEntitiesModule;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    fSerializer.DeserializeDataSetRecord(JSON, Dm.Entity, ['Ignored']);
    Assert.isTrue(Dm.EntityId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityCode.AsInteger = 2);
    Assert.isTrue(Dm.EntityName.AsString = 'Ezequiel Juliano Müller');
    Assert.isTrue(Dm.EntityBirthday.AsDateTime = StrToDate('15/10/1987'));
    Assert.isTrue(Dm.EntityAccessDateTime.AsDateTime = StrToDateTime('17/02/2017 16:37:50'));
    Assert.isTrue(Dm.EntityAccessTime.AsDateTime = StrToTime('16:40:50'));
    Assert.isTrue(Dm.EntityActive.AsBoolean = True);
    Assert.isTrue(Dm.EntitySalary.AsCurrency = 100);
    Assert.isTrue(Dm.EntityAmount.AsFloat = 100);
    Assert.isTrue(Dm.EntityBlobFld.AsString = '<html><body><h1>BLOB</h1></body></html>');
    Assert.isTrue(GUIDToString(Dm.EntityGUID.AsGuid) = '{9386C957-5379-4370-8492-8FA464A9CF0C}');

    Dm.Item.First;
    Assert.isTrue(Dm.ItemId.AsLargeInt = 1);
    Assert.isTrue(Dm.ItemName.AsString = 'Ezequiel');

    Dm.Item.Next;
    Assert.isTrue(Dm.ItemId.AsLargeInt = 2);
    Assert.isTrue(Dm.ItemName.AsString = 'Juliano');

    Dm.Departament.First;
    Assert.isTrue(Dm.DepartamentName.AsString = 'Depto1');

    fSerializer.DeserializeDataSetRecord(JSON_LOWERCASE, Dm.EntityLowerCase);
    Assert.isTrue(Dm.EntityLowerCaseId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityLowerCaseName.AsString = 'Ezequiel Juliano Müller');

    fSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase);
    Assert.isTrue(Dm.EntityUpperCaseId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCaseName.AsString = 'Ezequiel Juliano Müller');

    fSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.isTrue(Dm.EntityUpperCase2Id.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCase2Name.AsString = 'Ezequiel Juliano Müller');

    fSerializer.DeserializeDataSetRecord(JSON_ASIS, Dm.EntityAsIs);
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.EmptyDataSet;
    fSerializer.DeserializeDataSet(JSON_LIST, Dm.EntityAsIs);
    Dm.EntityAsIs.First;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.Next;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 2);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.EmptyDataSet;
    Dm.EntityAsIs.LoadJSONArrayFromJSONObjectProperty('items', JSON_ITEMS, ncAsIs);
    Dm.EntityAsIs.First;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Pedro Henrique de Oliveira');

    Dm.EntityAsIs.Next;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 2);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Rogers Abe');
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntity;

  procedure CheckObject(const AEntity: TEntity);
  begin
    Assert.isTrue(AEntity.Id = 1);
    Assert.isTrue(AEntity.Code = 2);
    Assert.isTrue(AEntity.Name = 'Ezequiel Juliano Müller');
    Assert.isTrue(AEntity.Salary = 100);
    Assert.isTrue(DateToStr(AEntity.Birthday) = '15/10/1987');
    Assert.isTrue(DateTimeToStr(AEntity.AccessDateTime) = '17/02/2017 16:37:50');
    Assert.isTrue(TimeToStr(AEntity.AccessTime) = '16:40:50');
    Assert.isTrue(AEntity.Active = True);
    Assert.isTrue(AEntity.Role = TRole.roGuest);
    Assert.isTrue(DateTimeToStr(TimeStampToDateTime(AEntity.Teporization)) = '17/02/2017 16:37:50');
    Assert.isTrue(AEntity.Department <> nil);
    Assert.isTrue(AEntity.Department.Id = 1);
    Assert.isTrue(AEntity.Department.Name = 'Development');
    Assert.isTrue(AEntity.DepartmentNull = nil);
    Assert.isTrue(AEntity.Notes.Count = 2);
    Assert.isTrue(AEntity.Notes[0].Description = 'EntNote1');
    Assert.isTrue(AEntity.Notes[1].Description = 'EntNote2');
    Assert.isTrue(AEntity.NotesEmpty.Count = 0);
    Assert.isTrue(AEntity.AppreciationAs.AsString = 'Yes');
    Assert.isTrue(AEntity.Appreciation.AsString = 'Yes');
  end;

const
  JSON_PROPERTIES = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":"1987-10-15",' + '"AccessDateTime":"2017-02-17T16:37:50",' + '"AccessTime":"16:40:50",' +
    '"Active":true,' + '"Role":"roGuest",' + '"Teporization":63623032670000,' + '"Department":{' + '"Id":1,' +
    '"Name":"Development",' + '"Notes":[' + '{' + '"Description":"DepNote1"' + '},' + '{' + '"Description":"DepNote2"' +
    '}' + ']' + '},' + '"DepartmentNull":null,' + '"Notes":[' + '{' + '"Description":"EntNote1"' + '},' + '{' +
    '"Description":"EntNote2"' + '}' + '],' + '"NotesEmpty":[],' + '"AppreciationAs":"Yes",' + '"Appreciation":{' +
    '"type":"ustring",' + '"value":"Yes"' + '}' + '}';

  JSON_FIELDS = '{' + '"FId":1,' + '"FCode":2,' + '"FName":"Ezequiel Juliano Müller",' + '"FSalary":100,' +
    '"FBirthday":"1987-10-15",' + '"FAccessDateTime":"2017-02-17T16:37:50",' + '"FAccessTime":"16:40:50",' +
    '"FActive":true,' + '"FRole":"roGuest",' + '"FTeporization":63623032670000,' + '"FDepartment":{' + '"FId":1,' +
    '"FName":"Development",' + '"FNotes":[' + '{' + '"FDescription":"DepNote1"' + '},' + '{' +
    '"FDescription":"DepNote2"' + '}' + ']' + '},' + '"FDepartmentNull":null,' + '"FNotes":[' + '{' +
    '"FDescription":"EntNote1"' + '},' + '{' + '"FDescription":"EntNote2"' + '}' + '],' + '"FNotesEmpty":[],' +
    '"FAppreciationAs":"Yes",' + '"FAppreciation":{' + '"type":"ustring",' + '"value":"Yes"' + '}' + '}';
var
  O: TEntity;
begin
  O := TEntity.Create;
  try
    fSerializer.DeserializeObject(JSON_PROPERTIES, O);
    CheckObject(O);
  finally
    O.Free;
  end;

  O := TEntity.Create;
  try
    fSerializer.DeserializeObject(JSON_FIELDS, O, stFields);
    CheckObject(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomMemberSerializer;
const
  JSON = '{' + '"Entity":{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller"' + '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' + '"NotesAsString":"Ezequiel Juliano Müller"' + '}';
var
  O: TSale;
begin
  O := TSale.Create;
  try
    fSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Entity.Id = 1);
    Assert.isTrue(O.Entity.Code = 2);
    Assert.isTrue(O.Entity.Name = 'Ezequiel Juliano Müller');
    Assert.isTrue(O.Notes.DataString = 'Ezequiel Juliano Müller');
    Assert.isTrue(O.NotesAsString.DataString = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomSerializer;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller"' + '}';
var
  O: TEntityCustom;
begin
  O := TEntityCustom.Create;
  try
    fSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Id = 1);
    Assert.isTrue(O.Code = 2);
    Assert.isTrue(O.Name = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomValueTypeSerializer;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"NullableInteger":3' + '}';
var
  O: TEntityCustomWithNullables;
begin
  O := TEntityCustomWithNullables.Create;
  try
    fSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Id = 1);
    Assert.isTrue(O.Code = 2);
    Assert.isTrue(O.Name = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntitySerializationType;
const
  JSON_FIELDS = '{' + '"FId":1,' + '"FCode":2,' + '"FName":"Ezequiel Juliano Müller"' + '}';

  JSON_PROPERTIES = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller"' + '}';
var
  OFields: TEntitySerializeFields;
  OProperties: TEntitySerializeProperties;
begin
  OFields := TEntitySerializeFields.Create;
  try
    fSerializer.DeserializeObject(JSON_FIELDS, OFields);
    Assert.isTrue(OFields.Id = 1);
    Assert.isTrue(OFields.Code = 2);
    Assert.isTrue(OFields.Name = 'Ezequiel Juliano Müller');
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    fSerializer.DeserializeObject(JSON_PROPERTIES, OProperties);
    Assert.isTrue(OProperties.Id = 1);
    Assert.isTrue(OProperties.Code = 2);
    Assert.isTrue(OProperties.Name = 'Ezequiel Juliano Müller');
  finally
    OProperties.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityWithArray;
  procedure CheckObject(const AEntity: TEntityWithArray);
  begin
    Assert.isTrue(AEntity.Id = 1);
    Assert.isTrue(AEntity.Names[0] = 'Pedro');
    Assert.isTrue(AEntity.Names[1] = 'Oliveira');
    Assert.isTrue(AEntity.Values[0] = 1);
    Assert.isTrue(AEntity.Values[1] = 2);
    Assert.isTrue(AEntity.Booleans[0] = True);
    Assert.isTrue(AEntity.Booleans[1] = False);
    Assert.isTrue(AEntity.Booleans[2] = True);
  end;

const
  JSON_WITH_ARRAY = '{' + '"Id":1,' + '"Names":["Pedro","Oliveira"],' + '"Values":[1,2],"Booleans":[true,false,true]}';
var
  O: TEntityWithArray;
begin
  O := TEntityWithArray.Create;
  try
    fSerializer.DeserializeObject(JSON_WITH_ARRAY, O);
    CheckObject(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyUnassigned_JSONExists;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false,' +
	'  "mentor": { ' +
  '			"mentor": null, ' +
  '			"skills": "superb programmer", ' +
  '			"firstname": "mentor firstname", ' +
  '			"lastname": "mentor lasttname", ' +
  '			"dob": null, ' +
  '			"married": false, ' +
  '			"id": 2 ' +
  '			}' +
  '  }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNotNull(lProgrammerEx.Mentor);
    Assert.IsNull(lProgrammerEx.Mentor.Mentor);
    Assert.AreEqual('mentor firstname', lProgrammerEx.Mentor.FirstName);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyUnassigned_JSONExists_Polimorphic;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false,' +
	'  "mentor": { ' +
  '			"mentor": null, ' +
  '			"skills": "superb programmer", ' +
  '			"firstname": "mentor firstname", ' +
  '			"lastname": "mentor lasttname", ' +
  '			"dob": null, ' +
  '			"married": false, ' +
  '			"id": 2 ' +
  '			}' +
  '  }';
var
  lProgrammerEx: TProgrammerEx2;
begin
  lProgrammerEx := TProgrammerEx2.Create;
  try
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNotNull(lProgrammerEx.Mentor);
    Assert.IsTrue(lProgrammerEx.Mentor is TProgrammerEx2, lProgrammerEx.Mentor.ClassName);
    Assert.AreEqual('mentor firstname', lProgrammerEx.Mentor.FirstName);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyUnAssigned_JSONNull;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false,' +
	'  "mentor": null ' +
  '  }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNull(lProgrammerEx.Mentor);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedField_WithFieldsAssigned_JSONNull;
const
  lJSON = '{' +
  ' "fskills": "",' +
  ' "fid": 2,' +
  ' "ffirstname": "child firstname",' +
  ' "flastname": "child lastname",' +
  ' "fdob": null,' +
  ' "fmarried": false,' +
	' "fmentor": null ' +
  ' }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    lProgrammerEx.Mentor := TProgrammerEx.Create;
    fSerializer.DeserializeObject(lJSON, lProgrammerEx, stFields);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNull(lProgrammerEx.Mentor);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithFieldsUnassigned_JSONExists;
const
  lJSON = '{' +
  ' "fskills": "",' +
  ' "fid": 2,' +
  ' "ffirstname": "child firstname",' +
  ' "flastname": "child lastname",' +
  ' "fdob": null,' +
  ' "fmarried": false,' +
	' "fmentor": { ' +
  '			"fmentor": null, ' +
  '			"fskills": "superb programmer", ' +
  '			"ffirstname": "mentor firstname", ' +
  '			"flastname": "mentor lasttname", ' +
  '			"fdob": null, ' +
  '			"fmarried": false, ' +
  '			"fid": 2 ' +
  '			}' +
  ' }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    fSerializer.DeserializeObject(lJSON, lProgrammerEx, stFields);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNotNull(lProgrammerEx.Mentor);
    Assert.IsNull(lProgrammerEx.Mentor.Mentor);
    Assert.AreEqual('mentor firstname', lProgrammerEx.Mentor.FirstName);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyAssigned_JSONExists;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false,' +
	'  "mentor": { ' +
  '			"mentor": null, ' +
  '			"skills": "superb programmer", ' +
  '			"firstname": "mentor firstname", ' +
  '			"lastname": "mentor lasttname", ' +
  '			"dob": null, ' +
  '			"married": false, ' +
  '			"id": 2 ' +
  '			}' +
  '  }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    lProgrammerEx.Mentor := TProgrammerEx.Create;
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNotNull(lProgrammerEx.Mentor);
    Assert.IsNull(lProgrammerEx.Mentor.Mentor);
    Assert.AreEqual('mentor firstname', lProgrammerEx.Mentor.FirstName);
  finally
    lProgrammerEx.Free;
  end;
end;


procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyAssigned_JSONNotExists;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false' +
  '  }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    lProgrammerEx.Mentor := TProgrammerEx.Create;
    lProgrammerEx.Mentor.FirstName := 'existent_value';
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNotNull(lProgrammerEx.Mentor);
    Assert.AreEqual('existent_value', lProgrammerEx.Mentor.FirstName);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeOwnedProperty_WithPropertyAssigned_JSONNull;
const
  lJSON = '{' +
  ' "skills": "",' +
  '  "id": 2,' +
  '  "firstname": "child firstname",' +
  '  "lastname": "child lastname",' +
  '  "dob": null,' +
  '  "married": false,' +
	'  "mentor": null ' +
  '  }';
var
  lProgrammerEx: TProgrammerEx;
begin
  lProgrammerEx := TProgrammerEx.Create;
  try
    lProgrammerEx.Mentor := TProgrammerEx.Create;
    fSerializer.DeserializeObject(lJSON, lProgrammerEx);
    Assert.AreEqual('child firstname', lProgrammerEx.FirstName);
    Assert.IsNull(lProgrammerEx.Mentor);
  finally
    lProgrammerEx.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDoNotSerializeDoNotDeSerialize;
var
  lObj: TPartialSerializableType;
  lStr: string;
begin
  lObj := TPartialSerializableType.Create;
  try
    lStr := fSerializer.SerializeObject(lObj);
    Assert.DoesNotContain(lStr, 'prop1', False);
    Assert.Contains(lStr, 'prop2', False);
    Assert.DoesNotContain(lStr, 'prop3', False);
    Assert.Contains(lStr, 'prop4', False);
  finally
    lObj.Free;
  end;

  lObj := TPartialSerializableType.Create;
  try
    fSerializer.DeserializeObject('{"prop1":"x1","prop2":"x2","prop3":"x3","prop4":"x4"}', lObj);
    Assert.areEqual('x1', lObj.Prop1);
    Assert.areEqual('prop2', lObj.Prop2);
    Assert.areEqual('prop3', lObj.Prop3);
    Assert.areEqual('x4', lObj.Prop4);
  finally
    lObj.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeAllNullableTypes;
var
  lObj1, lObj2: BusinessObjectsU.TNullablesTest;
  lSerWithNulls, lSerWithoutNulls: string;
begin
  Assert.IsTrue(MVCSerializeNulls, 'By Default "MVCSerializeNulls" must be true');
  lObj1 := BusinessObjectsU.TNullablesTest.Create;
  try
    lObj1.LoadSomeData;
    lSerWithNulls := fSerializer.SerializeObject(lObj1);
    lObj2 := BusinessObjectsU.TNullablesTest.Create;
    try
      fSerializer.DeserializeObject(lSerWithNulls, lObj2);
      Assert.IsTrue(lObj1.Equals(lObj2));
    finally
      lObj2.Free;
    end;
  finally
    lObj1.Free;
  end;

  MVCSerializeNulls := False;
  try
    lObj1 := BusinessObjectsU.TNullablesTest.Create;
    try
      //lObj1.LoadSomeData;
      lSerWithoutNulls := fSerializer.SerializeObject(lObj1);
      Assert.AreNotEqual(lSerWithNulls, lSerWithoutNulls);
      lObj2 := BusinessObjectsU.TNullablesTest.Create;
      try
        fSerializer.DeserializeObject(lSerWithoutNulls, lObj2);
        Assert.IsTrue(lObj1.Equals(lObj2));
      finally
        lObj2.Free;
      end;
    finally
      lObj1.Free;
    end;
  finally
    MVCSerializeNulls := True;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeAllTypes;
var
  lObj1, lObj2: TMyObject;
  lSer: string;
begin
  lObj1 := GetMyObject;
  try
    lSer := fSerializer.SerializeObject(lObj1);
    lObj2 := TMyObject.Create;
    try
      fSerializer.DeserializeObject(lSer, lObj2);
      Assert.isTrue(lObj1.Equals(lObj2));
    finally
      lObj2.Free;
    end;
  finally
    lObj1.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeAllTypesInList;
var
  lList1, lList2: TObjectList<TMyObject>;
  lSer: string;
  I: Integer;
  lObj: TMyObject;
begin
  lList1 := TObjectList<TMyObject>.Create;
  try
    for I := 0 to 9 do
    begin
      lObj := GetMyObject;
      lObj.PropJSONObject.I['value'] := I;
      lList1.Add(lObj);
    end;

    lSer := fSerializer.SerializeCollection(lList1);

    lList2 := TObjectList<TMyObject>.Create;
    try
      fSerializer.DeserializeCollection(lSer, lList2, TMyObject);
      for I := 0 to 9 do
      begin
        Assert.isTrue(lList1[I].Equals(lList2[I]));
      end;
    finally
      lList2.Free;
    end;
  finally
    lList1.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeCollection;
const
  JSON = '[' + '{' + '"Description":"Description 1"' + '},' + '{' + '"Description":"Description 2"' + '},' + '{' +
    '"Description":"Description 3"' + '},' + '{' + '"Description":"Description 4"' + '}' + ']';

  JSON_FIELDS = '[' + '{' + '"FDescription":"Description 1"' + '},' + '{' + '"FDescription":"Description 2"' + '},' +
    '{' + '"FDescription":"Description 3"' + '},' + '{' + '"FDescription":"Description 4"' + '}' + ']';
var
  O: TObjectList<TNote>;
  S: string;
begin
  O := TObjectList<TNote>.Create(True);
  try
    O.Add(TNote.Create('Description 1'));
    O.Add(TNote.Create('Description 2'));
    O.Add(TNote.Create('Description 3'));
    O.Add(TNote.Create('Description 4'));

    S := fSerializer.SerializeCollection(O);
    Assert.areEqual(JSON, S);

    S := fSerializer.SerializeCollection(O, stFields);
    Assert.areEqual(JSON_FIELDS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDataSet;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":"1987-10-15",' + '"AccessDateTime":"2017-02-17T16:37:50.000+01:00",' + '"AccessTime":"16:40:50",' +
    '"Active":true,' + '"Amount":100,' + '"BlobFld":"PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' +
    '"Items":[' + '{' + '"Id":1,' + '"Name":"Ezequiel"' + '},' + '{' + '"Id":2,' + '"Name":"Juliano"' + '}' + '],' +
    '"Departament":{' + '"Name":"Depto1"' + '},' + '"GUID":"{9386C957-5379-4370-8492-8FA464A9CF0C}"' + '}';

  JSON_LOWERCASE = '{' + '"id":1,' + '"name":"Ezequiel Juliano Müller"' + '}';

  JSON_UPPERCASE = '{' + '"ID":1,' + '"NAME":"Ezequiel Juliano Müller"' + '}';

  JSON_ASIS = '{' + '"Id_Id":1,' + '"Name_Name":"Ezequiel Juliano Müller"' + '}';

  JSON_LIST = '[' + '{' + '"Id_Id":1,' + '"Name_Name":"Ezequiel Juliano Müller"' + '},' + '{' + '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' + '}' + ']';

var
  Dm: TEntitiesModule;
  S: string;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    Dm.Entity.Insert;
    Dm.EntityId.AsLargeInt := 1;
    Dm.EntityCode.AsInteger := 2;
    Dm.EntityName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityBirthday.AsDateTime := StrToDate('15/10/1987');
    Dm.EntityAccessDateTime.AsDateTime := StrToDateTime('17/02/2017 16:37:50');
    Dm.EntityAccessTime.AsDateTime := StrToTime('16:40:50');
    Dm.EntityActive.AsBoolean := True;
    Dm.EntitySalary.AsCurrency := 100;
    Dm.EntityAmount.AsFloat := 100;
    Dm.EntityBlobFld.AsString := '<html><body><h1>BLOB</h1></body></html>';
    Dm.EntityGUID.AsGuid := StringToGUID('{9386C957-5379-4370-8492-8FA464A9CF0C}');

    Dm.Item.Insert;
    Dm.ItemId.AsLargeInt := 1;
    Dm.ItemName.AsString := 'Ezequiel';
    Dm.Item.Post;

    Dm.Item.Insert;
    Dm.ItemId.AsLargeInt := 2;
    Dm.ItemName.AsString := 'Juliano';
    Dm.Item.Post;

    Dm.Departament.Insert;
    Dm.DepartamentName.AsString := 'Depto1';
    Dm.Departament.Post;

    Dm.Entity.Post;
    S := fSerializer.SerializeDataSetRecord(Dm.Entity, ['Ignored'], ncAsIs);
    Assert.areEqual(JSON, S, False);

    Dm.EntityLowerCase.Insert;
    Dm.EntityLowerCaseId.AsLargeInt := 1;
    Dm.EntityLowerCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityLowerCase.Post;
    S := fSerializer.SerializeDataSetRecord(Dm.EntityLowerCase);
    Assert.areEqual(JSON_LOWERCASE, S, False, 'json lowercase');

    Dm.EntityUpperCase.Insert;
    Dm.EntityUpperCaseId.AsLargeInt := 1;
    Dm.EntityUpperCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase.Post;
    S := fSerializer.SerializeDataSetRecord(Dm.EntityUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S, False, 'json uppercase (1)');

    Dm.EntityUpperCase2.Insert;
    Dm.EntityUpperCase2Id.AsLargeInt := 1;
    Dm.EntityUpperCase2Name.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase2.Post;
    S := fSerializer.SerializeDataSetRecord(Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S, False, 'json uppercase (2)');

    Dm.EntityAsIs.Insert;
    Dm.EntityAsIsId.AsLargeInt := 1;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;
    S := fSerializer.SerializeDataSetRecord(Dm.EntityAsIs);
    Assert.areEqual(JSON_ASIS, S, False, 'json as is');

    Dm.EntityAsIs.Append;
    Dm.EntityAsIsId.AsLargeInt := 2;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;

    // serialize dataset
    S := fSerializer.SerializeDataSet(Dm.EntityAsIs);
    Assert.areEqual(JSON_LIST, S, False, 'json list');

    // serialize dataset as object
    S := fSerializer.SerializeObject(Dm.EntityAsIs);
    Assert.areEqual(JSON_LIST, S, False, 'json list');

  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDateTimeProperty;
var
  lObj1, lObj2: TMyObjectWithUTC;
  lSer: string;
begin
  lObj1 := TMyObjectWithUTC.Create;
  try
    lObj1.MyDateTime := EncodeDateTime(2020, 11, 4, 12, 12, 12, 0);
    lSer := fSerializer.SerializeObject(lObj1);
    lObj2 := TMyObjectWithUTC.Create;
    try
      fSerializer.DeserializeObject(lSer, lObj2);
      Assert.isTrue(lObj1.Equals(lObj2));
    finally
      lObj2.Free;
    end;
  finally
    lObj1.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeSerializeEntityWithEnums;
const
  JSON = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Daniele Teti",' + '"Color":"RED",' + '"MonthName":"January",' +
    '"MonthName2":"meFebruary",' + '"MonthOrder":0' + '}';
var
  O: TEntityWithEnums;
  S: string;
begin
  O := TEntityWithEnums.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Daniele Teti';
    O.Color := TColorEnum.RED;
    O.MonthName := TMonthEnum.meJanuary;
    O.MonthName2 := TMonthEnum.meFebruary;
    O.MonthOrder := TMonthEnum.meJanuary;
    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;

  O := TEntityWithEnums.Create;
  try
    fSerializer.DeserializeObject(S, O);
    Assert.areEqual(int64(1), O.Id);
    Assert.areEqual(2, O.Code);
    Assert.areEqual('Daniele Teti', O.Name);
    Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthName));
    Assert.areEqual(Ord(TMonthEnum.meFebruary), Ord(O.MonthName2));
    Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthOrder));
    Assert.areEqual(Ord(TColorEnum.RED), Ord(O.Color));
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEmptyDataSet;
var
  Dm: TEntitiesModule;
  S: string;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    Dm.Entity.EmptyDataSet;
    S := fSerializer.SerializeDataSet(Dm.Entity);
    Assert.areEqual('[]', S);
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntity;
const
  JSON_PROPERTIES = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":"1987-10-15",' + '"AccessDateTime":"2017-02-17T16:37:50.000+01:00",' + '"AccessTime":"16:40:50",' +
    '"Active":true,' + '"Role":"roGuest",' + '"Teporization":63623032670000,' + '"Department":{' + '"Id":1,' +
    '"Name":"Development",' + '"Notes":[' + '{' + '"Description":"DepNote1"' + '},' + '{' + '"Description":"DepNote2"' +
    '}' + ']' + '},' + '"DepartmentNull":null,' + '"Notes":[' + '{' + '"Description":"EntNote1"' + '},' + '{' +
    '"Description":"EntNote2"' + '}' + '],' + '"NotesEmpty":[],' + '"AppreciationAs":"Yes",' + '"Appreciation":{' +
    '"type":"ustring",' + '"value":"Yes"' + '}' + '}';

  JSON_FIELDS = '{' + '"FId":1,' + '"FCode":2,' + '"FName":"Ezequiel Juliano Müller",' + '"FSalary":100,' +
    '"FBirthday":"1987-10-15",' + '"FAccessDateTime":"2017-02-17T16:37:50.000+01:00",' + '"FAccessTime":"16:40:50",' +
    '"FActive":true,' + '"FRole":"roGuest",' + '"FTeporization":63623032670000,' + '"FDepartment":{' + '"FId":1,' +
    '"FName":"Development",' + '"FNotes":[' + '{' + '"FDescription":"DepNote1"' + '},' + '{' +
    '"FDescription":"DepNote2"' + '}' + ']' + '},' + '"FDepartmentNull":null,' + '"FNotes":[' + '{' +
    '"FDescription":"EntNote1"' + '},' + '{' + '"FDescription":"EntNote2"' + '}' + '],' + '"FNotesEmpty":[],' +
    '"FAppreciationAs":"Yes",' + '"FAppreciation":{' + '"type":"ustring",' + '"value":"Yes"' + '}' + '}';

  JSON_NULLS = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller",' + '"Salary":100,' +
    '"Birthday":null,' + '"AccessDateTime":null,' + '"AccessTime":null,' + '"Active":true,' + '"Role":"roGuest",' +
    '"Teporization":63623032670000,' + '"Department":{' + '"Id":1,' + '"Name":"Development",' + '"Notes":[' + '{' +
    '"Description":"DepNote1"' + '},' + '{' + '"Description":"DepNote2"' + '}' + ']' + '},' + '"DepartmentNull":null,' +
    '"Notes":[' + '{' + '"Description":"EntNote1"' + '},' + '{' + '"Description":"EntNote2"' + '}' + '],' +
    '"NotesEmpty":[],' + '"AppreciationAs":"Yes",' + '"Appreciation":{' + '"type":"ustring",' + '"value":"Yes"' +
    '}' + '}';
var
  O: TEntity;
  S: string;
begin
  O := TEntity.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';
    O.Salary := 100;
    O.Birthday := StrToDate('15/10/1987');
    O.AccessDateTime := StrToDateTime('17/02/2017 16:37:50');
    O.AccessTime := StrToTime('16:40:50');
    O.Active := True;
    O.Role := roGuest;
    O.Teporization := DateTimeToTimeStamp(StrToDateTime('17/02/2017 16:37:50'));
    O.Appreciation := 'Yes';
    O.AppreciationAs := 'Yes';
    O.Ignored := 'Yes';
    O.Transient := 'Yes';
    O.Notes.Add(TNote.Create('EntNote1'));
    O.Notes.Add(TNote.Create('EntNote2'));
    O.Department.Id := 1;
    O.Department.Name := 'Development';
    O.Department.Notes.Add(TNote.Create('DepNote1'));
    O.Department.Notes.Add(TNote.Create('DepNote2'));

    S := fSerializer.SerializeObject(O, stProperties, ['Ignored']);
    Assert.areEqual(JSON_PROPERTIES, S);

    S := fSerializer.SerializeObject(O, stFields, ['FIgnored']);
    Assert.areEqual(JSON_FIELDS, S);

    O.Birthday := 0;
    O.AccessDateTime := 0;
    O.AccessTime := 0;
    S := fSerializer.SerializeObject(O, stProperties, ['Ignored']);
    Assert.areEqual(JSON_NULLS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomMemberSerializer;
const
  JSON = '{' + '"Entity":{' + '"AId":1,' + '"ACode":2,' + '"AName":"Ezequiel Juliano Müller"' + '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' + '"NotesAsString":"Ezequiel Juliano Müller"' + '}';
var
  O: TSale;
  S: string;
begin
  O := TSale.Create;
  try
    O.Entity.Id := 1;
    O.Entity.Code := 2;
    O.Entity.Name := 'Ezequiel Juliano Müller';
    O.Notes.WriteString('Ezequiel Juliano Müller');
    O.NotesAsString.WriteString('Ezequiel Juliano Müller');

    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomSerializer;
const
  JSON = '{' + '"AId":1,' + '"ACode":2,' + '"AName":"Ezequiel Juliano Müller"' + '}';
var
  O: TEntityCustom;
  S: string;
begin
  O := TEntityCustom.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityLowerCaseNames;
const
  JSON = '{' + '"id":1,' + '"code":2,' + '"name":"Ezequiel Juliano Müller"' + '}';
var
  O: TEntityLowerCase;
  S: string;
begin
  O := TEntityLowerCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityNameAs;
const
  JSON = '{' + '"Id_Id":1,' + '"Code_Code":2,' + '"Name_Name":"Ezequiel Juliano Müller"' + '}';
var
  O: TEntityNameAs;
  S: string;
begin
  O := TEntityNameAs.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);

    S := fSerializer.SerializeObject(O, stFields);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntitySerializationType;
const
  JSON_FIELDS = '{' + '"FId":1,' + '"FCode":2,' + '"FName":"Ezequiel Juliano Müller"' + '}';

  JSON_PROPERTIES = '{' + '"Id":1,' + '"Code":2,' + '"Name":"Ezequiel Juliano Müller"' + '}';
var
  OFields: TEntitySerializeFields;
  OProperties: TEntitySerializeProperties;
  S: string;
begin
  OFields := TEntitySerializeFields.Create;
  try
    OFields.Id := 1;
    OFields.Code := 2;
    OFields.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(OFields);
    Assert.areEqual(JSON_FIELDS, S);
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    OProperties.Id := 1;
    OProperties.Code := 2;
    OProperties.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(OProperties);
    Assert.areEqual(JSON_PROPERTIES, S);
  finally
    OProperties.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityUpperCaseNames;
const
  JSON = '{' + '"ID":1,' + '"CODE":2,' + '"NAME":"Ezequiel Juliano Müller"' + '}';
var
  O: TEntityUpperCase;
  S: string;
begin
  O := TEntityUpperCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityWithArray;
const
  JSON_WITH_ARRAY = '{' + '"Id":1,' + '"Names":["Pedro","Oliveira"],' +
    '"Values":[1,2],"Booleans":[true,false,true]' + '}';
var
  O: TEntityWithArray;
  S: string;
begin
  O := TEntityWithArray.Create;
  try
    O.Id := 1;
    O.Names := ['Pedro', 'Oliveira'];
    O.Values := [1, 2];
    O.Booleans := [True, False, True];
    S := fSerializer.SerializeObject(O);
    Assert.areEqual(JSON_WITH_ARRAY, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeListOfSomething;
var
  lList, lList2: TListOfSomething;
  lData: string;
begin
  lList := TListOfSomething.Create;
  try
    lData := fSerializer.SerializeObject(lList);

    lList2 := TListOfSomething.Create;
    try
      lList2.ListOfString.Clear;
      lList2.ListOfInteger.Clear;
      lList2.ListOfBoolean.Clear;
      lList2.ListOfDouble.Clear;

      fSerializer.DeserializeObject(lData, lList2);

      Assert.areEqual(2, lList2.ListOfString.Count);
      Assert.areEqual(2, lList2.ListOfInteger.Count);
      Assert.areEqual(2, lList2.ListOfBoolean.Count);
      Assert.areEqual(2, lList2.ListOfDouble.Count);

      Assert.areEqual(lList.ListOfString[0], lList2.ListOfString[0]);
      Assert.areEqual(lList.ListOfString[1], lList2.ListOfString[1]);

      Assert.areEqual(lList.ListOfInteger[0], lList2.ListOfInteger[0]);
      Assert.areEqual(lList.ListOfInteger[1], lList2.ListOfInteger[1]);

      Assert.areEqual(lList.ListOfBoolean[0], lList2.ListOfBoolean[0]);
      Assert.areEqual(lList.ListOfBoolean[1], lList2.ListOfBoolean[1]);

      Assert.areEqual(lList.ListOfDouble[0], lList2.ListOfDouble[0], 0.0001);
      Assert.areEqual(lList.ListOfDouble[1], lList2.ListOfDouble[1], 0.0001);
    finally
      lList2.Free;
    end;
  finally
    lList.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeListWithNulls;
var
  lPeople: TPeople;
  lPerson: TPerson;
  lStr: string;
  lJObj: TJsonObject;
begin
  lPeople := TPeople.Create;
  try
    lPerson := TPerson.Create;
    lPerson.Id := 1;
    lPerson.FirstName := 'Daniele';
    lPerson.LastName := 'Teti';
    lPeople.Add(lPerson);
    lPeople.Add(nil);
    lStr := fSerializer.SerializeObject(lPeople);
    lJObj := TJsonObject.Parse(lStr) as TJsonObject;
    try
      // this test should test that the serialization happens even for "nil" objects
      Assert.IsFalse(lJObj.A['List'].Items[0].IsNull);
      Assert.isTrue(lJObj.A['List'].Items[1].IsNull);
    finally
      lJObj.Free;
    end;
  finally
    lPeople.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeListWithNulls2;
var
  lPeople: TPeople;
  lPerson: TPerson;
  lStr: string;
  lJArr: TJsonArray;
begin
  lPeople := TPeople.Create;
  try
    lPerson := TPerson.Create;
    lPerson.Id := 1;
    lPerson.FirstName := 'Daniele';
    lPerson.LastName := 'Teti';
    lPeople.Add(lPerson);
    lPeople.Add(nil);
    lStr := fSerializer.SerializeCollection(lPeople);
    lJArr := TJsonObject.Parse(lStr) as TJsonArray;
    try
      // this test should test that the serialization happens even for "nil" objects
      Assert.IsFalse(lJArr.Items[0].IsNull);
      Assert.IsTrue(lJArr.Items[1].IsNull);
    finally
      lJArr.Free;
    end;
  finally
    lPeople.Free;
  end;
end;


procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeEntityWithInterface;
const
  JSON = '{' + '"Id":1,' + '"Name":"João Antônio Duarte",' + '"ChildEntity":{' + '"Code":10,' +
    '"Description":"Child Entity"' + '}' + '}';
var
  LEntity: IEntityWithInterface;
  LJson: string;
begin
  LEntity := TEntityWithInterface.Create;
  LEntity.Id := 1;
  LEntity.Name := 'João Antônio Duarte';
  LEntity.ChildEntity.Code := 10;
  LEntity.ChildEntity.Description := 'Child Entity';

  LJson := fSerializer.SerializeObject(LEntity);
  Assert.areEqual(JSON, LJson);

  LEntity := TEntityWithInterface.Create;
  fSerializer.DeserializeObject(LJson, LEntity);
  Assert.areEqual(Integer(1), LEntity.Id);
  Assert.areEqual('João Antônio Duarte', LEntity.Name);
  Assert.areEqual(Integer(10), LEntity.ChildEntity.Code);
  Assert.areEqual('Child Entity', LEntity.ChildEntity.Description);
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeSerializeEntityWithSet;
const
  O1 = '{"MonthsSet":"meJanuary,meMarch","ColorsSet":""}';
  O2 = '{"MonthsSet":"","ColorsSet":"RED"}';
  O3 = '{"MonthsSet":"meJanuary,meFebruary,meMarch","ColorsSet":"RED,GREEN,BLUE"}';
var
  O: TEntityWithSets;
  S: string;
  OClone: TEntityWithSets;
begin
  O := TEntityWithSets.Create;
  try
    O.MonthsSet := [meJanuary, meMarch];
    O.ColorsSet := [];
    S := fSerializer.SerializeObject(O);
    Assert.AreEqual(O1, S);
    OClone := TEntityWithSets.Create;
    try
      fSerializer.DeserializeObject(S, OClone);
      Assert.IsTrue(OClone.MonthsSet = [meJanuary,meMarch]);
      Assert.IsTrue(OClone.ColorsSet = []);
    finally
      OClone.Free;
    end;

    ////////
    O.MonthsSet := [];
    O.ColorsSet := [TColorEnum.RED];
    S := fSerializer.SerializeObject(O);
    Assert.AreEqual(O2, S);
    OClone := TEntityWithSets.Create;
    try
      fSerializer.DeserializeObject(S, OClone);
      Assert.IsTrue(OClone.MonthsSet = []);
      Assert.IsTrue(OClone.ColorsSet = [RED]);
    finally
      OClone.Free;
    end;


    ///////
    O.MonthsSet := [meJanuary, meMarch, meFebruary];
    O.ColorsSet := [TColorEnum.RED, TColorEnum.GREEN, TColorEnum.BLUE];
    S := fSerializer.SerializeObject(O);
    Assert.AreEqual(O3, S);

    OClone := TEntityWithSets.Create;
    try
      fSerializer.DeserializeObject(S, OClone);
      Assert.IsTrue(OClone.MonthsSet = [meJanuary, meFebruary, meMarch]);
      Assert.IsTrue(OClone.ColorsSet = [RED, GREEN, BLUE]);
    finally
      OClone.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeGenericEntity;
const
  JSON = '{' + '"Code":1,' + '"Description":"General Description",' + '"Items":[' + '{"Description":"Description 01"},'
    + '{"Description":"Description 02"},' + '{"Description":"Description 03"},' + '{"Description":"Description 04"},' +
    '{"Description":"Description 05"}' + ']' + '}';

  NESTED_JSON = '{' + '"Code":1,' + '"Description":"General Description",' + '"Items":[' + '{' + '"Code":10,' +
    '"Description":"Item_01",' + '"Items":[' + '{"Description":"Description 01"}' + ']' + '},' + '{' + '"Code":11,' +
    '"Description":"Item_02",' + '"Items":[' + '{"Description":"Description 02"}' + ']' + '},' + '{' + '"Code":12,' +
    '"Description":"Item_03",' + '"Items":[' + '{"Description":"Description 03"}' + ']' + '}' + ']' + '}';
var
  LGenericEntity: TGenericEntity<TNote>;
  LNestedGenericEntity: TNestedGenericEntity;
  LJson: string;
begin
  LGenericEntity := TGenericEntity<TNote>.Create;
  try
    LGenericEntity.Code := 1;
    LGenericEntity.Description := 'General Description';

    LGenericEntity.Items.Add(TNote.Create('Description 01'));
    LGenericEntity.Items.Add(TNote.Create('Description 02'));
    LGenericEntity.Items.Add(TNote.Create('Description 03'));
    LGenericEntity.Items.Add(TNote.Create('Description 04'));
    LGenericEntity.Items.Add(TNote.Create('Description 05'));

    LJson := fSerializer.SerializeObject(LGenericEntity);

    Assert.areEqual(JSON, LJson);
  finally
    LGenericEntity.Free;
  end;

  LGenericEntity := TGenericEntity<TNote>.Create;
  try
    fSerializer.DeserializeObject(LJson, LGenericEntity);

    Assert.areEqual(Integer(1), LGenericEntity.Code);
    Assert.areEqual('General Description', LGenericEntity.Description);
    Assert.areEqual(Integer(5), LGenericEntity.Items.Count);
    Assert.areEqual('Description 01', LGenericEntity.Items[0].Description);
    Assert.areEqual('Description 02', LGenericEntity.Items[1].Description);
    Assert.areEqual('Description 03', LGenericEntity.Items[2].Description);
    Assert.areEqual('Description 04', LGenericEntity.Items[3].Description);
    Assert.areEqual('Description 05', LGenericEntity.Items[4].Description);

  finally
    LGenericEntity.Free;
  end;

  LNestedGenericEntity := TNestedGenericEntity.Create;
  try
    LNestedGenericEntity.Code := 1;
    LNestedGenericEntity.Description := 'General Description';

    LGenericEntity := TGenericEntity<TNote>.Create;
    LGenericEntity.Code := 10;
    LGenericEntity.Description := 'Item_01';
    LGenericEntity.Items.Add(TNote.Create('Description 01'));
    LNestedGenericEntity.Items.Add(LGenericEntity);

    LGenericEntity := TGenericEntity<TNote>.Create;
    LGenericEntity.Code := 11;
    LGenericEntity.Description := 'Item_02';
    LGenericEntity.Items.Add(TNote.Create('Description 02'));
    LNestedGenericEntity.Items.Add(LGenericEntity);

    LGenericEntity := TGenericEntity<TNote>.Create;
    LGenericEntity.Code := 12;
    LGenericEntity.Description := 'Item_03';
    LGenericEntity.Items.Add(TNote.Create('Description 03'));
    LNestedGenericEntity.Items.Add(LGenericEntity);

    LJson := fSerializer.SerializeObject(LNestedGenericEntity);

    Assert.areEqual(NESTED_JSON, LJson);
  finally
    LNestedGenericEntity.Free;
  end;

  LNestedGenericEntity := TNestedGenericEntity.Create;;
  try
    fSerializer.DeserializeObject(LJson, LNestedGenericEntity);

    Assert.areEqual(Integer(1), LNestedGenericEntity.Code);
    Assert.areEqual('General Description', LNestedGenericEntity.Description);
    Assert.areEqual(Integer(3), LNestedGenericEntity.Items.Count);

    Assert.areEqual(Integer(10), LNestedGenericEntity.Items[0].Code);
    Assert.areEqual('Item_01', LNestedGenericEntity.Items[0].Description);
    Assert.areEqual(Integer(1), LNestedGenericEntity.Items[0].Items.Count);
    Assert.areEqual('Description 01', LNestedGenericEntity.Items[0].Items[0].Description);

    Assert.areEqual(Integer(11), LNestedGenericEntity.Items[1].Code);
    Assert.areEqual('Item_02', LNestedGenericEntity.Items[1].Description);
    Assert.areEqual(Integer(1), LNestedGenericEntity.Items[1].Items.Count);
    Assert.areEqual('Description 02', LNestedGenericEntity.Items[1].Items[0].Description);

    Assert.areEqual(Integer(12), LNestedGenericEntity.Items[2].Code);
    Assert.areEqual('Item_03', LNestedGenericEntity.Items[2].Description);
    Assert.areEqual(Integer(1), LNestedGenericEntity.Items[2].Items.Count);
    Assert.areEqual('Description 03', LNestedGenericEntity.Items[2].Items[0].Description);

  finally
    LNestedGenericEntity.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeGuid;
const
  JSON = '{' + '"GuidValue":"{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}",' +
    '"GuidValue2":"ca09dc98-85ba-46e8-aba2-117c2fa8ef25",' +
    '"NullableGuid":"{EABA9B61-6812-4F0A-9469-D247EB2DA8F4}",' +
    '"NullableGuid2":"fa51caa7-7d48-46ba-bfde-34c1f740e066",' +
    '"Id":1,' + '"Code":2,' +
    '"Name":"João Antônio"' + '}';
var
  LEntity: TEntityCustomWithGuid;
  LJson: string;
begin
  LEntity := TEntityCustomWithGuid.Create;
  try
    LEntity.Id := 1;
    LEntity.Code := 2;
    LEntity.Name := 'João Antônio';
    LEntity.GuidValue := StringToGUID('{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}');
    LEntity.GuidValue2 := StringToGUID('{CA09DC98-85BA-46E8-ABA2-117C2FA8EF25}');
    LEntity.NullableGuid := StringToGUID('{EABA9B61-6812-4F0A-9469-D247EB2DA8F4}');
    LEntity.NullableGuid2 := StringToGUID('{FA51CAA7-7D48-46BA-BFDE-34C1F740E066}');

    LJson := fSerializer.SerializeObject(LEntity);
    Assert.AreEqual(JSON, LJson);
  finally
    LEntity.Free;
  end;

  LEntity := TEntityCustomWithGuid.Create;
  try
    fSerializer.DeserializeObject(LJson, LEntity);
    Assert.AreEqual(int64(1), LEntity.Id);
    Assert.AreEqual(Integer(2), LEntity.Code);
    Assert.AreEqual('João Antônio', LEntity.Name);
    Assert.AreEqual(StringToGUID('{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}'), LEntity.GuidValue);
    Assert.AreEqual(StringToGUID('{CA09DC98-85BA-46E8-ABA2-117C2FA8EF25}'), LEntity.GuidValue2);
    Assert.AreEqual(StringToGUID('{EABA9B61-6812-4F0A-9469-D247EB2DA8F4}'), LEntity.NullableGuid.Value);
    Assert.AreEqual(StringToGUID('{FA51CAA7-7D48-46BA-BFDE-34C1F740E066}'), LEntity.NullableGuid2.Value);
  finally
    LEntity.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeMultipleGenericEntity;
const
  JSON = '{' + '"Code":1,' + '"Description":"General Description",' + '"Items":[' + '{"Description":"Description 01"},'
    + '{"Description":"Description 02"},' + '{"Description":"Description 03"},' + '{"Description":"Description 04"},' +
    '{"Description":"Description 05"}' + '],' + '"Items2":[' + '{"Description":"Description2 01"},' +
    '{"Description":"Description2 02"},' + '{"Description":"Description2 03"},' + '{"Description":"Description2 04"},' +
    '{"Description":"Description2 05"}' + ']' + '}';
var
  LGenericEntity: TMultipleGenericEntity<TNote, TNote>;
  LJson: string;
begin
  LGenericEntity := TMultipleGenericEntity<TNote, TNote>.Create;
  try
    LGenericEntity.Code := 1;
    LGenericEntity.Description := 'General Description';

    LGenericEntity.Items.Add(TNote.Create('Description 01'));
    LGenericEntity.Items.Add(TNote.Create('Description 02'));
    LGenericEntity.Items.Add(TNote.Create('Description 03'));
    LGenericEntity.Items.Add(TNote.Create('Description 04'));
    LGenericEntity.Items.Add(TNote.Create('Description 05'));

    LGenericEntity.Items2.Add(TNote.Create('Description2 01'));
    LGenericEntity.Items2.Add(TNote.Create('Description2 02'));
    LGenericEntity.Items2.Add(TNote.Create('Description2 03'));
    LGenericEntity.Items2.Add(TNote.Create('Description2 04'));
    LGenericEntity.Items2.Add(TNote.Create('Description2 05'));

    LJson := fSerializer.SerializeObject(LGenericEntity);

    Assert.areEqual(JSON, LJson);
  finally
    LGenericEntity.Free;
  end;

  LGenericEntity := TMultipleGenericEntity<TNote, TNote>.Create;
  try
    fSerializer.DeserializeObject(LJson, LGenericEntity);

    Assert.areEqual(Integer(1), LGenericEntity.Code);
    Assert.areEqual('General Description', LGenericEntity.Description);

    Assert.areEqual(Integer(5), LGenericEntity.Items.Count);
    Assert.areEqual('Description 01', LGenericEntity.Items[0].Description);
    Assert.areEqual('Description 02', LGenericEntity.Items[1].Description);
    Assert.areEqual('Description 03', LGenericEntity.Items[2].Description);
    Assert.areEqual('Description 04', LGenericEntity.Items[3].Description);
    Assert.areEqual('Description 05', LGenericEntity.Items[4].Description);

    Assert.areEqual(Integer(5), LGenericEntity.Items2.Count);
    Assert.areEqual('Description2 01', LGenericEntity.Items2[0].Description);
    Assert.areEqual('Description2 02', LGenericEntity.Items2[1].Description);
    Assert.areEqual('Description2 03', LGenericEntity.Items2[2].Description);
    Assert.areEqual('Description2 04', LGenericEntity.Items2[3].Description);
    Assert.areEqual('Description2 05', LGenericEntity.Items2[4].Description);

  finally
    LGenericEntity.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeNil;
begin
  Assert.areEqual('null', fSerializer.SerializeObject(nil));
end;

procedure TMVCTestSerializerJsonDataObjects.TestStringDictionary;
var
  lDict: TMVCStringDictionary;
  lSerString: string;
  lDict2: TMVCStringDictionary;
  lEntityDict: TEntityWithStringDictionary;
  lEntityDict2: TEntityWithStringDictionary;
begin
  lDict := TMVCStringDictionary.Create;
  try
    lDict['prop1'] := 'value1';
    lDict['prop2'] := 'value2';
    lDict['prop3'] := 'value3';
    lSerString := fSerializer.SerializeObject(lDict);
    lDict2 := TMVCStringDictionary.Create;
    try
      fSerializer.DeserializeObject(lSerString, lDict2);
      Assert.isTrue(lDict2.ContainsKey('prop1'));
      Assert.isTrue(lDict2.ContainsKey('prop2'));
      Assert.isTrue(lDict2.ContainsKey('prop3'));
      Assert.areEqual(lDict['prop1'], lDict2['prop1']);
      Assert.areEqual(lDict['prop2'], lDict2['prop2']);
      Assert.areEqual(lDict['prop3'], lDict2['prop3']);
    finally
      lDict2.Free;
    end;
  finally
    lDict.Free;
  end;

  lEntityDict := TEntityWithStringDictionary.Create;
  try
    lEntityDict.Dict['prop1'] := 'value1';
    lEntityDict.Dict['prop2'] := 'value2';
    lEntityDict.Dict['prop3'] := 'value3';
    lSerString := fSerializer.SerializeObject(lEntityDict);

    lEntityDict2 := TEntityWithStringDictionary.Create;
    try
      fSerializer.DeserializeObject(lSerString, lEntityDict2);
      Assert.isTrue(lEntityDict2.Dict.ContainsKey('prop1'));
      Assert.isTrue(lEntityDict2.Dict.ContainsKey('prop2'));
      Assert.isTrue(lEntityDict2.Dict.ContainsKey('prop3'));
      Assert.areEqual(lEntityDict.Dict['prop1'], lEntityDict2.Dict['prop1']);
      Assert.areEqual(lEntityDict.Dict['prop2'], lEntityDict2.Dict['prop2']);
      Assert.areEqual(lEntityDict.Dict['prop3'], lEntityDict2.Dict['prop3']);
    finally
      lEntityDict2.Free;
    end;
  finally
    lEntityDict.Free;
  end;
end;

{ TMVCEntityCustomSerializerJsonDataObjects }

procedure TMVCEntityCustomSerializerJsonDataObjects.Deserialize(const ASerializedObject: TObject;
  var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
var
  JsonObject: TJsonObject;
  EntityCustom: TEntityCustom;
begin
  JsonObject := ASerializedObject as TJsonObject;
  if Assigned(JsonObject) then
  begin
    EntityCustom := AElementValue.AsObject as TEntityCustom;
    if Assigned(EntityCustom) then
    begin
      EntityCustom.Id := JsonObject['AId'].LongValue;
      EntityCustom.Code := JsonObject['ACode'].IntValue;
      EntityCustom.Name := JsonObject['AName'].Value;
    end;
  end;
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin
  DeserializeRoot(ASerializerObject, AElementValue.AsObject, AAttributes);
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
var
  LEntity: TEntityCustom;
  LJson: TJDOJsonObject;
  lAttr: TCustomAttribute;
  lAsLowerCase: Boolean;
begin
  LEntity := TEntityCustom(AObject);
  LJson := ASerializerObject as TJDOJsonObject;
  lAsLowerCase := False;
  for lAttr in AAttributes do
  begin
    if lAttr is MVCNameCaseAttribute then
    begin
      lAsLowerCase := MVCNameCaseAttribute(lAttr).KeyCase = ncLowerCase;
      break;
    end;
  end;

  if lAsLowerCase then
  begin
    LEntity.Id := LJson.I['id'];
    LEntity.Code := LJson.I['code'];
    LEntity.Name := LJson.S['name'];
  end
  else
  begin
    // as is (upper case is not supported by the custom type serializer)
    LEntity.Id := LJson.I['Id'];
    LEntity.Code := LJson.I['Code'];
    LEntity.Name := LJson.S['Name'];
  end;
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.Serialize(const AElementValue: TValue;
  var ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  EntityCustom: TEntityCustom;
begin
  EntityCustom := AElementValue.AsObject as TEntityCustom;
  if Assigned(EntityCustom) then
  begin
    ASerializerObject := TJsonObject.Create;
    TJsonObject(ASerializerObject).L['AId'] := EntityCustom.Id;
    TJsonObject(ASerializerObject).I['ACode'] := EntityCustom.Code;
    TJsonObject(ASerializerObject).S['AName'] := EntityCustom.Name;
  end;
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
var
  lEntityCustom: TEntityCustom;
begin
  lEntityCustom := AElementValue.AsObject as TEntityCustom;
  if Assigned(lEntityCustom) then
  begin
    TJsonObject(ASerializerObject).O[APropertyName].L['AId'] := lEntityCustom.Id;
    TJsonObject(ASerializerObject).O[APropertyName].I['ACode'] := lEntityCustom.Code;
    TJsonObject(ASerializerObject).O[APropertyName].S['AName'] := lEntityCustom.Name;
  end
  else
  begin
    TJsonObject(ASerializerObject).Values[APropertyName] := nil;
  end;
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
var
  lEntityCustom: TEntityCustom;
begin
  ASerializerObject := nil;
  lEntityCustom := AObject as TEntityCustom;
  if Assigned(lEntityCustom) then
  begin
    ASerializerObject := TJsonObject.Create;
    TJsonObject(ASerializerObject).L['AId'] := lEntityCustom.Id;
    TJsonObject(ASerializerObject).I['ACode'] := lEntityCustom.Code;
    TJsonObject(ASerializerObject).S['AName'] := lEntityCustom.Name;
  end;
end;

{ TMVCNullableIntegerSerializerJsonDataObjects }

procedure TMVCNullableIntegerSerializerJsonDataObjects.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
begin

end;

initialization

TDUnitX.RegisterTestFixture(TMVCTestSerializerJsonDataObjects);

end.
