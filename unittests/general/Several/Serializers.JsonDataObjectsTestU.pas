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
    FSerializer: IMVCSerializer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { serialize declarations }
    [Test]
    procedure TestSerializeAllTypes;
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
    procedure TestDeserializeEntityWithArray;
    { full cycle }
    [Test]
    procedure TestSerializeDeSerializeEntityWithEnums;
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
    procedure SerializeRoot(const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure SerializeAttribute(const AElementValue: TValue;
      const APropertyName: string; const ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeAttribute(var AElementValue: TValue;
      const APropertyName: string; const ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);

  end;

  TMVCNullableIntegerSerializerJsonDataObjects = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure DeserializeAttribute(var AElementValue: TValue;
      const APropertyName: string; const ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject;
      const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeAttribute(const AElementValue: TValue;
      const APropertyName: string; const ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject;
      out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
  end;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects.CustomTypes,
  MVCFramework.Commons, System.TypInfo, BOs;

const
  LINE_BREAK = #$A;
  TAB_SPACE = #9;

  { TMVCTestSerializerJsonDataObjects }

procedure TMVCTestSerializerJsonDataObjects.Setup;
begin
  inherited;
  FSerializer := TMVCJsonDataObjectsSerializer.Create;
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TStream), TMVCStreamSerializerJsonDataObject.Create);
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TStringStream), TMVCStreamSerializerJsonDataObject.Create);
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TMemoryStream), TMVCStreamSerializerJsonDataObject.Create);
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TEntityCustom), TMVCEntityCustomSerializerJsonDataObjects.Create);
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TMVCNullable<Integer>),
    TMVCNullableIntegerSerializerJsonDataObjects.Create);
end;

procedure TMVCTestSerializerJsonDataObjects.TearDown;
begin
  inherited;
  FSerializer := nil;
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
  JSON_PROPERTIES =
    '[' +
    '{' +
    '"Description":"Description 1"' +
    '},' +
    '{' +
    '"Description":"Description 2"' +
    '},' +
    '{' +
    '"Description":"Description 3"' +
    '},' +
    '{' +
    '"Description":"Description 4"' +
    '}' +
    ']';

  JSON_FIELDS =
    '[' +
    '{' +
    '"FDescription":"Description 1"' +
    '},' +
    '{' +
    '"FDescription":"Description 2"' +
    '},' +
    '{' +
    '"FDescription":"Description 3"' +
    '},' +
    '{' +
    '"FDescription":"Description 4"' +
    '}' +
    ']';
var
  O: TObjectList<TNote>;
begin
  O := TObjectList<TNote>.Create(True);
  try
    FSerializer.DeserializeCollection(JSON_PROPERTIES, O, TNote);
    CheckObjectList(O);
  finally
    O.Free;
  end;

  O := TObjectList<TNote>.Create(True);
  try
    FSerializer.DeserializeCollection(JSON_FIELDS, O, TNote, stFields);
    CheckObjectList(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeDataSet;
const
  JSON =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Amount":100,' +
    '"BlobFld":"PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' +
    '"Items":[' +
    '{' +
    '"Id":1,' +
    '"Name":"Ezequiel"' +
    '},' +
    '{' +
    '"Id":2,' +
    '"Name":"Juliano"' +
    '}' +
    '],' +
    '"Departament":{' +
    '"Name":"Depto1"' +
    '},' +
    '"GUID":"{9386C957-5379-4370-8492-8FA464A9CF0C}"' +
    '}';

  JSON_LOWERCASE =
    '{' +
    '"id":1,' +
    '"name":"Ezequiel Juliano Müller"' +
    '}';

  JSON_UPPERCASE =
    '{' +
    '"ID":1,' +
    '"NAME":"Ezequiel Juliano Müller"' +
    '}';

  JSON_ASIS =
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '}';

  JSON_LIST =
    '[' +
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '},' +
    '{' +
    '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '}' +
    ']';

  JSON_ITEMS =
    '{' +
    '"items":[' +
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Pedro Henrique de Oliveira"' +
    '},' +
    '{' +
    '"Id_Id":2,' +
    '"Name_Name":"Rogers Abe"' +
    '}' +
    '],' +
    '"meta":{"count":"2"}}';
var
  Dm: TEntitiesModule;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    FSerializer.DeserializeDataSetRecord(JSON, Dm.Entity, ['Ignored']);
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

    FSerializer.DeserializeDataSetRecord(JSON_LOWERCASE, Dm.EntityLowerCase);
    Assert.isTrue(Dm.EntityLowerCaseId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityLowerCaseName.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase);
    Assert.isTrue(Dm.EntityUpperCaseId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCaseName.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.isTrue(Dm.EntityUpperCase2Id.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCase2Name.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_ASIS, Dm.EntityAsIs);
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.EmptyDataSet;
    FSerializer.DeserializeDataSet(JSON_LIST, Dm.EntityAsIs);
    Dm.EntityAsIs.First;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.Next;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 2);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.EmptyDataSet;
    Dm.EntityAsIs.LoadJSONArrayFromJSONObjectProperty(JSON_ITEMS, 'items');
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
  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Role":"roGuest",' +
    '"Teporization":63623032670000,' +
    '"Department":{' +
    '"Id":1,' +
    '"Name":"Development",' +
    '"Notes":[' +
    '{' +
    '"Description":"DepNote1"' +
    '},' +
    '{' +
    '"Description":"DepNote2"' +
    '}' +
    ']' +
    '},' +
    '"DepartmentNull":null,' +
    '"Notes":[' +
    '{' +
    '"Description":"EntNote1"' +
    '},' +
    '{' +
    '"Description":"EntNote2"' +
    '}' +
    '],' +
    '"NotesEmpty":[],' +
    '"AppreciationAs":"Yes",' +
    '"Appreciation":{' +
    '"type":"ustring",' +
    '"value":"Yes"' +
    '}' +
    '}';

  JSON_FIELDS =
    '{' +
    '"FId":1,' +
    '"FCode":2,' +
    '"FName":"Ezequiel Juliano Müller",' +
    '"FSalary":100,' +
    '"FBirthday":"1987-10-15",' +
    '"FAccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"FAccessTime":"16:40:50",' +
    '"FActive":true,' +
    '"FRole":"roGuest",' +
    '"FTeporization":63623032670000,' +
    '"FDepartment":{' +
    '"FId":1,' +
    '"FName":"Development",' +
    '"FNotes":[' +
    '{' +
    '"FDescription":"DepNote1"' +
    '},' +
    '{' +
    '"FDescription":"DepNote2"' +
    '}' +
    ']' +
    '},' +
    '"FDepartmentNull":null,' +
    '"FNotes":[' +
    '{' +
    '"FDescription":"EntNote1"' +
    '},' +
    '{' +
    '"FDescription":"EntNote2"' +
    '}' +
    '],' +
    '"FNotesEmpty":[],' +
    '"FAppreciationAs":"Yes",' +
    '"FAppreciation":{' +
    '"type":"ustring",' +
    '"value":"Yes"' +
    '}' +
    '}';
var
  O: TEntity;
begin
  O := TEntity.Create;
  try
    FSerializer.DeserializeObject(JSON_PROPERTIES, O);
    CheckObject(O);
  finally
    O.Free;
  end;

  O := TEntity.Create;
  try
    FSerializer.DeserializeObject(JSON_FIELDS, O, stFields);
    CheckObject(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomMemberSerializer;
const
  JSON =
    '{' +
    '"Entity":{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' +
    '"NotesAsString":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TSale;
begin
  O := TSale.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
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
  JSON =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TEntityCustom;
begin
  O := TEntityCustom.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Id = 1);
    Assert.isTrue(O.Code = 2);
    Assert.isTrue(O.Name = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomValueTypeSerializer;
const
  JSON =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"NullableInteger":3' +
    '}';
var
  O: TEntityCustomWithNullables;
begin
  O := TEntityCustomWithNullables.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Id = 1);
    Assert.isTrue(O.Code = 2);
    Assert.isTrue(O.Name = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntitySerializationType;
const
  JSON_FIELDS =
    '{' +
    '"FId":1,' +
    '"FCode":2,' +
    '"FName":"Ezequiel Juliano Müller"' +
    '}';

  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller"' +
    '}';
var
  OFields: TEntitySerializeFields;
  OProperties: TEntitySerializeProperties;
begin
  OFields := TEntitySerializeFields.Create;
  try
    FSerializer.DeserializeObject(JSON_FIELDS, OFields);
    Assert.isTrue(OFields.Id = 1);
    Assert.isTrue(OFields.Code = 2);
    Assert.isTrue(OFields.Name = 'Ezequiel Juliano Müller');
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    FSerializer.DeserializeObject(JSON_PROPERTIES, OProperties);
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
  end;

const
  JSON_WITH_ARRAY =
    '{' +
    '"Id":1,' +
    '"Names":["Pedro","Oliveira"],' +
    '"Values":[1,2]' +
    '}';
var
  O: TEntityWithArray;
begin
  O := TEntityWithArray.Create;
  try
    FSerializer.DeserializeObject(JSON_WITH_ARRAY, O);
    CheckObject(O);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeAllTypes;
var
  lObj1, lObj2: TMyObject;
  lSer: string;
begin
  lObj1 := GetMyObject;
  try
    lSer := FSerializer.SerializeObject(lObj1);
    lObj2 := TMyObject.Create;
    try
      FSerializer.DeserializeObject(lSer, lObj2);
      Assert.IsTrue(lObj1.Equals(lObj2));
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
      lObj :=GetMyObject;
      lObj.PropJSONObject.I['value'] := I;
      lList1.Add(lObj);
    end;

    lSer := FSerializer.SerializeCollection(lList1);

    lList2 := TObjectList<TMyObject>.Create;
    try
      FSerializer.DeserializeCollection(lSer, lList2, TMyObject);
      for I := 0 to 9 do
      begin
        Assert.IsTrue(lList1[I].Equals(lList2[I]));
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
  JSON =
    '[' +
    '{' +
    '"Description":"Description 1"' +
    '},' +
    '{' +
    '"Description":"Description 2"' +
    '},' +
    '{' +
    '"Description":"Description 3"' +
    '},' +
    '{' +
    '"Description":"Description 4"' +
    '}' +
    ']';

  JSON_FIELDS =
    '[' +
    '{' +
    '"FDescription":"Description 1"' +
    '},' +
    '{' +
    '"FDescription":"Description 2"' +
    '},' +
    '{' +
    '"FDescription":"Description 3"' +
    '},' +
    '{' +
    '"FDescription":"Description 4"' +
    '}' +
    ']';
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

    S := FSerializer.SerializeCollection(O);
    Assert.areEqual(JSON, S);

    S := FSerializer.SerializeCollection(O, stFields);
    Assert.areEqual(JSON_FIELDS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDataSet;
const
  JSON =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Amount":100,' +
    '"BlobFld":"PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' +
    '"Items":[' +
    '{' +
    '"Id":1,' +
    '"Name":"Ezequiel"' +
    '},' +
    '{' +
    '"Id":2,' +
    '"Name":"Juliano"' +
    '}' +
    '],' +
    '"Departament":{' +
    '"Name":"Depto1"' +
    '},' +
    '"GUID":"{9386C957-5379-4370-8492-8FA464A9CF0C}"' +
    '}';

  JSON_LOWERCASE =
    '{' +
    '"id":1,' +
    '"name":"Ezequiel Juliano Müller"' +
    '}';

  JSON_UPPERCASE =
    '{' +
    '"ID":1,' +
    '"NAME":"Ezequiel Juliano Müller"' +
    '}';

  JSON_ASIS =
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '}';

  JSON_LIST =
    '[' +
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '},' +
    '{' +
    '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '}' +
    ']';

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
    S := FSerializer.SerializeDataSetRecord(Dm.Entity, ['Ignored'], ncAsIs);
    Assert.areEqual(JSON, S);

    Dm.EntityLowerCase.Insert;
    Dm.EntityLowerCaseId.AsLargeInt := 1;
    Dm.EntityLowerCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityLowerCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityLowerCase);
    Assert.areEqual(JSON_LOWERCASE, S, 'json lowercase');

    Dm.EntityUpperCase.Insert;
    Dm.EntityUpperCaseId.AsLargeInt := 1;
    Dm.EntityUpperCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S, 'json uppercase (1)');

    Dm.EntityUpperCase2.Insert;
    Dm.EntityUpperCase2Id.AsLargeInt := 1;
    Dm.EntityUpperCase2Name.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase2.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S, 'json uppercase (2)');

    Dm.EntityAsIs.Insert;
    Dm.EntityAsIsId.AsLargeInt := 1;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityAsIs);
    Assert.areEqual(JSON_ASIS, S, 'json as is');

    Dm.EntityAsIs.Append;
    Dm.EntityAsIsId.AsLargeInt := 2;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;

    // serialize dataset
    S := FSerializer.SerializeDataSet(Dm.EntityAsIs);
    Assert.areEqual(JSON_LIST, S, 'json list');

    // serialize dataset as object
    S := FSerializer.SerializeObject(Dm.EntityAsIs);
    Assert.areEqual(JSON_LIST, S, 'json list');

  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeSerializeEntityWithEnums;
const
  JSON =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Daniele Teti",' +
    '"Color":"RED",' +
    '"MonthName":"January",' +
    '"MonthOrder":0' +
    '}';
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
    O.MonthOrder := TMonthEnum.meJanuary;
    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;

  O := TEntityWithEnums.Create;
  try
    FSerializer.DeserializeObject(S, O);
    Assert.areEqual(int64(1), O.Id);
    Assert.areEqual(2, O.Code);
    Assert.areEqual('Daniele Teti', O.Name);
    Assert.areEqual(Ord(TMonthEnum.meJanuary), Ord(O.MonthName));
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
    S := FSerializer.SerializeDataSet(Dm.Entity);
    Assert.areEqual('[]', S);
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntity;
const
  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Role":"roGuest",' +
    '"Teporization":63623032670000,' +
    '"Department":{' +
    '"Id":1,' +
    '"Name":"Development",' +
    '"Notes":[' +
    '{' +
    '"Description":"DepNote1"' +
    '},' +
    '{' +
    '"Description":"DepNote2"' +
    '}' +
    ']' +
    '},' +
    '"DepartmentNull":null,' +
    '"Notes":[' +
    '{' +
    '"Description":"EntNote1"' +
    '},' +
    '{' +
    '"Description":"EntNote2"' +
    '}' +
    '],' +
    '"NotesEmpty":[],' +
    '"AppreciationAs":"Yes",' +
    '"Appreciation":{' +
    '"type":"ustring",' +
    '"value":"Yes"' +
    '}' +
    '}';

  JSON_FIELDS =
    '{' +
    '"FId":1,' +
    '"FCode":2,' +
    '"FName":"Ezequiel Juliano Müller",' +
    '"FSalary":100,' +
    '"FBirthday":"1987-10-15",' +
    '"FAccessDateTime":"2017-02-17T16:37:50.000Z",' +
    '"FAccessTime":"16:40:50",' +
    '"FActive":true,' +
    '"FRole":"roGuest",' +
    '"FTeporization":63623032670000,' +
    '"FDepartment":{' +
    '"FId":1,' +
    '"FName":"Development",' +
    '"FNotes":[' +
    '{' +
    '"FDescription":"DepNote1"' +
    '},' +
    '{' +
    '"FDescription":"DepNote2"' +
    '}' +
    ']' +
    '},' +
    '"FDepartmentNull":null,' +
    '"FNotes":[' +
    '{' +
    '"FDescription":"EntNote1"' +
    '},' +
    '{' +
    '"FDescription":"EntNote2"' +
    '}' +
    '],' +
    '"FNotesEmpty":[],' +
    '"FAppreciationAs":"Yes",' +
    '"FAppreciation":{' +
    '"type":"ustring",' +
    '"value":"Yes"' +
    '}' +
    '}';

  JSON_NULLS =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":null,' +
    '"AccessDateTime":null,' +
    '"AccessTime":null,' +
    '"Active":true,' +
    '"Role":"roGuest",' +
    '"Teporization":63623032670000,' +
    '"Department":{' +
    '"Id":1,' +
    '"Name":"Development",' +
    '"Notes":[' +
    '{' +
    '"Description":"DepNote1"' +
    '},' +
    '{' +
    '"Description":"DepNote2"' +
    '}' +
    ']' +
    '},' +
    '"DepartmentNull":null,' +
    '"Notes":[' +
    '{' +
    '"Description":"EntNote1"' +
    '},' +
    '{' +
    '"Description":"EntNote2"' +
    '}' +
    '],' +
    '"NotesEmpty":[],' +
    '"AppreciationAs":"Yes",' +
    '"Appreciation":{' +
    '"type":"ustring",' +
    '"value":"Yes"' +
    '}' +
    '}';
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

    S := FSerializer.SerializeObject(O, stProperties, ['Ignored']);
    Assert.areEqual(JSON_PROPERTIES, S);

    S := FSerializer.SerializeObject(O, stFields, ['FIgnored']);
    Assert.areEqual(JSON_FIELDS, S);

    O.Birthday := 0;
    O.AccessDateTime := 0;
    O.AccessTime := 0;
    S := FSerializer.SerializeObject(O, stProperties, ['Ignored']);
    Assert.areEqual(JSON_NULLS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomMemberSerializer;
const
  JSON =
    '{' +
    '"Entity":{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano Müller"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' +
    '"NotesAsString":"Ezequiel Juliano Müller"' +
    '}';
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

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomSerializer;
const
  JSON =
    '{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TEntityCustom;
  S: string;
begin
  O := TEntityCustom.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityLowerCaseNames;
const
  JSON =
    '{' +
    '"id":1,' +
    '"code":2,' +
    '"name":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TEntityLowerCase;
  S: string;
begin
  O := TEntityLowerCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityNameAs;
const
  JSON =
    '{' +
    '"Id_Id":1,' +
    '"Code_Code":2,' +
    '"Name_Name":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TEntityNameAs;
  S: string;
begin
  O := TEntityNameAs.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);

    S := FSerializer.SerializeObject(O, stFields);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntitySerializationType;
const
  JSON_FIELDS =
    '{' +
    '"FId":1,' +
    '"FCode":2,' +
    '"FName":"Ezequiel Juliano Müller"' +
    '}';

  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller"' +
    '}';
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

    S := FSerializer.SerializeObject(OFields);
    Assert.areEqual(JSON_FIELDS, S);
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    OProperties.Id := 1;
    OProperties.Code := 2;
    OProperties.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(OProperties);
    Assert.areEqual(JSON_PROPERTIES, S);
  finally
    OProperties.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityUpperCaseNames;
const
  JSON =
    '{' +
    '"ID":1,' +
    '"CODE":2,' +
    '"NAME":"Ezequiel Juliano Müller"' +
    '}';
var
  O: TEntityUpperCase;
  S: string;
begin
  O := TEntityUpperCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityWithArray;
const
  JSON_WITH_ARRAY =
    '{' +
    '"Id":1,' +
    '"Names":["Pedro","Oliveira"],' +
    '"Values":[1,2]' +
    '}';
var
  O: TEntityWithArray;
  S: string;
begin
  O := TEntityWithArray.Create;
  try
    O.Id := 1;
    O.Names := ['Pedro', 'Oliveira'];
    O.Values := [1, 2];

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON_WITH_ARRAY, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeEntityWithInterface;
const
  JSON =
    '{' +
    '"Id":1,' +
    '"Name":"João Antônio Duarte",' +
    '"ChildEntity":{'+
    '"Code":10,'+
    '"Description":"Child Entity"'+
    '}' +
    '}';
var
  LEntity: IEntityWithInterface;
  LJson: string;
begin
  LEntity := TEntityWithInterface.Create;
  LEntity.Id := 1;
  LEntity.Name := 'João Antônio Duarte';
  LEntity.ChildEntity.Code := 10;
  LEntity.ChildEntity.Description := 'Child Entity';

  LJson := FSerializer.SerializeObject(LEntity);
  Assert.AreEqual(JSON, LJson);

  LEntity := TEntityWithInterface.Create;
  FSerializer.DeserializeObject(LJson, LEntity);
  Assert.AreEqual(Integer(1), LEntity.Id);
  Assert.AreEqual('João Antônio Duarte', LEntity.Name);
  Assert.AreEqual(Integer(10), LEntity.ChildEntity.Code);
  Assert.AreEqual('Child Entity', LEntity.ChildEntity.Description);
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeGenericEntity;
const
  JSON =
    '{' +
    '"Code":1,'  +
    '"Description":"General Description",' +
    '"Items":[' +
    '{"Description":"Description 01"},' +
    '{"Description":"Description 02"},' +
    '{"Description":"Description 03"},' +
    '{"Description":"Description 04"},' +
    '{"Description":"Description 05"}' +
    ']'+
    '}';
var
  LGenericEntity: TGenericEntity<TNote>;
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

    LJson := FSerializer.SerializeObject(LGenericEntity);

    Assert.AreEqual(JSON, LJson);
  finally
    LGenericEntity.Free;
  end;

  LGenericEntity := TGenericEntity<TNote>.Create;
  try
    FSerializer.DeserializeObject(LJson, LGenericEntity);

    Assert.AreEqual(Integer(1), LGenericEntity.Code);
    Assert.AreEqual('General Description', LGenericEntity.Description);
    Assert.AreEqual(Integer(5), LGenericEntity.Items.Count);
    Assert.AreEqual('Description 01', LGenericEntity.Items[0].Description);
    Assert.AreEqual('Description 02', LGenericEntity.Items[1].Description);
    Assert.AreEqual('Description 03', LGenericEntity.Items[2].Description);
    Assert.AreEqual('Description 04', LGenericEntity.Items[3].Description);
    Assert.AreEqual('Description 05', LGenericEntity.Items[4].Description);

  finally
    LGenericEntity.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeGuid;
const
  JSON =
    '{' +
    '"GuidValue":"{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}",' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"João Antônio"' +
    '}';
var
  LEntity: TEntityCustomWithGuid;
  LJson: string;
begin
  LEntity := TEntityCustomWithGuid.Create;
  try
    LEntity.Id := 1;
    LEntity.Code := 2;
    LEntity.name := 'João Antônio';
    LEntity.GuidValue := StringToGuid('{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}');

    LJson := FSerializer.SerializeObject(LEntity);
    Assert.AreEqual(JSON, LJson);
  finally
    LEntity.Free;
  end;

  LEntity := TEntityCustomWithGuid.Create;
  try
    FSerializer.DeserializeObject(LJson, LEntity);
    Assert.AreEqual(Int64(1), LEntity.Id);
    Assert.AreEqual(Integer(2), LEntity.Code);
    Assert.AreEqual('João Antônio', LEntity.name);
    Assert.AreEqual(StringToGuid('{AEED1A0F-9061-40F0-9FDA-D69AE7F20222}'), LEntity.GuidValue);
  finally
    LEntity.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDeserializeMultipleGenericEntity;
const
  JSON =
    '{' +
    '"Code":1,'  +
    '"Description":"General Description",' +
    '"Items":[' +
    '{"Description":"Description 01"},' +
    '{"Description":"Description 02"},' +
    '{"Description":"Description 03"},' +
    '{"Description":"Description 04"},' +
    '{"Description":"Description 05"}' +
    '],'+
    '"Items2":[' +
    '{"Description":"Description2 01"},' +
    '{"Description":"Description2 02"},' +
    '{"Description":"Description2 03"},' +
    '{"Description":"Description2 04"},' +
    '{"Description":"Description2 05"}' +
    ']'+
    '}';
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


    LJson := FSerializer.SerializeObject(LGenericEntity);

    Assert.AreEqual(JSON, LJson);
  finally
    LGenericEntity.Free;
  end;

  LGenericEntity := TMultipleGenericEntity<TNote, TNote>.Create;
  try
    FSerializer.DeserializeObject(LJson, LGenericEntity);

    Assert.AreEqual(Integer(1), LGenericEntity.Code);
    Assert.AreEqual('General Description', LGenericEntity.Description);

    Assert.AreEqual(Integer(5), LGenericEntity.Items.Count);
    Assert.AreEqual('Description 01', LGenericEntity.Items[0].Description);
    Assert.AreEqual('Description 02', LGenericEntity.Items[1].Description);
    Assert.AreEqual('Description 03', LGenericEntity.Items[2].Description);
    Assert.AreEqual('Description 04', LGenericEntity.Items[3].Description);
    Assert.AreEqual('Description 05', LGenericEntity.Items[4].Description);

    Assert.AreEqual(Integer(5), LGenericEntity.Items2.Count);
    Assert.AreEqual('Description2 01', LGenericEntity.Items2[0].Description);
    Assert.AreEqual('Description2 02', LGenericEntity.Items2[1].Description);
    Assert.AreEqual('Description2 03', LGenericEntity.Items2[2].Description);
    Assert.AreEqual('Description2 04', LGenericEntity.Items2[3].Description);
    Assert.AreEqual('Description2 05', LGenericEntity.Items2[4].Description);

  finally
    LGenericEntity.Free;
  end;

end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeNil;
begin
  Assert.areEqual('null', FSerializer.SerializeObject(nil));
end;

procedure TMVCTestSerializerJsonDataObjects.TestStringDictionary;
var
  lDict: TMVCStringDictionary;
  lSerString: string;
  lDict2: TMVCStringDictionary;
begin
  lDict := TMVCStringDictionary.Create;
  try
    lDict['prop1'] := 'value1';
    lDict['prop2'] := 'value2';
    lDict['prop3'] := 'value3';
    lSerString := FSerializer.SerializeObject(lDict);
    lDict2 := TMVCStringDictionary.Create;
    try
      FSerializer.DeserializeObject(lSerString, lDict2);
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
end;

{ TMVCEntityCustomSerializerJsonDataObjects }

procedure TMVCEntityCustomSerializerJsonDataObjects.Deserialize(
  const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
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

procedure TMVCEntityCustomSerializerJsonDataObjects.DeserializeAttribute(
  var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin
  DeserializeRoot(ASerializerObject, AElementValue.AsObject, AAttributes);
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.DeserializeRoot(
  const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
var
  lEntity: TEntityCustom;
  lJSON: TJDOJsonObject;
  lAttr: TCustomAttribute;
  lAsLowerCase: Boolean;
begin
  lEntity := TEntityCustom(AObject);
  lJSON := ASerializerObject as TJDOJsonObject;
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
    lEntity.Id := lJSON.I['id'];
    lEntity.Code := lJSON.I['code'];
    lEntity.Name := lJSON.S['name'];
  end
  else
  begin
    // as is (upper case is not supported by the custom type serializer)
    lEntity.Id := lJSON.I['Id'];
    lEntity.Code := lJSON.I['Code'];
    lEntity.Name := lJSON.S['Name'];
  end;
end;

procedure TMVCEntityCustomSerializerJsonDataObjects.Serialize(
  const AElementValue: TValue; var ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
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

procedure TMVCEntityCustomSerializerJsonDataObjects.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
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

procedure TMVCEntityCustomSerializerJsonDataObjects.SerializeRoot(
  const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>;
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

procedure TMVCNullableIntegerSerializerJsonDataObjects.DeserializeAttribute(
  var AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.DeserializeRoot(
  const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.SerializeAttribute(
  const AElementValue: TValue; const APropertyName: string;
  const ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TMVCNullableIntegerSerializerJsonDataObjects.SerializeRoot(
  const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
begin

end;

initialization

TDUnitX.RegisterTestFixture(TMVCTestSerializerJsonDataObjects);

end.
