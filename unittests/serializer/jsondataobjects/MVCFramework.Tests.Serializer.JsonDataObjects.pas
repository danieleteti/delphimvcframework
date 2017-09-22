// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano M�ller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Tests.Serializer.JsonDataObjects;

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
  JsonDataObjects;

type

  [TestFixture]
  TMVCTestSerializerJsonDataObjects = class(TObject)
  private
    FSerializer: IMVCSerializer;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    { serialize declarations }
    [Test]
    procedure TestSerializeEntity;
    [Test]
    procedure TestSerializeNil;
    [Test]
    procedure TestSerializeEntityUpperCaseNames;
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
    procedure TestDeserializeEntityCustomMemberSerializer;
    [Test]
    procedure TestDeserializeEntitySerializationType;
    [Test]
    procedure TestDeserializeCollection;
    [Test]
    procedure TestDeserializeDataSet;
    { full cycle }
    [Test]
    procedure TestSerializeDeSerializeEntityWithEnums;
  end;

  TMVCEntityCustomSerializerJsonDataObjects = class(TInterfacedObject, IMVCTypeSerializer)
  private
    { private declarations }
  protected
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
  public
    { public declarations }
  end;

implementation

const
  LINE_BREAK = #$A;
  TAB_SPACE = #9;

  { TMVCTestSerializerJsonDataObjects }

procedure TMVCTestSerializerJsonDataObjects.SetUp;
begin
  inherited;
  FSerializer := TMVCJsonDataObjectsSerializer.Create;
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TEntityCustom), TMVCEntityCustomSerializerJsonDataObjects.Create);
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
    '"Name":"Ezequiel Juliano M�ller",' +
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
    '}' +
    '}';

  JSON_LOWERCASE =
    '{' +
    '"id":1,' +
    '"name":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_UPPERCASE =
    '{' +
    '"ID":1,' +
    '"NAME":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_ASIS =
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_LIST =
    '[' +
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '},' +
    '{' +
    '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '}' +
    ']';
var
  Dm: TEntitiesModule;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    FSerializer.DeserializeDataSetRecord(JSON, Dm.Entity, ['Ignored']);
    Assert.isTrue(Dm.EntityId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityCode.AsInteger = 2);
    Assert.isTrue(Dm.EntityName.AsString = 'Ezequiel Juliano M�ller');
    Assert.isTrue(Dm.EntityBirthday.AsDateTime = StrToDate('15/10/1987'));
    Assert.isTrue(Dm.EntityAccessDateTime.AsDateTime = StrToDateTime('17/02/2017 16:37:50'));
    Assert.isTrue(Dm.EntityAccessTime.AsDateTime = StrToTime('16:40:50'));
    Assert.isTrue(Dm.EntityActive.AsBoolean = True);
    Assert.isTrue(Dm.EntitySalary.AsCurrency = 100);
    Assert.isTrue(Dm.EntityAmount.AsFloat = 100);
    Assert.isTrue(Dm.EntityBlobFld.AsString = '<html><body><h1>BLOB</h1></body></html>');

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
    Assert.isTrue(Dm.EntityLowerCaseName.AsString = 'Ezequiel Juliano M�ller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase);
    Assert.isTrue(Dm.EntityUpperCaseId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCaseName.AsString = 'Ezequiel Juliano M�ller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.isTrue(Dm.EntityUpperCase2Id.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityUpperCase2Name.AsString = 'Ezequiel Juliano M�ller');

    FSerializer.DeserializeDataSetRecord(JSON_ASIS, Dm.EntityAsIs);
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano M�ller');

    Dm.EntityAsIs.EmptyDataSet;
    FSerializer.DeserializeDataSet(JSON_LIST, Dm.EntityAsIs);
    Dm.EntityAsIs.First;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano M�ller');

    Dm.EntityAsIs.Next;
    Assert.isTrue(Dm.EntityAsIsId.AsLargeInt = 2);
    Assert.isTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano M�ller');
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntity;

  procedure CheckObject(const AEntity: TEntity);
  begin
    Assert.isTrue(AEntity.Id = 1);
    Assert.isTrue(AEntity.Code = 2);
    Assert.isTrue(AEntity.Name = 'Ezequiel Juliano M�ller');
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
    '"Name":"Ezequiel Juliano M�ller",' +
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
    '"FName":"Ezequiel Juliano M�ller",' +
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
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano M�ller"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' +
    '"NotesAsString":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TSale;
begin
  O := TSale.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Entity.Id = 1);
    Assert.isTrue(O.Entity.Code = 2);
    Assert.isTrue(O.Entity.Name = 'Ezequiel Juliano M�ller');
    Assert.isTrue(O.Notes.DataString = 'Ezequiel Juliano M�ller');
    Assert.isTrue(O.NotesAsString.DataString = 'Ezequiel Juliano M�ller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomSerializer;
const
  JSON =
    '{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TEntityCustom;
begin
  O := TEntityCustom.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    Assert.isTrue(O.Id = 1);
    Assert.isTrue(O.Code = 2);
    Assert.isTrue(O.Name = 'Ezequiel Juliano M�ller');
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
    '"FName":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano M�ller"' +
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
    Assert.isTrue(OFields.Name = 'Ezequiel Juliano M�ller');
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    FSerializer.DeserializeObject(JSON_PROPERTIES, OProperties);
    Assert.isTrue(OProperties.Id = 1);
    Assert.isTrue(OProperties.Code = 2);
    Assert.isTrue(OProperties.Name = 'Ezequiel Juliano M�ller');
  finally
    OProperties.Free;
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
    '"Name":"Ezequiel Juliano M�ller",' +
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
    '}' +
    '}';

  JSON_LOWERCASE =
    '{' +
    '"id":1,' +
    '"name":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_UPPERCASE =
    '{' +
    '"ID":1,' +
    '"NAME":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_ASIS =
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_LIST =
    '[' +
    '{' +
    '"Id_Id":1,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '},' +
    '{' +
    '"Id_Id":2,' +
    '"Name_Name":"Ezequiel Juliano M�ller"' +
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
    Dm.EntityName.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityBirthday.AsDateTime := StrToDate('15/10/1987');
    Dm.EntityAccessDateTime.AsDateTime := StrToDateTime('17/02/2017 16:37:50');
    Dm.EntityAccessTime.AsDateTime := StrToTime('16:40:50');
    Dm.EntityActive.AsBoolean := True;
    Dm.EntitySalary.AsCurrency := 100;
    Dm.EntityAmount.AsFloat := 100;
    Dm.EntityBlobFld.AsString := '<html><body><h1>BLOB</h1></body></html>';

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
    Dm.EntityLowerCaseName.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityLowerCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityLowerCase);
    Assert.areEqual(JSON_LOWERCASE, S);

    Dm.EntityUpperCase.Insert;
    Dm.EntityUpperCaseId.AsLargeInt := 1;
    Dm.EntityUpperCaseName.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityUpperCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S);

    Dm.EntityUpperCase2.Insert;
    Dm.EntityUpperCase2Id.AsLargeInt := 1;
    Dm.EntityUpperCase2Name.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityUpperCase2.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase2, [], ncUpperCase);
    Assert.areEqual(JSON_UPPERCASE, S);

    Dm.EntityAsIs.Insert;
    Dm.EntityAsIsId.AsLargeInt := 1;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityAsIs.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityAsIs);
    Assert.areEqual(JSON_ASIS, S);

    Dm.EntityAsIs.Append;
    Dm.EntityAsIsId.AsLargeInt := 2;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano M�ller';
    Dm.EntityAsIs.Post;
    S := FSerializer.SerializeDataSet(Dm.EntityAsIs);
    Assert.areEqual(JSON_LIST, S);
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
    '"Color":"RED"' +
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
    Assert.areEqual(Ord(TColorEnum.RED), Ord(O.Color));
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntity;
const
  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano M�ller",' +
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
    '"FName":"Ezequiel Juliano M�ller",' +
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
    '"Name":"Ezequiel Juliano M�ller",' +
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
    O.Name := 'Ezequiel Juliano M�ller';
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
    '"AName":"Ezequiel Juliano M�ller"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' +
    '"NotesAsString":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TSale;
  S: string;
begin
  O := TSale.Create;
  try
    O.Entity.Id := 1;
    O.Entity.Code := 2;
    O.Entity.Name := 'Ezequiel Juliano M�ller';
    O.Notes.WriteString('Ezequiel Juliano M�ller');
    O.NotesAsString.WriteString('Ezequiel Juliano M�ller');

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
    '"AName":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TEntityCustom;
  S: string;
begin
  O := TEntityCustom.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano M�ller';

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
    '"name":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TEntityLowerCase;
  S: string;
begin
  O := TEntityLowerCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano M�ller';

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
    '"Name_Name":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TEntityNameAs;
  S: string;
begin
  O := TEntityNameAs.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano M�ller';

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
    '"FName":"Ezequiel Juliano M�ller"' +
    '}';

  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano M�ller"' +
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
    OFields.Name := 'Ezequiel Juliano M�ller';

    S := FSerializer.SerializeObject(OFields);
    Assert.areEqual(JSON_FIELDS, S);
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    OProperties.Id := 1;
    OProperties.Code := 2;
    OProperties.Name := 'Ezequiel Juliano M�ller';

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
    '"NAME":"Ezequiel Juliano M�ller"' +
    '}';
var
  O: TEntityUpperCase;
  S: string;
begin
  O := TEntityUpperCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano M�ller';

    S := FSerializer.SerializeObject(O);
    Assert.areEqual(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeNil;
begin
  Assert.areEqual('null', FSerializer.SerializeObject(nil));
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

initialization

TDUnitX.RegisterTestFixture(TMVCTestSerializerJsonDataObjects);

end.
