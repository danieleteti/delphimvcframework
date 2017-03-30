// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Tests.Serializer.JsonDataObjects;

interface

uses
  TestFramework,
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

  TMVCTestSerializerJsonDataObjects = class(TTestCase, IMVCTestSerializer)
  private
    FSerializer: IMVCSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { serialize declarations }
    procedure TestSerializeEntity;
    procedure TestSerializeEntityUpperCaseNames;
    procedure TestSerializeEntityLowerCaseNames;
    procedure TestSerializeEntityNameAs;
    procedure TestSerializeEntityCustomSerializer;
    procedure TestSerializeEntityCustomMemberSerializer;
    procedure TestSerializeEntitySerializationType;
    procedure TestSerializeCollection;
    procedure TestSerializeDataSet;
    { deserialize declarations }
    procedure TestDeserializeEntity;
    procedure TestDeserializeEntityCustomSerializer;
    procedure TestDeserializeEntityCustomMemberSerializer;
    procedure TestDeserializeEntitySerializationType;
    procedure TestDeserializeCollection;
    procedure TestDeserializeDataSet;
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
    CheckTrue(AList.Count = 4);
    CheckTrue(AList.Items[0].Description = 'Description 1');
    CheckTrue(AList.Items[1].Description = 'Description 2');
    CheckTrue(AList.Items[2].Description = 'Description 3');
    CheckTrue(AList.Items[3].Description = 'Description 4');
  end;

const
  JSON_PROPERTIES =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 1"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 2"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 3"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 4"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;

  JSON_FIELDS =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 1"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 2"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 3"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 4"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;
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
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"Salary": 100,' + LINE_BREAK +
    TAB_SPACE + '"Birthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"AccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"AccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"Active": true,' + LINE_BREAK +
    TAB_SPACE + '"Amount": 100,' + LINE_BREAK +
    TAB_SPACE + '"BlobFld": "PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' + LINE_BREAK +
    TAB_SPACE + '"Items": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Name": "Ezequiel"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Id": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Name": "Juliano"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"Departament": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name": "Depto1"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_LOWERCASE =
    '{' + LINE_BREAK +
    TAB_SPACE + '"id": 1,' + LINE_BREAK +
    TAB_SPACE + '"name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_UPPERCASE =
    '{' + LINE_BREAK +
    TAB_SPACE + '"ID": 1,' + LINE_BREAK +
    TAB_SPACE + '"NAME": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_ASIS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id_Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_LIST =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id_Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id_Id": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;
var
  Dm: TEntitiesModule;
begin
  Dm := TEntitiesModule.Create(nil);
  try
    FSerializer.DeserializeDataSetRecord(JSON, Dm.Entity, ['Ignored']);
    CheckTrue(Dm.EntityId.AsLargeInt = 1);
    CheckTrue(Dm.EntityCode.AsInteger = 2);
    CheckTrue(Dm.EntityName.AsString = 'Ezequiel Juliano Müller');
    CheckTrue(Dm.EntityBirthday.AsDateTime = StrToDate('15/10/1987'));
    CheckTrue(Dm.EntityAccessDateTime.AsDateTime = StrToDateTime('17/02/2017 16:37:50'));
    CheckTrue(Dm.EntityAccessTime.AsDateTime = StrToTime('16:40:50'));
    CheckTrue(Dm.EntityActive.AsBoolean = True);
    CheckTrue(Dm.EntitySalary.AsCurrency = 100);
    CheckTrue(Dm.EntityAmount.AsFloat = 100);
    CheckTrue(Dm.EntityBlobFld.AsString = '<html><body><h1>BLOB</h1></body></html>');

    Dm.Item.First;
    CheckTrue(Dm.ItemId.AsLargeInt = 1);
    CheckTrue(Dm.ItemName.AsString = 'Ezequiel');

    Dm.Item.Next;
    CheckTrue(Dm.ItemId.AsLargeInt = 2);
    CheckTrue(Dm.ItemName.AsString = 'Juliano');

    Dm.Departament.First;
    CheckTrue(Dm.DepartamentName.AsString = 'Depto1');

    FSerializer.DeserializeDataSetRecord(JSON_LOWERCASE, Dm.EntityLowerCase);
    CheckTrue(Dm.EntityLowerCaseId.AsLargeInt = 1);
    CheckTrue(Dm.EntityLowerCaseName.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase);
    CheckTrue(Dm.EntityUpperCaseId.AsLargeInt = 1);
    CheckTrue(Dm.EntityUpperCaseName.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_UPPERCASE, Dm.EntityUpperCase2, [], ncUpperCase);
    CheckTrue(Dm.EntityUpperCase2Id.AsLargeInt = 1);
    CheckTrue(Dm.EntityUpperCase2Name.AsString = 'Ezequiel Juliano Müller');

    FSerializer.DeserializeDataSetRecord(JSON_ASIS, Dm.EntityAsIs);
    CheckTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    CheckTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.EmptyDataSet;
    FSerializer.DeserializeDataSet(JSON_LIST, Dm.EntityAsIs);
    Dm.EntityAsIs.First;
    CheckTrue(Dm.EntityAsIsId.AsLargeInt = 1);
    CheckTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');

    Dm.EntityAsIs.Next;
    CheckTrue(Dm.EntityAsIsId.AsLargeInt = 2);
    CheckTrue(Dm.EntityAsIsName.AsString = 'Ezequiel Juliano Müller');
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntity;

  procedure CheckObject(const AEntity: TEntity);
  begin
    CheckTrue(AEntity.Id = 1);
    CheckTrue(AEntity.Code = 2);
    CheckTrue(AEntity.Name = 'Ezequiel Juliano Müller');
    CheckTrue(AEntity.Salary = 100);
    CheckTrue(DateToStr(AEntity.Birthday) = '15/10/1987');
    CheckTrue(DateTimeToStr(AEntity.AccessDateTime) = '17/02/2017 16:37:50');
    CheckTrue(TimeToStr(AEntity.AccessTime) = '16:40:50');
    CheckTrue(AEntity.Active = True);
    CheckTrue(AEntity.Role = TRole.roGuest);
    CheckTrue(DateTimeToStr(TimeStampToDateTime(AEntity.Teporization)) = '17/02/2017 16:37:50');
    CheckTrue(AEntity.Department <> nil);
    CheckTrue(AEntity.Department.Id = 1);
    CheckTrue(AEntity.Department.Name = 'Development');
    CheckTrue(AEntity.DepartmentNull = nil);
    CheckTrue(AEntity.Notes.Count = 2);
    CheckTrue(AEntity.Notes[0].Description = 'EntNote1');
    CheckTrue(AEntity.Notes[1].Description = 'EntNote2');
    CheckTrue(AEntity.NotesEmpty.Count = 0);
    CheckTrue(AEntity.AppreciationAs.AsString = 'Yes');
    CheckTrue(AEntity.Appreciation.AsString = 'Yes');
  end;

const
  JSON_PROPERTIES =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"Salary": 100,' + LINE_BREAK +
    TAB_SPACE + '"Birthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"AccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"AccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"Active": true,' + LINE_BREAK +
    TAB_SPACE + '"Role": 1,' + LINE_BREAK +
    TAB_SPACE + '"Teporization": 63623032670000,' + LINE_BREAK +
    TAB_SPACE + '"Department": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name": "Development",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + ']' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"DepartmentNull": null,' + LINE_BREAK +
    TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"NotesEmpty": [],' + LINE_BREAK +
    TAB_SPACE + '"AppreciationAs": "Yes",' + LINE_BREAK +
    TAB_SPACE + '"Appreciation": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"type": "ustring",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"value": "Yes"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_FIELDS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + '"FCode": 2,' + LINE_BREAK +
    TAB_SPACE + '"FName": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"FSalary": 100,' + LINE_BREAK +
    TAB_SPACE + '"FBirthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"FAccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"FAccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"FActive": true,' + LINE_BREAK +
    TAB_SPACE + '"FRole": 1,' + LINE_BREAK +
    TAB_SPACE + '"FTeporization": 63623032670000,' + LINE_BREAK +
    TAB_SPACE + '"FDepartment": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FName": "Development",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FNotes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "DepNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "DepNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + ']' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"FDepartmentNull": null,' + LINE_BREAK +
    TAB_SPACE + '"FNotes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "EntNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "EntNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"FNotesEmpty": [],' + LINE_BREAK +
    TAB_SPACE + '"FAppreciationAs": "Yes",' + LINE_BREAK +
    TAB_SPACE + '"FAppreciation": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"type": "ustring",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"value": "Yes"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    '{' + LINE_BREAK +
    TAB_SPACE + '"Entity": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"AId": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"ACode": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"AName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"Notes": "RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' + LINE_BREAK +
    TAB_SPACE + '"NotesAsString": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
var
  O: TSale;
begin
  O := TSale.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    CheckTrue(O.Entity.Id = 1);
    CheckTrue(O.Entity.Code = 2);
    CheckTrue(O.Entity.Name = 'Ezequiel Juliano Müller');
    CheckTrue(O.Notes.DataString = 'Ezequiel Juliano Müller');
    CheckTrue(O.NotesAsString.DataString = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntityCustomSerializer;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"AId": 1,' + LINE_BREAK +
    TAB_SPACE + '"ACode": 2,' + LINE_BREAK +
    TAB_SPACE + '"AName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
var
  O: TEntityCustom;
begin
  O := TEntityCustom.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    CheckTrue(O.Id = 1);
    CheckTrue(O.Code = 2);
    CheckTrue(O.Name = 'Ezequiel Juliano Müller');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestDeserializeEntitySerializationType;
const
  JSON_FIELDS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + '"FCode": 2,' + LINE_BREAK +
    TAB_SPACE + '"FName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_PROPERTIES =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
var
  OFields: TEntitySerializeFields;
  OProperties: TEntitySerializeProperties;
begin
  OFields := TEntitySerializeFields.Create;
  try
    FSerializer.DeserializeObject(JSON_FIELDS, OFields);
    CheckTrue(OFields.Id = 1);
    CheckTrue(OFields.Code = 2);
    CheckTrue(OFields.Name = 'Ezequiel Juliano Müller');
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    FSerializer.DeserializeObject(JSON_PROPERTIES, OProperties);
    CheckTrue(OProperties.Id = 1);
    CheckTrue(OProperties.Code = 2);
    CheckTrue(OProperties.Name = 'Ezequiel Juliano Müller');
  finally
    OProperties.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeCollection;
const
  JSON =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 1"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 2"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 3"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Description": "Description 4"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;

  JSON_FIELDS =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 1"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 2"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 3"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FDescription": "Description 4"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);

    S := FSerializer.SerializeCollection(O, stFields);
    CheckEqualsString(JSON_FIELDS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeDataSet;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"Salary": 100,' + LINE_BREAK +
    TAB_SPACE + '"Birthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"AccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"AccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"Active": true,' + LINE_BREAK +
    TAB_SPACE + '"Amount": 100,' + LINE_BREAK +
    TAB_SPACE + '"BlobFld": "PGh0bWw+PGJvZHk+PGgxPkJMT0I8L2gxPjwvYm9keT48L2h0bWw+",' + LINE_BREAK +
    TAB_SPACE + '"Items": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Name": "Ezequiel"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Id": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Name": "Juliano"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"Departament": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name": "Depto1"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_LOWERCASE =
    '{' + LINE_BREAK +
    TAB_SPACE + '"id": 1,' + LINE_BREAK +
    TAB_SPACE + '"name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_UPPERCASE =
    '{' + LINE_BREAK +
    TAB_SPACE + '"ID": 1,' + LINE_BREAK +
    TAB_SPACE + '"NAME": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_ASIS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id_Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_LIST =
    '[' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id_Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id_Id": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    ']' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);

    Dm.EntityLowerCase.Insert;
    Dm.EntityLowerCaseId.AsLargeInt := 1;
    Dm.EntityLowerCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityLowerCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityLowerCase);
    CheckEqualsString(JSON_LOWERCASE, S);

    Dm.EntityUpperCase.Insert;
    Dm.EntityUpperCaseId.AsLargeInt := 1;
    Dm.EntityUpperCaseName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase);
    CheckEqualsString(JSON_UPPERCASE, S);

    Dm.EntityUpperCase2.Insert;
    Dm.EntityUpperCase2Id.AsLargeInt := 1;
    Dm.EntityUpperCase2Name.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityUpperCase2.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityUpperCase2, [], ncUpperCase);
    CheckEqualsString(JSON_UPPERCASE, S);

    Dm.EntityAsIs.Insert;
    Dm.EntityAsIsId.AsLargeInt := 1;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;
    S := FSerializer.SerializeDataSetRecord(Dm.EntityAsIs);
    CheckEqualsString(JSON_ASIS, S);

    Dm.EntityAsIs.Append;
    Dm.EntityAsIsId.AsLargeInt := 2;
    Dm.EntityAsIsName.AsString := 'Ezequiel Juliano Müller';
    Dm.EntityAsIs.Post;
    S := FSerializer.SerializeDataSet(Dm.EntityAsIs);
    CheckEqualsString(JSON_LIST, S);
  finally
    Dm.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntity;
const
  JSON_PROPERTIES =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"Salary": 100,' + LINE_BREAK +
    TAB_SPACE + '"Birthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"AccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"AccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"Active": true,' + LINE_BREAK +
    TAB_SPACE + '"Role": 1,' + LINE_BREAK +
    TAB_SPACE + '"Teporization": 63623032670000,' + LINE_BREAK +
    TAB_SPACE + '"Department": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name": "Development",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + ']' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"DepartmentNull": null,' + LINE_BREAK +
    TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"NotesEmpty": [],' + LINE_BREAK +
    TAB_SPACE + '"AppreciationAs": "Yes",' + LINE_BREAK +
    TAB_SPACE + '"Appreciation": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"type": "ustring",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"value": "Yes"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_FIELDS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + '"FCode": 2,' + LINE_BREAK +
    TAB_SPACE + '"FName": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"FSalary": 100,' + LINE_BREAK +
    TAB_SPACE + '"FBirthday": "1987-10-15",' + LINE_BREAK +
    TAB_SPACE + '"FAccessDateTime": "2017-02-17 16:37:50",' + LINE_BREAK +
    TAB_SPACE + '"FAccessTime": "16:40:50",' + LINE_BREAK +
    TAB_SPACE + '"FActive": true,' + LINE_BREAK +
    TAB_SPACE + '"FRole": 1,' + LINE_BREAK +
    TAB_SPACE + '"FTeporization": 63623032670000,' + LINE_BREAK +
    TAB_SPACE + '"FDepartment": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FName": "Development",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"FNotes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "DepNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "DepNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + ']' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"FDepartmentNull": null,' + LINE_BREAK +
    TAB_SPACE + '"FNotes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "EntNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"FDescription": "EntNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"FNotesEmpty": [],' + LINE_BREAK +
    TAB_SPACE + '"FAppreciationAs": "Yes",' + LINE_BREAK +
    TAB_SPACE + '"FAppreciation": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"type": "ustring",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"value": "Yes"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_NULLS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller",' + LINE_BREAK +
    TAB_SPACE + '"Salary": 100,' + LINE_BREAK +
    TAB_SPACE + '"Birthday": null,' + LINE_BREAK +
    TAB_SPACE + '"AccessDateTime": null,' + LINE_BREAK +
    TAB_SPACE + '"AccessTime": null,' + LINE_BREAK +
    TAB_SPACE + '"Active": true,' + LINE_BREAK +
    TAB_SPACE + '"Role": 1,' + LINE_BREAK +
    TAB_SPACE + '"Teporization": 63623032670000,' + LINE_BREAK +
    TAB_SPACE + '"Department": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Name": "Development",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "DepNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + ']' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"DepartmentNull": null,' + LINE_BREAK +
    TAB_SPACE + '"Notes": [' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote1"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '{' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + TAB_SPACE + '"Description": "EntNote2"' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '}' + LINE_BREAK +
    TAB_SPACE + '],' + LINE_BREAK +
    TAB_SPACE + '"NotesEmpty": [],' + LINE_BREAK +
    TAB_SPACE + '"AppreciationAs": "Yes",' + LINE_BREAK +
    TAB_SPACE + '"Appreciation": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"type": "ustring",' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"value": "Yes"' + LINE_BREAK +
    TAB_SPACE + '}' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON_PROPERTIES, S);

    S := FSerializer.SerializeObject(O, stFields, ['FIgnored']);
    CheckEqualsString(JSON_FIELDS, S);

    O.Birthday := 0;
    O.AccessDateTime := 0;
    O.AccessTime := 0;
    S := FSerializer.SerializeObject(O, stProperties, ['Ignored']);
    CheckEqualsString(JSON_NULLS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomMemberSerializer;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Entity": {' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"AId": 1,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"ACode": 2,' + LINE_BREAK +
    TAB_SPACE + TAB_SPACE + '"AName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    TAB_SPACE + '},' + LINE_BREAK +
    TAB_SPACE + '"Notes": "RXplcXVpZWwgSnVsaWFubyBN/GxsZXI=",' + LINE_BREAK +
    TAB_SPACE + '"NotesAsString": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityCustomSerializer;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"AId": 1,' + LINE_BREAK +
    TAB_SPACE + '"ACode": 2,' + LINE_BREAK +
    TAB_SPACE + '"AName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityLowerCaseNames;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"id": 1,' + LINE_BREAK +
    TAB_SPACE + '"code": 2,' + LINE_BREAK +
    TAB_SPACE + '"name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityNameAs;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id_Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code_Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name_Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);

    S := FSerializer.SerializeObject(O, stFields);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntitySerializationType;
const
  JSON_FIELDS =
    '{' + LINE_BREAK +
    TAB_SPACE + '"FId": 1,' + LINE_BREAK +
    TAB_SPACE + '"FCode": 2,' + LINE_BREAK +
    TAB_SPACE + '"FName": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;

  JSON_PROPERTIES =
    '{' + LINE_BREAK +
    TAB_SPACE + '"Id": 1,' + LINE_BREAK +
    TAB_SPACE + '"Code": 2,' + LINE_BREAK +
    TAB_SPACE + '"Name": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON_FIELDS, S);
  finally
    OFields.Free;
  end;

  OProperties := TEntitySerializeProperties.Create;
  try
    OProperties.Id := 1;
    OProperties.Code := 2;
    OProperties.Name := 'Ezequiel Juliano Müller';

    S := FSerializer.SerializeObject(OProperties);
    CheckEqualsString(JSON_PROPERTIES, S);
  finally
    OProperties.Free;
  end;
end;

procedure TMVCTestSerializerJsonDataObjects.TestSerializeEntityUpperCaseNames;
const
  JSON =
    '{' + LINE_BREAK +
    TAB_SPACE + '"ID": 1,' + LINE_BREAK +
    TAB_SPACE + '"CODE": 2,' + LINE_BREAK +
    TAB_SPACE + '"NAME": "Ezequiel Juliano Müller"' + LINE_BREAK +
    '}' + LINE_BREAK;
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
    CheckEqualsString(JSON, S);
  finally
    O.Free;
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

RegisterTest(TMVCTestSerializerJsonDataObjects.Suite);

end.
