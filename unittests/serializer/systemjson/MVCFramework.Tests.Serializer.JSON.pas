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

unit MVCFramework.Tests.Serializer.JSON;

interface

uses
  TestFramework,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  System.JSON,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JSON,
  MVCFramework.Tests.Serializer.Intf,
  MVCFramework.Tests.Serializer.Entities;

type

  TMVCTestSerializerJSON = class(TTestCase, IMVCTestSerializer)
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
    procedure TestSerializeCollection;
    { deserialize declarations }
    procedure TestDeserializeEntity;
    procedure TestDeserializeEntityCustomSerializer;
    procedure TestDeserializeEntityCustomMemberSerializer;
    procedure TestDeserializeCollection;
  end;

  TMVCEntityCustomSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  private
    { private declarations }
  protected
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
  public
    { public declarations }
  end;

implementation

{ TMVCTestSerializerJSON }

procedure TMVCTestSerializerJSON.SetUp;
begin
  inherited;
  FSerializer := TMVCJSONSerializer.Create;
  FSerializer.RegisterTypeSerializer(System.TypeInfo(TEntityCustom), TMVCEntityCustomSerializerJSON.Create);
end;

procedure TMVCTestSerializerJSON.TearDown;
begin
  inherited;
  FSerializer := nil;
end;

procedure TMVCTestSerializerJSON.TestDeserializeCollection;

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

procedure TMVCTestSerializerJSON.TestDeserializeEntity;

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
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano Müller",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17 16:37:50",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Role":1,' +
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
    '"FAccessDateTime":"2017-02-17 16:37:50",' +
    '"FAccessTime":"16:40:50",' +
    '"FActive":true,' +
    '"FRole":1,' +
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

procedure TMVCTestSerializerJSON.TestDeserializeEntityCustomMemberSerializer;
const
  JSON =
    '{' +
    '"Entity":{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubw==",' +
    '"NotesAsString":"Ezequiel Juliano"' +
    '}';
var
  O: TSale;
begin
  O := TSale.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    CheckTrue(O.Entity.Id = 1);
    CheckTrue(O.Entity.Code = 2);
    CheckTrue(O.Entity.Name = 'Ezequiel Juliano');
    CheckTrue(O.Notes.DataString = 'Ezequiel Juliano');
    CheckTrue(O.NotesAsString.DataString = 'Ezequiel Juliano');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestDeserializeEntityCustomSerializer;
const
  JSON =
    '{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano"' +
    '}';
var
  O: TEntityCustom;
begin
  O := TEntityCustom.Create;
  try
    FSerializer.DeserializeObject(JSON, O);
    CheckTrue(O.Id = 1);
    CheckTrue(O.Code = 2);
    CheckTrue(O.Name = 'Ezequiel Juliano');
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeCollection;
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
    CheckEqualsString(JSON, S);

    S := FSerializer.SerializeCollection(O, stFields);
    CheckEqualsString(JSON_FIELDS, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeEntity;
const
  JSON_PROPERTIES =
    '{' +
    '"Id":1,' +
    '"Code":2,' +
    '"Name":"Ezequiel Juliano",' +
    '"Salary":100,' +
    '"Birthday":"1987-10-15",' +
    '"AccessDateTime":"2017-02-17 16:37:50",' +
    '"AccessTime":"16:40:50",' +
    '"Active":true,' +
    '"Role":1,' +
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
    '"FName":"Ezequiel Juliano",' +
    '"FSalary":100,' +
    '"FBirthday":"1987-10-15",' +
    '"FAccessDateTime":"2017-02-17 16:37:50",' +
    '"FAccessTime":"16:40:50",' +
    '"FActive":true,' +
    '"FRole":1,' +
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
    '"Name":"Ezequiel Juliano",' +
    '"Salary":100,' +
    '"Birthday":null,' +
    '"AccessDateTime":null,' +
    '"AccessTime":null,' +
    '"Active":true,' +
    '"Role":1,' +
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
    O.Name := 'Ezequiel Juliano';
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

procedure TMVCTestSerializerJSON.TestSerializeEntityCustomMemberSerializer;
const
  JSON =
    '{' +
    '"Entity":{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano"' +
    '},' +
    '"Notes":"RXplcXVpZWwgSnVsaWFubw==",' +
    '"NotesAsString":"Ezequiel Juliano"' +
    '}';
var
  O: TSale;
  S: string;
begin
  O := TSale.Create;
  try
    O.Entity.Id := 1;
    O.Entity.Code := 2;
    O.Entity.Name := 'Ezequiel Juliano';
    O.Notes.WriteString('Ezequiel Juliano');
    O.NotesAsString.WriteString('Ezequiel Juliano');

    S := FSerializer.SerializeObject(O);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeEntityCustomSerializer;
const
  JSON =
    '{' +
    '"AId":1,' +
    '"ACode":2,' +
    '"AName":"Ezequiel Juliano"' +
    '}';
var
  O: TEntityCustom;
  S: string;
begin
  O := TEntityCustom.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano';

    S := FSerializer.SerializeObject(O);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeEntityLowerCaseNames;
const
  JSON =
    '{' +
    '"id":1,' +
    '"code":2,' +
    '"name":"Ezequiel Juliano"' +
    '}';
var
  O: TEntityLowerCase;
  S: string;
begin
  O := TEntityLowerCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano';

    S := FSerializer.SerializeObject(O);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeEntityNameAs;
const
  JSON =
    '{' +
    '"Id_Id":1,' +
    '"Code_Code":2,' +
    '"Name_Name":"Ezequiel Juliano"' +
    '}';
var
  O: TEntityNameAs;
  S: string;
begin
  O := TEntityNameAs.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano';

    S := FSerializer.SerializeObject(O);
    CheckEqualsString(JSON, S);

    S := FSerializer.SerializeObject(O, stFields);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

procedure TMVCTestSerializerJSON.TestSerializeEntityUpperCaseNames;
const
  JSON =
    '{' +
    '"ID":1,' +
    '"CODE":2,' +
    '"NAME":"Ezequiel Juliano"' +
    '}';
var
  O: TEntityUpperCase;
  S: string;
begin
  O := TEntityUpperCase.Create;
  try
    O.Id := 1;
    O.Code := 2;
    O.Name := 'Ezequiel Juliano';

    S := FSerializer.SerializeObject(O);
    CheckEqualsString(JSON, S);
  finally
    O.Free;
  end;
end;

{ TMVCEntityCustomSerializerJSON }

procedure TMVCEntityCustomSerializerJSON.Deserialize(
  const ASerializedObject: TObject; var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
var
  JSONObject: TJSONObject;
  EntityCustom: TEntityCustom;
begin
  JSONObject := ASerializedObject as TJSONObject;
  if Assigned(JSONObject) then
  begin
    EntityCustom := AElementValue.AsObject as TEntityCustom;
    if Assigned(EntityCustom) then
    begin
      EntityCustom.Id := StrToInt64(JSONObject.GetValue('AId').Value);
      EntityCustom.Code := StrToInt64(JSONObject.GetValue('ACode').Value);
      EntityCustom.Name := JSONObject.GetValue('AName').Value;
    end;
  end;
end;

procedure TMVCEntityCustomSerializerJSON.Serialize(
  const AElementValue: TValue; var ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  EntityCustom: TEntityCustom;
begin
  EntityCustom := AElementValue.AsObject as TEntityCustom;
  if Assigned(EntityCustom) then
  begin
    ASerializerObject := TJSONObject.Create;
    TJSONObject(ASerializerObject).AddPair('AId', TJSONNumber.Create(EntityCustom.Id));
    TJSONObject(ASerializerObject).AddPair('ACode', TJSONNumber.Create(EntityCustom.Code));
    TJSONObject(ASerializerObject).AddPair('AName', TJSONString.Create(EntityCustom.Name));
  end;
end;

initialization

RegisterTest(TMVCTestSerializerJSON.Suite);

end.
