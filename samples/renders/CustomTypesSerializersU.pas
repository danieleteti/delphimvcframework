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

unit CustomTypesSerializersU;

interface

uses
  MVCFramework.Serializer.Intf,
  System.Rtti;

type
  // Custom serializer for TUserRoles type
  TUserRolesSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure Serialize(
      const AElementValue: TValue;
      var ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );
    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

implementation

uses
  JsonDataObjects, CustomTypesU;

{ TUserPasswordSerializer }

procedure TUserRolesSerializer.Deserialize(const ASerializedObject: TObject;
  var AElementValue: TValue; const AAttributes: TArray<TCustomAttribute>);
begin
  // todo: if you need, implement the deserialize method
end;

procedure TUserRolesSerializer.Serialize(const AElementValue: TValue;
  var ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  lJSONArr: TJDOJsonArray;
  lRole: string;
  I: Integer;
begin
  { Here I want to serialize the userroles array as json array }

  // I know that the selected serializer uses JsonDataObject as serialization engine.
  // You have to check the serializer documentation to find out what are the
  // correct objects to create here!
  lJSONArr := TJDOJsonArray.Create;

  // Assign to the var parameter the correct object
  ASerializerObject := lJSONArr;

  // Then fill the returned object with the correct values
  // reading from the AElementValue
  lJSONArr.Add('--begin--'); { just to prove that the custom serializaion happends }
  for I := 0 to AElementValue.GetArrayLength - 1 do
  begin
    lRole := AElementValue.GetArrayElement(i).AsString;
    lJSONArr.Add(lRole);
  end;
  lJSONArr.Add('--end--'); { just to prove that the custom serializaion happends }
end;

end.
