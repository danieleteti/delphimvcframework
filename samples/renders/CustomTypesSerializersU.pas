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

unit CustomTypesSerializersU;

interface

uses
  MVCFramework.Serializer.Intf,
  System.Rtti,
  MVCFramework.Serializer.Commons;

type
  // Custom serializer for TUserRoles type
  TUserRolesSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    // procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
    // const AAttributes: TArray<TCustomAttribute>);
    // procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
    // const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction = nil);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
  end;

  // Custom serializer for TNullableAliasSerializer type
  TNullableAliasSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure Serialize(const AElementValue: TValue; var ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
    procedure Deserialize(const ASerializedObject: TObject; var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>);
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>;
      const ASerializationAction: TMVCSerializationAction);
    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: System.TArray<System.TCustomAttribute>);
  end;

implementation

uses
  JsonDataObjects,
  CustomTypesU,
  MVCFramework.Serializer.JsonDataObjects,
  System.SysUtils;

{ TUserPasswordSerializer }

procedure TUserRolesSerializer.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
begin
  raise Exception.Create('To implement');
end;

procedure TUserRolesSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TUserRolesSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
var
  lJSONArr: TJDOJsonArray;
  lRole: string;
  I: Integer;
begin
  // I know that the selected serializer uses JsonDataObject as serialization engine.
  // You have to check the serializer documentation to find out what are the
  // correct objects to create here!

  lJSONArr := (ASerializerObject as TJDOJsonObject).A[APropertyName];
  { Here I want to serialize the userroles array as json array }
  // reading from the AElementValue
  lJSONArr.Add('--begin--'); { just to prove that the custom serialization happends }
  for I := 0 to AElementValue.GetArrayLength - 1 do
  begin
    lRole := AElementValue.GetArrayElement(I).AsString;
    lJSONArr.Add(lRole);
  end;
  lJSONArr.Add('--end--'); { just to prove that the custom serialization happends }
end;

{ TNullableAliasSerializer }

procedure TNullableAliasSerializer.Deserialize(const ASerializedObject: TObject;
  var AElementValue: TValue;
  const AAttributes: TArray<TCustomAttribute>);
begin

end;

procedure TNullableAliasSerializer.DeserializeAttribute(var AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TNullableAliasSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TNullableAliasSerializer.Serialize(const AElementValue: TValue;
  var ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
begin
  raise Exception.Create('TODO');
  // ASerializerObject := TJsonValue.Create;
  // TJsonValue(ASerializerObject).Value := AElementValue.AsType<TNullableRecordAlias>.Value;
end;

procedure TNullableAliasSerializer.SerializeAttribute(const AElementValue: TValue;
  const APropertyName: string;
  const ASerializerObject: TObject; const AAttributes: System.TArray<System.TCustomAttribute>);
begin

end;

procedure TNullableAliasSerializer.SerializeRoot(const AObject: TObject;
  out ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
begin
  raise EMVCSerializationException.CreateFmt('%s cannot be used as root object', [ClassName]);
end;

procedure TUserRolesSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
  const AAttributes: System.TArray<System.TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction = nil);
begin
  raise EMVCSerializationException.CreateFmt('%s cannot be used as root object', [ClassName]);
end;

end.
