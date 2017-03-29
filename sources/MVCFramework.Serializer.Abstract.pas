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

unit MVCFramework.Serializer.Abstract;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons;

type

  TMVCAbstractSerializer = class abstract(TInterfacedObject)
  private
    FRttiContext: TRttiContext;
    FTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
  protected
    function GetRttiContext: TRttiContext;
    function GetTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
    function GetSerializationType(const AObject: TObject; const ADefaultValue: TMVCSerializationType = stDefault): TMVCSerializationType;
    function GetNameCase(const AObject: TObject; const ADefaultValue: TMVCNameCase = ncAsIs): TMVCNameCase; overload;
    function GetNameCase(const AComponent: TComponent; const ADefaultValue: TMVCNameCase = ncAsIs): TMVCNameCase; overload;
    function GetDataType(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: TMVCDataType): TMVCDataType;
    function GetNameAs(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: string): string;
    function IsIgnoredAttribute(const AAttributes: TMVCIgnoredList; const AName: string): Boolean;
    function IsIgnoredComponent(const AOwner: TComponent; const AComponentName: string): Boolean;
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMVCAbstractSerializer }

constructor TMVCAbstractSerializer.Create;
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
  FTypeSerializers := TDictionary<PTypeInfo, IMVCTypeSerializer>.Create;
end;

destructor TMVCAbstractSerializer.Destroy;
begin
  FTypeSerializers.Free;
  FRttiContext.Free;
  inherited Destroy;
end;

function TMVCAbstractSerializer.GetNameCase(const AObject: TObject; const ADefaultValue: TMVCNameCase): TMVCNameCase;
var
  ObjType: TRttiType;
  Att: TCustomAttribute;
begin
  Result := ADefaultValue;
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  for Att in ObjType.GetAttributes do
    if Att is MVCNameCaseAttribute then
      Exit(MVCNameCaseAttribute(Att).KeyCase);
end;

function TMVCAbstractSerializer.GetDataType(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: TMVCDataType): TMVCDataType;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
begin
  Result := ADefaultValue;
  if Assigned(AOwner) then
  begin
    ObjType := GetRttiContext.GetType(AOwner.ClassType);
    ObjFld := ObjType.GetField(AComponentName);
    if Assigned(ObjFld) then
      for Att in ObjFld.GetAttributes do
        if Att is MVCDataSetFieldAttribute then
          Exit(MVCDataSetFieldAttribute(Att).DataType);
  end;
end;

function TMVCAbstractSerializer.GetNameAs(const AOwner: TComponent; const AComponentName, ADefaultValue: string): string;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
begin
  Result := ADefaultValue;
  if Assigned(AOwner) then
  begin
    ObjType := GetRttiContext.GetType(AOwner.ClassType);
    ObjFld := ObjType.GetField(AComponentName);
    if Assigned(ObjFld) then
      for Att in ObjFld.GetAttributes do
        if Att is MVCNameAsAttribute then
          Exit(MVCNameAsAttribute(Att).Name);
  end;
end;

function TMVCAbstractSerializer.GetNameCase(const AComponent: TComponent; const ADefaultValue: TMVCNameCase): TMVCNameCase;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
begin
  Result := ADefaultValue;
  if Assigned(AComponent) and Assigned(AComponent.Owner) then
  begin
    ObjType := GetRttiContext.GetType(AComponent.Owner.ClassType);
    ObjFld := ObjType.GetField(AComponent.Name);
    if Assigned(ObjFld) then
      for Att in ObjFld.GetAttributes do
        if Att is MVCNameCaseAttribute then
          Exit(MVCNameCaseAttribute(Att).KeyCase);
  end;
end;

function TMVCAbstractSerializer.GetRttiContext: TRttiContext;
begin
  Result := FRttiContext;
end;

function TMVCAbstractSerializer.GetSerializationType(
  const AObject: TObject;
  const ADefaultValue: TMVCSerializationType): TMVCSerializationType;
var
  ObjType: TRttiType;
  Att: TCustomAttribute;
begin
  Result := ADefaultValue;
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  for Att in ObjType.GetAttributes do
    if Att is MVCSerializeAttribute then
      Exit(MVCSerializeAttribute(Att).SerializationType);
end;

function TMVCAbstractSerializer.GetTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
begin
  Result := FTypeSerializers;
end;

function TMVCAbstractSerializer.IsIgnoredAttribute(const AAttributes: TMVCIgnoredList; const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(AAttributes) to High(AAttributes) do
    if (AAttributes[I] = AName) then
      Exit(True);
end;

function TMVCAbstractSerializer.IsIgnoredComponent(const AOwner: TComponent; const AComponentName: string): Boolean;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
begin
  Result := False;
  if Assigned(AOwner) then
  begin
    ObjType := GetRttiContext.GetType(AOwner.ClassType);
    ObjFld := ObjType.GetField(AComponentName);
    if Assigned(ObjFld) then
      for Att in ObjFld.GetAttributes do
        if Att is MVCDoNotSerializeAttribute then
          Exit(True);
  end;
end;

procedure TMVCAbstractSerializer.RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
begin
  FTypeSerializers.AddOrSetValue(ATypeInfo, AInstance);
end;

end.
