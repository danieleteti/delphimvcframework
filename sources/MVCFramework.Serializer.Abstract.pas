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

  TMVCAbstractSerializer = class Abstract(TInterfacedObject)
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
    function GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo): TClass;
  public
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMVCAbstractSerializer }

uses
  MVCFramework.Cache,
  MVCFramework.Logger,
  System.SysUtils;

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

function TMVCAbstractSerializer.GetDataType(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: TMVCDataType)
  : TMVCDataType;
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

function TMVCAbstractSerializer.GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo): TClass;

  function ExtractGenericArguments(ATypeInfo: PTypeInfo): string;
  var
    LOpen: Integer;
    LClose: Integer;
    LTypeInfoName: string;
  begin
    LTypeInfoName := UTF8ToString(ATypeInfo.Name);
    LOpen := Pos('<', LTypeInfoName);
    LClose := Pos('>', LTypeInfoName);

    if LOpen <= 0 then
      Exit('');

    Result := LTypeInfoName.Substring(LOpen, LClose - LOpen - 1);
  end;

var
  LType: string;
  LGetEnumerator: TRttiMethod;
begin
  LGetEnumerator := GetRttiContext.GetType(ATypeInfo).GetMethod('GetEnumerator');
  if not Assigned(LGetEnumerator) then
  begin
    Exit(nil);
  end;

  LType := ExtractGenericArguments(LGetEnumerator.ReturnType.Handle);
  Result := GetRttiContext.FindType(LType).AsInstance.MetaclassType;
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
  lSerializationTypeCacheKey: string;
  lValue: TValue;
  lFound: Boolean;
begin
  lSerializationTypeCacheKey := AObject.QualifiedClassName + '::sertype';
  if TMVCCacheSingleton.Instance.Contains(lSerializationTypeCacheKey, lValue) then
  begin
    if TMVCSerializationType(lValue.AsOrdinal) = stUnknown then
      { no serializationtype attribute is present in the rtti, just return the default value requested by the user }
      Exit(ADefaultValue)
    else
      Exit(TMVCSerializationType(lValue.AsOrdinal));
  end;

  lFound := False;
  Result := ADefaultValue;
  ObjType := GetRttiContext.GetType(AObject.ClassType);
  for Att in ObjType.GetAttributes do
  begin
    if Att is MVCSerializeAttribute then
    begin
      Result := MVCSerializeAttribute(Att).SerializationType;
      Assert(Result <> stUnknown, 'You cannot use stUnknown in the MVCSerialize attribute. It is for internal use only.');
      { If the serialization type has been "searched" in the rtti, then we can save the
        result of this expensive search in the cache }
      TMVCCacheSingleton.Instance.SetValue(lSerializationTypeCacheKey, TValue.FromOrdinal(TypeInfo(TMVCSerializationType), Ord(Result)));
      lFound := True;
      Break;
    end;
  end;
  { if no serializationtype attribute found in the type, then we can save in the cache this information
    using the sentinal value stUnknown }
  if not lFound then
    TMVCCacheSingleton.Instance.SetValue(lSerializationTypeCacheKey, TValue.FromOrdinal(TypeInfo(TMVCSerializationType), Ord(stUnknown)));
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
  for I := low(AAttributes) to high(AAttributes) do
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
  {$IFDEF NEXTGEN}
  LogD('Registering TypeSerializer for: ' + PChar(Pointer(ATypeInfo.Name)));
  {$ELSE}
  LogD('Registering TypeSerializer for: ' + string(ATypeInfo.Name));
  {$ENDIF}

  FTypeSerializers.AddOrSetValue(ATypeInfo, AInstance);
end;

end.
