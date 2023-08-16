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

unit MVCFramework.Serializer.Abstract;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons, MVCFramework.Commons;

type

  TMVCAbstractSerializer = class Abstract(TInterfacedObject)
  private
    FRttiContext: TRttiContext;
    FTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
  protected
    FConfig: TMVCConfig;
    function GetRttiContext: TRttiContext;
    function GetSerializationType(const AObject: TObject; const ADefaultValue: TMVCSerializationType = stDefault): TMVCSerializationType;
    function GetDataType(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: TMVCDataType): TMVCDataType;
  public
    class function IsIgnoredAttribute(const AAttributes: TMVCIgnoredList; const AName: string): Boolean;
    class function IsIgnoredComponent(const AOwner: TComponent; const AComponentName: string): Boolean;
    class function GetNameCase(const AComponent: TComponent; const ADefaultValue: TMVCNameCase = ncAsIs): TMVCNameCase; overload;
    class function GetNameCase(const AObject: TObject; const ADefaultValue: TMVCNameCase = ncAsIs): TMVCNameCase; overload;
    class function GetNameAs(const AOwner: TComponent; const AComponentName: string; const ADefaultValue: string): string;
    function GetTypeSerializers: TDictionary<PTypeInfo, IMVCTypeSerializer>;
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);
    function GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo; out ARttiType: TRttiType): Boolean; overload;
    function GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo): TClass; overload;
    constructor Create(const Config: TMVCConfig); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

implementation

{ TMVCAbstractSerializer }

uses
  MVCFramework.Cache,
  MVCFramework.Logger,
  System.SysUtils;

constructor TMVCAbstractSerializer.Create(const Config: TMVCConfig);
begin
  inherited Create;
  FConfig := Config;
  FRttiContext := TRttiContext.Create;
  FTypeSerializers := TDictionary<PTypeInfo, IMVCTypeSerializer>.Create;
end;

constructor TMVCAbstractSerializer.Create;
begin
  Create(nil);
end;

destructor TMVCAbstractSerializer.Destroy;
begin
  FTypeSerializers.Free;
  FRttiContext.Free;
  inherited Destroy;
end;

class function TMVCAbstractSerializer.GetNameCase(const AObject: TObject; const ADefaultValue: TMVCNameCase): TMVCNameCase;
var
  ObjType: TRttiType;
  Att: TCustomAttribute;
  RTTIContext: TRttiContext;
begin
  Result := ADefaultValue;
  RTTIContext := TRttiContext.Create;
  try
    ObjType := RTTIContext.GetType(AObject.ClassType);
    for Att in ObjType.GetAttributes do
    begin
      if Att is MVCNameCaseAttribute then
      begin
        Exit(MVCNameCaseAttribute(Att).KeyCase);
      end;
    end;
  finally
    RTTIContext.Free;
  end;
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

class function TMVCAbstractSerializer.GetNameAs(const AOwner: TComponent; const AComponentName, ADefaultValue: string): string;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
  RTTIContext: TRttiContext;
begin
  Result := ADefaultValue;
  if Assigned(AOwner) then
  begin
    RTTIContext := TRttiContext.Create;
    try
      ObjType := RTTIContext.GetType(AOwner.ClassType);
      ObjFld := ObjType.GetField(AComponentName);
      if Assigned(ObjFld) then
      begin
        for Att in ObjFld.GetAttributes do
        begin
          if Att is MVCNameAsAttribute then
          begin
            Exit(MVCNameAsAttribute(Att).Name);
          end;
        end;
      end;
    finally
      RTTIContext.Free;
    end;
  end;
end;

class function TMVCAbstractSerializer.GetNameCase(const AComponent: TComponent; const ADefaultValue: TMVCNameCase): TMVCNameCase;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
  RTTIContext: TRttiContext;
begin
  Result := ADefaultValue;
  if Assigned(AComponent) and Assigned(AComponent.Owner) then
  begin
    RTTIContext := TRttiContext.Create;
    try
      ObjType := RTTIContext.GetType(AComponent.Owner.ClassType);
      ObjFld := ObjType.GetField(AComponent.Name);
      if Assigned(ObjFld) then
      begin
        for Att in ObjFld.GetAttributes do
        begin
          if Att is MVCNameCaseAttribute then
          begin
            Exit(MVCNameCaseAttribute(Att).KeyCase);
          end;
        end;
      end;
    finally
      RTTIContext.Free;
    end;
  end;
end;

function TMVCAbstractSerializer.GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo): TClass;
var
  LRttiType: TRttiType;
begin
  if not GetObjectTypeOfGenericList(ATypeInfo, LRttiType) then
    Exit(nil);

  if LRttiType.IsInstance then
    Result := LRttiType.AsInstance.MetaclassType
  else
    Result := nil;
end;

function TMVCAbstractSerializer.GetObjectTypeOfGenericList(const ATypeInfo: PTypeInfo; out ARttiType: TRttiType): Boolean;

  function ExtractGenericArguments(ATypeInfo: PTypeInfo): string;
  var
    LOpen: Integer;
    LClose: Integer;
    LTypeInfoName: string;
    I: Integer;
  begin
    LTypeInfoName := UTF8ToString(ATypeInfo.Name);
    LOpen := Pos('<', LTypeInfoName);
    LClose := Pos('>', LTypeInfoName);

    if LTypeInfoName.CountChar('>') > 1 then
    begin
      for I := LTypeInfoName.Length - 1 downto 0 do
      begin
        if LTypeInfoName[I] = '>' then
        begin
          LClose := I;
          Break;
        end;
      end;
    end;

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
    ARttiType := nil;
    Exit(False);
  end;

  LType := ExtractGenericArguments(LGetEnumerator.ReturnType.Handle);
  ARttiType := GetRttiContext.FindType(LType);
  Result := True;
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
  if AObject = nil then
  begin
    Exit(ADefaultValue);
  end;
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

class function TMVCAbstractSerializer.IsIgnoredAttribute(const AAttributes: TMVCIgnoredList; const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := low(AAttributes) to high(AAttributes) do
    if AnsiSameText(AAttributes[I], AName) then
      Exit(True);
end;

class function TMVCAbstractSerializer.IsIgnoredComponent(const AOwner: TComponent; const AComponentName: string): Boolean;
var
  ObjType: TRttiType;
  ObjFld: TRttiField;
  Att: TCustomAttribute;
  RTTIContext: TRttiContext;
begin
  Result := False;
  if Assigned(AOwner) then
  begin
    RTTIContext := TRttiContext.Create;
    try
      ObjType := RTTIContext.GetType(AOwner.ClassType);
      ObjFld := ObjType.GetField(AComponentName);
      if Assigned(ObjFld) then
      begin
        for Att in ObjFld.GetAttributes do
        begin
          if Att is MVCDoNotSerializeAttribute then
          begin
            Exit(True);
          end;
        end;
      end;
    finally
      RTTIContext.Free;
    end;
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
