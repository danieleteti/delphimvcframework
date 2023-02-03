// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit MVCFramework.Rtti.Utils;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  System.SysUtils,
  Data.DB;

type

  TRttiUtils = class sealed
  private
    class constructor Create;
    class destructor Destroy;
  public
    class var GlContext: TRttiContext;
  public
    class function GetMethod(AObject: TObject; AMethodName: string): TRttiMethod;

    class function GetField(AObject: TObject; const APropertyName: string): TValue; overload;
    class function GetFieldType(AProp: TRttiProperty): string;
    class procedure SetField(AObject: TObject; const APropertyName: string; const AValue: TValue); overload;

    class function GetPropertyType(AObject: TObject; APropertyName: string): string;
    class function GetProperty(AObject: TObject; const APropertyName: string): TValue;
    // class function GetPropertyAsString(AObject: TObject; const APropertyName: string): string; overload;
    // class function GetPropertyAsString(AObject: TObject; AProperty: TRttiProperty): string; overload;
    class function ExistsProperty(AObject: TObject; const APropertyName: string; out AProperty: TRttiProperty): Boolean;
    class procedure SetProperty(AObject: TObject; const APropertyName: string; const AValue: TValue); overload; static;

    class function MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue;
      ARaiseExceptionIfNotFound: Boolean = true): TValue;

    class procedure ObjectToDataSet(AObject: TObject; AField: TField; var AValue: Variant);
    class procedure DatasetToObject(ADataset: TDataset; AObject: TObject);

    class function Clone(AObject: TObject): TObject; static;
    class procedure CopyObject(ASourceObject, ATargetObject: TObject); static;

    class procedure CopyObjectAS<T: class>(ASourceObject, ATargetObject: TObject); static;
    class function CreateObject(ARttiType: TRttiType; const AParams: TArray<TValue> = nil): TObject; overload; static;
    class function CreateObject(AQualifiedClassName: string; const AParams: TArray<TValue> = nil): TObject;
      overload; static;

    class function GetAttribute<T: TCustomAttribute>(const AObject: TRttiObject): T; overload;
    class function GetAttribute<T: TCustomAttribute>(const AObject: TRttiType): T; overload;

    class function HasAttribute<T: TCustomAttribute>(const AObject: TRttiObject): Boolean; overload;
    class function HasAttribute<T: TCustomAttribute>(const AObject: TRttiObject; out AAttribute: T): Boolean; overload;
    class function HasAttribute<T: class>(AObject: TObject; out AAttribute: T): Boolean; overload;
    class function HasAttribute<T: class>(ARttiMember: TRttiMember; out AAttribute: T): Boolean; overload;
    class function HasAttribute<T: class>(ARttiMember: TRttiType; out AAttribute: T): Boolean; overload;

    class function TValueAsString(const AValue: TValue; const APropertyType, ACustomFormat: string): string;
    class function EqualValues(ASource, ADestination: TValue): Boolean;
    class function FindByProperty<T: class>(AList: TObjectList<T>; APropertyName: string; APropertyValue: TValue): T;
    class procedure ForEachProperty(AClazz: TClass; AProc: TProc<TRttiProperty>);
    // class function HasStringValueAttribute<T: class>(ARttiMember: TRttiMember; out AValue: string): Boolean;
    class function BuildClass(AQualifiedName: string; AParams: array of TValue): TObject;
    class function FindType(AQualifiedName: string): TRttiType;
    class function GetGUID<T>: TGUID;
    class function GetArrayContainedRTTIType(const RTTIType: TRttiType): TRttiType;
  end;

{$IF not defined(BERLINORBETTER)}
  TValueHelper = record helper for TValue
  public
    function IsObjectInstance: Boolean;
  end;
{$ENDIF}

function FieldFor(const APropertyName: string): string; inline;

implementation

uses
  MVCFramework.DuckTyping,
  MVCFramework.Serializer.Commons;

class function TRttiUtils.MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue;
  ARaiseExceptionIfNotFound: Boolean): TValue;
var
  m: TRttiMethod;
  T: TRttiType;
  Found: Boolean;
  ParLen: Integer;
  MethodParamsLen: Integer;
begin
  Found := False;
  T := GlContext.GetType(AObject.ClassInfo);
  ParLen := Length(AParameters);
  m := nil;
  for m in T.GetMethods do
  begin
    MethodParamsLen := Length(m.GetParameters);
    if m.Name.Equals(AMethodName) and (MethodParamsLen = ParLen) then
    begin
      Found := true;
      Break;
    end;
  end;

  if Found then
    Result := m.Invoke(AObject, AParameters)
  else if ARaiseExceptionIfNotFound then
    raise Exception.CreateFmt('Cannot find compatible method "%s" in the object', [AMethodName]);
end;

function FieldFor(const APropertyName: string): string; inline;
begin
  Result := 'F' + APropertyName;
end;

class function TRttiUtils.GetAttribute<T>(const AObject: TRttiObject): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in AObject.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
end;

class function TRttiUtils.GetArrayContainedRTTIType(
  const RTTIType: TRttiType): TRttiType;
var
  lName: string;
begin
  lName := RTTIType.Name;
  if not lName.StartsWith('TArray<')  then
  begin
    raise EMVCDeserializationException.CreateFmt('%s is not an array', [lName]);
  end;
  lName := lName.Remove(0, 7);
  lName := lName.Remove(lName.Length - 1);
  Result := GlContext.FindType(lName);
end;

class function TRttiUtils.GetAttribute<T>(const AObject: TRttiType): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in AObject.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
end;

class function TRttiUtils.GetField(AObject: TObject; const APropertyName: string): TValue;
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(APropertyName));
  if Assigned(Field) then
    Result := Field.GetValue(AObject)
  else
  begin
    Prop := ARttiType.GetProperty(APropertyName);
    if not Assigned(Prop) then
      raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, APropertyName]);
    Result := Prop.GetValue(AObject);
  end;
end;

class function TRttiUtils.GetProperty(AObject: TObject; const APropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(APropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, APropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(AObject)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]', [ARttiType.ToString, APropertyName]);
end;

// class function TRttiUtils.GetPropertyAsString(AObject: TObject; AProperty: TRttiProperty): string;
// var
// P: TValue;
// FT: string;
// CustomFormat: string;
// begin
// if AProperty.IsReadable then
// begin
// P := AProperty.GetValue(AObject);
// FT := GetFieldType(AProperty);
// HasStringValueAttribute<StringValueAttribute>(AProperty, CustomFormat);
// Result := TValueAsString(P, FT, CustomFormat);
// end
// else
// Result := '';
// end;
//
// class function TRttiUtils.GetPropertyAsString(AObject: TObject; const APropertyName: string): string;
// var
// Prop: TRttiProperty;
// begin
// Prop := GlContext.GetType(AObject.ClassType).GetProperty(APropertyName);
// if Assigned(Prop) then
// Result := GetPropertyAsString(AObject, Prop)
// else
// Result := '';
// end;

class function TRttiUtils.GetPropertyType(AObject: TObject; APropertyName: string): string;
begin
  Result := GetFieldType(GlContext.GetType(AObject.ClassInfo).GetProperty(APropertyName));
end;

class function TRttiUtils.HasAttribute<T>(const AObject: TRttiObject): Boolean;
begin
  Result := Assigned(GetAttribute<T>(AObject));
end;

class function TRttiUtils.HasAttribute<T>(ARttiMember: TRttiMember; out AAttribute: T): Boolean;
var
  attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := False;
  attrs := ARttiMember.GetAttributes;
  for Attr in attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(true);
    end;
end;

class function TRttiUtils.HasAttribute<T>(ARttiMember: TRttiType; out AAttribute: T): Boolean;
var
  attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := False;
  attrs := ARttiMember.GetAttributes;
  for Attr in attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(true);
    end;

end;

class function TRttiUtils.HasAttribute<T>(const AObject: TRttiObject; out AAttribute: T): Boolean;
begin
  AAttribute := GetAttribute<T>(AObject);
  Result := Assigned(AAttribute);
end;

// class function TRttiUtils.HasStringValueAttribute<T>(ARttiMember: TRttiMember; out AValue: string): Boolean;
// var
// Attr: T; // StringValueAttribute;
// begin
// Result := HasAttribute<T>(ARTTIMember, Attr);
// if Result then
// AValue := StringValueAttribute(Attr).Value
// else
// AValue := '';
// end;

class procedure TRttiUtils.SetField(AObject: TObject; const APropertyName: string; const AValue: TValue);
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(APropertyName));
  if Assigned(Field) then
    Field.SetValue(AObject, AValue)
  else
  begin
    Prop := ARttiType.GetProperty(APropertyName);
    if Assigned(Prop) then
    begin
      if Prop.IsWritable then
        Prop.SetValue(AObject, AValue)
    end
    else
      raise Exception.CreateFmt('Cannot get RTTI for field or property [%s.%s]', [ARttiType.ToString, APropertyName]);
  end;
end;

class procedure TRttiUtils.SetProperty(AObject: TObject; const APropertyName: string; const AValue: TValue);
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(APropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, APropertyName]);
  if Prop.IsWritable then
    Prop.SetValue(AObject, AValue)
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]', [ARttiType.ToString, APropertyName]);
end;

class function TRttiUtils.TValueAsString(const AValue: TValue; const APropertyType, ACustomFormat: string): string;
begin
  case AValue.Kind of
    tkUnknown:
      Result := '';
    tkInteger:
      Result := IntToStr(AValue.AsInteger);
    tkChar:
      Result := AValue.AsString;
    tkEnumeration:
      if APropertyType = 'boolean' then
        Result := BoolToStr(AValue.AsBoolean, true)
      else
        Result := '(enumeration)';
    tkFloat:
      begin
        if APropertyType = 'datetime' then
        begin
          if ACustomFormat = '' then
            Exit(DateTimeToStr(AValue.AsExtended))
          else
            Exit(FormatDateTime(ACustomFormat, AValue.AsExtended))
        end
        else if APropertyType = 'date' then
        begin
          if ACustomFormat = '' then
            Exit(DateToStr(AValue.AsExtended))
          else
            Exit(FormatDateTime(ACustomFormat, Trunc(AValue.AsExtended)))
        end
        else if APropertyType = 'time' then
        begin
          if ACustomFormat = '' then
            Exit(TimeToStr(AValue.AsExtended))
          else
            Exit(FormatDateTime(ACustomFormat, Frac(AValue.AsExtended)))
        end;
        if ACustomFormat.IsEmpty then
          Result := FloatToStr(AValue.AsExtended)
        else
          Result := FormatFloat(ACustomFormat, AValue.AsExtended);
      end;
    tkString:
      Result := AValue.AsString;
    tkSet:
      Result := '';
    tkClass:
      Result := AValue.AsObject.QualifiedClassName;
    tkMethod:
      Result := '';
    tkWChar:
      Result := AValue.AsString;

    tkLString:
      Result := AValue.AsString;

    tkWString:
      Result := AValue.AsString;

    tkVariant:
      Result := string(AValue.AsVariant);

    tkArray:
      Result := '(array)';
    tkRecord:
      Result := '(record)';
    tkInterface:
      Result := '(interface)';

    tkInt64:
      Result := IntToStr(AValue.AsInt64);

    tkDynArray:
      Result := '(array)';

    tkUString:
      Result := AValue.AsString;
    tkClassRef:
      Result := '(classref)';

    tkPointer:
      Result := '(pointer)';

    tkProcedure:
      Result := '(procedure)';
  else
    Result := '';
  end;
end;

class function TRttiUtils.GetFieldType(AProp: TRttiProperty): string;
var
  _PropInfo: PTypeInfo;
begin
  _PropInfo := AProp.PropertyType.Handle;
  if _PropInfo.Kind in [tkString, tkWString, tkChar, tkWChar, tkLString, tkUString] then
    Result := 'string'
  else if _PropInfo.Kind in [tkInteger, tkInt64] then
    Result := 'integer'
  else if _PropInfo = TypeInfo(TDate) then
    Result := 'date'
  else if _PropInfo = TypeInfo(TDateTime) then
    Result := 'datetime'
  else if _PropInfo = TypeInfo(Currency) then
    Result := 'decimal'
  else if _PropInfo = TypeInfo(TTime) then
  begin
    Result := 'time'
  end
  else if _PropInfo.Kind = tkFloat then
  begin
    Result := 'float'
  end
  else if (_PropInfo.Kind = tkEnumeration) { and (_PropInfo.Name = 'Boolean') } then
    Result := 'boolean'
  else if AProp.PropertyType.IsInstance and AProp.PropertyType.AsInstance.MetaclassType.InheritsFrom(TStream) then
    Result := 'blob'
  else
    Result := EmptyStr;
end;

class function TRttiUtils.GetGUID<T>: TGUID;
var
  Tp: TRttiType;
begin
  Tp := GlContext.GetType(TypeInfo(T));
  if not(Tp.TypeKind = tkInterface) then
    raise Exception.Create('Type is no interface');
  Result := TRttiInterfaceType(Tp).GUID;
end;

class function TRttiUtils.GetMethod(AObject: TObject; AMethodName: string): TRttiMethod;
var
  T: TRttiType;
begin
  T := GlContext.GetType(AObject.ClassInfo);
  Result := T.GetMethod(AMethodName);
end;

class procedure TRttiUtils.ObjectToDataSet(AObject: TObject; AField: TField; var AValue: Variant);
begin
  AValue := GetProperty(AObject, AField.FieldName).AsVariant;
end;

class procedure TRttiUtils.DatasetToObject(ADataset: TDataset; AObject: TObject);
var
  ARttiType: TRttiType;
  props: TArray<TRttiProperty>;
  Prop: TRttiProperty;
  f: TField;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  props := ARttiType.GetProperties;
  for Prop in props do
    if not SameText(Prop.Name, 'ID') then
    begin
      f := ADataset.FindField(Prop.Name);
      if Assigned(f) and not f.ReadOnly then
      begin
        if f is TIntegerField then
          SetProperty(AObject, Prop.Name, TIntegerField(f).Value)
        else
          SetProperty(AObject, Prop.Name, TValue.From<Variant>(f.Value))
      end;
    end;
end;

class destructor TRttiUtils.Destroy;
begin
  GlContext.Free;
end;

class function TRttiUtils.EqualValues(ASource, ADestination: TValue): Boolean;
begin
  // Really UniCodeCompareStr (Annoying VCL Name for backwards compatablity)
  Result := AnsiCompareStr(ASource.ToString, ADestination.ToString) = 0;
end;

class function TRttiUtils.ExistsProperty(AObject: TObject; const APropertyName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GlContext.GetType(AObject.ClassInfo).GetProperty(APropertyName);
  Result := Assigned(AProperty);
end;

class function TRttiUtils.FindByProperty<T>(AList: TObjectList<T>; APropertyName: string; APropertyValue: TValue): T;
var
  elem: T;
  V: TValue;
  Found: Boolean;
begin
  Found := False;
  for elem in AList do
  begin
    V := GetProperty(elem, APropertyName);
    case V.Kind of
      tkInteger:
        Found := V.AsInteger = APropertyValue.AsInteger;
      tkFloat:
        Found := abs(V.AsExtended - APropertyValue.AsExtended) < 0.001;
      tkString, tkLString, tkWString, tkUString:
        Found := V.AsString = APropertyValue.AsString;
      tkInt64:
        Found := V.AsInt64 = APropertyValue.AsInt64;
    else
      raise Exception.Create('Property type not supported');
    end;
    if Found then
      Exit(elem);
  end;
  Result := nil;
end;

class function TRttiUtils.FindType(AQualifiedName: string): TRttiType;
begin
  Result := GlContext.FindType(AQualifiedName);
end;

class procedure TRttiUtils.ForEachProperty(AClazz: TClass; AProc: TProc<TRttiProperty>);
var
  _rtti: TRttiType;
  P: TRttiProperty;
begin
  _rtti := GlContext.GetType(AClazz);
  if Assigned(_rtti) then
  begin
    for P in _rtti.GetProperties do
      AProc(P);
  end;
end;

class procedure TRttiUtils.CopyObject(ASourceObject, ATargetObject: TObject);
var
  _ARttiType: TRttiType;
  Field: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
  targetCollection: IWrappedList;
  sourceCollection: IWrappedList;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
  Tar: TObject;
begin
  if not Assigned(ATargetObject) then
    Exit;

  _ARttiType := GlContext.GetType(ASourceObject.ClassType);
  cloned := ATargetObject;
  master := ASourceObject;
  for Field in _ARttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then
      Field.SetValue(cloned, Field.GetValue(master))
    else
    begin
      Src := Field.GetValue(ASourceObject).AsObject;
      if Src is TStream then
      begin
        sourceStream := TStream(Src);
        SavedPosition := sourceStream.Position;
        sourceStream.Position := 0;
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetStream := TMemoryStream.Create;
          Field.SetValue(cloned, targetStream);
        end
        else
          targetStream := Field.GetValue(cloned).AsObject as TStream;
        targetStream.Position := 0;
        targetStream.CopyFrom(sourceStream, sourceStream.Size);
        targetStream.Position := SavedPosition;
        sourceStream.Position := SavedPosition;
      end
      else if TDuckTypedList.CanBeWrappedAsList(Src) then
      begin
        sourceCollection := WrapAsList(Src);
        Tar := Field.GetValue(cloned).AsObject;
        if Assigned(Tar) then
        begin
          targetCollection := WrapAsList(Tar);
          targetCollection.Clear;
          for I := 0 to sourceCollection.Count - 1 do
            targetCollection.Add(TRttiUtils.Clone(sourceCollection.GetItem(I)));
        end;
      end
      else
      begin
        sourceObject := Src;

        if Field.GetValue(cloned).IsEmpty then
        begin
          targetObject := TRttiUtils.Clone(sourceObject);
          Field.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := Field.GetValue(cloned).AsObject;
          TRttiUtils.CopyObject(sourceObject, targetObject);
        end;
      end;
    end;
  end;
end;

{$IF CompilerVersion >= 24.0}

class procedure TRttiUtils.CopyObjectAS<T>(ASourceObject, ATargetObject: TObject);
var
  _ARttiType: TRttiType;
  _ARttiTypeTarget: TRttiType;
  Field, FieldDest: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
  targetCollection: IWrappedList;
  sourceCollection: IWrappedList;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
  Tar: TObject;
begin
  if not Assigned(ATargetObject) then
    Exit;

  _ARttiType := GlContext.GetType(ASourceObject.ClassType);
  _ARttiTypeTarget := GlContext.GetType(ATargetObject.ClassType);

  cloned := ATargetObject;
  master := ASourceObject;
  for Field in _ARttiType.GetFields do
  begin
    FieldDest := _ARttiTypeTarget.GetField(Field.Name);
    if not Assigned(FieldDest) then
      continue;
    if not Field.FieldType.IsInstance then
    begin
      FieldDest.SetValue(cloned, Field.GetValue(master));
    end
    else
    begin
      Src := Field.GetValue(ASourceObject).AsObject;
      if not Assigned(Src) then
      begin
        FieldDest.SetValue(cloned, Src);

      end
      else if Src is TStream then
      begin
        sourceStream := TStream(Src);
        SavedPosition := sourceStream.Position;
        sourceStream.Position := 0;
        if FieldDest.GetValue(cloned).IsEmpty then
        begin
          targetStream := TMemoryStream.Create;
          FieldDest.SetValue(cloned, targetStream);
        end
        else
          targetStream := FieldDest.GetValue(cloned).AsObject as TStream;
        targetStream.Position := 0;
        targetStream.CopyFrom(sourceStream, sourceStream.Size);
        targetStream.Position := SavedPosition;
        sourceStream.Position := SavedPosition;
      end
      else if TDuckTypedList.CanBeWrappedAsList(Src) then
      begin
        sourceCollection := WrapAsList(Src);
        Tar := FieldDest.GetValue(cloned).AsObject;
        if Assigned(Tar) then
        begin
          targetCollection := WrapAsList(Tar);
          targetCollection.Clear;
          for I := 0 to sourceCollection.Count - 1 do
            targetCollection.Add(TRttiUtils.Clone(sourceCollection.GetItem(I)));
        end;
      end
      else
      begin
        sourceObject := Src;

        if FieldDest.GetValue(cloned).IsEmpty then
        begin
          targetObject := TRttiUtils.Clone(sourceObject);
          FieldDest.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := FieldDest.GetValue(cloned).AsObject;
          TRttiUtils.CopyObject(sourceObject, targetObject);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

class constructor TRttiUtils.Create;
begin
  GlContext := TRttiContext.Create;
end;

class function TRttiUtils.CreateObject(AQualifiedClassName: string; const AParams: TArray<TValue> = nil): TObject;
var
  rttitype: TRttiType;
begin
  rttitype := GlContext.FindType(AQualifiedClassName);
  if Assigned(rttitype) then
    Result := CreateObject(rttitype, AParams)
  else
    raise Exception.Create('Cannot find RTTI for ' + AQualifiedClassName +
      '. Hint: Is the specified classtype linked in the module?');
end;

class function TRttiUtils.CreateObject(ARttiType: TRttiType; const AParams: TArray<TValue> = nil): TObject;
var
  Method: TRttiMethod;
  metaClass: TClass;
  lParamsCount: Integer;
begin
  if AParams = nil then
  begin
    lParamsCount := 0;
  end
  else
  begin
    lParamsCount := Length(AParams);
  end;

  { First solution, clear and slow }
  metaClass := nil;
  Method := nil;
  for Method in ARttiType.GetMethods do
  begin
    if Method.HasExtendedInfo and Method.IsConstructor then
    begin
      if Length(Method.GetParameters) = lParamsCount then
      begin
        metaClass := ARttiType.AsInstance.MetaclassType;
        Break;
      end;
    end;
  end;
  if Assigned(metaClass) then
  begin
    if AParams = nil then
    begin
      Result := Method.Invoke(metaClass, []).AsObject;
    end
    else
    begin
      Result := Method.Invoke(metaClass, AParams).AsObject;
    end;
  end
  else
  begin
    raise Exception.Create('Cannot find a parameterless constructor for ' + ARttiType.ToString);
  end;

  { Second solution, dirty and fast }
  // Result := TObject(ARttiType.GetMethod('Create')
  // .Invoke(ARttiType.AsInstance.MetaclassType, []).AsObject);
end;

class function TRttiUtils.BuildClass(AQualifiedName: string; AParams: array of TValue): TObject;
var
  T: TRttiType;
  V: TValue;
begin

  T := FindType(AQualifiedName);
  V := T.GetMethod('Create').Invoke(T.AsInstance.MetaclassType, AParams);
  Result := V.AsObject;
end;

class function TRttiUtils.Clone(AObject: TObject): TObject;
var
  _ARttiType: TRttiType;
  lField: TRttiField;
  lMaster, lCloned: TObject;
  Src: TObject;
  lSourceStream: TStream;
  lSavedPosition: Int64;
  lTargetStream: TStream;
  lTargetCollection: TObjectList<TObject>;
  lSourceCollection: TObjectList<TObject>;
  I: Integer;
  lSourceObject: TObject;
  lTargetObject: TObject;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;

  _ARttiType := GlContext.GetType(AObject.ClassType);
  lCloned := CreateObject(_ARttiType);
  lMaster := AObject;
  for lField in _ARttiType.GetFields do
  begin
    if not lField.FieldType.IsInstance then
    begin
      lField.SetValue(lCloned, lField.GetValue(lMaster))
    end
    else
    begin
      Src := lField.GetValue(AObject).AsObject;
      if Src is TStream then
      begin
        lSourceStream := TStream(Src);
        lSavedPosition := lSourceStream.Position;
        lSourceStream.Position := 0;
        if lField.GetValue(lCloned).IsEmpty then
        begin
          lTargetStream := TMemoryStream.Create;
          lField.SetValue(lCloned, lTargetStream);
        end
        else
          lTargetStream := lField.GetValue(lCloned).AsObject as TStream;
        lTargetStream.Position := 0;
        lTargetStream.CopyFrom(lSourceStream, lSourceStream.Size);
        lTargetStream.Position := lSavedPosition;
        lSourceStream.Position := lSavedPosition;
      end
      else if Src is TObjectList<TObject> then
      begin
        lSourceCollection := TObjectList<TObject>(Src);
        if lField.GetValue(lCloned).IsEmpty then
        begin
          lTargetCollection := TObjectList<TObject>.Create;
          lField.SetValue(lCloned, lTargetCollection);
        end
        else
          lTargetCollection := lField.GetValue(lCloned).AsObject as TObjectList<TObject>;
        for I := 0 to lSourceCollection.Count - 1 do
        begin
          lTargetCollection.Add(TRttiUtils.Clone(lSourceCollection[I]));
        end;
      end
      else
      begin
        lSourceObject := Src;

        if lField.GetValue(lCloned).IsEmpty then
        begin
          lTargetObject := TRttiUtils.Clone(lSourceObject);
          lField.SetValue(lCloned, lTargetObject);
        end
        else
        begin
          lTargetObject := lField.GetValue(lCloned).AsObject;
          TRttiUtils.CopyObject(lSourceObject, lTargetObject);
        end;
        lField.SetValue(lCloned, lTargetObject);
      end;
    end;

  end;
  Result := lCloned;
end;

class function TRttiUtils.HasAttribute<T>(AObject: TObject; out AAttribute: T): Boolean;
begin
  Result := HasAttribute<T>(GlContext.GetType(AObject.ClassType), AAttribute)
end;

{$IF not defined(BERLINORBETTER)}

{ TValueHelper }

function TValueHelper.IsObjectInstance: Boolean;
begin
  Result := (Self.TypeInfo <> nil) and (Self.TypeInfo^.Kind = tkClass);
end;

{$ENDIF}

end.
