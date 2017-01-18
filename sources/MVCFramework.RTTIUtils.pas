// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RTTIUtils;

interface

{$I dmvcframework.inc}

uses
  RTTI,
  DB,
  Generics.Collections,
  System.SysUtils;

type
  TRTTIUtils = class sealed
  public
    class var ctx: TRttiContext;
    class var TValueToStringFormatSettings: TFormatSettings;

  public
    class function MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue;
      RaiseExceptionIfNotFound: boolean = true): TValue;
    class function GetMethod(AObject: TObject; AMethodName: string): TRttiMethod;
    class procedure SetProperty(Obj: TObject; const PropertyName: string; const Value: TValue); overload; static;
    class function GetFieldType(AProp: TRttiProperty): string;
    class function GetPropertyType(AObject: TObject; APropertyName: string): string;
    class procedure ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant);
    class function ExistsProperty(AObject: TObject; const APropertyName: string; out AProperty: TRttiProperty): boolean;
    class procedure DatasetToObject(Dataset: TDataset; Obj: TObject);
    class function GetProperty(Obj: TObject; const PropertyName: string): TValue;
    class function GetPropertyAsString(Obj: TObject; const PropertyName: string): string; overload;

    class function GetPropertyAsString(Obj: TObject; AProperty: TRttiProperty): string; overload;
    class function GetField(Obj: TObject; const PropertyName: string): TValue; overload;
    class procedure SetField(Obj: TObject; const PropertyName: string; const Value: TValue); overload;
    class function Clone(Obj: TObject): TObject; static;
    class procedure CopyObject(SourceObj, TargetObj: TObject); static;
{$IFDEF XE3ORBETTER} // not supported before xe3
    class procedure CopyObjectAS<T: class>(SourceObj, TargetObj: TObject); static;
{$ENDIF}
    class function CreateObject(ARttiType: TRttiType): TObject; overload; static;
    class function CreateObject(AQualifiedClassName: string): TObject; overload; static;
    class function GetAttribute<T: TCustomAttribute>(const Obj: TRttiObject): T; overload;
    class function GetAttribute<T: TCustomAttribute>(const Obj: TRttiType): T; overload;

    class function HasAttribute<T: TCustomAttribute>(const Obj: TRttiObject): boolean; overload;
    class function HasAttribute<T: TCustomAttribute>(const Obj: TRttiObject; out AAttribute: T): boolean; overload;
    class function HasAttribute<T: class>(aObj: TObject; out AAttribute: T): boolean; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiMember; out AAttribute: T): boolean; overload;
    class function HasAttribute<T: class>(ARTTIMember: TRttiType; out AAttribute: T): boolean; overload;

    class function TValueAsString(const Value: TValue; const PropertyType, CustomFormat: string): string;
    class function EqualValues(source, destination: TValue): boolean;
    class function FindByProperty<T: class>(List: TObjectList<T>; PropertyName: string; PropertyValue: TValue): T;
    class procedure ForEachProperty(Clazz: TClass; Proc: TProc<TRttiProperty>);
    class function HasStringValueAttribute<T: class>(ARTTIMember: TRttiMember; out Value: string): boolean;
    class function BuildClass(AQualifiedName: string; Params: array of TValue): TObject;
    class function FindType(QualifiedName: string): TRttiType;
    class function GetGUID<T>: TGUID;

  end;

function FieldFor(const PropertyName: string): string; inline;

implementation

uses
  Classes,
  TypInfo,
  ObjectsMappers,
  MVCFramework.DuckTyping;

class function TRTTIUtils.MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue;
  RaiseExceptionIfNotFound: boolean): TValue;
var
  m: TRttiMethod;
  T: TRttiType;
  Found: boolean;
  ParLen: Integer;
  MethodParamsLen: Integer;
begin
  Found := False;
  T := ctx.GetType(AObject.ClassInfo);
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
  else if RaiseExceptionIfNotFound then
    raise Exception.CreateFmt('Cannot find compatible method "%s" in the object', [AMethodName]);
end;

function FieldFor(const PropertyName: string): string; inline;
begin
  Result := 'F' + PropertyName;
end;

class function TRTTIUtils.GetAttribute<T>(const Obj: TRttiObject): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in Obj.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
end;

class function TRTTIUtils.GetAttribute<T>(const Obj: TRttiType): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in Obj.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
end;

class function TRTTIUtils.GetField(Obj: TObject; const PropertyName: string): TValue;
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(PropertyName));
  if Assigned(Field) then
    Result := Field.GetValue(Obj)
  else
  begin
    Prop := ARttiType.GetProperty(PropertyName);
    if not Assigned(Prop) then
      raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, PropertyName]);
    Result := Prop.GetValue(Obj);
  end;
end;

class function TRTTIUtils.GetProperty(Obj: TObject; const PropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, PropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(Obj)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]', [ARttiType.ToString, PropertyName]);
end;

class function TRTTIUtils.GetPropertyAsString(Obj: TObject; AProperty: TRttiProperty): string;
var
  P: TValue;
  FT: string;
  CustomFormat: string;
begin
  if AProperty.IsReadable then
  begin
    P := AProperty.GetValue(Obj);
    FT := GetFieldType(AProperty);
    HasStringValueAttribute<StringValueAttribute>(AProperty, CustomFormat);
    Result := TValueAsString(P, FT, CustomFormat);
  end
  else
    Result := '';
end;

class function TRTTIUtils.GetPropertyAsString(Obj: TObject; const PropertyName: string): string;
var
  Prop: TRttiProperty;
begin
  Prop := ctx.GetType(Obj.ClassType).GetProperty(PropertyName);
  if Assigned(Prop) then
    Result := GetPropertyAsString(Obj, Prop)
  else
    Result := '';
end;

class function TRTTIUtils.GetPropertyType(AObject: TObject; APropertyName: string): string;
begin
  Result := GetFieldType(ctx.GetType(AObject.ClassInfo).GetProperty(APropertyName));
end;

class function TRTTIUtils.HasAttribute<T>(const Obj: TRttiObject): boolean;
begin
  Result := Assigned(GetAttribute<T>(Obj));
end;

class function TRTTIUtils.HasAttribute<T>(ARTTIMember: TRttiMember; out AAttribute: T): boolean;
var
  attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := False;
  attrs := ARTTIMember.GetAttributes;
  for Attr in attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(true);
    end;
end;

class function TRTTIUtils.HasAttribute<T>(ARTTIMember: TRttiType; out AAttribute: T): boolean;
var
  attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
begin
  AAttribute := nil;
  Result := False;
  attrs := ARTTIMember.GetAttributes;
  for Attr in attrs do
    if Attr is T then
    begin
      AAttribute := T(Attr);
      Exit(true);
    end;

end;

class function TRTTIUtils.HasAttribute<T>(const Obj: TRttiObject; out AAttribute: T): boolean;
begin
  AAttribute := GetAttribute<T>(Obj);
  Result := Assigned(AAttribute);
end;

class function TRTTIUtils.HasStringValueAttribute<T>(ARTTIMember: TRttiMember; out Value: string): boolean;
var
  Attr: T; // StringValueAttribute;
begin
  Result := HasAttribute<T>(ARTTIMember, Attr);
  if Result then
    Value := StringValueAttribute(Attr).Value
  else
    Value := '';
end;

class procedure TRTTIUtils.SetField(Obj: TObject; const PropertyName: string; const Value: TValue);
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(PropertyName));
  if Assigned(Field) then
    Field.SetValue(Obj, Value)
  else
  begin
    Prop := ARttiType.GetProperty(PropertyName);
    if Assigned(Prop) then
    begin
      if Prop.IsWritable then
        Prop.SetValue(Obj, Value)
    end
    else
      raise Exception.CreateFmt('Cannot get RTTI for field or property [%s.%s]', [ARttiType.ToString, PropertyName]);
  end;
end;

class procedure TRTTIUtils.SetProperty(Obj: TObject; const PropertyName: string; const Value: TValue);
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, PropertyName]);
  if Prop.IsWritable then
    Prop.SetValue(Obj, Value)
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]', [ARttiType.ToString, PropertyName]);
end;

class function TRTTIUtils.TValueAsString(const Value: TValue; const PropertyType, CustomFormat: string): string;
begin
  case Value.Kind of
    tkUnknown:
      Result := '';
    tkInteger:
      Result := IntToStr(Value.AsInteger);
    tkChar:
      Result := Value.AsString;
    tkEnumeration:
      if PropertyType = 'boolean' then
        Result := BoolToStr(Value.AsBoolean, true)
      else
        Result := '(enumeration)';
    tkFloat:
      begin
        if PropertyType = 'datetime' then
        begin
          if CustomFormat = '' then
            Exit(DateTimeToStr(Value.AsExtended))
          else
            Exit(FormatDateTime(CustomFormat, Value.AsExtended))
        end
        else if PropertyType = 'date' then
        begin
          if CustomFormat = '' then
            Exit(DateToStr(Value.AsExtended))
          else
            Exit(FormatDateTime(CustomFormat, Trunc(Value.AsExtended)))
        end
        else if PropertyType = 'time' then
        begin
          if CustomFormat = '' then
            Exit(TimeToStr(Value.AsExtended))
          else
            Exit(FormatDateTime(CustomFormat, Frac(Value.AsExtended)))
        end;
        if CustomFormat.IsEmpty then
          Result := FloatToStr(Value.AsExtended)
        else
          Result := FormatFloat(CustomFormat, Value.AsExtended);
      end;
    tkString:
      Result := Value.AsString;
    tkSet:
      Result := '';
    tkClass:
      Result := Value.AsObject.QualifiedClassName;
    tkMethod:
      Result := '';
    tkWChar:
      Result := Value.AsString;

    tkLString:
      Result := Value.AsString;

    tkWString:
      Result := Value.AsString;

    tkVariant:
      Result := string(Value.AsVariant);

    tkArray:
      Result := '(array)';
    tkRecord:
      Result := '(record)';
    tkInterface:
      Result := '(interface)';

    tkInt64:
      Result := IntToStr(Value.AsInt64);

    tkDynArray:
      Result := '(array)';

    tkUString:
      Result := Value.AsString;
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

class function TRTTIUtils.GetFieldType(AProp: TRttiProperty): string;
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

class function TRTTIUtils.GetGUID<T>: TGUID;
var
  Tp: TRttiType;
begin
  Tp := ctx.GetType(TypeInfo(T));
  if not(Tp.TypeKind = tkInterface) then
    raise Exception.Create('Type is no interface');
  Result := TRttiInterfaceType(Tp).GUID;
end;

class function TRTTIUtils.GetMethod(AObject: TObject; AMethodName: string): TRttiMethod;
var
  T: TRttiType;
begin
  T := ctx.GetType(AObject.ClassInfo);
  Result := T.GetMethod(AMethodName);
end;

class procedure TRTTIUtils.ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant);
begin
  Value := GetProperty(Obj, Field.FieldName).AsVariant;
end;

class procedure TRTTIUtils.DatasetToObject(Dataset: TDataset; Obj: TObject);
var
  ARttiType: TRttiType;
  props: TArray<TRttiProperty>;
  Prop: TRttiProperty;
  f: TField;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  props := ARttiType.GetProperties;
  for Prop in props do
    if not SameText(Prop.Name, 'ID') then
    begin
      f := Dataset.FindField(Prop.Name);
      if Assigned(f) and not f.ReadOnly then
      begin
        if f is TIntegerField then
          SetProperty(Obj, Prop.Name, TIntegerField(f).Value)
        else
          SetProperty(Obj, Prop.Name, TValue.From<Variant>(f.Value))
      end;
    end;
end;

class function TRTTIUtils.EqualValues(source, destination: TValue): boolean;
begin
  // Really UniCodeCompareStr (Annoying VCL Name for backwards compatablity)
  Result := AnsiCompareStr(source.ToString, destination.ToString) = 0;
end;

class function TRTTIUtils.ExistsProperty(AObject: TObject; const APropertyName: string;
  out AProperty: TRttiProperty): boolean;
begin
  AProperty := ctx.GetType(AObject.ClassInfo).GetProperty(APropertyName);
  Result := Assigned(AProperty);
end;

class function TRTTIUtils.FindByProperty<T>(List: TObjectList<T>; PropertyName: string; PropertyValue: TValue): T;
var
  elem: T;
  V: TValue;
  Found: boolean;
begin
  Found := False;
  for elem in List do
  begin
    V := GetProperty(elem, PropertyName);
    case V.Kind of
      tkInteger:
        Found := V.AsInteger = PropertyValue.AsInteger;
      tkFloat:
        Found := abs(V.AsExtended - PropertyValue.AsExtended) < 0.001;
      tkString, tkLString, tkWString, tkUString:
        Found := V.AsString = PropertyValue.AsString;
      tkInt64:
        Found := V.AsInt64 = PropertyValue.AsInt64;
    else
      raise Exception.Create('Property type not supported');
    end;
    if Found then
      Exit(elem);
  end;
  Result := nil;
end;

class function TRTTIUtils.FindType(QualifiedName: string): TRttiType;
begin
  Result := ctx.FindType(QualifiedName);
end;

class procedure TRTTIUtils.ForEachProperty(Clazz: TClass; Proc: TProc<TRttiProperty>);
var
  _rtti: TRttiType;
  P: TRttiProperty;
begin
  _rtti := ctx.GetType(Clazz);
  if Assigned(_rtti) then
  begin
    for P in _rtti.GetProperties do
      Proc(P);
  end;
end;

class procedure TRTTIUtils.CopyObject(SourceObj, TargetObj: TObject);
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
  if not Assigned(TargetObj) then
    Exit;

  _ARttiType := ctx.GetType(SourceObj.ClassType);
  cloned := TargetObj;
  master := SourceObj;
  for Field in _ARttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then
      Field.SetValue(cloned, Field.GetValue(master))
    else
    begin
      Src := Field.GetValue(SourceObj).AsObject;
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
            targetCollection.Add(TRTTIUtils.Clone(sourceCollection.GetItem(I)));
        end;
      end
      else
      begin
        sourceObject := Src;

        if Field.GetValue(cloned).IsEmpty then
        begin
          targetObject := TRTTIUtils.Clone(sourceObject);
          Field.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := Field.GetValue(cloned).AsObject;
          TRTTIUtils.CopyObject(sourceObject, targetObject);
        end;
      end;
    end;
  end;
end;

{$IF CompilerVersion >= 24.0}


class procedure TRTTIUtils.CopyObjectAS<T>(SourceObj, TargetObj: TObject);
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
  if not Assigned(TargetObj) then
    Exit;

  _ARttiType := ctx.GetType(SourceObj.ClassType);
  _ARttiTypeTarget := ctx.GetType(TargetObj.ClassType);

  cloned := TargetObj;
  master := SourceObj;
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
      Src := Field.GetValue(SourceObj).AsObject;
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
            targetCollection.Add(TRTTIUtils.Clone(sourceCollection.GetItem(I)));
        end;
      end
      else
      begin
        sourceObject := Src;

        if FieldDest.GetValue(cloned).IsEmpty then
        begin
          targetObject := TRTTIUtils.Clone(sourceObject);
          FieldDest.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := FieldDest.GetValue(cloned).AsObject;
          TRTTIUtils.CopyObject(sourceObject, targetObject);
        end;
      end;
    end;
  end;
end;
{$ENDIF}


class function TRTTIUtils.CreateObject(AQualifiedClassName: string): TObject;
var
  rttitype: TRttiType;
begin
  rttitype := ctx.FindType(AQualifiedClassName);
  if Assigned(rttitype) then
    Result := CreateObject(rttitype)
  else
    raise Exception.Create('Cannot find RTTI for ' + AQualifiedClassName +
      '. Hint: Is the specified classtype linked in the module?');
end;

class function TRTTIUtils.CreateObject(ARttiType: TRttiType): TObject;
var
  Method: TRttiMethod;
  metaClass: TClass;
begin
  { First solution, clear and slow }
  metaClass := nil;
  Method := nil;
  for Method in ARttiType.GetMethods do
    if Method.HasExtendedInfo and Method.IsConstructor then
      if Length(Method.GetParameters) = 0 then
      begin
        metaClass := ARttiType.AsInstance.MetaclassType;
        Break;
      end;
  if Assigned(metaClass) then
    Result := Method.Invoke(metaClass, []).AsObject
  else
    raise Exception.Create('Cannot find a propert constructor for ' + ARttiType.ToString);

  { Second solution, dirty and fast }
  // Result := TObject(ARttiType.GetMethod('Create')
  // .Invoke(ARttiType.AsInstance.MetaclassType, []).AsObject);
end;

class function TRTTIUtils.BuildClass(AQualifiedName: string; Params: array of TValue): TObject;
var
  T: TRttiType;
  V: TValue;
begin

  T := FindType(AQualifiedName);
  V := T.GetMethod('Create').Invoke(T.AsInstance.MetaclassType, Params);
  Result := V.AsObject;
end;

class function TRTTIUtils.Clone(Obj: TObject): TObject;
var
  _ARttiType: TRttiType;
  Field: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
  targetCollection: TObjectList<TObject>;
  sourceCollection: TObjectList<TObject>;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
begin
  Result := nil;
  if not Assigned(Obj) then
    Exit;

  _ARttiType := ctx.GetType(Obj.ClassType);
  cloned := CreateObject(_ARttiType);
  master := Obj;
  for Field in _ARttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then
      Field.SetValue(cloned, Field.GetValue(master))
    else
    begin
      Src := Field.GetValue(Obj).AsObject;
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
      else if Src is TObjectList<TObject> then
      begin
        sourceCollection := TObjectList<TObject>(Src);
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetCollection := TObjectList<TObject>.Create;
          Field.SetValue(cloned, targetCollection);
        end
        else
          targetCollection := Field.GetValue(cloned).AsObject as TObjectList<TObject>;
        for I := 0 to sourceCollection.Count - 1 do
        begin
          targetCollection.Add(TRTTIUtils.Clone(sourceCollection[I]));
        end;
      end
      else
      begin
        sourceObject := Src;

        if Field.GetValue(cloned).IsEmpty then
        begin
          targetObject := TRTTIUtils.Clone(sourceObject);
          Field.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := Field.GetValue(cloned).AsObject;
          TRTTIUtils.CopyObject(sourceObject, targetObject);
        end;
        Field.SetValue(cloned, targetObject);
      end;
    end;

  end;
  Result := cloned;
end;

{ TListDuckTyping }

class function TRTTIUtils.HasAttribute<T>(aObj: TObject; out AAttribute: T): boolean;
begin
  Result := HasAttribute<T>(ctx.GetType(aObj.ClassType), AAttribute)
end;

end.
