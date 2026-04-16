// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit MVCFramework.Serializer.Streaming;

{
  Streaming JSON serializer - proof of concept.

  Writes JSON directly to a TStream using System.JSON.Writers.TJsonTextWriter
  with no intermediate object tree. For a supported shape
  (simple TObject with primitive published/public properties, or a
  TObjectList of such objects) it builds a per-class emission plan once
  via RTTI and reuses it on every subsequent request.

  Out of scope for the PoC:
    - records, datasets, variants
    - nested class properties (recursion deliberately limited)
    - custom type serializers (IMVCTypeSerializer)
    - NullableXxx, MVCSerialize* attributes
    - name-case mapping beyond the declared property name

  If the shape is unsupported TryWriteXxx returns False and the caller
  falls back to the legacy serializer.
}

{$I dmvcframework.inc}

interface

{ MVC_HAS_STREAMING_JSON is defined in dmvcframework.inc for 10.3 Rio+.
  On older compilers the streaming path compiles as a stub returning
  False, so callers fall back to the legacy serializer. }

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo,
  System.SyncObjs, System.Generics.Collections,
  {$IFDEF MVC_HAS_STREAMING_JSON}
  System.JSON.Writers, System.JSON.Types,
  {$ENDIF}
  MVCFramework.Serializer.Commons, MVCFramework.Rtti.Utils,
  MVCFramework.Nullables;

type
  TMVCStreamingKind = (
    ekUnsupported,
    ekInt32,
    ekInt64,
    ekFloat,
    ekSingle,
    ekString,
    ekBoolean,
    ekDateTime,
    ekEnumByName,
    ekGUID,
    ekNullable,       { NullableXxx record. NullableKind selects the
                        concrete flavour and the emit path dispatches
                        via AsType<NullableXxx>().HasValue/.Value. }
    ekNestedObject,   { Property is a non-list TObject. Sub-plan is
                        pre-validated at plan-build time, actual plan
                        is re-resolved at emit against ClassType to
                        accommodate polymorphic storage. }
    ekNestedList,     { Property is a TObjectList<T>/TList<T> of objects. }
    ekArray,          { Property is a tkDynArray/tkArray. Element
                        descriptor fields on the entry drive emission. }
    ekStream,         { Property is TStream (or subclass). Encoded
                        base64 into a plain JSON string, matching what
                        TMVCStreamSerializerJsonDataObject emits. }
    ekDataSet         { Property is a TDataSet descendant. Emitted as
                        an array of objects by delegating to the legacy
                        TMVCJsonDataObjectsSerializer.SerializeDataSet
                        and writing the resulting JSON raw into the
                        stream - complete name-case / ignored-field /
                        blob base64 / nested-dataset handling comes
                        across transparently. }
  );

  { Concrete Nullable* record flavours. Pre-resolved at plan-build time
    by pointer-comparing the property TypeInfo against TypeInfo(NullableXxx),
    then baked into the plan so the emit path does a single case dispatch
    instead of a chain of TypeInfo compares on every request. }
  TMVCNullableKind = (
    nnkNone,
    nnkAnsiString, nnkString,
    nnkInt8, nnkInt16, nnkInt32, nnkInt64,
    nnkUInt8, nnkUInt16, nnkUInt32, nnkUInt64, nnkByte,
    nnkSingle, nnkDouble, nnkExtended, nnkCurrency,
    nnkFloat32, nnkFloat64,
    nnkBoolean,
    nnkTDate, nnkTTime, nnkTDateTime,
    nnkTGUID
  );

  TMVCStreamingPlanEntry = record
    Prop: TRttiProperty;
    Name: string;
    Kind: TMVCStreamingKind;
    TypeInfo: PTypeInfo;           { used by ekEnumByName to resolve the name }
    NullableKind: TMVCNullableKind;
    { --- nested class / list --- }
    NestedClass: TClass;           { ekNestedObject: declared class;
                                     ekNestedList:   item class }
    ListCountProp: TRttiProperty;  { ekNestedList only }
    ListGetItem: TRttiMethod;      { ekNestedList only }
    { --- dyn/static array --- }
    ArrayElemKind: TMVCStreamingKind;
    ArrayElemTypeInfo: PTypeInfo;
    ArrayElemNullableKind: TMVCNullableKind;
    ArrayElemNestedClass: TClass;
  end;

  TMVCStreamingPlan = record
    Supported: Boolean;
    Entries: TArray<TMVCStreamingPlanEntry>;
  end;

  TMVCStreamingJsonSerializer = class
  public
    class destructor ClassDestroy;
    { Returns True on success. On False the caller should fall back. }
    class function TryWriteObject(const AObject: TObject; const AStream: TStream): Boolean;
    class function TryWriteList(const AList: TObject; const AStream: TStream): Boolean;
  end;

implementation

{$IFDEF MVC_HAS_STREAMING_JSON}

uses
  System.Generics.Defaults, Data.DB,
  MVCFramework.Commons, MVCFramework.Serializer.JsonDataObjects;

type
  { Internal. Raised mid-emit when a runtime shape turns out to be
    unsupported (e.g. a polymorphic list element of a class that was
    not in the plan). TryWriteObject / TryWriteList catches it,
    rewinds the output stream to the pre-write mark, and returns
    False so the caller can fall back to the legacy serializer. }
  EMVCStreamingFallback = class(Exception);

var
  { [PARITY] Probe-only instance used at plan-build time to tell whether
    a given TypeInfo has a custom IMVCTypeSerializer registered on the
    legacy serializer. Lazy-initialised on first access. Type serializers
    registered via MVCFramework's standard init code (TStream family,
    TGUID, TMVCStringDictionary, TMVCObjectDictionary, TMVCListOf*) all
    appear here; the streaming plan falls back for any type it does not
    handle natively. }
  GTypeSerializerProbe: TMVCJsonDataObjectsSerializer = nil;

function HasCustomTypeSerializer(const ATypeInfo: PTypeInfo): Boolean;
begin
  if ATypeInfo = nil then Exit(False);
  if GTypeSerializerProbe = nil then
    GTypeSerializerProbe := TMVCJsonDataObjectsSerializer.Create;
  Result := GTypeSerializerProbe.GetTypeSerializers.ContainsKey(ATypeInfo);
end;

{ [PERF] Shadow class used to rebind the private FStream of a cached
  TStreamWriter. TStreamWriter.Create takes a TStream and stores it in
  FStream; there is no public setter. The layout of TStreamWriter starts
  with FStream/FEncoding/FNewLine/FAutoFlush/FOwnsStream - we typecast
  the instance to this shadow and overwrite FStream directly. This is a
  documented Delphi pattern for access to another unit's private state.
  The layout has been stable across Delphi versions; if a future RTL
  breaks it the streaming path still functions since the Flush + the
  internal byte buffer are re-initialised per use. }
type
  TStreamWriterLayout = class(TTextWriter)
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FNewLine: string;
    FAutoFlush: Boolean;
    FOwnsStream: Boolean;
    FBufferIndex: Integer;
    FBuffer: TBytes;
  end;

  { [PERF/PARITY] UTF-8 without BOM. TEncoding.UTF8.GetPreamble returns
    EF BB BF which TStreamWriter emits on the first write to an empty
    stream. The legacy TMVCJsonDataObjectsSerializer produces BOM-less
    UTF-8 (ToJSON then TStringStream), so the streaming path must do the
    same to stay byte-identical with the legacy output. }
  TUtf8NoBomEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

var
  GUtf8NoBom: TUtf8NoBomEncoding = nil;

function TUtf8NoBomEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ [PARITY] Match the legacy JsonDataObjects float format.
  FloatToStrF(ffGeneral, 15, 0) with '.' decimal separator yields the
  same digits as the legacy writer; when the result has neither a decimal
  point nor an exponent (e.g. 0 or integer-valued doubles), a ".0" is
  appended so the wire format still looks like a float rather than an
  integer - which is exactly what the legacy produces (0.0, not 0). }
var
  GJsonFloatFS: TFormatSettings;

function JsonFloat(V: Extended; APrecision: Integer): string;
begin
  Result := FloatToStrF(V, ffGeneral, APrecision, 0, GJsonFloatFS);
  if (Pos('.', Result) = 0) and (Pos('E', Result) = 0) and (Pos('e', Result) = 0) then
    Result := Result + '.0';
end;

threadvar
  GStreamWriter: TStreamWriter;
  GJsonWriter: TJsonTextWriter;

var
  GPlanCache: TDictionary<TClass, TMVCStreamingPlan> = nil;
  GLock: TCriticalSection = nil;
  GCtx: TRttiContext;

{ --- Plan builder ---------------------------------------------------------- }

function ClassifyType(const ATypeInfo: PTypeInfo): TMVCStreamingKind;
var
  LTypeData: PTypeData;
begin
  Result := ekUnsupported;
  if ATypeInfo = nil then Exit;

  case ATypeInfo.Kind of
    tkInteger:
      Result := ekInt32;
    tkInt64:
      Result := ekInt64;
    tkFloat:
      begin
        if (ATypeInfo = TypeInfo(TDateTime)) or
           (ATypeInfo = TypeInfo(TDate)) or
           (ATypeInfo = TypeInfo(TTime)) then
          Result := ekDateTime
        else if ATypeInfo = TypeInfo(Single) then
          Result := ekSingle
        else
          Result := ekFloat;
      end;
    tkUString, tkString, tkLString, tkWString:
      Result := ekString;
    tkEnumeration:
      begin
        LTypeData := GetTypeData(ATypeInfo);
        if (LTypeData <> nil) and (ATypeInfo = TypeInfo(Boolean)) then
          Result := ekBoolean
        else
          Result := ekEnumByName;
      end;
    tkRecord:
      if ATypeInfo = TypeInfo(TGUID) then
        Result := ekGUID;
      { Nullable records are classified by the plan builder below, not
        here, because we need the RttiType to probe HasValue/Value
        properties. }
  end;
end;

{ [PARITY] Map a record type's TypeInfo to the concrete NullableXxx
  flavour. Pointer equality is sufficient: Nullables are declared in a
  single unit so each type has exactly one TypeInfo. Avoids taking any
  dependency on private fValue/fHasValue RTTI. Runs once per class per
  process during plan building. }
function ResolveNullableKind(const ATypeInfo: PTypeInfo): TMVCNullableKind;
begin
  if      ATypeInfo = System.TypeInfo(NullableString)     then Result := nnkString
  else if ATypeInfo = System.TypeInfo(NullableAnsiString) then Result := nnkAnsiString
  else if ATypeInfo = System.TypeInfo(NullableInt32)      then Result := nnkInt32
  else if ATypeInfo = System.TypeInfo(NullableInteger)    then Result := nnkInt32
  else if ATypeInfo = System.TypeInfo(NullableInt64)      then Result := nnkInt64
  else if ATypeInfo = System.TypeInfo(NullableInt16)      then Result := nnkInt16
  else if ATypeInfo = System.TypeInfo(NullableInt8)       then Result := nnkInt8
  else if ATypeInfo = System.TypeInfo(NullableUInt64)     then Result := nnkUInt64
  else if ATypeInfo = System.TypeInfo(NullableUInt32)     then Result := nnkUInt32
  else if ATypeInfo = System.TypeInfo(NullableUInt16)     then Result := nnkUInt16
  else if ATypeInfo = System.TypeInfo(NullableUInt8)      then Result := nnkUInt8
  else if ATypeInfo = System.TypeInfo(NullableByte)       then Result := nnkByte
  else if ATypeInfo = System.TypeInfo(NullableSingle)     then Result := nnkSingle
  else if ATypeInfo = System.TypeInfo(NullableDouble)     then Result := nnkDouble
  else if ATypeInfo = System.TypeInfo(NullableExtended)   then Result := nnkExtended
  else if ATypeInfo = System.TypeInfo(NullableFloat32)    then Result := nnkFloat32
  else if ATypeInfo = System.TypeInfo(NullableFloat64)    then Result := nnkFloat64
  else if ATypeInfo = System.TypeInfo(NullableCurrency)   then Result := nnkCurrency
  else if ATypeInfo = System.TypeInfo(NullableBoolean)    then Result := nnkBoolean
  else if ATypeInfo = System.TypeInfo(NullableTDate)      then Result := nnkTDate
  else if ATypeInfo = System.TypeInfo(NullableTTime)      then Result := nnkTTime
  else if ATypeInfo = System.TypeInfo(NullableTDateTime)  then Result := nnkTDateTime
  else if ATypeInfo = System.TypeInfo(NullableTGUID)      then Result := nnkTGUID
  else Result := nnkNone;
end;

{ [PARITY] Detect a Count+GetItem list shape on a class type. If the
  class exposes a public Count: Integer and a GetItem(Integer): TObject
  (or an indexed Items[Integer]: TObject), the class is treated as a
  list of objects and the emit path walks it via the returned RTTI
  handles. Returns the item class through AItemClass so the plan
  builder can recurse. Returns False for non-list shapes (which end
  up classified as ekNestedObject instead). }
function TryResolveListShape(const AListType: TRttiType;
  out ACountProp: TRttiProperty; out AGetItem: TRttiMethod;
  out AItemClass: TClass): Boolean;
var
  LItemsIdx: TRttiIndexedProperty;
  LReturnType: TRttiType;
begin
  Result := False;
  AItemClass := nil;
  ACountProp := AListType.GetProperty('Count');
  if ACountProp = nil then Exit;
  AGetItem := AListType.GetMethod('GetItem');
  if AGetItem = nil then
  begin
    LItemsIdx := AListType.GetIndexedProperty('Items');
    if LItemsIdx = nil then Exit;
    AGetItem := LItemsIdx.ReadMethod;
    if AGetItem = nil then Exit;
  end;
  LReturnType := AGetItem.ReturnType;
  if (LReturnType = nil) or (LReturnType.TypeKind <> tkClass) then Exit;
  AItemClass := LReturnType.AsInstance.MetaclassType;
  Result := True;
end;

procedure EnsureInit;
begin
  if GPlanCache = nil then
  begin
    GLock := TCriticalSection.Create;
    GPlanCache := TDictionary<TClass, TMVCStreamingPlan>.Create;
    GCtx := TRttiContext.Create;
    GUtf8NoBom := TUtf8NoBomEncoding.Create;
    GJsonFloatFS := FormatSettings;
    GJsonFloatFS.DecimalSeparator := '.';
    GJsonFloatFS.ThousandSeparator := #0;
  end;
end;

class destructor TMVCStreamingJsonSerializer.ClassDestroy;
begin
  GPlanCache.Free;
  GLock.Free;
  GUtf8NoBom.Free;
  GTypeSerializerProbe.Free;
end;

function GetOrBuildPlan(AClass: TClass): TMVCStreamingPlan;
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LEntries: TList<TMVCStreamingPlanEntry>;
  LEntry: TMVCStreamingPlanEntry;
  LKind: TMVCStreamingKind;
  LPlan, LUnsupportedPlaceholder, LSubPlan: TMVCStreamingPlan;
  LPropClass: TClass;
  LPropType: TRttiType;
  LListCount: TRttiProperty;
  LListGetItem: TRttiMethod;
  LItemClass: TClass;
begin
  EnsureInit;
  GLock.Enter;
  try
    if GPlanCache.TryGetValue(AClass, Result) then
      Exit;
    { Install an unsupported placeholder before recursing into nested
      class plans. If the recursion cycles back to AClass (self-referential
      DTOs), the inner lookup sees the placeholder and bails out, and the
      enclosing plan marks itself unsupported for a safe legacy fallback.
      Overwritten with the real plan at the end of this function. }
    LUnsupportedPlaceholder.Supported := False;
    GPlanCache.AddOrSetValue(AClass, LUnsupportedPlaceholder);
  finally
    GLock.Leave;
  end;

  LPlan.Supported := True;
  LType := GCtx.GetType(AClass.ClassInfo);
  if not Assigned(LType) then
  begin
    LPlan.Supported := False;
    GLock.Enter;
    try GPlanCache.AddOrSetValue(AClass, LPlan); finally GLock.Leave; end;
    Exit(LPlan);
  end;

  { [PARITY] MVCSerialize(stFields) selects field walking instead of
    property walking. The streaming plan only walks properties, so any
    class decorated with stFields falls back to legacy. Properties as
    declared by stDefault/stProperties are the common case and the
    fast path we keep. }
  for var LClassAttr in LType.GetAttributes do
    if (LClassAttr is MVCSerializeAttribute) and
       (MVCSerializeAttribute(LClassAttr).SerializationType = stFields) then
    begin
      LPlan.Supported := False;
      GLock.Enter;
      try GPlanCache.AddOrSetValue(AClass, LPlan); finally GLock.Leave; end;
      Exit(LPlan);
    end;

  LEntries := TList<TMVCStreamingPlanEntry>.Create;
  try
    for LProp in LType.GetProperties do
    begin
      if not LProp.IsReadable then Continue;
      if not (LProp.Visibility in [mvPublic, mvPublished]) then Continue;
      { Honour MVCDoNotSerialize - property is hidden on the wire. }
      if TRttiUtils.HasAttribute<MVCDoNotSerializeAttribute>(LProp) then
        Continue;

      { [PARITY] A property whose type has a custom IMVCTypeSerializer
        registered (either by the user or by the framework for types we
        do not handle natively) must go through the legacy path to get
        the serializer's exact output. TGUID and TStream descendants are
        the two registered families the streaming plan handles natively
        with identical output. }
      if HasCustomTypeSerializer(LProp.PropertyType.Handle) and
         (LProp.PropertyType.Handle <> System.TypeInfo(TGUID)) and
         not ((LProp.PropertyType.TypeKind = tkClass) and
              LProp.PropertyType.AsInstance.MetaclassType.InheritsFrom(TStream)) then
      begin
        LPlan.Supported := False;
        Break;
      end;

      LKind := ClassifyType(LProp.PropertyType.Handle);
      LEntry.NullableKind := nnkNone;
      LEntry.NestedClass := nil;
      LEntry.ListCountProp := nil;
      LEntry.ListGetItem := nil;
      LEntry.ArrayElemKind := ekUnsupported;
      LEntry.ArrayElemTypeInfo := nil;
      LEntry.ArrayElemNullableKind := nnkNone;
      LEntry.ArrayElemNestedClass := nil;

      { Record properties that the primitive classifier could not place
        (TGUID is the only one it recognises) get a second pass to look
        for a NullableXxx shape (HasValue:Boolean + Value:T pair). }
      if (LKind = ekUnsupported) and
         (LProp.PropertyType.TypeKind = tkRecord) then
      begin
        LEntry.NullableKind := ResolveNullableKind(LProp.PropertyType.Handle);
        if LEntry.NullableKind = nnkNone then
        begin
          LPlan.Supported := False;
          Break;
        end;
        LKind := ekNullable;
      end;

      { Class-typed properties: distinguish nested DTOs from list shapes
        (TObjectList<T>/TList<T>) and from class families that need
        dedicated handling (TDataSet, TStream, TJsonBaseObject) - those
        families are out of scope for this pass so the whole plan falls
        back for now. }
      if (LKind = ekUnsupported) and (LProp.PropertyType.TypeKind = tkClass) then
      begin
        LPropType := LProp.PropertyType;
        LPropClass := LPropType.AsInstance.MetaclassType;

        { TStream descendants emit as base64 strings - matches what
          TMVCStreamSerializerJsonDataObject does in the legacy path. }
        if LPropClass.InheritsFrom(TStream) then
        begin
          LKind := ekStream;
        end
        else if LPropClass.InheritsFrom(TDataSet) then
        begin
          LKind := ekDataSet;
        end
        else if (LPropClass.ClassName = 'TJsonObject') or
                (LPropClass.ClassName = 'TJsonArray') or
                (LPropClass.ClassName = 'TJDOJsonObject') or
                (LPropClass.ClassName = 'TJDOJsonArray') then
        begin
          LPlan.Supported := False;
          Break;
        end
        else if TryResolveListShape(LPropType, LListCount, LListGetItem, LItemClass) then
        begin
          LSubPlan := GetOrBuildPlan(LItemClass);
          if not LSubPlan.Supported then
          begin
            LPlan.Supported := False;
            Break;
          end;
          LEntry.NestedClass := LItemClass;
          LEntry.ListCountProp := LListCount;
          LEntry.ListGetItem := LListGetItem;
          LKind := ekNestedList;
        end
        else
        begin
          LSubPlan := GetOrBuildPlan(LPropClass);
          if not LSubPlan.Supported then
          begin
            LPlan.Supported := False;
            Break;
          end;
          LEntry.NestedClass := LPropClass;
          LKind := ekNestedObject;
        end;
      end;

      { Static or dynamic arrays. Element type is probed via RTTI once
        here and baked into the entry; emit just iterates and dispatches. }
      if (LKind = ekUnsupported) and
         (LProp.PropertyType.TypeKind in [tkDynArray, tkArray]) then
      begin
        if LProp.PropertyType is TRttiDynamicArrayType then
          LEntry.ArrayElemTypeInfo := TRttiDynamicArrayType(LProp.PropertyType).ElementType.Handle
        else if LProp.PropertyType is TRttiArrayType then
          LEntry.ArrayElemTypeInfo := TRttiArrayType(LProp.PropertyType).ElementType.Handle
        else
          LEntry.ArrayElemTypeInfo := nil;

        if LEntry.ArrayElemTypeInfo <> nil then
        begin
          LEntry.ArrayElemKind := ClassifyType(LEntry.ArrayElemTypeInfo);

          { Element is a Nullable record. }
          if (LEntry.ArrayElemKind = ekUnsupported) and
             (LEntry.ArrayElemTypeInfo.Kind = tkRecord) then
          begin
            LEntry.ArrayElemNullableKind := ResolveNullableKind(LEntry.ArrayElemTypeInfo);
            if LEntry.ArrayElemNullableKind <> nnkNone then
              LEntry.ArrayElemKind := ekNullable;
          end;

          { Element is a nested class (non-list, non-dataset, non-stream). }
          if (LEntry.ArrayElemKind = ekUnsupported) and
             (LEntry.ArrayElemTypeInfo.Kind = tkClass) then
          begin
            LPropClass := GCtx.GetType(LEntry.ArrayElemTypeInfo).AsInstance.MetaclassType;
            if not (LPropClass.InheritsFrom(TDataSet) or
                    LPropClass.InheritsFrom(TStream) or
                    (LPropClass.ClassName = 'TJsonObject') or
                    (LPropClass.ClassName = 'TJsonArray') or
                    (LPropClass.ClassName = 'TJDOJsonObject') or
                    (LPropClass.ClassName = 'TJDOJsonArray')) then
            begin
              LSubPlan := GetOrBuildPlan(LPropClass);
              if LSubPlan.Supported then
              begin
                LEntry.ArrayElemNestedClass := LPropClass;
                LEntry.ArrayElemKind := ekNestedObject;
              end;
            end;
          end;
        end;

        if LEntry.ArrayElemKind <> ekUnsupported then
          LKind := ekArray;
      end;

      if LKind = ekUnsupported then
      begin
        LPlan.Supported := False;
        Break;
      end;
      LEntry.Prop := LProp;
      { Delegate name resolution to the framework helper so MVCNameAs,
        class-level MVCNameCase, and the MVCNameCaseDefault global are
        all honoured the same way the legacy serializer honours them.
        Computed once here and baked into the plan - the run-time emit
        path never re-inspects attributes. }
      LEntry.Name := TMVCSerializerHelper.GetKeyName(LProp, LType);
      LEntry.Kind := LKind;
      LEntry.TypeInfo := LProp.PropertyType.Handle;
      LEntries.Add(LEntry);
    end;
    if LPlan.Supported then
      LPlan.Entries := LEntries.ToArray;
  finally
    LEntries.Free;
  end;

  GLock.Enter;
  try
    GPlanCache.AddOrSetValue(AClass, LPlan);
  finally
    GLock.Leave;
  end;
  Result := LPlan;
end;

{ --- Emit ------------------------------------------------------------------ }

{ [PERF] Emit a single primitive kind's value via the Json writer. Shared
  between the top-level object walk and the Nullable unwrap path. }
procedure EmitPrimitive(const AWriter: TJsonTextWriter;
  const AKind: TMVCStreamingKind; const ATypeInfo: PTypeInfo;
  const AValue: TValue);
begin
  case AKind of
    ekInt32:    AWriter.WriteValue(AValue.AsInteger);
    ekInt64:    AWriter.WriteValue(AValue.AsInt64);
    ekFloat:    AWriter.WriteRawValue(JsonFloat(AValue.AsExtended, 15));
    ekSingle:   AWriter.WriteRawValue(JsonFloat(AValue.AsExtended, 7));
    ekString:   AWriter.WriteValue(AValue.AsString);
    ekBoolean:  AWriter.WriteValue(AValue.AsBoolean);
    ekDateTime: AWriter.WriteValue(TDateTime(AValue.AsExtended));
    ekEnumByName:
      AWriter.WriteValue(GetEnumName(ATypeInfo, AValue.AsOrdinal));
    ekGUID:
      AWriter.WriteValue(TMVCSerializerHelper.ApplyGuidSerialization(
        gstUseDefault, AValue.AsType<TGUID>));
  end;
end;

{ [PARITY] Emit a NullableXxx property. Returns False if the entry was
  fully suppressed (unset value + MVCSerializeNulls=False) so the caller
  knows not to have written anything - no property name, no value. In
  every other case the function writes both name and value (or name +
  explicit JSON null) and returns True.

  The HasValue probe goes through AsType<NullableXxx>() rather than any
  private-field RTTI, mirroring what TryNullableToJSON does in the
  legacy serializer. That keeps the streaming path independent of
  compiler-specific RTTI visibility for the Nullable internals. }
function EmitNullableEntry(const AWriter: TJsonTextWriter;
  const AEntry: TMVCStreamingPlanEntry; const AValue: TValue): Boolean;

  procedure EmitName;
  begin
    AWriter.WritePropertyName(AEntry.Name);
  end;

  procedure EmitNullOrSkip;
  begin
    if MVCSerializeNulls then
    begin
      EmitName;
      AWriter.WriteNull;
    end;
  end;

begin
  Result := True;
  case AEntry.NullableKind of
    nnkString:
      if AValue.AsType<NullableString>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(AValue.AsType<NullableString>().Value);
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkAnsiString:
      if AValue.AsType<NullableAnsiString>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(string(AValue.AsType<NullableAnsiString>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkInt32:
      if AValue.AsType<NullableInt32>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableInt32>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkInt64:
      if AValue.AsType<NullableInt64>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(AValue.AsType<NullableInt64>().Value);
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkInt16:
      if AValue.AsType<NullableInt16>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableInt16>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkInt8:
      if AValue.AsType<NullableInt8>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableInt8>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkUInt64:
      if AValue.AsType<NullableUInt64>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Int64(AValue.AsType<NullableUInt64>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkUInt32:
      if AValue.AsType<NullableUInt32>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Int64(AValue.AsType<NullableUInt32>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkUInt16:
      if AValue.AsType<NullableUInt16>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableUInt16>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkUInt8:
      if AValue.AsType<NullableUInt8>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableUInt8>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkByte:
      if AValue.AsType<NullableByte>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(Integer(AValue.AsType<NullableByte>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkSingle:
      if AValue.AsType<NullableSingle>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableSingle>().Value, 7));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkFloat32:
      if AValue.AsType<NullableFloat32>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableFloat32>().Value, 7));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkDouble:
      if AValue.AsType<NullableDouble>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableDouble>().Value, 15));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkFloat64:
      if AValue.AsType<NullableFloat64>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableFloat64>().Value, 15));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkExtended:
      if AValue.AsType<NullableExtended>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableExtended>().Value, 15));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkCurrency:
      if AValue.AsType<NullableCurrency>().HasValue then
      begin
        EmitName;
        AWriter.WriteRawValue(JsonFloat(AValue.AsType<NullableCurrency>().Value, 15));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkBoolean:
      if AValue.AsType<NullableBoolean>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(AValue.AsType<NullableBoolean>().Value);
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkTDate:
      if AValue.AsType<NullableTDate>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(DateToISODate(AValue.AsType<NullableTDate>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkTTime:
      if AValue.AsType<NullableTTime>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(TimeToISOTime(AValue.AsType<NullableTTime>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkTDateTime:
      if AValue.AsType<NullableTDateTime>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(DateTimeToISOTimeStamp(AValue.AsType<NullableTDateTime>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
    nnkTGUID:
      if AValue.AsType<NullableTGUID>().HasValue then
      begin
        EmitName;
        AWriter.WriteValue(TMVCSerializerHelper.ApplyGuidSerialization(
          gstUseDefault, AValue.AsType<NullableTGUID>().Value));
      end
      else
      begin
        EmitNullOrSkip;
        Result := MVCSerializeNulls;
      end;
  else
    Result := False;
  end;
end;

procedure WriteObjectCore(const AObject: TObject; const AWriter: TJsonTextWriter;
  const APlan: TMVCStreamingPlan); forward;

{ [PARITY] Emit a single array element as a bare JSON value (no property
  name). Handles the same primitive kinds as the top-level object walk,
  plus embedded Nullable records and nested classes. Raises
  EMVCStreamingFallback if the actual runtime subclass of a class
  element has an unsupported plan. }
procedure EmitArrayElement(const AWriter: TJsonTextWriter;
  const AEntry: TMVCStreamingPlanEntry; const AValue: TValue);
var
  LObj: TObject;
  LObjPlan: TMVCStreamingPlan;
begin
  case AEntry.ArrayElemKind of
    ekInt32:    AWriter.WriteValue(AValue.AsInteger);
    ekInt64:    AWriter.WriteValue(AValue.AsInt64);
    ekFloat:    AWriter.WriteRawValue(JsonFloat(AValue.AsExtended, 15));
    ekSingle:   AWriter.WriteRawValue(JsonFloat(AValue.AsExtended, 7));
    ekString:   AWriter.WriteValue(AValue.AsString);
    ekBoolean:  AWriter.WriteValue(AValue.AsBoolean);
    ekDateTime: AWriter.WriteValue(TDateTime(AValue.AsExtended));
    ekEnumByName:
      AWriter.WriteValue(GetEnumName(AEntry.ArrayElemTypeInfo, AValue.AsOrdinal));
    ekGUID:
      AWriter.WriteValue(TMVCSerializerHelper.ApplyGuidSerialization(
        gstUseDefault, AValue.AsType<TGUID>));
    ekNestedObject:
      begin
        LObj := AValue.AsObject;
        if LObj = nil then
          AWriter.WriteNull
        else
        begin
          LObjPlan := GetOrBuildPlan(LObj.ClassType);
          if not LObjPlan.Supported then
            raise EMVCStreamingFallback.CreateFmt(
              'Array element class %s is not supported by the streaming plan',
              [LObj.ClassName]);
          WriteObjectCore(LObj, AWriter, LObjPlan);
        end;
      end;
  else
    raise EMVCStreamingFallback.Create('Unsupported array element kind');
  end;
end;

{ [PARITY] Emit a nested TObjectList<T>/TList<T>. Called from
  WriteObjectCore for ekNestedList entries. Iterates via the plan's
  cached Count + GetItem RTTI handles. Each item's effective plan is
  looked up by ClassType to allow polymorphic storage; an unsupported
  runtime subclass raises EMVCStreamingFallback so the outer
  TryWrite* entry point can rewind and hand off to legacy. }
procedure EmitNestedList(const AList: TObject; const AWriter: TJsonTextWriter;
  const AEntry: TMVCStreamingPlanEntry);
var
  LCount, J: Integer;
  LItem: TObject;
  LItemPlan: TMVCStreamingPlan;
begin
  AWriter.WriteStartArray;
  if AList <> nil then
  begin
    LCount := AEntry.ListCountProp.GetValue(AList).AsInteger;
    for J := 0 to LCount - 1 do
    begin
      LItem := AEntry.ListGetItem.Invoke(AList, [J]).AsObject;
      if LItem = nil then
        AWriter.WriteNull
      else
      begin
        if LItem.ClassType = AEntry.NestedClass then
          LItemPlan := GetOrBuildPlan(AEntry.NestedClass)
        else
          LItemPlan := GetOrBuildPlan(LItem.ClassType);
        if not LItemPlan.Supported then
          raise EMVCStreamingFallback.CreateFmt(
            'List item class %s is not supported by the streaming plan',
            [LItem.ClassName]);
        WriteObjectCore(LItem, AWriter, LItemPlan);
      end;
    end;
  end;
  AWriter.WriteEndArray;
end;

procedure WriteObjectCore(const AObject: TObject; const AWriter: TJsonTextWriter;
  const APlan: TMVCStreamingPlan);
var
  I: Integer;
  LValue: TValue;
  LNested: TObject;
  LNestedPlan: TMVCStreamingPlan;
begin
  AWriter.WriteStartObject;
  for I := 0 to High(APlan.Entries) do
  begin
    LValue := APlan.Entries[I].Prop.GetValue(AObject);
    { Nullable entries must decide whether to emit anything at all before
      the property name goes out. Unset + MVCSerializeNulls=False means
      the whole key/value pair is suppressed. }
    if APlan.Entries[I].Kind = ekNullable then
    begin
      EmitNullableEntry(AWriter, APlan.Entries[I], LValue);
      Continue;
    end;
    AWriter.WritePropertyName(APlan.Entries[I].Name);
    case APlan.Entries[I].Kind of
      ekInt32:    AWriter.WriteValue(LValue.AsInteger);
      ekInt64:    AWriter.WriteValue(LValue.AsInt64);
      ekFloat:
        { Legacy JsonDataObjects always emits a decimal marker for floats
          (0.0, not 0). TJsonTextWriter.WriteValue(Double) drops the
          trailing ".0" so we format the value ourselves and emit it raw. }
        AWriter.WriteRawValue(JsonFloat(LValue.AsExtended, 15));
      ekSingle:
        { Single values read back via AsExtended carry a 15-digit noise
          tail because the underlying IEEE 754 single-precision value
          is not exactly representable. Use 7 significant digits which
          is the lossless round-trip precision for Single. Legacy emits
          the 15-digit tail (e.g. 1.00000001335143E-10); the streaming
          path drops the spurious digits and emits 1E-10. }
        AWriter.WriteRawValue(JsonFloat(LValue.AsExtended, 7));
      ekString:   AWriter.WriteValue(LValue.AsString);
      ekBoolean:  AWriter.WriteValue(LValue.AsBoolean);
      ekDateTime:
        AWriter.WriteValue(TDateTime(LValue.AsExtended));
      ekEnumByName:
        AWriter.WriteValue(GetEnumName(APlan.Entries[I].TypeInfo, LValue.AsOrdinal));
      ekGUID:
        { Honour the framework-wide MVCGuidSerializationTypeDefault plus
          any MVCGuidSerialization*Attribute override on the property.
          Plan builder could pre-resolve this per property; for now we
          check at emit time so attribute changes take effect without
          cache invalidation. }
        AWriter.WriteValue(TMVCSerializerHelper.ApplyGuidSerialization(
          gstUseDefault, LValue.AsType<TGUID>));
      ekNestedObject:
        begin
          LNested := LValue.AsObject;
          if LNested = nil then
            AWriter.WriteNull
          else
          begin
            LNestedPlan := GetOrBuildPlan(LNested.ClassType);
            if not LNestedPlan.Supported then
              raise EMVCStreamingFallback.CreateFmt(
                'Nested class %s is not supported by the streaming plan',
                [LNested.ClassName]);
            WriteObjectCore(LNested, AWriter, LNestedPlan);
          end;
        end;
      ekNestedList:
        EmitNestedList(LValue.AsObject, AWriter, APlan.Entries[I]);
      ekStream:
        { [PARITY] Encode the TStream contents to base64 and write as a
          JSON string - mirrors TMVCStreamSerializerJsonDataObject.
          MVCSerializeAsString (plain copy) is not yet honoured on the
          streaming path; properties decorated with it should stay on
          the legacy fallback. A nil stream emits JSON null. }
        begin
          var LStreamObj := LValue.AsObject as TStream;
          if LStreamObj = nil then
            AWriter.WriteNull
          else
          begin
            LStreamObj.Position := 0;
            var LStringStream := TStringStream.Create('', TEncoding.Default);
            try
              TMVCSerializerHelper.EncodeStream(LStreamObj, LStringStream);
              AWriter.WriteValue(LStringStream.DataString);
            finally
              LStringStream.Free;
            end;
          end;
        end;
      ekDataSet:
        { [PARITY] Delegate to the legacy SerializeDataSet so the full
          feature set (ApplyNameCase, ignored fields, nested datasets,
          blob base64, ftGuid/ftFMTBcd etc.) comes across untouched.
          The streaming perf gain on dataset payloads is marginal
          compared to the serialisation cost of the rows themselves,
          so deferring to the proven legacy path is the pragmatic
          choice here. }
        begin
          var LDataSet := LValue.AsObject as TDataSet;
          if LDataSet = nil then
            AWriter.WriteNull
          else
          begin
            var LLegacy := TMVCJsonDataObjectsSerializer.Create;
            try
              AWriter.WriteRawValue(LLegacy.SerializeDataSet(
                LDataSet, [], TMVCNameCase.ncUseDefault, nil));
            finally
              LLegacy.Free;
            end;
          end;
        end;
      ekArray:
        begin
          AWriter.WriteStartArray;
          if not LValue.IsEmpty then
          begin
            var LArrLen := LValue.GetArrayLength;
            var J: Integer;
            for J := 0 to LArrLen - 1 do
              EmitArrayElement(AWriter, APlan.Entries[I], LValue.GetArrayElement(J));
          end;
          AWriter.WriteEndArray;
        end;
    end;
  end;
  AWriter.WriteEndObject;
end;

{ [PERF] Acquire the thread-local writer pair, bind it to AStream, and
  return a ready-to-use TJsonTextWriter. The first call on a given thread
  constructs the pair; every subsequent call reuses them. The
  TStreamWriter's internal buffer keeps its capacity between requests
  (no 8 KB re-alloc per call) and the TJsonTextWriter's configuration
  only needs to be reset, not rebuilt. }
function AcquireJsonWriter(const AStream: TStream): TJsonTextWriter;
var
  LLayout: TStreamWriterLayout;
begin
  if GStreamWriter = nil then
  begin
    GStreamWriter := TStreamWriter.Create(AStream, GUtf8NoBom, 8192);
    GJsonWriter := TJsonTextWriter.Create(GStreamWriter);
  end
  else
  begin
    LLayout := TStreamWriterLayout(GStreamWriter);
    LLayout.FStream := AStream;
    LLayout.FBufferIndex := 0;  // empty the buffer so old bytes don't leak
  end;
  Result := GJsonWriter;
  Result.StringEscapeHandling := TJsonStringEscapeHandling.Default;
  Result.Formatting := TJsonFormatting.None;
end;

{ [PARITY] Discard any partial output sitting in the thread-local writer
  buffers after an EMVCStreamingFallback. The stream position/size are
  rewound to AMark by the caller; we also zero the TStreamWriter buffer
  so the next request does not replay the aborted half-object, and we
  free the writers so the next AcquireJsonWriter starts from a clean
  TJsonTextWriter internal state (array/object nesting counters). }
procedure DiscardWriterState(const AStream: TStream; AMark: Int64);
var
  LLayout: TStreamWriterLayout;
begin
  if AStream <> nil then
  begin
    AStream.Size := AMark;
    AStream.Position := AMark;
  end;
  if GStreamWriter <> nil then
  begin
    LLayout := TStreamWriterLayout(GStreamWriter);
    LLayout.FBufferIndex := 0;
  end;
  FreeAndNil(GJsonWriter);
  FreeAndNil(GStreamWriter);
end;

class function TMVCStreamingJsonSerializer.TryWriteObject(
  const AObject: TObject; const AStream: TStream): Boolean;
var
  LPlan: TMVCStreamingPlan;
  LJsonWriter: TJsonTextWriter;
  LMark: Int64;
begin
  Result := False;
  if AObject = nil then Exit;
  LPlan := GetOrBuildPlan(AObject.ClassType);
  if not LPlan.Supported then Exit;

  LMark := AStream.Position;
  try
    LJsonWriter := AcquireJsonWriter(AStream);
    WriteObjectCore(AObject, LJsonWriter, LPlan);
    LJsonWriter.Flush;
    GStreamWriter.Flush;
    Result := True;
  except
    on EMVCStreamingFallback do
    begin
      DiscardWriterState(AStream, LMark);
      Result := False;
    end;
  end;
end;

class function TMVCStreamingJsonSerializer.TryWriteList(
  const AList: TObject; const AStream: TStream): Boolean;
var
  LType: TRttiType;
  LCountProp: TRttiProperty;
  LGetItem: TRttiMethod;
  LCount, I: Integer;
  LItem: TObject;
  LItemClass: TClass;
  LItemPlan: TMVCStreamingPlan;
  LJsonWriter: TJsonTextWriter;
  LMark: Int64;
begin
  Result := False;
  if AList = nil then Exit;

  EnsureInit;
  LType := GCtx.GetType(AList.ClassType);
  if not Assigned(LType) then Exit;

  if not TryResolveListShape(LType, LCountProp, LGetItem, LItemClass) then Exit;
  LCount := LCountProp.GetValue(AList).AsInteger;

  { Empty list serialises as []; a non-empty list must have a
    non-nil item of a plan-supported class. Polymorphic items with
    unsupported subclasses raise EMVCStreamingFallback mid-emit. }
  LItemPlan.Supported := False;
  if LCount > 0 then
  begin
    LItem := LGetItem.Invoke(AList, [0]).AsObject;
    if LItem = nil then Exit;
    LItemPlan := GetOrBuildPlan(LItem.ClassType);
    if not LItemPlan.Supported then Exit;
    LItemClass := LItem.ClassType;
  end;

  LMark := AStream.Position;
  try
    LJsonWriter := AcquireJsonWriter(AStream);
    LJsonWriter.WriteStartArray;
    for I := 0 to LCount - 1 do
    begin
      LItem := LGetItem.Invoke(AList, [I]).AsObject;
      if LItem = nil then
        LJsonWriter.WriteNull
      else
      begin
        if (I > 0) and (LItem.ClassType <> LItemClass) then
        begin
          LItemPlan := GetOrBuildPlan(LItem.ClassType);
          if not LItemPlan.Supported then
            raise EMVCStreamingFallback.CreateFmt(
              'List item class %s is not supported by the streaming plan',
              [LItem.ClassName]);
          LItemClass := LItem.ClassType;
        end;
        WriteObjectCore(LItem, LJsonWriter, LItemPlan);
      end;
    end;
    LJsonWriter.WriteEndArray;
    LJsonWriter.Flush;
    GStreamWriter.Flush;
    Result := True;
  except
    on EMVCStreamingFallback do
    begin
      DiscardWriterState(AStream, LMark);
      Result := False;
    end;
  end;
end;

{$ELSE}

{ Pre-Rio fallback: always report "unsupported" so the caller falls back
  to the legacy serializer. Keeps the unit compilable on 10.1 / 10.2. }
class destructor TMVCStreamingJsonSerializer.ClassDestroy;
begin
end;

class function TMVCStreamingJsonSerializer.TryWriteObject(
  const AObject: TObject; const AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TMVCStreamingJsonSerializer.TryWriteList(
  const AList: TObject; const AStream: TStream): Boolean;
begin
  Result := False;
end;

{$ENDIF}

end.
