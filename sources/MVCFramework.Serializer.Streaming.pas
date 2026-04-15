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

{ System.JSON.Writers.TJsonTextWriter was added in Delphi 10.3 Rio.
  On 10.1 / 10.2 the streaming path compiles as a stub that always
  returns False, so callers fall back to the legacy serializer. }
{$IF Defined(RIOORBETTER)}
  {$DEFINE MVC_HAS_STREAMING_JSON}
{$IFEND}

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo,
  System.SyncObjs, System.Generics.Collections,
  {$IFDEF MVC_HAS_STREAMING_JSON}
  System.JSON.Writers, System.JSON.Types,
  {$ENDIF}
  MVCFramework.Serializer.Commons, MVCFramework.Rtti.Utils;

type
  TMVCStreamingKind = (
    ekUnsupported,
    ekInt32,
    ekInt64,
    ekFloat,
    ekString,
    ekBoolean,
    ekDateTime
  );

  TMVCStreamingPlanEntry = record
    Prop: TRttiProperty;
    Name: string;
    Kind: TMVCStreamingKind;
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
  System.Generics.Defaults;

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
        if ATypeInfo = TypeInfo(TDateTime) then
          Result := ekDateTime
        else
          Result := ekFloat;
      end;
    tkUString, tkString, tkLString, tkWString:
      Result := ekString;
    tkEnumeration:
      begin
        LTypeData := GetTypeData(ATypeInfo);
        if (LTypeData <> nil) and (ATypeInfo = TypeInfo(Boolean)) then
          Result := ekBoolean;
      end;
  end;
end;

procedure EnsureInit;
begin
  if GPlanCache = nil then
  begin
    GLock := TCriticalSection.Create;
    GPlanCache := TDictionary<TClass, TMVCStreamingPlan>.Create;
    GCtx := TRttiContext.Create;
  end;
end;

class destructor TMVCStreamingJsonSerializer.ClassDestroy;
begin
  GPlanCache.Free;
  GLock.Free;
end;

function GetOrBuildPlan(AClass: TClass): TMVCStreamingPlan;
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LEntries: TList<TMVCStreamingPlanEntry>;
  LEntry: TMVCStreamingPlanEntry;
  LKind: TMVCStreamingKind;
  LPlan: TMVCStreamingPlan;
begin
  EnsureInit;
  GLock.Enter;
  try
    if GPlanCache.TryGetValue(AClass, Result) then
      Exit;
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

  LEntries := TList<TMVCStreamingPlanEntry>.Create;
  try
    for LProp in LType.GetProperties do
    begin
      if not LProp.IsReadable then Continue;
      if not (LProp.Visibility in [mvPublic, mvPublished]) then Continue;
      { Honour MVCDoNotSerialize - property is hidden on the wire. }
      if TRttiUtils.HasAttribute<MVCDoNotSerializeAttribute>(LProp) then
        Continue;
      LKind := ClassifyType(LProp.PropertyType.Handle);
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

procedure WriteObjectCore(const AObject: TObject; const AWriter: TJsonTextWriter;
  const APlan: TMVCStreamingPlan);
var
  I: Integer;
  LValue: TValue;
begin
  AWriter.WriteStartObject;
  for I := 0 to High(APlan.Entries) do
  begin
    AWriter.WritePropertyName(APlan.Entries[I].Name);
    LValue := APlan.Entries[I].Prop.GetValue(AObject);
    case APlan.Entries[I].Kind of
      ekInt32:    AWriter.WriteValue(LValue.AsInteger);
      ekInt64:    AWriter.WriteValue(LValue.AsInt64);
      ekFloat:    AWriter.WriteValue(LValue.AsExtended);
      ekString:   AWriter.WriteValue(LValue.AsString);
      ekBoolean:  AWriter.WriteValue(LValue.AsBoolean);
      ekDateTime: AWriter.WriteValue(TDateTime(LValue.AsExtended));
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
    GStreamWriter := TStreamWriter.Create(AStream, TEncoding.UTF8, 8192);
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

class function TMVCStreamingJsonSerializer.TryWriteObject(
  const AObject: TObject; const AStream: TStream): Boolean;
var
  LPlan: TMVCStreamingPlan;
  LJsonWriter: TJsonTextWriter;
begin
  Result := False;
  if AObject = nil then Exit;
  LPlan := GetOrBuildPlan(AObject.ClassType);
  if not LPlan.Supported then Exit;

  LJsonWriter := AcquireJsonWriter(AStream);
  WriteObjectCore(AObject, LJsonWriter, LPlan);
  LJsonWriter.Flush;
  GStreamWriter.Flush;
  Result := True;
end;

class function TMVCStreamingJsonSerializer.TryWriteList(
  const AList: TObject; const AStream: TStream): Boolean;
var
  LCtx: TRttiContext;
  LType: TRttiType;
  LCountProp: TRttiProperty;
  LItemsIdx: TRttiIndexedProperty;
  LGetItem: TRttiMethod;
  LCount, I: Integer;
  LItem: TObject;
  LItemClass: TClass;
  LItemPlan: TMVCStreamingPlan;
  LJsonWriter: TJsonTextWriter;
begin
  Result := False;
  if AList = nil then Exit;

  LCtx := TRttiContext.Create;
  try
    LType := LCtx.GetType(AList.ClassType);
    if not Assigned(LType) then Exit;

    LCountProp := LType.GetProperty('Count');
    if not Assigned(LCountProp) then Exit;

    { Prefer direct GetItem method (TList<T> exposes it); fall back to the
      Items indexed property if the method is hidden by visibility. }
    LGetItem := LType.GetMethod('GetItem');
    if not Assigned(LGetItem) then
    begin
      LItemsIdx := LType.GetIndexedProperty('Items');
      if not Assigned(LItemsIdx) then Exit;
      LGetItem := LItemsIdx.ReadMethod;
      if not Assigned(LGetItem) then Exit;
    end;

    LCount := LCountProp.GetValue(AList).AsInteger;

    { Probe item class via first element; empty list is still a valid JSON []. }
    LItemClass := nil;
    if LCount > 0 then
    begin
      LItem := LGetItem.Invoke(AList, [0]).AsObject;
      if LItem = nil then Exit;
      LItemClass := LItem.ClassType;
      LItemPlan := GetOrBuildPlan(LItemClass);
      if not LItemPlan.Supported then Exit;
    end;
  finally
    LCtx.Free;
  end;

  LJsonWriter := AcquireJsonWriter(AStream);
  LJsonWriter.WriteStartArray;
  LCtx := TRttiContext.Create;
  try
    LType := LCtx.GetType(AList.ClassType);
    LGetItem := LType.GetMethod('GetItem');
    if not Assigned(LGetItem) then
      LGetItem := LType.GetIndexedProperty('Items').ReadMethod;
    for I := 0 to LCount - 1 do
    begin
      LItem := LGetItem.Invoke(AList, [I]).AsObject;
      if LItem = nil then
        LJsonWriter.WriteNull
      else
        WriteObjectCore(LItem, LJsonWriter, LItemPlan);
    end;
  finally
    LCtx.Free;
  end;
  LJsonWriter.WriteEndArray;
  LJsonWriter.Flush;
  GStreamWriter.Flush;
  Result := True;
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
