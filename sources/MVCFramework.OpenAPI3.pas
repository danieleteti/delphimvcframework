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
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************
//
// Native OpenAPI 3.1 emitter for DMVCFramework.
//
// Produces an OpenAPI 3.1 JSON document by aggregating contributions from
// pluggable IMVCOpenAPISource implementations. Two built-in sources:
//
//   * TMVCMinimalAPIOpenAPISource — inspects routes registered via
//     TMVCEngine.Root / TMVCEngine.Prefix(...) (the Minimal API surface).
//     Reads handler parameter types, route constraints, group data, and
//     per-endpoint typed configuration (WithName / WithSummary /
//     WithDescription / WithTags / WithDeprecated / Produces<T>).
//
//   * TMVCControllerOpenAPISource — inspects controllers registered on the
//     engine via RTTI. Reads MVCPath/MVCHTTPMethod/MVCSwag* attributes.
//     (Controllers source: see future MVCFramework.OpenAPI3.Controllers.)
//
// The output is a TJsonObject (JsonDataObjects) that conforms to OpenAPI
// 3.1.0. The shape is built incrementally — schemas are de-duplicated into
// components.schemas, paths are accumulated into the paths object.
//
// ***************************************************************************

unit MVCFramework.OpenAPI3;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.MinimalAPI;

const
  OPENAPI_VERSION = '3.1.0';

type
  // -------------------------------------------------------------------------
  // Document info — supplied by the application at middleware construction
  // time. Maps 1:1 to the OpenAPI `info` object (subset).
  // -------------------------------------------------------------------------

  TMVCOpenAPIInfo = record
    Title: string;
    Version: string;
    Description: string;
    ContactName: string;
    ContactEmail: string;
    ContactUrl: string;
    LicenseName: string;
    LicenseUrl: string;
  end;

  // -------------------------------------------------------------------------
  // Schema builder. Owns the components.schemas sub-object of the working
  // document and translates Delphi PTypeInfo references into JSON Schema
  // fragments (or $ref pointers for named types).
  //
  // The returned objects are caller-owned: each NewSchemaFor() call mints a
  // fresh TJsonObject the caller must either Add() to a parent or Free().
  // -------------------------------------------------------------------------

  TMVCOpenAPISchemaBuilder = class
  strict private
    fComponentsSchemas: TJsonObject;
    fRttiCtx: TRttiContext;
    function IsNullableTypeInfo(ATypeInfo: PTypeInfo): Boolean;
    function NullableInnerKind(const ANullableName: string;
      out APrimitive: string; out AFormat: string): Boolean;
    procedure FillPrimitiveSchema(const ASchema: TJsonObject;
      ATypeInfo: PTypeInfo);
    procedure FillNullableSchema(const ASchema: TJsonObject;
      ATypeInfo: PTypeInfo);
    function SchemaNameFor(ATypeInfo: PTypeInfo): string;
    procedure EnsureClassSchemaRegistered(AClass: TClass;
      const AName: string);
    procedure EnsureRecordSchemaRegistered(ATypeInfo: PTypeInfo;
      const AName: string);
    function NewRefObject(const ASchemaName: string): TJsonObject;
  public
    constructor Create(AComponentsSchemas: TJsonObject);
    destructor Destroy; override;
    // Mints a fresh schema fragment for ATypeInfo. For named types
    // (class, record), the schema is registered (lazy, deduped) in the
    // components.schemas section and a $ref object is returned. For
    // primitives, an inline schema is returned. Arrays (TArray<T>)
    // produce {"type":"array","items":<inner>}.
    function NewSchemaFor(ATypeInfo: PTypeInfo): TJsonObject;
    // Same as NewSchemaFor but takes a TRttiType — used when we already
    // have a resolved RTTI type (e.g. for record fields).
    function NewSchemaForRttiType(ARttiType: TRttiType): TJsonObject;
    property ComponentsSchemas: TJsonObject read fComponentsSchemas;
  end;

  // -------------------------------------------------------------------------
  // Source interface — a contributor that emits operations into the document.
  //
  // CollectOperations() receives the working `paths` object and the shared
  // schema builder. It appends per-route entries into paths (creating nested
  // verb objects as needed) and uses the builder for any type references.
  //
  // Sources are stateless from the caller's POV — invoking CollectOperations
  // multiple times against the same document MUST be idempotent (overwriting
  // existing entries is fine; duplicating them is a bug).
  // -------------------------------------------------------------------------

  IMVCOpenAPISource = interface
    ['{C5E2A8F7-3D4B-4E12-A3F0-71B6D8E94A12}']
    procedure CollectOperations(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
  end;

  // -------------------------------------------------------------------------
  // Minimal API source. Iterates the registry of a TMVCMinimalAPIMiddleware
  // and emits one operation per (route, verb) pair.
  // -------------------------------------------------------------------------

  TMVCMinimalAPIOpenAPISource = class(TInterfacedObject, IMVCOpenAPISource)
  strict private
    fRegistry: TMVCMinimalRegistry;
    function ConvertPathPattern(const APattern: string;
      out APathParams: TArray<string>;
      out AConstraints: TDictionary<string, string>): string;
    function IsBodyMethod(AVerb: TMVCHTTPMethodType): Boolean;
    function VerbToString(AVerb: TMVCHTTPMethodType): string;
    function IsFrameworkContextType(ATypeInfo: PTypeInfo): Boolean;
    function LooksLikeRecord(ATypeInfo: PTypeInfo): Boolean;
    procedure EmitParam(const AParams: TJsonArray;
      const AName, ALocation: string; ARequired: Boolean;
      const ASchema: TJsonObject; const AConstraint: string);
    procedure EmitOperation(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder;
      const ARoute: TMVCMinimalRoute);
  public
    constructor Create(ARegistry: TMVCMinimalRegistry);
    procedure CollectOperations(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
  end;

  // -------------------------------------------------------------------------
  // Classic controllers source. Iterates fEngine.Controllers via RTTI and
  // emits one operation per (controller, action, verb) triple.
  //
  // Reads (in order of precedence):
  //   * controller URLSegment (override on AddController), then class MVCPath
  //   * method MVCPath (suffix)
  //   * method MVCHTTPMethod / MVCHTTPMethods (verb set)
  //   * method parameter attributes: MVCFromQueryString / MVCFromHeader /
  //     MVCFromCookie / MVCFromBody; absence -> path/query inference
  //   * action return type as response schema (if not IMVCResponse)
  //   * MVCDoc / MVCSwagSummary for description / tags
  //
  // Designed to be additive over MVCFramework.OpenAPI3 — does not replace
  // the existing TMVCSwaggerMiddleware (Swagger 2 / SwagDoc), which remains
  // available for backward compatibility.
  // -------------------------------------------------------------------------

  TMVCControllerOpenAPISource = class(TInterfacedObject, IMVCOpenAPISource)
  strict private
    fEngine: TMVCEngine;
    fRttiCtx: TRttiContext;
    function ConvertPathPattern(const APattern: string;
      out APathParams: TArray<string>): string;
    function VerbToString(AVerb: TMVCHTTPMethodType): string;
    function ReadClassPath(AClass: TClass; const AURLSegment: string): string;
    procedure EmitControllerOperation(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder;
      AControllerClass: TClass;
      const AControllerPath: string;
      AMethod: TRttiMethod;
      AVerb: TMVCHTTPMethodType;
      const AMethodPath: string);
  public
    constructor Create(AEngine: TMVCEngine);
    destructor Destroy; override;
    procedure CollectOperations(const APathsObject: TJsonObject;
      const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
  end;

  // -------------------------------------------------------------------------
  // Document orchestrator. Builds the final OpenAPI 3.1 TJsonObject by
  // delegating to registered sources. Caller owns the returned object.
  // -------------------------------------------------------------------------

  TMVCOpenAPIDocumentBuilder = class
  strict private
    fSources: TList<IMVCOpenAPISource>;
    fInfo: TMVCOpenAPIInfo;
    fServers: TArray<string>;
    procedure WriteInfo(const ADoc: TJsonObject);
    procedure WriteServers(const ADoc: TJsonObject);
  public
    constructor Create(const AInfo: TMVCOpenAPIInfo);
    destructor Destroy; override;
    procedure AddSource(const ASource: IMVCOpenAPISource);
    procedure AddServer(const AURL: string);
    function Build: TJsonObject;
    property Info: TMVCOpenAPIInfo read fInfo;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.SyncObjs,
  MVCFramework.Router,
  MVCFramework.Swagger.Commons;

function IsNullableTypeInfo_External(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo <> nil)
        and (ATypeInfo^.Kind = tkRecord)
        and string(ATypeInfo^.Name).StartsWith('Nullable');
end;

// ============================================================================
// TMVCOpenAPISchemaBuilder
// ============================================================================

constructor TMVCOpenAPISchemaBuilder.Create(AComponentsSchemas: TJsonObject);
begin
  inherited Create;
  fComponentsSchemas := AComponentsSchemas;
  fRttiCtx := TRttiContext.Create;
end;

destructor TMVCOpenAPISchemaBuilder.Destroy;
begin
  fRttiCtx.Free;
  inherited;
end;

function TMVCOpenAPISchemaBuilder.IsNullableTypeInfo(
  ATypeInfo: PTypeInfo): Boolean;
var
  lName: string;
begin
  if ATypeInfo = nil then Exit(False);
  if ATypeInfo^.Kind <> tkRecord then Exit(False);
  lName := string(ATypeInfo^.Name);
  Result := lName.StartsWith('Nullable');
end;

function TMVCOpenAPISchemaBuilder.NullableInnerKind(
  const ANullableName: string; out APrimitive: string;
  out AFormat: string): Boolean;
var
  lInner: string;
begin
  // Strip the "Nullable" prefix. The mapping is exhaustive over the records
  // generated by `python -m invoke generate-nullables` in MVCFramework.Nullables.
  if not ANullableName.StartsWith('Nullable') then Exit(False);
  lInner := Copy(ANullableName, 9, MaxInt);
  AFormat := '';
  if (lInner = 'AnsiString') or (lInner = 'String') then
  begin
    APrimitive := 'string';
    Exit(True);
  end;
  if lInner = 'Currency' then begin APrimitive := 'number'; AFormat := 'currency'; Exit(True); end;
  if lInner = 'Boolean' then begin APrimitive := 'boolean'; Exit(True); end;
  if lInner = 'TDate' then begin APrimitive := 'string'; AFormat := 'date'; Exit(True); end;
  if lInner = 'TTime' then begin APrimitive := 'string'; AFormat := 'time'; Exit(True); end;
  if lInner = 'TDateTime' then begin APrimitive := 'string'; AFormat := 'date-time'; Exit(True); end;
  if lInner = 'TGUID' then begin APrimitive := 'string'; AFormat := 'uuid'; Exit(True); end;
  if (lInner = 'Single') or (lInner = 'Float32') then begin APrimitive := 'number'; AFormat := 'float'; Exit(True); end;
  if (lInner = 'Double') or (lInner = 'Float64') then begin APrimitive := 'number'; AFormat := 'double'; Exit(True); end;
  if lInner = 'Extended' then begin APrimitive := 'number'; Exit(True); end;
  if (lInner = 'Int64') or (lInner = 'UInt64') then begin APrimitive := 'integer'; AFormat := 'int64'; Exit(True); end;
  if (lInner = 'Int32') or (lInner = 'Integer') or (lInner = 'UInt32') then
  begin APrimitive := 'integer'; AFormat := 'int32'; Exit(True); end;
  if (lInner = 'Int8') or (lInner = 'UInt8') or (lInner = 'Byte')
    or (lInner = 'Int16') or (lInner = 'UInt16') then
  begin APrimitive := 'integer'; Exit(True); end;
  if (lInner = 'NativeInt') or (lInner = 'NativeUInt') then
  begin APrimitive := 'integer'; AFormat := 'int64'; Exit(True); end;
  Result := False;
end;

procedure TMVCOpenAPISchemaBuilder.FillPrimitiveSchema(
  const ASchema: TJsonObject; ATypeInfo: PTypeInfo);
begin
  // Coarse mapping over the kinds the framework actually binds at the
  // route layer. Unknown kinds fall back to "string".
  if ATypeInfo = nil then
  begin
    ASchema.S['type'] := 'string';
    Exit;
  end;

  if ATypeInfo = TypeInfo(string) then
  begin
    ASchema.S['type'] := 'string';
    Exit;
  end;
  if ATypeInfo = TypeInfo(AnsiString) then
  begin
    ASchema.S['type'] := 'string';
    Exit;
  end;
  if ATypeInfo = TypeInfo(Boolean) then
  begin
    ASchema.S['type'] := 'boolean';
    Exit;
  end;
  if ATypeInfo = TypeInfo(Integer) then
  begin
    ASchema.S['type'] := 'integer';
    ASchema.S['format'] := 'int32';
    Exit;
  end;
  if ATypeInfo = TypeInfo(Int64) then
  begin
    ASchema.S['type'] := 'integer';
    ASchema.S['format'] := 'int64';
    Exit;
  end;
  if (ATypeInfo = TypeInfo(Double)) or (ATypeInfo = TypeInfo(Extended)) then
  begin
    ASchema.S['type'] := 'number';
    ASchema.S['format'] := 'double';
    Exit;
  end;
  if ATypeInfo = TypeInfo(Single) then
  begin
    ASchema.S['type'] := 'number';
    ASchema.S['format'] := 'float';
    Exit;
  end;
  if ATypeInfo = TypeInfo(Currency) then
  begin
    ASchema.S['type'] := 'number';
    Exit;
  end;
  if ATypeInfo = TypeInfo(TDate) then
  begin
    ASchema.S['type'] := 'string';
    ASchema.S['format'] := 'date';
    Exit;
  end;
  if ATypeInfo = TypeInfo(TTime) then
  begin
    ASchema.S['type'] := 'string';
    ASchema.S['format'] := 'time';
    Exit;
  end;
  if ATypeInfo = TypeInfo(TDateTime) then
  begin
    ASchema.S['type'] := 'string';
    ASchema.S['format'] := 'date-time';
    Exit;
  end;
  if ATypeInfo = TypeInfo(TGUID) then
  begin
    ASchema.S['type'] := 'string';
    ASchema.S['format'] := 'uuid';
    Exit;
  end;

  // Enum, char, etc.
  case ATypeInfo^.Kind of
    tkInteger, tkInt64:
      begin
        ASchema.S['type'] := 'integer';
      end;
    tkFloat:
      begin
        ASchema.S['type'] := 'number';
      end;
    tkChar, tkWChar, tkLString, tkUString, tkWString:
      begin
        ASchema.S['type'] := 'string';
      end;
    tkEnumeration:
      begin
        // OpenAPI enum: string with restricted values
        ASchema.S['type'] := 'string';
      end;
  else
    ASchema.S['type'] := 'string';
  end;
end;

procedure TMVCOpenAPISchemaBuilder.FillNullableSchema(
  const ASchema: TJsonObject; ATypeInfo: PTypeInfo);
var
  lPrim, lFormat: string;
begin
  if NullableInnerKind(string(ATypeInfo^.Name), lPrim, lFormat) then
  begin
    // OpenAPI 3.1 idiomatic nullable: type = [primitive, "null"]
    // We emit it as a plain "type" with a `nullable: true` companion so the
    // schema also reads correctly under tools still on 3.0 — this is
    // tolerated by 3.1.
    ASchema.S['type'] := lPrim;
    if lFormat <> '' then
      ASchema.S['format'] := lFormat;
    ASchema.B['nullable'] := True;
  end
  else
  begin
    // Unknown Nullable* — emit a permissive schema.
    ASchema.S['type'] := 'string';
    ASchema.B['nullable'] := True;
  end;
end;

function TMVCOpenAPISchemaBuilder.SchemaNameFor(ATypeInfo: PTypeInfo): string;
begin
  if ATypeInfo = nil then Exit('');
  Result := string(ATypeInfo^.Name);
  // Tidy synthetic generic names — keep readable shape but strip angle
  // brackets so they're URL-safe in $ref strings.
  Result := Result
    .Replace('<', '_', [rfReplaceAll])
    .Replace('>', '', [rfReplaceAll])
    .Replace(',', '_', [rfReplaceAll])
    .Replace(' ', '', [rfReplaceAll]);
end;

function TMVCOpenAPISchemaBuilder.NewRefObject(
  const ASchemaName: string): TJsonObject;
begin
  Result := TJsonObject.Create;
  try
    Result.S['$ref'] := '#/components/schemas/' + ASchemaName;
  except
    Result.Free;
    raise;
  end;
end;

procedure TMVCOpenAPISchemaBuilder.EnsureClassSchemaRegistered(
  AClass: TClass; const AName: string);
var
  lSchema, lProps: TJsonObject;
  lRttiType: TRttiType;
  lProp: TRttiProperty;
  lFieldSchema: TJsonObject;
begin
  if fComponentsSchemas.Contains(AName) then Exit;

  // Reserve the slot up-front to break cycles (a class that references
  // itself transitively would otherwise recurse forever).
  lSchema := TJsonObject.Create;
  fComponentsSchemas.O[AName] := lSchema;  // takes ownership
  lSchema.S['type'] := 'object';
  lProps := lSchema.O['properties'];

  lRttiType := fRttiCtx.GetType(AClass);
  for lProp in lRttiType.GetProperties do
  begin
    if not lProp.IsReadable then Continue;
    if lProp.Visibility <> mvPublished then
    begin
      // Also include public properties — matches the serializer's behavior
      // for non-RTTI-restricted classes. Skip private/protected.
      if lProp.Visibility <> mvPublic then Continue;
    end;
    lFieldSchema := NewSchemaForRttiType(lProp.PropertyType);
    lProps.O[lProp.Name] := lFieldSchema;  // takes ownership of the field schema
  end;
end;

procedure TMVCOpenAPISchemaBuilder.EnsureRecordSchemaRegistered(
  ATypeInfo: PTypeInfo; const AName: string);
var
  lSchema, lProps: TJsonObject;
  lRttiType: TRttiType;
  lField: TRttiField;
  lFieldSchema: TJsonObject;
  lFieldName: string;
begin
  if fComponentsSchemas.Contains(AName) then Exit;

  lSchema := TJsonObject.Create;
  fComponentsSchemas.O[AName] := lSchema;
  lSchema.S['type'] := 'object';
  lProps := lSchema.O['properties'];

  lRttiType := fRttiCtx.GetType(ATypeInfo);
  for lField in lRttiType.GetFields do
  begin
    lFieldName := lField.Name;
    // Strip Hungarian "f" prefix on common private-field convention so the
    // schema field names match what users see in JSON serialization.
    if (Length(lFieldName) > 1) and (lFieldName[1] = 'f')
       and CharInSet(lFieldName[2], ['A'..'Z']) then
      lFieldName := Copy(lFieldName, 2, MaxInt);
    lFieldSchema := NewSchemaForRttiType(lField.FieldType);
    lProps.O[lFieldName] := lFieldSchema;
  end;
end;

function TMVCOpenAPISchemaBuilder.NewSchemaFor(
  ATypeInfo: PTypeInfo): TJsonObject;
var
  lName: string;
begin
  Result := TJsonObject.Create;
  try
    if ATypeInfo = nil then
    begin
      Result.S['type'] := 'string';
      Exit;
    end;

    if IsNullableTypeInfo(ATypeInfo) then
    begin
      FillNullableSchema(Result, ATypeInfo);
      Exit;
    end;

    case ATypeInfo^.Kind of
      tkClass:
        begin
          lName := SchemaNameFor(ATypeInfo);
          EnsureClassSchemaRegistered(GetTypeData(ATypeInfo)^.ClassType, lName);
          Result.Free;
          Result := NewRefObject(lName);
        end;
      tkRecord:
        begin
          lName := SchemaNameFor(ATypeInfo);
          EnsureRecordSchemaRegistered(ATypeInfo, lName);
          Result.Free;
          Result := NewRefObject(lName);
        end;
      tkDynArray:
        begin
          Result.S['type'] := 'array';
          // Inner type via RTTI on the dynamic array
          Result.O['items'] := NewSchemaForRttiType(
            (fRttiCtx.GetType(ATypeInfo) as TRttiDynamicArrayType).ElementType);
        end;
      tkInterface:
        begin
          // Interfaces don't have schemas (DI services). Emit a permissive
          // marker — the route emitter will skip the param anyway.
          Result.S['type'] := 'object';
        end;
    else
      FillPrimitiveSchema(Result, ATypeInfo);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMVCOpenAPISchemaBuilder.NewSchemaForRttiType(
  ARttiType: TRttiType): TJsonObject;
begin
  if ARttiType = nil then
  begin
    Result := TJsonObject.Create;
    Result.S['type'] := 'string';
    Exit;
  end;
  Result := NewSchemaFor(ARttiType.Handle);
end;

// ============================================================================
// TMVCMinimalAPIOpenAPISource
// ============================================================================

constructor TMVCMinimalAPIOpenAPISource.Create(ARegistry: TMVCMinimalRegistry);
begin
  inherited Create;
  fRegistry := ARegistry;
end;

function TMVCMinimalAPIOpenAPISource.IsBodyMethod(
  AVerb: TMVCHTTPMethodType): Boolean;
begin
  Result := AVerb in [httpPOST, httpPUT, httpPATCH];
end;

function TMVCMinimalAPIOpenAPISource.VerbToString(
  AVerb: TMVCHTTPMethodType): string;
begin
  case AVerb of
    httpGET:     Result := 'get';
    httpPOST:    Result := 'post';
    httpPUT:     Result := 'put';
    httpDELETE:  Result := 'delete';
    httpPATCH:   Result := 'patch';
    httpHEAD:    Result := 'head';
    httpOPTIONS: Result := 'options';
    httpTRACE:   Result := 'trace';
  else
    Result := '';
  end;
end;

function TMVCMinimalAPIOpenAPISource.IsFrameworkContextType(
  ATypeInfo: PTypeInfo): Boolean;
var
  lName: string;
begin
  if ATypeInfo = nil then Exit(False);
  lName := string(ATypeInfo^.Name);
  // TWebContext, TMVCWebRequest, TMVCWebResponse, TMVCRenderer are
  // injected by the dispatcher — they MUST be hidden from the spec.
  Result := (lName = 'TWebContext')
         or (lName = 'TMVCWebRequest')
         or (lName = 'TMVCWebResponse')
         or (lName = 'TMVCRenderer')
         or (lName = 'TMVCMinimalRenderer');
end;

function TMVCMinimalAPIOpenAPISource.LooksLikeRecord(
  ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo <> nil) and (ATypeInfo^.Kind = tkRecord);
end;

function TMVCMinimalAPIOpenAPISource.ConvertPathPattern(const APattern: string;
  out APathParams: TArray<string>;
  out AConstraints: TDictionary<string, string>): string;
var
  lSegments: TArray<string>;
  lOut: TStringBuilder;
  I, lColon: Integer;
  lSeg, lInner, lParamName, lConstraint: string;
begin
  // Convert DMVC route syntax to OpenAPI:
  //   ($id)         -> {id}
  //   ($id:int)     -> {id} (constraint kept aside, reported under AConstraints)
  //
  // Static segments pass through.
  APathParams := nil;
  if AConstraints = nil then
    AConstraints := TDictionary<string, string>.Create;
  if APattern = '' then Exit('/');

  lOut := TStringBuilder.Create;
  try
    lSegments := APattern.Split(['/']);
    for I := 0 to High(lSegments) do
    begin
      if I > 0 then lOut.Append('/');
      lSeg := lSegments[I];
      if lSeg.StartsWith('($') and lSeg.EndsWith(')') then
      begin
        lInner := Copy(lSeg, 3, Length(lSeg) - 3);
        lColon := Pos(':', lInner);
        if lColon > 0 then
        begin
          lParamName := Copy(lInner, 1, lColon - 1);
          lConstraint := Copy(lInner, lColon + 1, MaxInt);
        end
        else
        begin
          lParamName := lInner;
          lConstraint := '';
        end;
        lOut.Append('{').Append(lParamName).Append('}');
        APathParams := APathParams + [lParamName];
        if lConstraint <> '' then
          AConstraints.AddOrSetValue(lParamName, lConstraint);
      end
      else
        lOut.Append(lSeg);
    end;
    Result := lOut.ToString;
  finally
    lOut.Free;
  end;
end;

procedure TMVCMinimalAPIOpenAPISource.EmitParam(const AParams: TJsonArray;
  const AName, ALocation: string; ARequired: Boolean;
  const ASchema: TJsonObject; const AConstraint: string);
var
  lParam: TJsonObject;
begin
  lParam := AParams.AddObject;
  lParam.S['name'] := AName;
  lParam.S['in'] := ALocation;
  lParam.B['required'] := ARequired;
  // ASchema is caller-owned; transfer ownership into the param object.
  lParam.O['schema'] := ASchema;
  if AConstraint <> '' then
  begin
    // Surface the route constraint as a JSON Schema format/type when it maps
    // cleanly. Otherwise drop it into description for human consumption.
    if (AConstraint = 'int') or (AConstraint = 'int64') then
    begin
      ASchema.S['type'] := 'integer';
      if AConstraint = 'int64' then ASchema.S['format'] := 'int64'
      else ASchema.S['format'] := 'int32';
    end
    else if AConstraint = 'float' then
    begin
      ASchema.S['type'] := 'number';
    end
    else if AConstraint = 'bool' then
    begin
      ASchema.S['type'] := 'boolean';
    end
    else if AConstraint = 'guid' then
    begin
      ASchema.S['type'] := 'string';
      ASchema.S['format'] := 'uuid';
    end
    else if AConstraint = 'date' then
    begin
      ASchema.S['type'] := 'string';
      ASchema.S['format'] := 'date';
    end;
  end;
end;

procedure TMVCMinimalAPIOpenAPISource.EmitOperation(
  const APathsObject: TJsonObject;
  const ASchemaBuilder: TMVCOpenAPISchemaBuilder;
  const ARoute: TMVCMinimalRoute);
var
  lOpenAPIPath, lVerb: string;
  lPathParams: TArray<string>;
  lConstraints: TDictionary<string, string>;
  lPathObj, lOperation, lRequestBody, lResponse, lContent, lMediaType, lResponses: TJsonObject;
  lParamsArr: TJsonArray;
  lUnmatchedPathParamIdx: Integer;
  lParamName, lConstraint, lTypeName: string;
  lTypeInfo: PTypeInfo;
  I: Integer;
  lSchema: TJsonObject;
  lTagsArr: TJsonArray;
  lTagsValue: TValue;
  lSummary, lDescription, lOperationId: string;
  lMetaValue: TValue;
  lRequestBodySet: Boolean;
  lRttiCtx: TRttiContext;
  lRttiType: TRttiType;
  lField: TRttiField;
  lLocAttr: TCustomAttribute;
  lFromQS, lFromHeader, lFromCookie, lFromBody: Boolean;
  lFieldParamName: string;
  lTagsArrInArr: TArray<string>;
  lTagItem: string;
  lProducesType: TValue;
begin
  lConstraints := TDictionary<string, string>.Create;
  try
    lOpenAPIPath := ConvertPathPattern(ARoute.PathPattern, lPathParams, lConstraints);
    lVerb := VerbToString(ARoute.Verb);
    if lVerb = '' then Exit;

    lPathObj := APathsObject.O[lOpenAPIPath];
    lOperation := lPathObj.O[lVerb];

    // operationId — prefer explicit Name, else synthesize from verb+path
    lOperationId := ARoute.Name;
    if lOperationId = '' then
      lOperationId := lVerb + lOpenAPIPath
        .Replace('/', '_', [rfReplaceAll])
        .Replace('{', '', [rfReplaceAll])
        .Replace('}', '', [rfReplaceAll]);
    lOperation.S['operationId'] := lOperationId;

    // Metadata: summary / description / tags / responses-hint / deprecated
    if ARoute.Metadata.TryGetValue('summary', lMetaValue) then
    begin
      lSummary := lMetaValue.AsString;
      if lSummary <> '' then
        lOperation.S['summary'] := lSummary;
    end;
    if ARoute.Metadata.TryGetValue('description', lMetaValue) then
    begin
      lDescription := lMetaValue.AsString;
      if lDescription <> '' then
        lOperation.S['description'] := lDescription;
    end;
    if ARoute.Metadata.TryGetValue('deprecated', lMetaValue) and
       not lMetaValue.IsEmpty and lMetaValue.AsBoolean then
      lOperation.B['deprecated'] := True;

    if ARoute.Metadata.TryGetValue('tags', lTagsValue) and not lTagsValue.IsEmpty then
    begin
      lTagsArr := lOperation.A['tags'];
      // Accept either a single string tag or an array of strings.
      if lTagsValue.IsType<string> then
        lTagsArr.Add(lTagsValue.AsString)
      else if lTagsValue.IsType<TArray<string>>() then
      begin
        lTagsArrInArr := lTagsValue.AsType<TArray<string>>;
        for lTagItem in lTagsArrInArr do
          lTagsArr.Add(lTagItem);
      end;
    end;

    lParamsArr := lOperation.A['parameters'];
    lUnmatchedPathParamIdx := 0;
    lRequestBodySet := False;

    // Walk handler parameter types
    for I := 0 to High(ARoute.ParamTypes) do
    begin
      lTypeInfo := ARoute.ParamTypes[I];
      if lTypeInfo = nil then Continue;

      // Hide framework context types
      if (lTypeInfo^.Kind = tkClass)
         and IsFrameworkContextType(lTypeInfo) then
        Continue;

      // Hide DI interfaces and DI-resolved classes that are also the
      // group data type — they're injected, not user-supplied.
      if lTypeInfo^.Kind = tkInterface then Continue;

      if (lTypeInfo^.Kind = tkClass)
         and (ARoute.GroupDataTypeInfo = lTypeInfo) then
        Continue;

      // Record parameter → hybrid binding, read [MVCFromXxx] attributes.
      if LooksLikeRecord(lTypeInfo) and not IsNullableTypeInfo_External(lTypeInfo) then
      begin
        lRttiCtx := TRttiContext.Create;
        try
          lRttiType := lRttiCtx.GetType(lTypeInfo);
          for lField in lRttiType.GetFields do
          begin
            // Form-file fields are multipart body inputs, not query/header/cookie
            // parameters — skip them here (they would produce an invalid schema).
            if (lField.FieldType.Handle = TypeInfo(TMVCFormFile))
               or SameText(lField.FieldType.QualifiedName,
                    'System.TArray<MVCFramework.MinimalAPI.TMVCFormFile>') then
              Continue;

            lFromQS := False; lFromHeader := False;
            lFromCookie := False; lFromBody := False;
            lFieldParamName := lField.Name;
            for lLocAttr in lField.GetAttributes do
            begin
              lTypeName := string(lLocAttr.ClassName);
              if lTypeName = 'MVCFromQueryStringAttribute' then
              begin
                lFromQS := True;
                lFieldParamName := lRttiCtx.GetType(lLocAttr.ClassType)
                  .GetProperty('ParamName').GetValue(lLocAttr).AsString;
              end
              else if lTypeName = 'MVCFromHeaderAttribute' then
              begin
                lFromHeader := True;
                lFieldParamName := lRttiCtx.GetType(lLocAttr.ClassType)
                  .GetProperty('ParamName').GetValue(lLocAttr).AsString;
              end
              else if lTypeName = 'MVCFromCookieAttribute' then
              begin
                lFromCookie := True;
                lFieldParamName := lRttiCtx.GetType(lLocAttr.ClassType)
                  .GetProperty('ParamName').GetValue(lLocAttr).AsString;
              end
              else if lTypeName = 'MVCFromBodyAttribute' then
                lFromBody := True;
            end;

            if lFromBody then
            begin
              // Whole-record body via [MVCFromBody] is unusual but possible.
              // Skip: handled separately by the record's own schema.
              Continue;
            end;

            lSchema := ASchemaBuilder.NewSchemaForRttiType(lField.FieldType);
            if lFromQS or (not lFromHeader and not lFromCookie) then
              EmitParam(lParamsArr, lFieldParamName, 'query', False, lSchema, '')
            else if lFromHeader then
              EmitParam(lParamsArr, lFieldParamName, 'header', False, lSchema, '')
            else if lFromCookie then
              EmitParam(lParamsArr, lFieldParamName, 'cookie', False, lSchema, '');
          end;
        finally
          lRttiCtx.Free;
        end;
        Continue;
      end;

      // Class param on body-method → request body schema
      if IsBodyMethod(ARoute.Verb) and (lTypeInfo^.Kind = tkClass)
         and not lRequestBodySet then
      begin
        lRequestBody := lOperation.O['requestBody'];
        lRequestBody.B['required'] := True;
        lContent := lRequestBody.O['content'];
        lMediaType := lContent.O['application/json'];
        lMediaType.O['schema'] := ASchemaBuilder.NewSchemaFor(lTypeInfo);
        lRequestBodySet := True;
        Continue;
      end;

      // Primitive (or non-DTO class on non-body methods) → path or query
      if lUnmatchedPathParamIdx <= High(lPathParams) then
      begin
        lParamName := lPathParams[lUnmatchedPathParamIdx];
        lConstraint := '';
        lConstraints.TryGetValue(lParamName, lConstraint);
        lSchema := ASchemaBuilder.NewSchemaFor(lTypeInfo);
        EmitParam(lParamsArr, lParamName, 'path', True, lSchema, lConstraint);
        Inc(lUnmatchedPathParamIdx);
      end
      else
      begin
        // Spill into query. Use a synthetic name based on the type since the
        // handler signature param name is not available via RTTI on closures.
        lParamName := string(lTypeInfo^.Name);
        if lParamName.StartsWith('T') and (Length(lParamName) > 1) then
          lParamName := LowerCase(Copy(lParamName, 2, 1)) + Copy(lParamName, 3, MaxInt)
        else if Length(lParamName) > 0 then
          lParamName := LowerCase(Copy(lParamName, 1, 1)) + Copy(lParamName, 2, MaxInt);
        lSchema := ASchemaBuilder.NewSchemaFor(lTypeInfo);
        EmitParam(lParamsArr, lParamName, 'query', False, lSchema, '');
      end;
    end;

    // Path params declared in the URL but not consumed by handler args still
    // need to be present in the spec — emit them as required strings.
    while lUnmatchedPathParamIdx <= High(lPathParams) do
    begin
      lParamName := lPathParams[lUnmatchedPathParamIdx];
      lConstraint := '';
      lConstraints.TryGetValue(lParamName, lConstraint);
      lSchema := TJsonObject.Create;
      lSchema.S['type'] := 'string';
      EmitParam(lParamsArr, lParamName, 'path', True, lSchema, lConstraint);
      Inc(lUnmatchedPathParamIdx);
    end;

    // Responses — for now emit a single default 200 (success). Later,
    // metadata['produces.200'] = <class> can be honored to attach a schema.
    lResponses := lOperation.O['responses'];
    if ARoute.Metadata.TryGetValue('produces.200', lProducesType)
       and not lProducesType.IsEmpty then
    begin
      lResponse := lResponses.O['200'];
      lResponse.S['description'] := 'OK';
      lContent := lResponse.O['content'];
      lMediaType := lContent.O['application/json'];
      lMediaType.O['schema'] := ASchemaBuilder.NewSchemaFor(lProducesType.AsType<PTypeInfo>);
    end
    else
    begin
      lResponse := lResponses.O['200'];
      lResponse.S['description'] := 'OK';
    end;
  finally
    lConstraints.Free;
  end;
end;

procedure TMVCMinimalAPIOpenAPISource.CollectOperations(
  const APathsObject: TJsonObject;
  const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
var
  I: Integer;
  lRoute: TMVCMinimalRoute;
  lRoutes: TArray<TMVCMinimalRoute>;
begin
  if fRegistry = nil then Exit;
  // The registry has no public iterator; rely on a snapshot via dynamic cast.
  // Since TMVCMinimalRegistry.fRoutes is private we'd normally need a public
  // GetRoutes; the framework provides it (added below).
  lRoutes := fRegistry.AllRoutes;
  for I := 0 to High(lRoutes) do
  begin
    lRoute := lRoutes[I];
    if lRoute = nil then Continue;
    if not TMVCMinimalRouteHelper.IsVisibleInOpenAPI(lRoute) then Continue;
    EmitOperation(APathsObject, ASchemaBuilder, lRoute);
  end;
end;

// ============================================================================
// TMVCControllerOpenAPISource
// ============================================================================

constructor TMVCControllerOpenAPISource.Create(AEngine: TMVCEngine);
begin
  inherited Create;
  fEngine := AEngine;
  fRttiCtx := TRttiContext.Create;
end;

destructor TMVCControllerOpenAPISource.Destroy;
begin
  fRttiCtx.Free;
  inherited;
end;

function TMVCControllerOpenAPISource.VerbToString(
  AVerb: TMVCHTTPMethodType): string;
begin
  case AVerb of
    httpGET:     Result := 'get';
    httpPOST:    Result := 'post';
    httpPUT:     Result := 'put';
    httpDELETE:  Result := 'delete';
    httpPATCH:   Result := 'patch';
    httpHEAD:    Result := 'head';
    httpOPTIONS: Result := 'options';
    httpTRACE:   Result := 'trace';
  else
    Result := '';
  end;
end;

function TMVCControllerOpenAPISource.ConvertPathPattern(
  const APattern: string; out APathParams: TArray<string>): string;
var
  lSegments: TArray<string>;
  lOut: TStringBuilder;
  I: Integer;
  lSeg, lInner: string;
begin
  APathParams := nil;
  if APattern = '' then Exit('/');
  lOut := TStringBuilder.Create;
  try
    lSegments := APattern.Split(['/']);
    for I := 0 to High(lSegments) do
    begin
      if I > 0 then lOut.Append('/');
      lSeg := lSegments[I];
      if lSeg.StartsWith('($') and lSeg.EndsWith(')') then
      begin
        lInner := Copy(lSeg, 3, Length(lSeg) - 3);
        // Strip optional constraint suffix (`:int`, `:guid`, ...)
        if Pos(':', lInner) > 0 then
          lInner := Copy(lInner, 1, Pos(':', lInner) - 1);
        lOut.Append('{').Append(lInner).Append('}');
        APathParams := APathParams + [lInner];
      end
      else
        lOut.Append(lSeg);
    end;
    Result := lOut.ToString;
  finally
    lOut.Free;
  end;
end;

function TMVCControllerOpenAPISource.ReadClassPath(AClass: TClass;
  const AURLSegment: string): string;
var
  lAttr: TCustomAttribute;
  lObjType: TRttiType;
begin
  // Precedence: URLSegment (passed to AddController) overrides class attribute
  if AURLSegment <> '' then Exit(AURLSegment);
  Result := '';
  lObjType := fRttiCtx.GetType(AClass);
  for lAttr in lObjType.GetAttributes do
    if lAttr is MVCPathAttribute then
    begin
      Result := MVCPathAttribute(lAttr).Path;
      Break;
    end;
end;

procedure TMVCControllerOpenAPISource.EmitControllerOperation(
  const APathsObject: TJsonObject;
  const ASchemaBuilder: TMVCOpenAPISchemaBuilder;
  AControllerClass: TClass;
  const AControllerPath: string;
  AMethod: TRttiMethod;
  AVerb: TMVCHTTPMethodType;
  const AMethodPath: string);
var
  lFullPattern, lOpenAPIPath, lVerbStr: string;
  lPathParams: TArray<string>;
  lPathObj, lOperation, lParam, lRequestBody, lContent, lMediaType, lResponse, lResponses: TJsonObject;
  lParamsArr: TJsonArray;
  lParameters: TArray<TRttiParameter>;
  lParameter: TRttiParameter;
  lAttr: TCustomAttribute;
  lParamName, lParamLocation, lTypeName, lOperationId: string;
  lFromQS, lFromHeader, lFromCookie, lFromBody: Boolean;
  lSchema: TJsonObject;
  lAttrType: TRttiType;
  lAttrProp: TRttiProperty;
  lParamNameOverride: string;
  lConsumedPathParams: TArray<string>;
  lParamTypeInfo: PTypeInfo;
  lRequestBodySet: Boolean;
  lReturnType: TRttiType;
  lDocAttr: TCustomAttribute;
  lOperationIsPath: Boolean;
  lTrimmedFullPattern: string;
begin
  // Compose the full pattern: controller path + method path.
  lFullPattern := AControllerPath;
  if AMethodPath <> '' then
  begin
    if not AMethodPath.StartsWith('/') then
      lFullPattern := lFullPattern + '/' + AMethodPath
    else
      lFullPattern := lFullPattern + AMethodPath;
  end;
  // Tidy double-slashes / trailing slashes
  while lFullPattern.Contains('//') do
    lFullPattern := lFullPattern.Replace('//', '/', []);
  // Ensure leading slash, drop a trailing slash unless it's the root.
  if not lFullPattern.StartsWith('/') then
    lFullPattern := '/' + lFullPattern;
  lTrimmedFullPattern := lFullPattern;
  if (Length(lTrimmedFullPattern) > 1)
     and lTrimmedFullPattern.EndsWith('/') then
    lTrimmedFullPattern := Copy(lTrimmedFullPattern, 1,
      Length(lTrimmedFullPattern) - 1);

  lOpenAPIPath := ConvertPathPattern(lTrimmedFullPattern, lPathParams);
  lVerbStr := VerbToString(AVerb);
  if lVerbStr = '' then Exit;

  lPathObj := APathsObject.O[lOpenAPIPath];
  lOperation := lPathObj.O[lVerbStr];

  lOperationId := AControllerClass.ClassName + '_' + AMethod.Name + '_' + lVerbStr;
  lOperation.S['operationId'] := lOperationId;

  // description from MVCDoc
  for lDocAttr in AMethod.GetAttributes do
    if lDocAttr is MVCDocAttribute then
    begin
      lOperation.S['description'] := MVCDocAttribute(lDocAttr).Value;
      Break;
    end;

  lParamsArr := lOperation.A['parameters'];
  lRequestBodySet := False;
  SetLength(lConsumedPathParams, 0);

  lParameters := AMethod.GetParameters;
  for lParameter in lParameters do
  begin
    lFromQS := False; lFromHeader := False;
    lFromCookie := False; lFromBody := False;
    lParamNameOverride := '';

    // Skip framework context types
    lTypeName := lParameter.ParamType.Name;
    if (lTypeName = 'TWebContext') or (lTypeName = 'TMVCWebRequest')
       or (lTypeName = 'TMVCWebResponse') then
      Continue;

    for lAttr in lParameter.GetAttributes do
    begin
      lTypeName := lAttr.ClassName;
      if lTypeName = 'MVCFromBodyAttribute' then
        lFromBody := True
      else if lTypeName = 'MVCFromQueryStringAttribute' then
      begin
        lFromQS := True;
        lAttrType := fRttiCtx.GetType(lAttr.ClassType);
        lAttrProp := lAttrType.GetProperty('ParamName');
        if lAttrProp <> nil then
          lParamNameOverride := lAttrProp.GetValue(lAttr).AsString;
      end
      else if lTypeName = 'MVCFromHeaderAttribute' then
      begin
        lFromHeader := True;
        lAttrType := fRttiCtx.GetType(lAttr.ClassType);
        lAttrProp := lAttrType.GetProperty('ParamName');
        if lAttrProp <> nil then
          lParamNameOverride := lAttrProp.GetValue(lAttr).AsString;
      end
      else if lTypeName = 'MVCFromCookieAttribute' then
      begin
        lFromCookie := True;
        lAttrType := fRttiCtx.GetType(lAttr.ClassType);
        lAttrProp := lAttrType.GetProperty('ParamName');
        if lAttrProp <> nil then
          lParamNameOverride := lAttrProp.GetValue(lAttr).AsString;
      end;
    end;

    if lFromBody and not lRequestBodySet then
    begin
      lRequestBody := lOperation.O['requestBody'];
      lRequestBody.B['required'] := True;
      lContent := lRequestBody.O['content'];
      lMediaType := lContent.O['application/json'];
      lMediaType.O['schema'] := ASchemaBuilder.NewSchemaForRttiType(
        lParameter.ParamType);
      lRequestBodySet := True;
      Continue;
    end;

    lParamName := lParameter.Name;
    if lParamNameOverride <> '' then
      lParamName := lParamNameOverride;

    if lFromQS then
      lParamLocation := 'query'
    else if lFromHeader then
      lParamLocation := 'header'
    else if lFromCookie then
      lParamLocation := 'cookie'
    else
    begin
      // No explicit location → infer: if the name matches a path capture,
      // it's a path param; otherwise it's a query param.
      lParamLocation := 'query';
      if (Length(lPathParams) > 0)
         and (System.StrUtils.IndexStr(lParameter.Name, lPathParams) >= 0) then
      begin
        lParamLocation := 'path';
        lParamName := lParameter.Name;
        lConsumedPathParams := lConsumedPathParams + [lParameter.Name];
      end;
    end;

    if lParameter.ParamType <> nil then
      lParamTypeInfo := lParameter.ParamType.Handle
    else
      lParamTypeInfo := nil;
    lSchema := ASchemaBuilder.NewSchemaFor(lParamTypeInfo);

    lParam := lParamsArr.AddObject;
    lParam.S['name'] := lParamName;
    lParam.S['in'] := lParamLocation;
    lParam.B['required'] := (lParamLocation = 'path');
    lParam.O['schema'] := lSchema;
  end;

  // Path params declared in the URL but not consumed -> emit as required strings
  for lParamName in lPathParams do
  begin
    lOperationIsPath := False;
    for lTypeName in lConsumedPathParams do
      if SameText(lTypeName, lParamName) then
      begin
        lOperationIsPath := True;
        Break;
      end;
    if lOperationIsPath then Continue;
    lParam := lParamsArr.AddObject;
    lParam.S['name'] := lParamName;
    lParam.S['in'] := 'path';
    lParam.B['required'] := True;
    lParam.O['schema'] := TJsonObject.Create;
    lParam.O['schema'].S['type'] := 'string';
  end;

  // ------------------------------------------------------------------
  // Enrichment: MVCSwagParam attributes can declare body / query / header
  // parameters that aren't visible from the method signature alone — they
  // map naturally onto the older controller style where the body is read
  // from `Context.Request.Body` rather than a typed argument.
  // ------------------------------------------------------------------
  for lAttr in AMethod.GetAttributes do
  begin
    if lAttr is MVCSwagParamAttribute then
    begin
      with MVCSwagParamAttribute(lAttr) do
      begin
        if ParamLocation = plBody then
        begin
          if (not lRequestBodySet) and (JsonSchemaClass <> nil) then
          begin
            lRequestBody := lOperation.O['requestBody'];
            lRequestBody.B['required'] := Required;
            lContent := lRequestBody.O['content'];
            lMediaType := lContent.O['application/json'];
            lMediaType.O['schema'] := ASchemaBuilder.NewSchemaFor(
              JsonSchemaClass.ClassInfo);
            lRequestBodySet := True;
          end;
        end
        else if ParamLocation in [plQuery, plHeader, plPath, plFormData] then
        begin
          // Avoid duplicating params already emitted from the signature.
          lOperationIsPath := False;
          for lTypeName in lConsumedPathParams do
            if SameText(lTypeName, ParamName) then
            begin
              lOperationIsPath := True;
              Break;
            end;
          if lOperationIsPath then Continue;
          lParam := lParamsArr.AddObject;
          lParam.S['name'] := ParamName;
          case ParamLocation of
            plQuery: lParam.S['in'] := 'query';
            plHeader: lParam.S['in'] := 'header';
            plPath: lParam.S['in'] := 'path';
            plFormData: lParam.S['in'] := 'query'; // OpenAPI 3 drops formData
          end;
          lParam.B['required'] := Required or (ParamLocation = plPath);
          lSchema := TJsonObject.Create;
          case ParamType of
            ptString: lSchema.S['type'] := 'string';
            ptNumber: lSchema.S['type'] := 'number';
            ptInteger: lSchema.S['type'] := 'integer';
            ptBoolean: lSchema.S['type'] := 'boolean';
            ptArray: lSchema.S['type'] := 'array';
            ptFile: begin lSchema.S['type'] := 'string'; lSchema.S['format'] := 'binary'; end;
          else
            lSchema.S['type'] := 'string';
          end;
          if ParamDescription <> '' then
            lParam.S['description'] := ParamDescription;
          if DefaultValue <> '' then
            lSchema.S['default'] := DefaultValue;
          lParam.O['schema'] := lSchema;
        end;
      end;
    end;
  end;

  // Responses — read return type AND MVCSwagResponses attributes. The
  // return type alone is useful only when the action returns a concrete DTO
  // (rare; most actions return IMVCResponse). Attributes are the canonical
  // way to declare per-status-code schemas.
  lResponses := lOperation.O['responses'];
  lReturnType := AMethod.ReturnType;

  // Default 200 from return type
  if (lReturnType <> nil) and (lReturnType.Name <> 'IMVCResponse')
     and (lReturnType.TypeKind in [tkClass, tkRecord, tkDynArray]) then
  begin
    lResponse := lResponses.O['200'];
    lResponse.S['description'] := 'OK';
    lContent := lResponse.O['content'];
    lMediaType := lContent.O['application/json'];
    lMediaType.O['schema'] := ASchemaBuilder.NewSchemaFor(lReturnType.Handle);
  end;

  // Per-status-code from MVCSwagResponses attributes
  for lAttr in AMethod.GetAttributes do
  begin
    if lAttr is MVCSwagResponsesAttribute then
    begin
      with MVCSwagResponsesAttribute(lAttr) do
      begin
        lResponse := lResponses.O[IntToStr(StatusCode)];
        if Description <> '' then
          lResponse.S['description'] := Description
        else
          lResponse.S['description'] := IntToStr(StatusCode);
        if JsonSchemaClass <> nil then
        begin
          lContent := lResponse.O['content'];
          lMediaType := lContent.O['application/json'];
          if IsArray then
          begin
            lSchema := TJsonObject.Create;
            lSchema.S['type'] := 'array';
            lSchema.O['items'] := ASchemaBuilder.NewSchemaFor(
              JsonSchemaClass.ClassInfo);
            lMediaType.O['schema'] := lSchema;
          end
          else
            lMediaType.O['schema'] := ASchemaBuilder.NewSchemaFor(
              JsonSchemaClass.ClassInfo);
        end;
      end;
    end;
  end;

  // If no responses ended up emitted, drop in a default 200 so the spec
  // remains valid.
  if lResponses.Count = 0 then
  begin
    lResponse := lResponses.O['200'];
    lResponse.S['description'] := 'OK';
  end;
end;

procedure TMVCControllerOpenAPISource.CollectOperations(
  const APathsObject: TJsonObject;
  const ASchemaBuilder: TMVCOpenAPISchemaBuilder);
var
  lDelegate: TMVCControllerDelegate;
  lObjType: TRttiType;
  lMethod: TRttiMethod;
  lControllerPath, lActionPath: string;
  lActionVerbs: TMVCHTTPMethods;
  V: TMVCHTTPMethodType;
  lActionPathFound: Boolean;
  lAttr: TCustomAttribute;
  lIsIgnored: Boolean;
begin
  if fEngine = nil then Exit;
  for lDelegate in fEngine.Controllers do
  begin
    lControllerPath := ReadClassPath(lDelegate.Clazz, lDelegate.URLSegment);
    lObjType := fRttiCtx.GetType(lDelegate.Clazz);

    // Class-level IgnorePath → skip the whole controller.
    lIsIgnored := False;
    for lAttr in lObjType.GetAttributes do
      if lAttr is MVCSwagIgnorePathAttribute then
      begin
        lIsIgnored := True;
        Break;
      end;
    if lIsIgnored then Continue;

    for lMethod in lObjType.GetMethods do
    begin
      if not (lMethod.Visibility in [mvPublic, mvPublished]) then Continue;

      // Skip methods explicitly opted out of the spec.
      lIsIgnored := False;
      for lAttr in lMethod.GetAttributes do
        if lAttr is MVCSwagIgnorePathAttribute then
        begin
          lIsIgnored := True;
          Break;
        end;
      if lIsIgnored then Continue;

      lActionPathFound := False;
      lActionPath := '';
      for lAttr in lMethod.GetAttributes do
      begin
        if lAttr is MVCPathAttribute then
        begin
          lActionPath := MVCPathAttribute(lAttr).Path;
          lActionPathFound := True;
        end;
      end;
      {Same answer the router gives: the union of every [MVCHTTPMethod],
       or all the verbs when the action declares none}
      lActionVerbs := TMVCRouter.AllowedMethods(lMethod.GetAttributes);

      if not lActionPathFound then Continue;
      if lActionVerbs = [] then Continue;

      for V in lActionVerbs do
        EmitControllerOperation(APathsObject, ASchemaBuilder,
          lDelegate.Clazz, lControllerPath, lMethod, V, lActionPath);
    end;
  end;
end;

// ============================================================================
// TMVCOpenAPIDocumentBuilder
// ============================================================================

constructor TMVCOpenAPIDocumentBuilder.Create(const AInfo: TMVCOpenAPIInfo);
begin
  inherited Create;
  fInfo := AInfo;
  fSources := TList<IMVCOpenAPISource>.Create;
end;

destructor TMVCOpenAPIDocumentBuilder.Destroy;
begin
  fSources.Free;
  inherited;
end;

procedure TMVCOpenAPIDocumentBuilder.AddSource(
  const ASource: IMVCOpenAPISource);
begin
  if ASource <> nil then
    fSources.Add(ASource);
end;

procedure TMVCOpenAPIDocumentBuilder.AddServer(const AURL: string);
begin
  fServers := fServers + [AURL];
end;

procedure TMVCOpenAPIDocumentBuilder.WriteInfo(const ADoc: TJsonObject);
var
  lInfoObj, lContact, lLicense: TJsonObject;
begin
  lInfoObj := ADoc.O['info'];
  lInfoObj.S['title'] := fInfo.Title;
  lInfoObj.S['version'] := fInfo.Version;
  if fInfo.Description <> '' then
    lInfoObj.S['description'] := fInfo.Description;
  if (fInfo.ContactName <> '') or (fInfo.ContactEmail <> '') or (fInfo.ContactUrl <> '') then
  begin
    lContact := lInfoObj.O['contact'];
    if fInfo.ContactName <> '' then lContact.S['name'] := fInfo.ContactName;
    if fInfo.ContactEmail <> '' then lContact.S['email'] := fInfo.ContactEmail;
    if fInfo.ContactUrl <> '' then lContact.S['url'] := fInfo.ContactUrl;
  end;
  if (fInfo.LicenseName <> '') or (fInfo.LicenseUrl <> '') then
  begin
    lLicense := lInfoObj.O['license'];
    if fInfo.LicenseName <> '' then lLicense.S['name'] := fInfo.LicenseName;
    if fInfo.LicenseUrl <> '' then lLicense.S['url'] := fInfo.LicenseUrl;
  end;
end;

procedure TMVCOpenAPIDocumentBuilder.WriteServers(const ADoc: TJsonObject);
var
  lServersArr: TJsonArray;
  lServerObj: TJsonObject;
  lURL: string;
begin
  if Length(fServers) = 0 then Exit;
  lServersArr := ADoc.A['servers'];
  for lURL in fServers do
  begin
    lServerObj := lServersArr.AddObject;
    lServerObj.S['url'] := lURL;
  end;
end;

function TMVCOpenAPIDocumentBuilder.Build: TJsonObject;
var
  lDoc, lComponents, lSchemas: TJsonObject;
  lPaths: TJsonObject;
  lBuilder: TMVCOpenAPISchemaBuilder;
  lSource: IMVCOpenAPISource;
begin
  lDoc := TJsonObject.Create;
  try
    lDoc.S['openapi'] := OPENAPI_VERSION;
    WriteInfo(lDoc);
    WriteServers(lDoc);

    lComponents := lDoc.O['components'];
    lSchemas := lComponents.O['schemas'];
    lPaths := lDoc.O['paths'];

    lBuilder := TMVCOpenAPISchemaBuilder.Create(lSchemas);
    try
      for lSource in fSources do
        lSource.CollectOperations(lPaths, lBuilder);
    finally
      lBuilder.Free;
    end;

    // If no schemas were registered, drop the components object so the spec
    // stays tidy (OpenAPI tolerates either presence or absence).
    if lSchemas.Count = 0 then
    begin
      lComponents.Remove('schemas');
      if lComponents.Count = 0 then
        lDoc.Remove('components');
    end;

    Result := lDoc;
  except
    lDoc.Free;
    raise;
  end;
end;

end.
