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

unit MVCFramework.ValidationEngine;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.SyncObjs,
  MVCFramework.Commons,
  MVCFramework.Validation;

type
  /// <summary>
  /// Per-property metadata cached for a single validation pass: the
  /// TRttiProperty reference (stable because we keep a TRttiContext
  /// alive), the already-filtered validator attribute list, and the
  /// property name used as field-path segment. Computed once per class.
  /// </summary>
  TMVCValidationPropertyInfo = record
    RttiProp: TRttiProperty;
    Name: string;
    Validators: TArray<TMVCValidatorBase>;
  end;

  /// <summary>
  /// Cache entry for validation metadata per class.
  /// Populated once per class by BuildCacheEntry; read concurrently by
  /// InternalValidate / IsValidatableClass without RTTI scans in the
  /// hot path.
  /// </summary>
  TValidationCacheEntry = record
    /// <summary>Properties that have at least one validator attribute,
    /// with validators already filtered. Iterating this list is the only
    /// work InternalValidate needs for the property-level step.</summary>
    PropertiesToValidate: TArray<TMVCValidationPropertyInfo>;
    /// <summary>Class-typed properties that need runtime recursion
    /// (nested DTO, collection). Only the RTTI ref is stored; the
    /// nested object's actual class may differ, so collection detection
    /// stays dynamic inside InternalValidate.</summary>
    PropertiesToRecurse: TArray<TRttiProperty>;
    /// <summary>True if at least one property in the class (or in a
    /// nested class) has validators. Used to OPT-IN validation.</summary>
    HasValidators: Boolean;
    /// <summary>True if the class derives from TMVCValidatable (so its
    /// OnValidate hook is called).</summary>
    IsValidatable: Boolean;
  end;

  /// <summary>
  /// The validation engine that performs validation on objects.
  /// Supports recursive validation for nested objects and collections.
  /// </summary>
  TMVCValidationEngine = class
  private
    class var FValidationCache: TDictionary<TClass, TValidationCacheEntry>;
    /// <summary>
    /// Multiple-reader / single-writer sync: the cache is written once
    /// per class (first lookup) and read on every subsequent call, so
    /// serialising readers on a mutex would create contention on every
    /// validation. MREW lets N readers proceed concurrently and only
    /// the rare writer is exclusive.
    /// </summary>
    class var FCacheLock: TMultiReadExclusiveWriteSynchronizer;
    /// <summary>
    /// Persistent RTTI context that keeps the RTTI pool alive for the
    /// lifetime of the engine. TRttiProperty references stored inside
    /// the cache are stable only as long as the pool isn't torn down,
    /// which in practice happens only at process shutdown.
    /// </summary>
    class var FRttiContext: TRttiContext;
    class constructor Create;
    class destructor Destroy;

    /// <summary>
    /// Builds validation cache entry for a class.
    /// Checks for validation attributes and OnValidate method.
    /// </summary>
    class function BuildCacheEntry(AClass: TClass): TValidationCacheEntry;

    /// <summary>
    /// Thread-safe cache lookup with double-checked insertion. Returns
    /// the cached entry, building and inserting one if needed. Never
    /// holds the write lock across the (potentially expensive)
    /// BuildCacheEntry call.
    /// </summary>
    class function GetOrBuildCacheEntry(AClass: TClass): TValidationCacheEntry;

    /// <summary>
    /// Internal recursive validation method.
    /// Validates an object and its nested objects/collections.
    /// </summary>
    /// <param name="AObject">Object to validate</param>
    /// <param name="AErrors">Dictionary to collect errors (created lazily on first error)</param>
    /// <param name="APath">Current path for nested objects (e.g., "Address.Street")</param>
    /// <param name="AVisited">List of already visited objects to prevent circular reference loops</param>
    class procedure InternalValidate(
      const AObject: TObject;
      var AErrors: TDictionary<string, string>;
      const APath: string;
      AVisited: TList<TObject>);
  public
    /// <summary>
    /// Checks if a class has validation attributes.
    /// Uses thread-safe cache for performance.
    /// Returns False for classes without validation attributes (OPT-IN behavior).
    /// </summary>
    class function IsValidatableClass(AClass: TClass): Boolean;

    /// <summary>
    /// Validates an object and returns errors.
    /// AUTOMATICALLY validates nested objects and collections.
    /// </summary>
    /// <param name="AObject">Object to validate</param>
    /// <param name="AErrors">Dictionary with errors (field path -> message).
    /// NIL if validation passes (no memory allocation for valid objects).
    /// Caller must free the dictionary if not nil.</param>
    /// <returns>True if valid (AErrors=nil), False if there are errors (AErrors<>nil)</returns>
    /// <remarks>
    /// Field paths use dot notation for nested objects: "Address.Street"
    /// Arrays/lists use index notation: "Items[0].Name"
    /// </remarks>
    class function Validate(const AObject: TObject;
      out AErrors: TDictionary<string, string>): Boolean;

    /// <summary>
    /// Validates an object and raises EMVCValidationException if invalid.
    /// AUTOMATICALLY validates nested objects and collections.
    /// </summary>
    class procedure ValidateAndRaise(const AObject: TObject);

    /// <summary>
    /// Clears the validatable class cache (useful for testing)
    /// </summary>
    class procedure ClearCache;
  end;

implementation

{ TMVCValidationEngine }

class constructor TMVCValidationEngine.Create;
begin
  FValidationCache := TDictionary<TClass, TValidationCacheEntry>.Create;
  FCacheLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FRttiContext := TRttiContext.Create;
end;

class destructor TMVCValidationEngine.Destroy;
begin
  FRttiContext.Free;
  FValidationCache.Free;
  FCacheLock.Free;
end;

class function TMVCValidationEngine.BuildCacheEntry(AClass: TClass): TValidationCacheEntry;
var
  LType: TRttiType;
  LProp: TRttiProperty;
  LAttr: TCustomAttribute;
  LMethod: TRttiMethod;
  LValidators: TList<TMVCValidatorBase>;
  LToValidate: TList<TMVCValidationPropertyInfo>;
  LToRecurse: TList<TRttiProperty>;
  LPropInfo: TMVCValidationPropertyInfo;
begin
  Result.PropertiesToValidate := nil;
  Result.PropertiesToRecurse := nil;
  Result.HasValidators := False;
  Result.IsValidatable := AClass.InheritsFrom(TMVCValidatable);

  // Uses the class-level FRttiContext: all TRttiProperty refs returned
  // here stay valid for the life of the engine.
  LType := FRttiContext.GetType(AClass);
  if LType = nil then
    Exit;

  LToValidate := TList<TMVCValidationPropertyInfo>.Create;
  LToRecurse := TList<TRttiProperty>.Create;
  LValidators := TList<TMVCValidatorBase>.Create;
  try
    for LProp in LType.GetProperties do
    begin
      if not LProp.IsReadable then
        Continue;

      // Collect validator attributes on this property - done once here
      // so InternalValidate never touches GetAttributes at runtime.
      LValidators.Clear;
      for LAttr in LProp.GetAttributes do
        if LAttr is TMVCValidatorBase then
          LValidators.Add(TMVCValidatorBase(LAttr));

      if LValidators.Count > 0 then
      begin
        LPropInfo.RttiProp := LProp;
        LPropInfo.Name := LProp.Name;
        LPropInfo.Validators := LValidators.ToArray;
        LToValidate.Add(LPropInfo);
        Result.HasValidators := True;
      end;

      // Class-typed properties are potential recursion targets (nested
      // DTO or collection). The nested object's actual class may differ
      // from the declared type, so the Count/Items detection stays at
      // runtime - we only pre-pick which properties are candidates.
      if (LProp.PropertyType <> nil) and
         (LProp.PropertyType.TypeKind = tkClass) then
        LToRecurse.Add(LProp);
    end;

    Result.PropertiesToValidate := LToValidate.ToArray;
    Result.PropertiesToRecurse := LToRecurse.ToArray;

    // Fail-fast: a class that declares its own OnValidate method but does NOT
    // inherit from TMVCValidatable will NOT be called (we use virtual dispatch
    // now). Raise an explicit exception instead of silently skipping it.
    if not Result.IsValidatable then
    begin
      LMethod := LType.GetMethod('OnValidate');
      if (LMethod <> nil) and (LMethod.Parent <> nil) and
         (LMethod.Parent.Handle = AClass.ClassInfo) then
        raise EMVCException.CreateFmt(
          'Class "%s" declares an OnValidate method but does not inherit from ' +
          'TMVCValidatable. Declare the class as "class(TMVCValidatable)" so ' +
          'the framework can invoke OnValidate via virtual dispatch.',
          [AClass.ClassName]);
    end;
  finally
    LValidators.Free;
    LToRecurse.Free;
    LToValidate.Free;
  end;
end;

class function TMVCValidationEngine.GetOrBuildCacheEntry(
  AClass: TClass): TValidationCacheEntry;
var
  LBuilt: TValidationCacheEntry;
begin
  // Read-only fast path: multiple threads can run this concurrently.
  FCacheLock.BeginRead;
  try
    if FValidationCache.TryGetValue(AClass, Result) then
      Exit;
  finally
    FCacheLock.EndRead;
  end;

  // Miss: build OUTSIDE the lock. RTTI introspection may allocate and
  // must not block other readers.
  LBuilt := BuildCacheEntry(AClass);

  // Double-check: another thread may have inserted meanwhile. Prefer
  // the winner entry to avoid duplicate storage.
  FCacheLock.BeginWrite;
  try
    if not FValidationCache.TryGetValue(AClass, Result) then
    begin
      FValidationCache.Add(AClass, LBuilt);
      Result := LBuilt;
    end;
  finally
    FCacheLock.EndWrite;
  end;
end;

class function TMVCValidationEngine.IsValidatableClass(AClass: TClass): Boolean;
var
  LCacheEntry: TValidationCacheEntry;
begin
  if AClass = nil then
    Exit(False);
  LCacheEntry := GetOrBuildCacheEntry(AClass);
  Result := LCacheEntry.HasValidators or LCacheEntry.IsValidatable;
end;

class procedure TMVCValidationEngine.InternalValidate(
  const AObject: TObject;
  var AErrors: TDictionary<string, string>;
  const APath: string;
  AVisited: TList<TObject>);

  procedure AddError(const AFieldPath, AMessage: string);
  begin
    // Lazy creation of errors dictionary
    if AErrors = nil then
      AErrors := TDictionary<string, string>.Create;
    // Add only first error per field
    if not AErrors.ContainsKey(AFieldPath) then
      AErrors.Add(AFieldPath, AMessage);
  end;

  function BuildFieldPath(const ASegment: string): string;
  begin
    if APath.IsEmpty then
      Result := ASegment
    else
      Result := APath + '.' + ASegment;
  end;

var
  LPropInfo: TMVCValidationPropertyInfo;
  LValue: TValue;
  LValidator: TMVCValidatorBase;
  LFieldPath: string;
  LProp: TRttiProperty;
  LNestedObj: TObject;
  LNestedType: TRttiType;
  LCountProp: TRttiProperty;
  LItemsProp: TRttiIndexedProperty;
  LListCount: Integer;
  LListItem: TObject;
  LItemValue: TValue;
  I: Integer;
  LCacheEntry: TValidationCacheEntry;
  LOnValidateErrors: TMVCValidationErrors;
  LPair: TPair<string, string>;
begin
  if AObject = nil then
    Exit;

  // Check for circular references
  if AVisited.Contains(AObject) then
    Exit;
  AVisited.Add(AObject);

  // Get cache entry (cheap: read-locked lookup on warm cache).
  LCacheEntry := GetOrBuildCacheEntry(AObject.ClassType);

  // 1. Apply attribute validators - flat loop over the already-filtered
  //    per-property list built once by BuildCacheEntry. No RTTI scan,
  //    no GetAttributes call in the hot path.
  for LPropInfo in LCacheEntry.PropertiesToValidate do
  begin
    LValue := LPropInfo.RttiProp.GetValue(AObject);
    LFieldPath := BuildFieldPath(LPropInfo.Name);
    for LValidator in LPropInfo.Validators do
    begin
      if not LValidator.Validate(LValue, AObject) then
        AddError(LFieldPath, LValidator.GetErrorMessage);
    end;
  end;

  // 2. Recurse into nested objects / collections. Only the RTTI refs of
  //    class-typed properties are pre-cached; the actual nested type may
  //    differ from the declared one (inheritance), so collection
  //    detection stays dynamic.
  for LProp in LCacheEntry.PropertiesToRecurse do
  begin
    LValue := LProp.GetValue(AObject);
    if (LValue.Kind <> tkClass) or LValue.IsEmpty then
      Continue;

    LNestedObj := LValue.AsObject;
    if LNestedObj = nil then
      Continue;

    LFieldPath := BuildFieldPath(LProp.Name);

    // Collection detection via Count + Items indexed property. Uses the
    // shared FRttiContext pool so the lookup is cached across calls.
    LNestedType := FRttiContext.GetType(LNestedObj.ClassType);
    if LNestedType = nil then
      Continue;

    LCountProp := LNestedType.GetProperty('Count');
    LItemsProp := LNestedType.GetIndexedProperty('Items');

    if Assigned(LCountProp) and Assigned(LItemsProp) then
    begin
      LListCount := LCountProp.GetValue(LNestedObj).AsInteger;
      for I := 0 to LListCount - 1 do
      begin
        LItemValue := LItemsProp.GetValue(LNestedObj, [I]);
        if (LItemValue.Kind = tkClass) and not LItemValue.IsEmpty then
        begin
          LListItem := LItemValue.AsObject;
          if LListItem <> nil then
            InternalValidate(LListItem, AErrors,
              Format('%s[%d]', [LFieldPath, I]), AVisited);
        end;
      end;
    end
    else
    begin
      InternalValidate(LNestedObj, AErrors, LFieldPath, AVisited);
    end;
  end;

  // 3. Call OnValidate method via virtual dispatch. Only for the root
  //    object (empty path) to avoid duplicate calls on nested objects.
  if APath.IsEmpty and LCacheEntry.IsValidatable then
  begin
    FillChar(LOnValidateErrors, SizeOf(LOnValidateErrors), 0);
    try
      TMVCValidatable(AObject).OnValidate(@LOnValidateErrors);
      if LOnValidateErrors.HasErrors then
      begin
        if AErrors = nil then
          AErrors := TDictionary<string, string>.Create;
        for LPair in LOnValidateErrors.Errors do
          if not AErrors.ContainsKey(LPair.Key) then
            AErrors.Add(LPair.Key, LPair.Value);
      end;
    finally
      LOnValidateErrors.Clear;
    end;
  end;
end;

class function TMVCValidationEngine.Validate(const AObject: TObject;
  out AErrors: TDictionary<string, string>): Boolean;
var
  LVisited: TList<TObject>;
begin
  // Lazy creation - dictionary is only created when first error is found
  AErrors := nil;

  if AObject = nil then
    Exit(True);

  LVisited := TList<TObject>.Create;
  try
    // Start recursive validation with empty path
    InternalValidate(AObject, AErrors, '', LVisited);
  finally
    LVisited.Free;
  end;

  // AErrors = nil means no errors (valid)
  // AErrors <> nil means errors found (invalid)
  Result := AErrors = nil;
end;

class procedure TMVCValidationEngine.ValidateAndRaise(const AObject: TObject);
var
  LErrors: TDictionary<string, string>;
begin
  if not Validate(AObject, LErrors) then
  begin
    // LErrors is guaranteed to be <> nil here (Validate returns False)
    try
      raise EMVCValidationException.Create(LErrors);
    finally
      LErrors.Free;
    end;
  end;
  // If validation passed, LErrors = nil, nothing to free
end;

class procedure TMVCValidationEngine.ClearCache;
begin
  FCacheLock.BeginWrite;
  try
    FValidationCache.Clear;
  finally
    FCacheLock.EndWrite;
  end;
end;

end.
