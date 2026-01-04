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
  MVCFramework.Validation;

type
  /// <summary>
  /// Record for collecting validation errors with lazy dictionary creation.
  /// Used by OnValidate method. The dictionary is only allocated when Add is called.
  /// </summary>
  TMVCValidationErrors = record
  private
    FErrors: TDictionary<string, string>;
  public
    /// <summary>
    /// Adds a validation error. Creates the internal dictionary on first call.
    /// </summary>
    procedure Add(const AFieldPath, AMessage: string);
    /// <summary>
    /// Returns True if any errors were added.
    /// </summary>
    function HasErrors: Boolean;
    /// <summary>
    /// Returns the internal dictionary (nil if no errors). Caller takes ownership.
    /// </summary>
    function ExtractErrors: TDictionary<string, string>;
    /// <summary>
    /// Clears and frees the internal dictionary if allocated.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  /// Pointer to TMVCValidationErrors record.
  /// </summary>
  PMVCValidationErrors = ^TMVCValidationErrors;

  /// <summary>
  /// Method signature for custom object-level validation.
  /// The method receives a pointer to the validation errors record.
  /// </summary>
  TOnValidateProc = procedure(const AErrors: PMVCValidationErrors) of object;

  /// <summary>
  /// Cache entry for validation metadata per class.
  /// </summary>
  TValidationCacheEntry = record
    /// <summary>True if the class has any validation attributes on properties</summary>
    HasValidators: Boolean;
    /// <summary>True if the class has an OnValidate method</summary>
    HasOnValidate: Boolean;
    /// <summary>Code address of the OnValidate method (for direct call)</summary>
    OnValidateCodeAddress: Pointer;
  end;

  /// <summary>
  /// The validation engine that performs validation on objects.
  /// Supports recursive validation for nested objects and collections.
  /// </summary>
  TMVCValidationEngine = class
  private
    class var FValidationCache: TDictionary<TClass, TValidationCacheEntry>;
    class var FCacheLock: TCriticalSection;
    class constructor Create;
    class destructor Destroy;

    /// <summary>
    /// Builds validation cache entry for a class.
    /// Checks for validation attributes and OnValidate method.
    /// </summary>
    class function BuildCacheEntry(AClass: TClass): TValidationCacheEntry;

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
  FCacheLock := TCriticalSection.Create;
end;

class destructor TMVCValidationEngine.Destroy;
begin
  FValidationCache.Free;
  FCacheLock.Free;
end;

{ TMVCValidationErrors }

procedure TMVCValidationErrors.Add(const AFieldPath, AMessage: string);
begin
  // Lazy creation of errors dictionary
  if FErrors = nil then
    FErrors := TDictionary<string, string>.Create;
  // Add only first error per field
  if not FErrors.ContainsKey(AFieldPath) then
    FErrors.Add(AFieldPath, AMessage);
end;

function TMVCValidationErrors.HasErrors: Boolean;
begin
  Result := (FErrors <> nil) and (FErrors.Count > 0);
end;

function TMVCValidationErrors.ExtractErrors: TDictionary<string, string>;
begin
  Result := FErrors;
  FErrors := nil;  // Transfer ownership to caller
end;

procedure TMVCValidationErrors.Clear;
begin
  FreeAndNil(FErrors);
end;

class function TMVCValidationEngine.BuildCacheEntry(AClass: TClass): TValidationCacheEntry;
var
  LCtx: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LAttr: TCustomAttribute;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
begin
  Result.HasValidators := False;
  Result.HasOnValidate := False;
  Result.OnValidateCodeAddress := nil;

  LCtx := TRttiContext.Create;
  try
    LType := LCtx.GetType(AClass);
    if LType = nil then
      Exit;

    // Check if at least one property has a validator attribute
    for LProp in LType.GetProperties do
    begin
      for LAttr in LProp.GetAttributes do
      begin
        if LAttr is TMVCValidatorBase then
        begin
          Result.HasValidators := True;
          Break;
        end;
      end;
      if Result.HasValidators then
        Break;
    end;

    // Check for OnValidate method with correct signature:
    // procedure OnValidate(const AErrors: PMVCValidationErrors)
    LMethod := LType.GetMethod('OnValidate');
    if LMethod <> nil then
    begin
      // Must be a procedure (no return type)
      if LMethod.ReturnType <> nil then
        Exit;

      LParams := LMethod.GetParameters;
      // Must have exactly 1 parameter
      if Length(LParams) <> 1 then
        Exit;

      // Parameter must be PMVCValidationErrors
      if (LParams[0].ParamType = nil) then
        Exit;

      if LParams[0].ParamType.Handle <> TypeInfo(PMVCValidationErrors) then
        Exit;

      Result.HasOnValidate := True;
      Result.OnValidateCodeAddress := LMethod.CodeAddress;
    end;
  finally
    LCtx.Free;
  end;
end;

class function TMVCValidationEngine.IsValidatableClass(AClass: TClass): Boolean;
var
  LCacheEntry: TValidationCacheEntry;
begin
  if AClass = nil then
    Exit(False);

  // Check cache first (thread-safe read)
  FCacheLock.Enter;
  try
    if FValidationCache.TryGetValue(AClass, LCacheEntry) then
      Exit(LCacheEntry.HasValidators or LCacheEntry.HasOnValidate);
  finally
    FCacheLock.Leave;
  end;

  // Not in cache, build entry via RTTI (outside lock for performance)
  LCacheEntry := BuildCacheEntry(AClass);

  // Add to cache (thread-safe write)
  FCacheLock.Enter;
  try
    if not FValidationCache.ContainsKey(AClass) then
      FValidationCache.Add(AClass, LCacheEntry);
  finally
    FCacheLock.Leave;
  end;

  Result := LCacheEntry.HasValidators or LCacheEntry.HasOnValidate;
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

var
  LCtx: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LAttr: TCustomAttribute;
  LValue: TValue;
  LValidator: TMVCValidatorBase;
  LFieldPath: string;
  LNestedObj: TObject;
  LNestedType: TRttiType;
  LCountProp: TRttiProperty;
  LItemsProp: TRttiIndexedProperty;
  LListCount: Integer;
  LListItem: TObject;
  LItemValue: TValue;
  I: Integer;
  LCacheEntry: TValidationCacheEntry;
  LOnValidateMethod: TMethod;
  LOnValidateProc: TOnValidateProc;
  LOnValidateErrors: TMVCValidationErrors;
begin
  if AObject = nil then
    Exit;

  // Check for circular references
  if AVisited.Contains(AObject) then
    Exit;
  AVisited.Add(AObject);

  // Get cache entry for this class
  FCacheLock.Enter;
  try
    if not FValidationCache.TryGetValue(AObject.ClassType, LCacheEntry) then
    begin
      // Build and cache entry if not found
      LCacheEntry := BuildCacheEntry(AObject.ClassType);
      FValidationCache.Add(AObject.ClassType, LCacheEntry);
    end;
  finally
    FCacheLock.Leave;
  end;

  LCtx := TRttiContext.Create;
  try
    LType := LCtx.GetType(AObject.ClassType);
    if LType = nil then
      Exit;

    // 1. Validate each property (attribute-based validation)
    for LProp in LType.GetProperties do
    begin
      // Skip non-readable properties
      if not LProp.IsReadable then
        Continue;

      // Build field path
      if APath.IsEmpty then
        LFieldPath := LProp.Name
      else
        LFieldPath := APath + '.' + LProp.Name;

      LValue := LProp.GetValue(AObject);

      // Execute validators on this property
      for LAttr in LProp.GetAttributes do
      begin
        if LAttr is TMVCValidatorBase then
        begin
          LValidator := TMVCValidatorBase(LAttr);
          if not LValidator.Validate(LValue, AObject) then
          begin
            AddError(LFieldPath, LValidator.GetErrorMessage);
          end;
        end;
      end;

      // 2. RECURSIVE VALIDATION for nested objects
      if (LValue.Kind = tkClass) and not LValue.IsEmpty then
      begin
        LNestedObj := LValue.AsObject;

        if LNestedObj <> nil then
        begin
          LNestedType := LCtx.GetType(LNestedObj.ClassType);
          if LNestedType = nil then
            Continue;

          // Try to detect if it's a collection (TObjectList, TList, etc.)
          LCountProp := LNestedType.GetProperty('Count');
          LItemsProp := LNestedType.GetIndexedProperty('Items');

          if Assigned(LCountProp) and Assigned(LItemsProp) then
          begin
            // It's a collection - validate each item
            LListCount := LCountProp.GetValue(LNestedObj).AsInteger;

            for I := 0 to LListCount - 1 do
            begin
              // Get item at index I using indexed property
              LItemValue := LItemsProp.GetValue(LNestedObj, [I]);

              if (LItemValue.Kind = tkClass) and not LItemValue.IsEmpty then
              begin
                LListItem := LItemValue.AsObject;
                if LListItem <> nil then
                begin
                  // Recursively validate the list item
                  InternalValidate(LListItem, AErrors,
                    Format('%s[%d]', [LFieldPath, I]), AVisited);
                end;
              end;
            end;
          end
          else
          begin
            // It's a single nested object - validate recursively
            InternalValidate(LNestedObj, AErrors, LFieldPath, AVisited);
          end;
        end;
      end;
    end;

    // 3. Call OnValidate method if it exists (object-level validation)
    // Only call for root object (empty path) to avoid duplicate calls on nested objects
    if APath.IsEmpty and LCacheEntry.HasOnValidate then
    begin
      LOnValidateMethod.Code := LCacheEntry.OnValidateCodeAddress;
      LOnValidateMethod.Data := AObject;
      LOnValidateProc := TOnValidateProc(LOnValidateMethod);
      // Initialize record on stack (zero allocation)
      FillChar(LOnValidateErrors, SizeOf(LOnValidateErrors), 0);
      // Call OnValidate with pointer to stack record
      LOnValidateProc(@LOnValidateErrors);
      // Merge any errors from OnValidate into main errors dictionary
      if LOnValidateErrors.HasErrors then
      begin
        if AErrors = nil then
          AErrors := LOnValidateErrors.ExtractErrors
        else
        begin
          // Merge errors
          for var LPair in LOnValidateErrors.FErrors do
            if not AErrors.ContainsKey(LPair.Key) then
              AErrors.Add(LPair.Key, LPair.Value);
          LOnValidateErrors.Clear;
        end;
      end;
    end;
  finally
    LCtx.Free;
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
  FCacheLock.Enter;
  try
    FValidationCache.Clear;
  finally
    FCacheLock.Leave;
  end;
end;

end.
