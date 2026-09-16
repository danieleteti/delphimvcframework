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

unit MVCFramework.Validation;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Commons;

type
  /// <summary>
  /// Record for collecting validation errors with lazy dictionary creation.
  /// The dictionary is only allocated when Add is called.
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
    /// Clears and frees the internal dictionary if allocated.
    /// </summary>
    procedure Clear;
    /// <summary>
    /// Read-only access to the internal dictionary (nil until the first Add
    /// call). The record retains ownership: consumers that need a persistent
    /// copy must allocate their own dictionary and copy the entries.
    /// </summary>
    property Errors: TDictionary<string, string> read FErrors;
  end;

  PMVCValidationErrors = ^TMVCValidationErrors;

  /// <summary>
  /// Base class for every object that can be validated at the framework
  /// boundary (DTO, command, request model, ...). Override OnValidate to
  /// collect cross-field / business rule errors on top of the declarative
  /// attribute-based validators. TMVCActiveRecord itself inherits from this
  /// class, so an entity can be used directly as a request DTO and will be
  /// validated along the same pipeline without extra code.
  /// </summary>
  TMVCValidatable = class
  public
    procedure OnValidate(const AErrors: PMVCValidationErrors); virtual;
  end;

  /// <summary>
  /// Abstract base class for all validators.
  /// Each validator must inherit from this class and implement the Validate method.
  /// </summary>
  TMVCValidatorBase = class abstract(TCustomAttribute)
  protected
    FErrorMessage: string;
  public
    /// <summary>
    /// Validates a value against the validator's rules.
    /// </summary>
    /// <param name="AValue">The value to validate (TValue supports any type)</param>
    /// <param name="AObject">The container object (for cross-field validators)</param>
    /// <returns>True if validation passes, False otherwise</returns>
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; virtual; abstract;

    /// <summary>
    /// Returns the error message for this validation.
    /// If FErrorMessage is empty, returns a default message.
    /// </summary>
    function GetErrorMessage: string; virtual;

    /// <summary>
    /// Custom error message (can be empty to use default)
    /// </summary>
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  /// <summary>
  /// Exception raised when validation fails.
  /// Inherits from EMVCException for seamless integration with framework error handling.
  /// </summary>
  EMVCValidationException = class(EMVCException)
  private
    FValidationErrors: TDictionary<string, string>;
  public
    /// <summary>
    /// Constructor with validation errors dictionary.
    /// The dictionary is copied internally.
    /// </summary>
    constructor Create(const AErrors: TDictionary<string, string>); reintroduce; overload;

    /// <summary>
    /// Constructor with single error
    /// </summary>
    constructor Create(const AFieldName: string; const AErrorMessage: string); reintroduce; overload;

    /// <summary>
    /// Destructor that frees the internal dictionary
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    /// All validation errors (field path -> message).
    /// Field paths use dot notation for nested objects: "Address.Street", "Items[0].Name"
    /// </summary>
    property ValidationErrors: TDictionary<string, string> read FValidationErrors;
  end;

  /// <summary>
  /// Exception raised when storage-side validation fails (TMVCActiveRecord.Validate).
  /// Inherits from EMVCValidationException so existing handlers keep working;
  /// the distinct type lets callers discriminate input-layer vs storage-layer
  /// errors by exception class.
  /// </summary>
  EMVCStorageValidationException = class(EMVCValidationException);

implementation

{ TMVCValidationErrors }

procedure TMVCValidationErrors.Add(const AFieldPath, AMessage: string);
begin
  if FErrors = nil then
    FErrors := TDictionary<string, string>.Create;
  if not FErrors.ContainsKey(AFieldPath) then
    FErrors.Add(AFieldPath, AMessage);
end;

function TMVCValidationErrors.HasErrors: Boolean;
begin
  Result := (FErrors <> nil) and (FErrors.Count > 0);
end;

procedure TMVCValidationErrors.Clear;
begin
  FreeAndNil(FErrors);
end;

{ TMVCValidatable }

procedure TMVCValidatable.OnValidate(const AErrors: PMVCValidationErrors);
begin
  // no-op: override in descendants to add business / cross-field errors.
end;

{ TMVCValidatorBase }

function TMVCValidatorBase.GetErrorMessage: string;
begin
  Result := FErrorMessage;
  if Result.IsEmpty then
    Result := 'Validation failed';
end;

{ EMVCValidationException }

constructor EMVCValidationException.Create(const AErrors: TDictionary<string, string>);
var
  LKey: string;
  LErrorItems: TArray<string>;
  LFieldNames: TArray<string>;
  LIndex: Integer;
  LMessage: string;
begin
  FValidationErrors := TDictionary<string, string>.Create;

  // Copy errors, build ErrorItems array for EMVCException compatibility, and
  // collect field names for the top-level exception message.
  SetLength(LErrorItems, AErrors.Count);
  SetLength(LFieldNames, AErrors.Count);
  LIndex := 0;
  for LKey in AErrors.Keys do
  begin
    FValidationErrors.Add(LKey, AErrors[LKey]);
    LErrorItems[LIndex] := Format('%s: %s', [LKey, AErrors[LKey]]);
    LFieldNames[LIndex] := LKey;
    Inc(LIndex);
  end;

  if Length(LFieldNames) = 0 then
    LMessage := 'Validation failed'
  else
    LMessage := 'Validation failed for fields: ' + string.Join(', ', LFieldNames);

  inherited Create(LMessage, '', 0, HTTP_STATUS.UnprocessableEntity, LErrorItems);
end;

constructor EMVCValidationException.Create(const AFieldName, AErrorMessage: string);
var
  LErrors: TDictionary<string, string>;
begin
  LErrors := TDictionary<string, string>.Create;
  try
    LErrors.Add(AFieldName, AErrorMessage);
    Create(LErrors);
  finally
    LErrors.Free;
  end;
end;

destructor EMVCValidationException.Destroy;
begin
  FValidationErrors.Free;
  inherited;
end;

end.
