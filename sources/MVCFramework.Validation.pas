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

implementation

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
begin
  FValidationErrors := TDictionary<string, string>.Create;

  // Copy errors and build ErrorItems array for EMVCException compatibility
  SetLength(LErrorItems, AErrors.Count);
  var LIndex := 0;
  for LKey in AErrors.Keys do
  begin
    FValidationErrors.Add(LKey, AErrors[LKey]);
    LErrorItems[LIndex] := Format('%s: %s', [LKey, AErrors[LKey]]);
    Inc(LIndex);
  end;

  // Call inherited constructor with 422 status and error items
  inherited Create('Validation failed', '', 0, HTTP_STATUS.UnprocessableEntity, LErrorItems);
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
