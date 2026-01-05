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

unit MVCFramework.Validators.CrossField;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  MVCFramework.Validation;

type
  TRequiredIfOperator = (
    roEquals,           // Required if other field equals value
    roNotEquals,        // Required if other field not equals value
    roIsNotEmpty,       // Required if other field is not empty
    roIsEmpty,          // Required if other field is empty
    roGreaterThan,      // Required if other field > value
    roLessThan,         // Required if other field < value
    roIn,               // Required if other field is in comma-separated list
    roNotIn             // Required if other field is not in comma-separated list
  );

  // ===========================================================================
  // CROSS-FIELD VALIDATORS
  // These validators compare field values with other fields in the same object
  // ===========================================================================

  /// <summary>
  /// Validates that two fields have the same value.
  /// Commonly used for password confirmation, email confirmation, etc.
  /// Example: MVCCompareField('ConfirmPassword') on Password field
  /// </summary>
  MVCCompareField = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
  public
    constructor Create(const AOtherFieldName: string; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
  end;

  /// <summary>
  /// Validates that two fields have different values.
  /// Example: Ensure new password differs from old password.
  /// </summary>
  MVCDifferentFrom = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
  public
    constructor Create(const AOtherFieldName: string; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
  end;

  /// <summary>
  /// Validates that a field is required based on another field's value.
  /// Example: MVCRequiredIf('PaymentMethod', roEquals, 'CreditCard')
  ///          makes CardNumber required only when PaymentMethod = 'CreditCard'
  /// </summary>
  MVCRequiredIf = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOperator: TRequiredIfOperator;
    FExpectedValue: string;
    function IsFieldRequired(const AOtherValue: TValue): Boolean;
    function ValueToString(const AValue: TValue): string;
  public
    constructor Create(const AOtherFieldName: string;
      AOperator: TRequiredIfOperator = roIsNotEmpty;
      const AExpectedValue: string = ''; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property Operator: TRequiredIfOperator read FOperator;
    property ExpectedValue: string read FExpectedValue;
  end;

  /// <summary>
  /// Validates that a field is prohibited (must be empty) based on another field's value.
  /// Opposite of MVCRequiredIf.
  /// Example: MVCProhibitedIf('AccountType', roEquals, 'Guest')
  ///          makes field prohibited when AccountType = 'Guest'
  /// </summary>
  MVCProhibitedIf = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOperator: TRequiredIfOperator;
    FExpectedValue: string;
    function IsFieldProhibited(const AOtherValue: TValue): Boolean;
    function ValueIsEmpty(const AValue: TValue): Boolean;
    function ValueToString(const AValue: TValue): string;
  public
    constructor Create(const AOtherFieldName: string;
      AOperator: TRequiredIfOperator = roIsNotEmpty;
      const AExpectedValue: string = ''; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property Operator: TRequiredIfOperator read FOperator;
    property ExpectedValue: string read FExpectedValue;
  end;

  /// <summary>
  /// Validates that a numeric field is less than another numeric field.
  /// Example: MVCLessThanField('MaxValue') on MinValue field
  /// </summary>
  MVCLessThanField = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOrEqual: Boolean;
  public
    constructor Create(const AOtherFieldName: string;
      AOrEqual: Boolean = False; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property OrEqual: Boolean read FOrEqual;
  end;

  /// <summary>
  /// Validates that a numeric field is greater than another numeric field.
  /// Example: MVCGreaterThanField('MinValue') on MaxValue field
  /// </summary>
  MVCGreaterThanField = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOrEqual: Boolean;
  public
    constructor Create(const AOtherFieldName: string;
      AOrEqual: Boolean = False; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property OrEqual: Boolean read FOrEqual;
  end;

  /// <summary>
  /// Validates that a date field is before another date field.
  /// Example: MVCDateBefore('EndDate') on StartDate field
  /// </summary>
  MVCDateBefore = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOrEqual: Boolean;
  public
    constructor Create(const AOtherFieldName: string;
      AOrEqual: Boolean = False; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property OrEqual: Boolean read FOrEqual;
  end;

  /// <summary>
  /// Validates that a date field is after another date field.
  /// Example: MVCDateAfter('StartDate') on EndDate field
  /// </summary>
  MVCDateAfter = class(TMVCValidatorBase)
  private
    FOtherFieldName: string;
    FOrEqual: Boolean;
  public
    constructor Create(const AOtherFieldName: string;
      AOrEqual: Boolean = False; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property OtherFieldName: string read FOtherFieldName;
    property OrEqual: Boolean read FOrEqual;
  end;

implementation

uses
  System.DateUtils,
  System.StrUtils;

// Helper function to get field value from object
function GetFieldValue(const AObject: TObject; const AFieldName: string): TValue;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LField: TRttiField;
begin
  Result := TValue.Empty;
  if AObject = nil then
    Exit;

  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AObject.ClassType);
    if LType = nil then
      Exit;

    // Try property first
    LProp := LType.GetProperty(AFieldName);
    if LProp <> nil then
    begin
      Result := LProp.GetValue(AObject);
      Exit;
    end;

    // Try field
    LField := LType.GetField('F' + AFieldName);
    if LField = nil then
      LField := LType.GetField(AFieldName);
    if LField <> nil then
      Result := LField.GetValue(AObject);
  finally
    LContext.Free;
  end;
end;

{ MVCCompareField }

constructor MVCCompareField.Create(const AOtherFieldName: string; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must match %s', [AOtherFieldName]);
end;

function MVCCompareField.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
begin
  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);

  // Both empty = equal
  if AValue.IsEmpty and LOtherValue.IsEmpty then
    Exit(True);

  // One empty, one not = not equal
  if AValue.IsEmpty or LOtherValue.IsEmpty then
    Exit(False);

  // Compare as strings for simplicity (works for most types)
  Result := AValue.ToString = LOtherValue.ToString;
end;

{ MVCDifferentFrom }

constructor MVCDifferentFrom.Create(const AOtherFieldName: string; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must be different from %s', [AOtherFieldName]);
end;

function MVCDifferentFrom.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
begin
  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);

  // Both empty = equal, so fail
  if AValue.IsEmpty and LOtherValue.IsEmpty then
    Exit(False);

  // One empty, one not = different, so pass
  if AValue.IsEmpty or LOtherValue.IsEmpty then
    Exit(True);

  // Compare as strings
  Result := AValue.ToString <> LOtherValue.ToString;
end;

{ MVCRequiredIf }

constructor MVCRequiredIf.Create(const AOtherFieldName: string;
  AOperator: TRequiredIfOperator; const AExpectedValue: string; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOperator := AOperator;
  FExpectedValue := AExpectedValue;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'This field is required';
end;

function MVCRequiredIf.ValueToString(const AValue: TValue): string;
begin
  if AValue.IsEmpty then
    Result := ''
  else
    Result := AValue.ToString;
end;

function MVCRequiredIf.IsFieldRequired(const AOtherValue: TValue): Boolean;
var
  LOtherStr: string;
  LNumValue: Extended;
  LExpectedNum: Extended;
  LValues: TArray<string>;
  LValue: string;
begin
  LOtherStr := ValueToString(AOtherValue);

  case FOperator of
    roEquals:
      Result := SameText(LOtherStr, FExpectedValue);
    roNotEquals:
      Result := not SameText(LOtherStr, FExpectedValue);
    roIsNotEmpty:
      Result := not LOtherStr.IsEmpty;
    roIsEmpty:
      Result := LOtherStr.IsEmpty;
    roGreaterThan:
      begin
        Result := False;
        if TryStrToFloat(LOtherStr, LNumValue) and TryStrToFloat(FExpectedValue, LExpectedNum) then
          Result := LNumValue > LExpectedNum;
      end;
    roLessThan:
      begin
        Result := False;
        if TryStrToFloat(LOtherStr, LNumValue) and TryStrToFloat(FExpectedValue, LExpectedNum) then
          Result := LNumValue < LExpectedNum;
      end;
    roIn:
      begin
        Result := False;
        LValues := FExpectedValue.Split([',']);
        for LValue in LValues do
          if SameText(LOtherStr.Trim, LValue.Trim) then
          begin
            Result := True;
            Break;
          end;
      end;
    roNotIn:
      begin
        Result := True;
        LValues := FExpectedValue.Split([',']);
        for LValue in LValues do
          if SameText(LOtherStr.Trim, LValue.Trim) then
          begin
            Result := False;
            Break;
          end;
      end;
  else
    Result := False;
  end;
end;

function MVCRequiredIf.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
  LValueStr: string;
begin
  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);

  // Check if field is required based on other field's value
  if not IsFieldRequired(LOtherValue) then
    Exit(True); // Not required, so always valid

  // Field is required - check if it has a value
  if AValue.IsEmpty then
    Exit(False);

  LValueStr := ValueToString(AValue);
  Result := not LValueStr.Trim.IsEmpty;
end;

{ MVCProhibitedIf }

constructor MVCProhibitedIf.Create(const AOtherFieldName: string;
  AOperator: TRequiredIfOperator; const AExpectedValue: string; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOperator := AOperator;
  FExpectedValue := AExpectedValue;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'This field must be empty';
end;

function MVCProhibitedIf.ValueToString(const AValue: TValue): string;
begin
  if AValue.IsEmpty then
    Result := ''
  else
    Result := AValue.ToString;
end;

function MVCProhibitedIf.ValueIsEmpty(const AValue: TValue): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);
  Result := ValueToString(AValue).Trim.IsEmpty;
end;

function MVCProhibitedIf.IsFieldProhibited(const AOtherValue: TValue): Boolean;
var
  LOtherStr: string;
  LNumValue: Extended;
  LExpectedNum: Extended;
  LValues: TArray<string>;
  LValue: string;
begin
  LOtherStr := ValueToString(AOtherValue);

  case FOperator of
    roEquals:
      Result := SameText(LOtherStr, FExpectedValue);
    roNotEquals:
      Result := not SameText(LOtherStr, FExpectedValue);
    roIsNotEmpty:
      Result := not LOtherStr.IsEmpty;
    roIsEmpty:
      Result := LOtherStr.IsEmpty;
    roGreaterThan:
      begin
        Result := False;
        if TryStrToFloat(LOtherStr, LNumValue) and TryStrToFloat(FExpectedValue, LExpectedNum) then
          Result := LNumValue > LExpectedNum;
      end;
    roLessThan:
      begin
        Result := False;
        if TryStrToFloat(LOtherStr, LNumValue) and TryStrToFloat(FExpectedValue, LExpectedNum) then
          Result := LNumValue < LExpectedNum;
      end;
    roIn:
      begin
        Result := False;
        LValues := FExpectedValue.Split([',']);
        for LValue in LValues do
          if SameText(LOtherStr.Trim, LValue.Trim) then
          begin
            Result := True;
            Break;
          end;
      end;
    roNotIn:
      begin
        Result := True;
        LValues := FExpectedValue.Split([',']);
        for LValue in LValues do
          if SameText(LOtherStr.Trim, LValue.Trim) then
          begin
            Result := False;
            Break;
          end;
      end;
  else
    Result := False;
  end;
end;

function MVCProhibitedIf.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
begin
  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);

  // Check if field is prohibited based on other field's value
  if not IsFieldProhibited(LOtherValue) then
    Exit(True); // Not prohibited, so any value is valid

  // Field is prohibited - must be empty
  Result := ValueIsEmpty(AValue);
end;

{ MVCLessThanField }

constructor MVCLessThanField.Create(const AOtherFieldName: string;
  AOrEqual: Boolean; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOrEqual := AOrEqual;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
  begin
    if FOrEqual then
      FErrorMessage := Format('Value must be less than or equal to %s', [AOtherFieldName])
    else
      FErrorMessage := Format('Value must be less than %s', [AOtherFieldName]);
  end;
end;

function MVCLessThanField.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
  LValue, LOther: Extended;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);
  if LOtherValue.IsEmpty then
    Exit(True);

  // Try to convert to numbers
  if not TryStrToFloat(AValue.ToString, LValue) then
    Exit(True);
  if not TryStrToFloat(LOtherValue.ToString, LOther) then
    Exit(True);

  if FOrEqual then
    Result := LValue <= LOther
  else
    Result := LValue < LOther;
end;

{ MVCGreaterThanField }

constructor MVCGreaterThanField.Create(const AOtherFieldName: string;
  AOrEqual: Boolean; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOrEqual := AOrEqual;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
  begin
    if FOrEqual then
      FErrorMessage := Format('Value must be greater than or equal to %s', [AOtherFieldName])
    else
      FErrorMessage := Format('Value must be greater than %s', [AOtherFieldName]);
  end;
end;

function MVCGreaterThanField.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
  LValue, LOther: Extended;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AObject = nil then
    Exit(True);

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);
  if LOtherValue.IsEmpty then
    Exit(True);

  // Try to convert to numbers
  if not TryStrToFloat(AValue.ToString, LValue) then
    Exit(True);
  if not TryStrToFloat(LOtherValue.ToString, LOther) then
    Exit(True);

  if FOrEqual then
    Result := LValue >= LOther
  else
    Result := LValue > LOther;
end;

{ MVCDateBefore }

constructor MVCDateBefore.Create(const AOtherFieldName: string;
  AOrEqual: Boolean; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOrEqual := AOrEqual;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
  begin
    if FOrEqual then
      FErrorMessage := Format('Date must be on or before %s', [AOtherFieldName])
    else
      FErrorMessage := Format('Date must be before %s', [AOtherFieldName]);
  end;
end;

function MVCDateBefore.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
  LDate, LOtherDate: TDateTime;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AObject = nil then
    Exit(True);

  // Get this value as date
  case AValue.Kind of
    tkFloat:
      LDate := AValue.AsExtended;
  else
    if not TryStrToDateTime(AValue.ToString, LDate) then
      Exit(True);
  end;

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);
  if LOtherValue.IsEmpty then
    Exit(True);

  // Get other value as date
  case LOtherValue.Kind of
    tkFloat:
      LOtherDate := LOtherValue.AsExtended;
  else
    if not TryStrToDateTime(LOtherValue.ToString, LOtherDate) then
      Exit(True);
  end;

  if FOrEqual then
    Result := CompareDateTime(LDate, LOtherDate) <= 0
  else
    Result := CompareDateTime(LDate, LOtherDate) < 0;
end;

{ MVCDateAfter }

constructor MVCDateAfter.Create(const AOtherFieldName: string;
  AOrEqual: Boolean; const AMessage: string);
begin
  inherited Create;
  FOtherFieldName := AOtherFieldName;
  FOrEqual := AOrEqual;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
  begin
    if FOrEqual then
      FErrorMessage := Format('Date must be on or after %s', [AOtherFieldName])
    else
      FErrorMessage := Format('Date must be after %s', [AOtherFieldName]);
  end;
end;

function MVCDateAfter.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LOtherValue: TValue;
  LDate, LOtherDate: TDateTime;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AObject = nil then
    Exit(True);

  // Get this value as date
  case AValue.Kind of
    tkFloat:
      LDate := AValue.AsExtended;
  else
    if not TryStrToDateTime(AValue.ToString, LDate) then
      Exit(True);
  end;

  LOtherValue := GetFieldValue(AObject, FOtherFieldName);
  if LOtherValue.IsEmpty then
    Exit(True);

  // Get other value as date
  case LOtherValue.Kind of
    tkFloat:
      LOtherDate := LOtherValue.AsExtended;
  else
    if not TryStrToDateTime(LOtherValue.ToString, LOtherDate) then
      Exit(True);
  end;

  if FOrEqual then
    Result := CompareDateTime(LDate, LOtherDate) >= 0
  else
    Result := CompareDateTime(LDate, LOtherDate) > 0;
end;

end.
