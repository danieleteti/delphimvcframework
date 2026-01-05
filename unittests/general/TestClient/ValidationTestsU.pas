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

unit ValidationTestsU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  MVCFramework.Validation,
  MVCFramework.ValidationEngine,
  MVCFramework.Validators,
  MVCFramework.Validators.CrossField;

type
  // Test model without validators (existing code scenario)
  TNoValidationModel = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  // Test model with validators
  TUserModel = class
  private
    FName: string;
    FEmail: string;
    FAge: Integer;
    FWebsite: string;
  public
    [MVCRequired('Name is required')]
    [MVCMinLength(2, 'Name must be at least 2 characters')]
    property Name: string read FName write FName;

    [MVCRequired]
    [MVCEmail('Invalid email format')]
    property Email: string read FEmail write FEmail;

    [MVCRange(18, 120, 'Age must be between 18 and 120')]
    property Age: Integer read FAge write FAge;

    [MVCUrl]
    property Website: string read FWebsite write FWebsite;
  end;

  // Test model for nested validation
  TAddressModel = class
  private
    FStreet: string;
    FCity: string;
    FPostalCode: string;
  public
    [MVCRequired]
    [MVCMinLength(5)]
    property Street: string read FStreet write FStreet;

    [MVCRequired]
    property City: string read FCity write FCity;

    [MVCPostalCode('IT')]
    property PostalCode: string read FPostalCode write FPostalCode;
  end;

  TCustomerModel = class
  private
    FName: string;
    FAddress: TAddressModel;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCRequired]
    property Name: string read FName write FName;

    [MVCRequired]
    property Address: TAddressModel read FAddress write FAddress;
  end;

  // Test model for collection validation
  TOrderItemModel = class
  private
    FProductName: string;
    FQuantity: Integer;
  public
    [MVCRequired]
    property ProductName: string read FProductName write FProductName;

    [MVCRequired]
    [MVCPositive]
    property Quantity: Integer read FQuantity write FQuantity;
  end;

  TOrderModel = class
  private
    FOrderNumber: string;
    FItems: TObjectList<TOrderItemModel>;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCRequired]
    [MVCPattern('^ORD-\d{8}$')]
    property OrderNumber: string read FOrderNumber write FOrderNumber;

    [MVCMinCount(1, 'Order must have at least one item')]
    property Items: TObjectList<TOrderItemModel> read FItems;
  end;

  // Test model for cross-field validation
  TPasswordModel = class
  private
    FPassword: string;
    FConfirmPassword: string;
    FOldPassword: string;
  public
    [MVCRequired]
    [MVCMinLength(8)]
    property Password: string read FPassword write FPassword;

    [MVCRequired]
    [MVCCompareField('Password', 'Passwords do not match')]
    property ConfirmPassword: string read FConfirmPassword write FConfirmPassword;

    [MVCDifferentFrom('Password', 'New password must be different from old password')]
    property OldPassword: string read FOldPassword write FOldPassword;
  end;

  // Test model for RequiredIf validation
  TPaymentMethodModel = class
  private
    FPaymentMethod: string;
    FCardNumber: string;
    FIBAN: string;
    FAmount: Currency;
  public
    [MVCRequired]
    [MVCIn('CreditCard,BankTransfer,Cash')]
    property PaymentMethod: string read FPaymentMethod write FPaymentMethod;

    [MVCRequiredIf('PaymentMethod', roEquals, 'CreditCard', 'Card number required for credit card payments')]
    [MVCCreditCard]
    property CardNumber: string read FCardNumber write FCardNumber;

    [MVCRequiredIf('PaymentMethod', roEquals, 'BankTransfer', 'IBAN required for bank transfers')]
    [MVCIBAN]
    property IBAN: string read FIBAN write FIBAN;

    [MVCRequired]
    [MVCPositive]
    property Amount: Currency read FAmount write FAmount;
  end;

  // Test model for date comparison
  TDateRangeModel = class
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FMinValue: Integer;
    FMaxValue: Integer;
  public
    [MVCRequired]
    property StartDate: TDateTime read FStartDate write FStartDate;

    [MVCRequired]
    [MVCDateAfter('StartDate', False, 'End date must be after start date')]
    property EndDate: TDateTime read FEndDate write FEndDate;

    [MVCRequired]
    property MinValue: Integer read FMinValue write FMinValue;

    [MVCRequired]
    [MVCGreaterThanField('MinValue', False, 'Max value must be greater than min value')]
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

  // ============================================================================
  // Test models for OnValidate (object-level validation)
  // ============================================================================

  /// <summary>
  /// Model with ONLY OnValidate, no property validators.
  /// Tests that OnValidate alone makes a class validatable.
  /// </summary>
  TOnValidateOnlyModel = class
  private
    FStartDate: TDate;
    FEndDate: TDate;
  public
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    procedure OnValidate(const AErrors: PMVCValidationErrors);
  end;

  /// <summary>
  /// Model with BOTH property validators AND OnValidate.
  /// Tests that both run and errors are accumulated.
  /// </summary>
  TOnValidateCombinedModel = class
  private
    FName: string;
    FEmail: string;
    FMinAmount: Currency;
    FMaxAmount: Currency;
  public
    [MVCRequired('Name is required')]
    property Name: string read FName write FName;

    [MVCEmail('Invalid email')]
    property Email: string read FEmail write FEmail;

    property MinAmount: Currency read FMinAmount write FMinAmount;
    property MaxAmount: Currency read FMaxAmount write FMaxAmount;

    procedure OnValidate(const AErrors: PMVCValidationErrors);
  end;

  /// <summary>
  /// Model with multiple cross-field validations in OnValidate.
  /// </summary>
  TOnValidateMultipleErrorsModel = class
  private
    FPassword: string;
    FConfirmPassword: string;
    FAge: Integer;
    FRetirementAge: Integer;
  public
    property Password: string read FPassword write FPassword;
    property ConfirmPassword: string read FConfirmPassword write FConfirmPassword;
    property Age: Integer read FAge write FAge;
    property RetirementAge: Integer read FRetirementAge write FRetirementAge;
    procedure OnValidate(const AErrors: PMVCValidationErrors);
  end;

  [TestFixture]
  TTestValidators = class
  public
    // ====== OPT-IN Tests ======
    [Test]
    procedure TestNoValidationModel_ShouldNotBeValidatable;

    [Test]
    procedure TestUserModel_ShouldBeValidatable;

    // ====== MVCRequired Tests ======
    [Test]
    procedure TestRequired_EmptyString_ShouldFail;

    [Test]
    procedure TestRequired_NonEmptyString_ShouldPass;

    [Test]
    procedure TestRequired_NilObject_ShouldFail;

    [Test]
    procedure TestRequired_ZeroInteger_ShouldPass;

    // ====== MVCMinLength/MaxLength Tests ======
    [Test]
    procedure TestMinLength_TooShort_ShouldFail;

    [Test]
    procedure TestMinLength_ExactLength_ShouldPass;

    [Test]
    procedure TestMaxLength_TooLong_ShouldFail;

    // ====== MVCEmail Tests ======
    [Test]
    procedure TestEmail_ValidEmail_ShouldPass;

    [Test]
    procedure TestEmail_InvalidEmail_ShouldFail;

    // ====== MVCRange Tests ======
    [Test]
    procedure TestRange_WithinRange_ShouldPass;

    [Test]
    procedure TestRange_BelowMin_ShouldFail;

    [Test]
    procedure TestRange_AboveMax_ShouldFail;

    // ====== MVCPattern Tests ======
    [Test]
    procedure TestPattern_ValidPattern_ShouldPass;

    [Test]
    procedure TestPattern_InvalidPattern_ShouldFail;

    // ====== MVCPositive Tests ======
    [Test]
    procedure TestPositive_PositiveValue_ShouldPass;

    [Test]
    procedure TestPositive_ZeroValue_ShouldFail;

    [Test]
    procedure TestPositive_NegativeValue_ShouldFail;

    // ====== Nested Validation Tests ======
    [Test]
    procedure TestNestedValidation_ValidData_ShouldPass;

    [Test]
    procedure TestNestedValidation_InvalidNested_ShouldFailWithPath;

    // ====== Collection Validation Tests ======
    [Test]
    procedure TestCollectionValidation_ValidItems_ShouldPass;

    [Test]
    procedure TestCollectionValidation_InvalidItem_ShouldFailWithIndex;

    [Test]
    procedure TestCollectionValidation_EmptyCollection_ShouldFailMinCount;

    // ====== Format Validators Tests ======
    [Test]
    procedure TestCreditCard_ValidLuhn_ShouldPass;

    [Test]
    procedure TestCreditCard_InvalidLuhn_ShouldFail;

    [Test]
    procedure TestPartitaIVA_Valid_ShouldPass;

    [Test]
    procedure TestPartitaIVA_Invalid_ShouldFail;

    [Test]
    procedure TestIBAN_Valid_ShouldPass;

    // ====== Exception Tests ======
    [Test]
    procedure TestValidateAndRaise_InvalidData_ShouldRaiseException;

    [Test]
    procedure TestValidateAndRaise_ValidData_ShouldNotRaise;

    // ====== Cross-Field Validation Tests ======
    [Test]
    procedure TestCompareField_MatchingValues_ShouldPass;

    [Test]
    procedure TestCompareField_DifferentValues_ShouldFail;

    [Test]
    procedure TestDifferentFrom_DifferentValues_ShouldPass;

    [Test]
    procedure TestDifferentFrom_SameValues_ShouldFail;

    [Test]
    procedure TestRequiredIf_ConditionMet_ValueProvided_ShouldPass;

    [Test]
    procedure TestRequiredIf_ConditionMet_ValueEmpty_ShouldFail;

    [Test]
    procedure TestRequiredIf_ConditionNotMet_ValueEmpty_ShouldPass;

    [Test]
    procedure TestDateAfter_ValidDates_ShouldPass;

    [Test]
    procedure TestDateAfter_InvalidDates_ShouldFail;

    [Test]
    procedure TestGreaterThanField_Valid_ShouldPass;

    [Test]
    procedure TestGreaterThanField_Invalid_ShouldFail;

    // ====== Global Validators Tests ======

    // Strong Password
    [Test]
    procedure TestStrongPassword_Valid_ShouldPass;
    [Test]
    procedure TestStrongPassword_TooShort_ShouldFail;
    [Test]
    procedure TestStrongPassword_NoUppercase_ShouldFail;
    [Test]
    procedure TestStrongPassword_NoSpecial_ShouldFail;

    // Slug
    [Test]
    procedure TestSlug_Valid_ShouldPass;
    [Test]
    procedure TestSlug_Invalid_ShouldFail;

    // Collection Items
    [Test]
    procedure TestMinItems_Valid_ShouldPass;
    [Test]
    procedure TestMinItems_Invalid_ShouldFail;
    [Test]
    procedure TestMaxItems_Valid_ShouldPass;
    [Test]
    procedure TestMaxItems_Invalid_ShouldFail;

    // Network
    [Test]
    procedure TestIPv4_Valid_ShouldPass;
    [Test]
    procedure TestIPv4_Invalid_ShouldFail;
    [Test]
    procedure TestMACAddress_Valid_ShouldPass;
    [Test]
    procedure TestMACAddress_Invalid_ShouldFail;
    [Test]
    procedure TestSemVer_Valid_ShouldPass;
    [Test]
    procedure TestSemVer_Invalid_ShouldFail;

    // Geographic
    [Test]
    procedure TestLatitude_Valid_ShouldPass;
    [Test]
    procedure TestLatitude_Invalid_ShouldFail;
    [Test]
    procedure TestLongitude_Valid_ShouldPass;
    [Test]
    procedure TestLongitude_Invalid_ShouldFail;
    [Test]
    procedure TestCountryCode_Valid_ShouldPass;
    [Test]
    procedure TestCountryCode_Invalid_ShouldFail;

    // Barcodes
    [Test]
    procedure TestEAN13_Valid_ShouldPass;
    [Test]
    procedure TestEAN13_Invalid_ShouldFail;
    [Test]
    procedure TestISBN10_Valid_ShouldPass;
    [Test]
    procedure TestISBN13_Valid_ShouldPass;
    [Test]
    procedure TestISBN_Invalid_ShouldFail;
    [Test]
    procedure TestVIN_Valid_ShouldPass;
    [Test]
    procedure TestVIN_Invalid_ShouldFail;

    // Banking
    [Test]
    procedure TestBIC_Valid_ShouldPass;
    [Test]
    procedure TestBIC_Invalid_ShouldFail;

    // EU VAT
    [Test]
    procedure TestEUVatNumber_Valid_ShouldPass;
    [Test]
    procedure TestEUVatNumber_Invalid_ShouldFail;

    // Italian Tax IDs
    [Test]
    procedure TestITCodiceFiscale_Valid_ShouldPass;
    [Test]
    procedure TestITCodiceFiscale_Invalid_ShouldFail;
    [Test]
    procedure TestITPartitaIVA_Valid_ShouldPass;
    [Test]
    procedure TestITPartitaIVA_Invalid_ShouldFail;

    // US Tax IDs
    [Test]
    procedure TestUSSSN_Valid_ShouldPass;
    [Test]
    procedure TestUSSSN_Invalid_ShouldFail;
    [Test]
    procedure TestUSAbaRouting_Valid_ShouldPass;
    [Test]
    procedure TestUSAbaRouting_Invalid_ShouldFail;

    // Brazilian Tax IDs
    [Test]
    procedure TestBRCPF_Valid_ShouldPass;
    [Test]
    procedure TestBRCPF_Invalid_ShouldFail;

    // ====== OnValidate (Object-Level Validation) Tests ======
    [Test]
    procedure TestOnValidateOnly_ValidData_ShouldPass;
    [Test]
    procedure TestOnValidateOnly_InvalidData_ShouldFail;
    [Test]
    procedure TestOnValidateOnly_ClassIsValidatable;
    [Test]
    procedure TestOnValidateCombined_AllValid_ShouldPass;
    [Test]
    procedure TestOnValidateCombined_PropertyErrors_ShouldFail;
    [Test]
    procedure TestOnValidateCombined_OnValidateErrors_ShouldFail;
    [Test]
    procedure TestOnValidateCombined_BothErrors_ShouldAccumulate;
    [Test]
    procedure TestOnValidateMultipleErrors_ShouldReturnAll;
  end;

implementation

{ TCustomerModel }

constructor TCustomerModel.Create;
begin
  inherited;
  FAddress := TAddressModel.Create;
end;

destructor TCustomerModel.Destroy;
begin
  FAddress.Free;
  inherited;
end;

{ TOrderModel }

constructor TOrderModel.Create;
begin
  inherited;
  FItems := TObjectList<TOrderItemModel>.Create(True);
end;

destructor TOrderModel.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TOnValidateOnlyModel }

procedure TOnValidateOnlyModel.OnValidate(const AErrors: PMVCValidationErrors);
begin
  if FEndDate < FStartDate then
    AErrors.Add('EndDate', 'End date must be after start date');
end;

{ TOnValidateCombinedModel }

procedure TOnValidateCombinedModel.OnValidate(const AErrors: PMVCValidationErrors);
begin
  if FMaxAmount < FMinAmount then
    AErrors.Add('MaxAmount', 'Max amount must be greater than min amount');
end;

{ TOnValidateMultipleErrorsModel }

procedure TOnValidateMultipleErrorsModel.OnValidate(const AErrors: PMVCValidationErrors);
begin
  if FPassword <> FConfirmPassword then
    AErrors.Add('ConfirmPassword', 'Passwords do not match');

  if FRetirementAge <= FAge then
    AErrors.Add('RetirementAge', 'Retirement age must be greater than current age');
end;

{ TTestValidators }

// ====== OPT-IN Tests ======

procedure TTestValidators.TestNoValidationModel_ShouldNotBeValidatable;
begin
  Assert.IsFalse(TMVCValidationEngine.IsValidatableClass(TNoValidationModel));
end;

procedure TTestValidators.TestUserModel_ShouldBeValidatable;
begin
  Assert.IsTrue(TMVCValidationEngine.IsValidatableClass(TUserModel));
end;

// ====== MVCRequired Tests ======

procedure TTestValidators.TestRequired_EmptyString_ShouldFail;
var
  LUser: TUserModel;
  LErrors: TDictionary<string, string>;
begin
  LUser := TUserModel.Create;
  try
    LUser.Name := '';
    LUser.Email := 'test@test.com';

    Assert.IsFalse(TMVCValidationEngine.Validate(LUser, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Name'));
    LErrors.Free;
  finally
    LUser.Free;
  end;
end;

procedure TTestValidators.TestRequired_NonEmptyString_ShouldPass;
var
  LUser: TUserModel;
  LErrors: TDictionary<string, string>;
begin
  LUser := TUserModel.Create;
  try
    LUser.Name := 'John';
    LUser.Email := 'john@test.com';
    LUser.Age := 25;

    Assert.IsTrue(TMVCValidationEngine.Validate(LUser, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LUser.Free;
  end;
end;

procedure TTestValidators.TestRequired_NilObject_ShouldFail;
var
  LCustomer: TCustomerModel;
  LErrors: TDictionary<string, string>;
begin
  LCustomer := TCustomerModel.Create;
  try
    LCustomer.Name := 'Test';
    LCustomer.Address.Free;
    LCustomer.FAddress := nil; // Set address to nil

    Assert.IsFalse(TMVCValidationEngine.Validate(LCustomer, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Address'));
    LErrors.Free;
  finally
    LCustomer.Free;
  end;
end;

procedure TTestValidators.TestRequired_ZeroInteger_ShouldPass;
var
  LValidator: MVCRequired;
  LValue: TValue;
begin
  LValidator := MVCRequired.Create;
  try
    LValue := TValue.From<Integer>(0);
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== MVCMinLength/MaxLength Tests ======

procedure TTestValidators.TestMinLength_TooShort_ShouldFail;
var
  LValidator: MVCMinLength;
  LValue: TValue;
begin
  LValidator := MVCMinLength.Create(5);
  try
    LValue := TValue.From<string>('abc');
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMinLength_ExactLength_ShouldPass;
var
  LValidator: MVCMinLength;
  LValue: TValue;
begin
  LValidator := MVCMinLength.Create(5);
  try
    LValue := TValue.From<string>('abcde');
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMaxLength_TooLong_ShouldFail;
var
  LValidator: MVCMaxLength;
  LValue: TValue;
begin
  LValidator := MVCMaxLength.Create(5);
  try
    LValue := TValue.From<string>('abcdefgh');
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== MVCEmail Tests ======

procedure TTestValidators.TestEmail_ValidEmail_ShouldPass;
var
  LValidator: MVCEmail;
  LValue: TValue;
begin
  LValidator := MVCEmail.Create;
  try
    LValue := TValue.From<string>('user@example.com');
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestEmail_InvalidEmail_ShouldFail;
var
  LValidator: MVCEmail;
  LValue: TValue;
begin
  LValidator := MVCEmail.Create;
  try
    LValue := TValue.From<string>('invalid-email');
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== MVCRange Tests ======

procedure TTestValidators.TestRange_WithinRange_ShouldPass;
var
  LValidator: MVCRange;
  LValue: TValue;
begin
  LValidator := MVCRange.Create(18, 120);
  try
    LValue := TValue.From<Integer>(25);
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestRange_BelowMin_ShouldFail;
var
  LValidator: MVCRange;
  LValue: TValue;
begin
  LValidator := MVCRange.Create(18, 120);
  try
    LValue := TValue.From<Integer>(15);
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestRange_AboveMax_ShouldFail;
var
  LValidator: MVCRange;
  LValue: TValue;
begin
  LValidator := MVCRange.Create(18, 120);
  try
    LValue := TValue.From<Integer>(150);
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== MVCPattern Tests ======

procedure TTestValidators.TestPattern_ValidPattern_ShouldPass;
var
  LValidator: MVCPattern;
  LValue: TValue;
begin
  LValidator := MVCPattern.Create('^ORD-\d{8}$');
  try
    LValue := TValue.From<string>('ORD-12345678');
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestPattern_InvalidPattern_ShouldFail;
var
  LValidator: MVCPattern;
  LValue: TValue;
begin
  LValidator := MVCPattern.Create('^ORD-\d{8}$');
  try
    LValue := TValue.From<string>('ORDER-123');
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== MVCPositive Tests ======

procedure TTestValidators.TestPositive_PositiveValue_ShouldPass;
var
  LValidator: MVCPositive;
  LValue: TValue;
begin
  LValidator := MVCPositive.Create;
  try
    LValue := TValue.From<Integer>(10);
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestPositive_ZeroValue_ShouldFail;
var
  LValidator: MVCPositive;
  LValue: TValue;
begin
  LValidator := MVCPositive.Create;
  try
    LValue := TValue.From<Integer>(0);
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestPositive_NegativeValue_ShouldFail;
var
  LValidator: MVCPositive;
  LValue: TValue;
begin
  LValidator := MVCPositive.Create;
  try
    LValue := TValue.From<Integer>(-5);
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== Nested Validation Tests ======

procedure TTestValidators.TestNestedValidation_ValidData_ShouldPass;
var
  LCustomer: TCustomerModel;
  LErrors: TDictionary<string, string>;
begin
  LCustomer := TCustomerModel.Create;
  try
    LCustomer.Name := 'John Doe';
    LCustomer.Address.Street := 'Main Street 123';
    LCustomer.Address.City := 'Rome';
    LCustomer.Address.PostalCode := '00100';

    Assert.IsTrue(TMVCValidationEngine.Validate(LCustomer, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LCustomer.Free;
  end;
end;

procedure TTestValidators.TestNestedValidation_InvalidNested_ShouldFailWithPath;
var
  LCustomer: TCustomerModel;
  LErrors: TDictionary<string, string>;
begin
  LCustomer := TCustomerModel.Create;
  try
    LCustomer.Name := 'John Doe';
    LCustomer.Address.Street := 'abc'; // Too short
    LCustomer.Address.City := '';      // Required
    LCustomer.Address.PostalCode := '123'; // Invalid IT postal code

    Assert.IsFalse(TMVCValidationEngine.Validate(LCustomer, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Address.Street'), 'Should have error for Address.Street');
    Assert.IsTrue(LErrors.ContainsKey('Address.City'), 'Should have error for Address.City');
    Assert.IsTrue(LErrors.ContainsKey('Address.PostalCode'), 'Should have error for Address.PostalCode');
    LErrors.Free;
  finally
    LCustomer.Free;
  end;
end;

// ====== Collection Validation Tests ======

procedure TTestValidators.TestCollectionValidation_ValidItems_ShouldPass;
var
  LOrder: TOrderModel;
  LItem: TOrderItemModel;
  LErrors: TDictionary<string, string>;
begin
  LOrder := TOrderModel.Create;
  try
    LOrder.OrderNumber := 'ORD-12345678';

    LItem := TOrderItemModel.Create;
    LItem.ProductName := 'Widget';
    LItem.Quantity := 5;
    LOrder.Items.Add(LItem);

    Assert.IsTrue(TMVCValidationEngine.Validate(LOrder, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LOrder.Free;
  end;
end;

procedure TTestValidators.TestCollectionValidation_InvalidItem_ShouldFailWithIndex;
var
  LOrder: TOrderModel;
  LItem1, LItem2: TOrderItemModel;
  LErrors: TDictionary<string, string>;
begin
  LOrder := TOrderModel.Create;
  try
    LOrder.OrderNumber := 'ORD-12345678';

    LItem1 := TOrderItemModel.Create;
    LItem1.ProductName := 'Widget';
    LItem1.Quantity := 5;
    LOrder.Items.Add(LItem1);

    LItem2 := TOrderItemModel.Create;
    LItem2.ProductName := '';  // Required!
    LItem2.Quantity := -1;     // Must be positive!
    LOrder.Items.Add(LItem2);

    Assert.IsFalse(TMVCValidationEngine.Validate(LOrder, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Items[1].ProductName'), 'Should have error for Items[1].ProductName');
    Assert.IsTrue(LErrors.ContainsKey('Items[1].Quantity'), 'Should have error for Items[1].Quantity');
    LErrors.Free;
  finally
    LOrder.Free;
  end;
end;

procedure TTestValidators.TestCollectionValidation_EmptyCollection_ShouldFailMinCount;
var
  LOrder: TOrderModel;
  LErrors: TDictionary<string, string>;
begin
  LOrder := TOrderModel.Create;
  try
    LOrder.OrderNumber := 'ORD-12345678';
    // Items list is empty

    Assert.IsFalse(TMVCValidationEngine.Validate(LOrder, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Items'), 'Should have error for Items (min count)');
    LErrors.Free;
  finally
    LOrder.Free;
  end;
end;

// ====== Format Validators Tests ======

procedure TTestValidators.TestCreditCard_ValidLuhn_ShouldPass;
var
  LValidator: MVCCreditCard;
  LValue: TValue;
begin
  LValidator := MVCCreditCard.Create;
  try
    // Valid Visa test card number
    LValue := TValue.From<string>('4111111111111111');
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestCreditCard_InvalidLuhn_ShouldFail;
var
  LValidator: MVCCreditCard;
  LValue: TValue;
begin
  LValidator := MVCCreditCard.Create;
  try
    LValue := TValue.From<string>('4111111111111112'); // Invalid checksum
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestPartitaIVA_Valid_ShouldPass;
var
  LValidator: MVCITPartitaIVA;
  LValue: TValue;
begin
  LValidator := MVCITPartitaIVA.Create;
  try
    LValue := TValue.From<string>('12345678903'); // Valid test P.IVA
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestPartitaIVA_Invalid_ShouldFail;
var
  LValidator: MVCITPartitaIVA;
  LValue: TValue;
begin
  LValidator := MVCITPartitaIVA.Create;
  try
    LValue := TValue.From<string>('12345678901'); // Invalid checksum
    Assert.IsFalse(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestIBAN_Valid_ShouldPass;
var
  LValidator: MVCIBAN;
  LValue: TValue;
begin
  LValidator := MVCIBAN.Create;
  try
    // Valid Italian IBAN
    LValue := TValue.From<string>('IT60X0542811101000000123456');
    Assert.IsTrue(LValidator.Validate(LValue, nil));
  finally
    LValidator.Free;
  end;
end;

// ====== Exception Tests ======

procedure TTestValidators.TestValidateAndRaise_InvalidData_ShouldRaiseException;
var
  LUser: TUserModel;
begin
  LUser := TUserModel.Create;
  try
    LUser.Name := '';
    LUser.Email := 'invalid';

    Assert.WillRaise(
      procedure
      begin
        TMVCValidationEngine.ValidateAndRaise(LUser);
      end,
      EMVCValidationException,
      'Should raise EMVCValidationException for invalid data');
  finally
    LUser.Free;
  end;
end;

procedure TTestValidators.TestValidateAndRaise_ValidData_ShouldNotRaise;
var
  LUser: TUserModel;
begin
  LUser := TUserModel.Create;
  try
    LUser.Name := 'John Doe';
    LUser.Email := 'john@example.com';
    LUser.Age := 25;

    Assert.WillNotRaise(
      procedure
      begin
        TMVCValidationEngine.ValidateAndRaise(LUser);
      end,
      Exception);
  finally
    LUser.Free;
  end;
end;

// ====== Cross-Field Validation Tests ======

procedure TTestValidators.TestCompareField_MatchingValues_ShouldPass;
var
  LModel: TPasswordModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPasswordModel.Create;
  try
    LModel.Password := 'SecurePass123!';
    LModel.ConfirmPassword := 'SecurePass123!';  // Matches!

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestCompareField_DifferentValues_ShouldFail;
var
  LModel: TPasswordModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPasswordModel.Create;
  try
    LModel.Password := 'SecurePass123!';
    LModel.ConfirmPassword := 'DifferentPass!';  // Different!

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('ConfirmPassword'), 'Should have error for ConfirmPassword');
    Assert.AreEqual('Passwords do not match', LErrors['ConfirmPassword']);
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestDifferentFrom_DifferentValues_ShouldPass;
var
  LModel: TPasswordModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPasswordModel.Create;
  try
    LModel.Password := 'NewSecurePass!';
    LModel.ConfirmPassword := 'NewSecurePass!';
    LModel.OldPassword := 'OldPassword123';  // Different from new password

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestDifferentFrom_SameValues_ShouldFail;
var
  LModel: TPasswordModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPasswordModel.Create;
  try
    LModel.Password := 'SamePassword!';
    LModel.ConfirmPassword := 'SamePassword!';
    LModel.OldPassword := 'SamePassword!';  // Same as new password!

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('OldPassword'), 'Should have error for OldPassword');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestRequiredIf_ConditionMet_ValueProvided_ShouldPass;
var
  LModel: TPaymentMethodModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPaymentMethodModel.Create;
  try
    LModel.PaymentMethod := 'CreditCard';
    LModel.CardNumber := '4111111111111111';  // Valid card number provided
    LModel.Amount := 100.00;

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestRequiredIf_ConditionMet_ValueEmpty_ShouldFail;
var
  LModel: TPaymentMethodModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPaymentMethodModel.Create;
  try
    LModel.PaymentMethod := 'CreditCard';
    LModel.CardNumber := '';  // Card number required but empty!
    LModel.Amount := 100.00;

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('CardNumber'), 'Should have error for CardNumber');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestRequiredIf_ConditionNotMet_ValueEmpty_ShouldPass;
var
  LModel: TPaymentMethodModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TPaymentMethodModel.Create;
  try
    LModel.PaymentMethod := 'Cash';  // Not CreditCard
    LModel.CardNumber := '';  // CardNumber not required for Cash
    LModel.Amount := 100.00;

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestDateAfter_ValidDates_ShouldPass;
var
  LModel: TDateRangeModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TDateRangeModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 1, 1);
    LModel.EndDate := EncodeDate(2024, 12, 31);  // After StartDate
    LModel.MinValue := 10;
    LModel.MaxValue := 100;

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestDateAfter_InvalidDates_ShouldFail;
var
  LModel: TDateRangeModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TDateRangeModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 12, 31);
    LModel.EndDate := EncodeDate(2024, 1, 1);  // Before StartDate!
    LModel.MinValue := 10;
    LModel.MaxValue := 100;

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('EndDate'), 'Should have error for EndDate');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestGreaterThanField_Valid_ShouldPass;
var
  LModel: TDateRangeModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TDateRangeModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 1, 1);
    LModel.EndDate := EncodeDate(2024, 12, 31);
    LModel.MinValue := 10;
    LModel.MaxValue := 100;  // Greater than MinValue

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestGreaterThanField_Invalid_ShouldFail;
var
  LModel: TDateRangeModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TDateRangeModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 1, 1);
    LModel.EndDate := EncodeDate(2024, 12, 31);
    LModel.MinValue := 100;
    LModel.MaxValue := 50;  // Less than MinValue!

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('MaxValue'), 'Should have error for MaxValue');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

// ====== OnValidate (Object-Level Validation) Tests ======

procedure TTestValidators.TestOnValidateOnly_ValidData_ShouldPass;
var
  LModel: TOnValidateOnlyModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateOnlyModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 1, 1);
    LModel.EndDate := EncodeDate(2024, 12, 31);  // After start date

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    // When validation passes, LErrors is nil (no allocation)
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateOnly_InvalidData_ShouldFail;
var
  LModel: TOnValidateOnlyModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateOnlyModel.Create;
  try
    LModel.StartDate := EncodeDate(2024, 12, 31);
    LModel.EndDate := EncodeDate(2024, 1, 1);  // Before start date!

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('EndDate'), 'Should have error for EndDate');
    Assert.AreEqual('End date must be after start date', LErrors['EndDate']);
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateOnly_ClassIsValidatable;
begin
  // A class with only OnValidate (no property validators) should be validatable
  Assert.IsTrue(TMVCValidationEngine.IsValidatableClass(TOnValidateOnlyModel));
end;

procedure TTestValidators.TestOnValidateCombined_AllValid_ShouldPass;
var
  LModel: TOnValidateCombinedModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateCombinedModel.Create;
  try
    LModel.Name := 'John';
    LModel.Email := 'john@example.com';
    LModel.MinAmount := 10;
    LModel.MaxAmount := 100;  // Greater than min

    Assert.IsTrue(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsNull(LErrors);
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateCombined_PropertyErrors_ShouldFail;
var
  LModel: TOnValidateCombinedModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateCombinedModel.Create;
  try
    LModel.Name := '';  // Empty - fails MVCRequired
    LModel.Email := 'john@example.com';
    LModel.MinAmount := 10;
    LModel.MaxAmount := 100;  // Valid cross-field

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Name'), 'Should have error for Name');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateCombined_OnValidateErrors_ShouldFail;
var
  LModel: TOnValidateCombinedModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateCombinedModel.Create;
  try
    LModel.Name := 'John';  // Valid
    LModel.Email := 'john@example.com';  // Valid
    LModel.MinAmount := 100;
    LModel.MaxAmount := 50;  // Less than min - fails OnValidate

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('MaxAmount'), 'Should have error for MaxAmount');
    Assert.AreEqual('Max amount must be greater than min amount', LErrors['MaxAmount']);
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateCombined_BothErrors_ShouldAccumulate;
var
  LModel: TOnValidateCombinedModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateCombinedModel.Create;
  try
    LModel.Name := '';  // Empty - fails MVCRequired
    LModel.Email := 'invalid';  // Invalid email
    LModel.MinAmount := 100;
    LModel.MaxAmount := 50;  // Less than min - fails OnValidate

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('Name'), 'Should have error for Name');
    Assert.IsTrue(LErrors.ContainsKey('Email'), 'Should have error for Email');
    Assert.IsTrue(LErrors.ContainsKey('MaxAmount'), 'Should have error for MaxAmount');
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

procedure TTestValidators.TestOnValidateMultipleErrors_ShouldReturnAll;
var
  LModel: TOnValidateMultipleErrorsModel;
  LErrors: TDictionary<string, string>;
begin
  LModel := TOnValidateMultipleErrorsModel.Create;
  try
    LModel.Password := 'secret123';
    LModel.ConfirmPassword := 'different';  // Doesn't match
    LModel.Age := 50;
    LModel.RetirementAge := 40;  // Less than age!

    Assert.IsFalse(TMVCValidationEngine.Validate(LModel, LErrors));
    Assert.IsTrue(LErrors.ContainsKey('ConfirmPassword'), 'Should have error for ConfirmPassword');
    Assert.IsTrue(LErrors.ContainsKey('RetirementAge'), 'Should have error for RetirementAge');
    Assert.AreEqual('Passwords do not match', LErrors['ConfirmPassword']);
    Assert.AreEqual('Retirement age must be greater than current age', LErrors['RetirementAge']);
    LErrors.Free;
  finally
    LModel.Free;
  end;
end;

// ====== Global Validators Tests ======

// Strong Password Tests
procedure TTestValidators.TestStrongPassword_Valid_ShouldPass;
var
  LValidator: MVCStrongPassword;
  LValue: TValue;
begin
  LValidator := MVCStrongPassword.Create(8, 1, 1, 1, 1);
  try
    LValue := TValue.From<string>('MyStr0ng!Pass');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid strong password should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestStrongPassword_TooShort_ShouldFail;
var
  LValidator: MVCStrongPassword;
  LValue: TValue;
begin
  LValidator := MVCStrongPassword.Create(8, 1, 1, 1, 1);
  try
    LValue := TValue.From<string>('Sh0rt!');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Short password should fail');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestStrongPassword_NoUppercase_ShouldFail;
var
  LValidator: MVCStrongPassword;
  LValue: TValue;
begin
  LValidator := MVCStrongPassword.Create(8, 1, 1, 1, 1);
  try
    LValue := TValue.From<string>('alllowercase123!');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Password without uppercase should fail');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestStrongPassword_NoSpecial_ShouldFail;
var
  LValidator: MVCStrongPassword;
  LValue: TValue;
begin
  LValidator := MVCStrongPassword.Create(8, 1, 1, 1, 1);
  try
    LValue := TValue.From<string>('NoSpecial123');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Password without special char should fail');
  finally
    LValidator.Free;
  end;
end;

// Slug Tests
procedure TTestValidators.TestSlug_Valid_ShouldPass;
var
  LValidator: MVCSlug;
  LValue: TValue;
begin
  LValidator := MVCSlug.Create;
  try
    LValue := TValue.From<string>('my-article-title-123');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid slug should pass');

    LValue := TValue.From<string>('simple');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Simple slug should pass');

    LValue := TValue.From<string>('test-123-slug');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Slug with numbers should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestSlug_Invalid_ShouldFail;
var
  LValidator: MVCSlug;
  LValue: TValue;
begin
  LValidator := MVCSlug.Create;
  try
    LValue := TValue.From<string>('Invalid Slug');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Slug with spaces should fail');

    LValue := TValue.From<string>('UPPERCASE');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Slug with uppercase should fail');

    LValue := TValue.From<string>('-leading-hyphen');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Slug with leading hyphen should fail');

    LValue := TValue.From<string>('trailing-hyphen-');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Slug with trailing hyphen should fail');
  finally
    LValidator.Free;
  end;
end;

// Collection Items Tests
procedure TTestValidators.TestMinItems_Valid_ShouldPass;
var
  LValidator: MVCMinCount;
  LValue: TValue;
  LList: TList<Integer>;
begin
  LValidator := MVCMinCount.Create(2);
  try
    LList := TList<Integer>.Create;
    try
      LList.Add(1);
      LList.Add(2);
      LList.Add(3);
      LValue := TValue.From<TObject>(LList);
      Assert.IsTrue(LValidator.Validate(LValue, nil), 'List with 3 items should pass min 2');
    finally
      LList.Free;
    end;
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMinItems_Invalid_ShouldFail;
var
  LValidator: MVCMinCount;
  LValue: TValue;
  LList: TList<Integer>;
begin
  LValidator := MVCMinCount.Create(3);
  try
    LList := TList<Integer>.Create;
    try
      LList.Add(1);
      LValue := TValue.From<TObject>(LList);
      Assert.IsFalse(LValidator.Validate(LValue, nil), 'List with 1 item should fail min 3');
    finally
      LList.Free;
    end;
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMaxItems_Valid_ShouldPass;
var
  LValidator: MVCMaxCount;
  LValue: TValue;
  LList: TList<Integer>;
begin
  LValidator := MVCMaxCount.Create(5);
  try
    LList := TList<Integer>.Create;
    try
      LList.Add(1);
      LList.Add(2);
      LValue := TValue.From<TObject>(LList);
      Assert.IsTrue(LValidator.Validate(LValue, nil), 'List with 2 items should pass max 5');
    finally
      LList.Free;
    end;
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMaxItems_Invalid_ShouldFail;
var
  LValidator: MVCMaxCount;
  LValue: TValue;
  LList: TList<Integer>;
begin
  LValidator := MVCMaxCount.Create(2);
  try
    LList := TList<Integer>.Create;
    try
      LList.Add(1);
      LList.Add(2);
      LList.Add(3);
      LList.Add(4);
      LValue := TValue.From<TObject>(LList);
      Assert.IsFalse(LValidator.Validate(LValue, nil), 'List with 4 items should fail max 2');
    finally
      LList.Free;
    end;
  finally
    LValidator.Free;
  end;
end;

// IPv4 Tests
procedure TTestValidators.TestIPv4_Valid_ShouldPass;
var
  LValidator: MVCIPv4;
  LValue: TValue;
begin
  LValidator := MVCIPv4.Create;
  try
    LValue := TValue.From<string>('192.168.1.1');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid private IP should pass');

    LValue := TValue.From<string>('8.8.8.8');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid public IP should pass');

    LValue := TValue.From<string>('0.0.0.0');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Zero IP should pass');

    LValue := TValue.From<string>('255.255.255.255');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Broadcast IP should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestIPv4_Invalid_ShouldFail;
var
  LValidator: MVCIPv4;
  LValue: TValue;
begin
  LValidator := MVCIPv4.Create;
  try
    LValue := TValue.From<string>('256.1.1.1');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'IP with 256 should fail');

    LValue := TValue.From<string>('192.168.1');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'IP with 3 octets should fail');

    LValue := TValue.From<string>('192.168.01.1');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'IP with leading zero should fail');

    LValue := TValue.From<string>('not.an.ip.addr');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Non-numeric IP should fail');
  finally
    LValidator.Free;
  end;
end;

// MAC Address Tests
procedure TTestValidators.TestMACAddress_Valid_ShouldPass;
var
  LValidator: MVCMACAddress;
  LValue: TValue;
begin
  LValidator := MVCMACAddress.Create;
  try
    LValue := TValue.From<string>('00:1A:2B:3C:4D:5E');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'MAC with colons should pass');

    LValue := TValue.From<string>('00-1A-2B-3C-4D-5E');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'MAC with dashes should pass');

    LValue := TValue.From<string>('001A2B3C4D5E');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'MAC without separators should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestMACAddress_Invalid_ShouldFail;
var
  LValidator: MVCMACAddress;
  LValue: TValue;
begin
  LValidator := MVCMACAddress.Create;
  try
    LValue := TValue.From<string>('00:1A:2B:3C:4D');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'MAC with 5 octets should fail');

    LValue := TValue.From<string>('00:1A:2B:3C:4D:5G');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'MAC with invalid hex should fail');

    LValue := TValue.From<string>('00:1A:2B:3C:4D:5E:6F');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'MAC with 7 octets should fail');
  finally
    LValidator.Free;
  end;
end;

// SemVer Tests
procedure TTestValidators.TestSemVer_Valid_ShouldPass;
var
  LValidator: MVCSemVer;
  LValue: TValue;
begin
  LValidator := MVCSemVer.Create;
  try
    LValue := TValue.From<string>('1.0.0');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Simple version should pass');

    LValue := TValue.From<string>('1.2.3');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Standard version should pass');

    LValue := TValue.From<string>('1.0.0-alpha');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Version with prerelease should pass');

    LValue := TValue.From<string>('1.0.0-alpha+build.123');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Version with prerelease and build should pass');

    LValue := TValue.From<string>('10.20.30');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Multi-digit version should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestSemVer_Invalid_ShouldFail;
var
  LValidator: MVCSemVer;
  LValue: TValue;
begin
  LValidator := MVCSemVer.Create;
  try
    LValue := TValue.From<string>('1.0');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Two part version should fail');

    LValue := TValue.From<string>('v1.0.0');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Version with v prefix should fail');

    LValue := TValue.From<string>('1.0.0.0');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Four part version should fail');

    LValue := TValue.From<string>('01.0.0');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Version with leading zero should fail');
  finally
    LValidator.Free;
  end;
end;

// Latitude Tests
procedure TTestValidators.TestLatitude_Valid_ShouldPass;
var
  LValidator: MVCLatitude;
  LValue: TValue;
begin
  LValidator := MVCLatitude.Create;
  try
    LValue := TValue.From<Double>(0.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Equator (0) should pass');

    LValue := TValue.From<Double>(45.5);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Mid-latitude should pass');

    LValue := TValue.From<Double>(90.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'North pole (90) should pass');

    LValue := TValue.From<Double>(-90.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'South pole (-90) should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestLatitude_Invalid_ShouldFail;
var
  LValidator: MVCLatitude;
  LValue: TValue;
begin
  LValidator := MVCLatitude.Create;
  try
    LValue := TValue.From<Double>(90.1);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Latitude > 90 should fail');

    LValue := TValue.From<Double>(-90.1);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Latitude < -90 should fail');

    LValue := TValue.From<Double>(180.0);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Latitude 180 should fail');
  finally
    LValidator.Free;
  end;
end;

// Longitude Tests
procedure TTestValidators.TestLongitude_Valid_ShouldPass;
var
  LValidator: MVCLongitude;
  LValue: TValue;
begin
  LValidator := MVCLongitude.Create;
  try
    LValue := TValue.From<Double>(0.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Prime meridian (0) should pass');

    LValue := TValue.From<Double>(90.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Longitude 90 should pass');

    LValue := TValue.From<Double>(180.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Longitude 180 should pass');

    LValue := TValue.From<Double>(-180.0);
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Longitude -180 should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestLongitude_Invalid_ShouldFail;
var
  LValidator: MVCLongitude;
  LValue: TValue;
begin
  LValidator := MVCLongitude.Create;
  try
    LValue := TValue.From<Double>(180.1);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Longitude > 180 should fail');

    LValue := TValue.From<Double>(-180.1);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Longitude < -180 should fail');

    LValue := TValue.From<Double>(360.0);
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Longitude 360 should fail');
  finally
    LValidator.Free;
  end;
end;

// Country Code Tests
procedure TTestValidators.TestCountryCode_Valid_ShouldPass;
var
  LValidator: MVCCountryCode;
  LValue: TValue;
begin
  LValidator := MVCCountryCode.Create;
  try
    LValue := TValue.From<string>('IT');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Italy code should pass');

    LValue := TValue.From<string>('US');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'USA code should pass');

    LValue := TValue.From<string>('BR');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Brazil code should pass');

    LValue := TValue.From<string>('de');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Lowercase code should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestCountryCode_Invalid_ShouldFail;
var
  LValidator: MVCCountryCode;
  LValue: TValue;
begin
  LValidator := MVCCountryCode.Create;
  try
    LValue := TValue.From<string>('XX');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid code XX should fail');

    LValue := TValue.From<string>('USA');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Three letter code should fail');

    LValue := TValue.From<string>('I');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'One letter code should fail');
  finally
    LValidator.Free;
  end;
end;

// EAN-13 Tests
procedure TTestValidators.TestEAN13_Valid_ShouldPass;
var
  LValidator: MVCEAN13;
  LValue: TValue;
begin
  LValidator := MVCEAN13.Create;
  try
    // Valid EAN-13: 5901234123457
    LValue := TValue.From<string>('5901234123457');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid EAN-13 should pass');

    // Valid EAN-13: 4006381333931
    LValue := TValue.From<string>('4006381333931');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid EAN-13 should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestEAN13_Invalid_ShouldFail;
var
  LValidator: MVCEAN13;
  LValue: TValue;
begin
  LValidator := MVCEAN13.Create;
  try
    LValue := TValue.From<string>('5901234123458');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid checksum should fail');

    LValue := TValue.From<string>('123456789012');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '12 digit number should fail');

    LValue := TValue.From<string>('12345678901234');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '14 digit number should fail');
  finally
    LValidator.Free;
  end;
end;

// ISBN-10 Tests
procedure TTestValidators.TestISBN10_Valid_ShouldPass;
var
  LValidator: MVCISBN;
  LValue: TValue;
begin
  LValidator := MVCISBN.Create;
  try
    // Valid ISBN-10: 0-306-40615-2
    LValue := TValue.From<string>('0-306-40615-2');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ISBN-10 with dashes should pass');

    // Valid ISBN-10: 0306406152
    LValue := TValue.From<string>('0306406152');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ISBN-10 without dashes should pass');

    // Valid ISBN-10 with X: 080442957X
    LValue := TValue.From<string>('080442957X');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ISBN-10 with X checksum should pass');
  finally
    LValidator.Free;
  end;
end;

// ISBN-13 Tests
procedure TTestValidators.TestISBN13_Valid_ShouldPass;
var
  LValidator: MVCISBN;
  LValue: TValue;
begin
  LValidator := MVCISBN.Create;
  try
    // Valid ISBN-13: 978-0-306-40615-7
    LValue := TValue.From<string>('978-0-306-40615-7');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ISBN-13 with dashes should pass');

    // Valid ISBN-13: 9780306406157
    LValue := TValue.From<string>('9780306406157');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ISBN-13 without dashes should pass');
  finally
    LValidator.Free;
  end;
end;

// ISBN Invalid Tests
procedure TTestValidators.TestISBN_Invalid_ShouldFail;
var
  LValidator: MVCISBN;
  LValue: TValue;
begin
  LValidator := MVCISBN.Create;
  try
    LValue := TValue.From<string>('0306406151');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'ISBN-10 with wrong checksum should fail');

    LValue := TValue.From<string>('9780306406158');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'ISBN-13 with wrong checksum should fail');

    LValue := TValue.From<string>('12345');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short ISBN should fail');
  finally
    LValidator.Free;
  end;
end;

// VIN Tests
procedure TTestValidators.TestVIN_Valid_ShouldPass;
var
  LValidator: MVCVIN;
  LValue: TValue;
begin
  LValidator := MVCVIN.Create;
  try
    // Valid VIN: 11111111111111111
    LValue := TValue.From<string>('11111111111111111');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid all-1s VIN should pass');

    // Valid VIN: 1M8GDM9AXKP042788
    LValue := TValue.From<string>('1M8GDM9AXKP042788');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid VIN should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestVIN_Invalid_ShouldFail;
var
  LValidator: MVCVIN;
  LValue: TValue;
begin
  LValidator := MVCVIN.Create;
  try
    LValue := TValue.From<string>('1234567890123456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '16 char VIN should fail');

    LValue := TValue.From<string>('1M8GDM9AXKP04278I');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'VIN with I should fail');

    LValue := TValue.From<string>('1M8GDM9AXKP04278O');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'VIN with O should fail');

    LValue := TValue.From<string>('1M8GDM9AXKP04278Q');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'VIN with Q should fail');
  finally
    LValidator.Free;
  end;
end;

// BIC Tests
procedure TTestValidators.TestBIC_Valid_ShouldPass;
var
  LValidator: MVCBic;
  LValue: TValue;
begin
  LValidator := MVCBic.Create;
  try
    LValue := TValue.From<string>('DEUTDEFF');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid 8-char BIC should pass');

    LValue := TValue.From<string>('DEUTDEFF500');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid 11-char BIC should pass');

    LValue := TValue.From<string>('BNPAFRPP');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid French BIC should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestBIC_Invalid_ShouldFail;
var
  LValidator: MVCBic;
  LValue: TValue;
begin
  LValidator := MVCBic.Create;
  try
    LValue := TValue.From<string>('DEUT');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short BIC should fail');

    LValue := TValue.From<string>('DEUTDEFF50');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '10-char BIC should fail');

    LValue := TValue.From<string>('1234DEFF');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'BIC starting with numbers should fail');
  finally
    LValidator.Free;
  end;
end;

// EU VAT Number Tests
procedure TTestValidators.TestEUVatNumber_Valid_ShouldPass;
var
  LValidator: MVCEUVatNumber;
  LValue: TValue;
begin
  LValidator := MVCEUVatNumber.Create;
  try
    LValue := TValue.From<string>('IT12345678901');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid Italian VAT should pass');

    LValue := TValue.From<string>('DE123456789');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid German VAT should pass');

    LValue := TValue.From<string>('FR12123456789');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid French VAT should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestEUVatNumber_Invalid_ShouldFail;
var
  LValidator: MVCEUVatNumber;
  LValue: TValue;
begin
  LValidator := MVCEUVatNumber.Create;
  try
    LValue := TValue.From<string>('XX12345678');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid country code should fail');

    LValue := TValue.From<string>('IT123');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short Italian VAT should fail');

    LValue := TValue.From<string>('DE12345');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short German VAT should fail');
  finally
    LValidator.Free;
  end;
end;

// Italian Codice Fiscale Tests
procedure TTestValidators.TestITCodiceFiscale_Valid_ShouldPass;
var
  LValidator: MVCITCodiceFiscale;
  LValue: TValue;
begin
  LValidator := MVCITCodiceFiscale.Create;
  try
    // Valid Codice Fiscale: RSSMRA85M01H501Q (Q is the correct check character)
    LValue := TValue.From<string>('RSSMRA85M01H501Q');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid Codice Fiscale should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestITCodiceFiscale_Invalid_ShouldFail;
var
  LValidator: MVCITCodiceFiscale;
  LValue: TValue;
begin
  LValidator := MVCITCodiceFiscale.Create;
  try
    LValue := TValue.From<string>('RSSMRA85M01H501X');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid check character should fail');

    LValue := TValue.From<string>('RSSMRA85M01H50');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short CF should fail');

    LValue := TValue.From<string>('1234567890123456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'All numeric CF should fail');
  finally
    LValidator.Free;
  end;
end;

// Italian Partita IVA Tests
procedure TTestValidators.TestITPartitaIVA_Valid_ShouldPass;
var
  LValidator: MVCITPartitaIVA;
  LValue: TValue;
begin
  LValidator := MVCITPartitaIVA.Create;
  try
    // Valid P.IVA: 12345678903
    LValue := TValue.From<string>('12345678903');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid Partita IVA should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestITPartitaIVA_Invalid_ShouldFail;
var
  LValidator: MVCITPartitaIVA;
  LValue: TValue;
begin
  LValidator := MVCITPartitaIVA.Create;
  try
    LValue := TValue.From<string>('12345678901');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid checksum should fail');

    LValue := TValue.From<string>('1234567890');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too short P.IVA should fail');

    LValue := TValue.From<string>('123456789012');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Too long P.IVA should fail');
  finally
    LValidator.Free;
  end;
end;

// US SSN Tests
procedure TTestValidators.TestUSSSN_Valid_ShouldPass;
var
  LValidator: MVCUSSSN;
  LValue: TValue;
begin
  LValidator := MVCUSSSN.Create;
  try
    LValue := TValue.From<string>('123-45-6789');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid SSN with dashes should pass');

    LValue := TValue.From<string>('123456789');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid SSN without dashes should pass');

    LValue := TValue.From<string>('001-01-0001');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid SSN with low numbers should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestUSSSN_Invalid_ShouldFail;
var
  LValidator: MVCUSSSN;
  LValue: TValue;
begin
  LValidator := MVCUSSSN.Create;
  try
    LValue := TValue.From<string>('000-12-3456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'SSN with area 000 should fail');

    LValue := TValue.From<string>('666-12-3456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'SSN with area 666 should fail');

    LValue := TValue.From<string>('900-12-3456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'SSN with area 900+ should fail');

    LValue := TValue.From<string>('123-00-3456');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'SSN with group 00 should fail');

    LValue := TValue.From<string>('123-45-0000');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'SSN with serial 0000 should fail');
  finally
    LValidator.Free;
  end;
end;

// US ABA Routing Tests
procedure TTestValidators.TestUSAbaRouting_Valid_ShouldPass;
var
  LValidator: MVCUSAbaRouting;
  LValue: TValue;
begin
  LValidator := MVCUSAbaRouting.Create;
  try
    // Valid ABA: 111000025
    LValue := TValue.From<string>('111000025');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ABA routing should pass');

    // Valid ABA: 121042882
    LValue := TValue.From<string>('121042882');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid ABA routing should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestUSAbaRouting_Invalid_ShouldFail;
var
  LValidator: MVCUSAbaRouting;
  LValue: TValue;
begin
  LValidator := MVCUSAbaRouting.Create;
  try
    LValue := TValue.From<string>('111000026');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid checksum should fail');

    LValue := TValue.From<string>('12345678');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '8 digit routing should fail');

    LValue := TValue.From<string>('1234567890');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '10 digit routing should fail');
  finally
    LValidator.Free;
  end;
end;

// Brazilian CPF Tests
procedure TTestValidators.TestBRCPF_Valid_ShouldPass;
var
  LValidator: MVCBRCPF;
  LValue: TValue;
begin
  LValidator := MVCBRCPF.Create;
  try
    // Valid CPF: 529.982.247-25
    LValue := TValue.From<string>('529.982.247-25');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid CPF with formatting should pass');

    // Valid CPF: 52998224725
    LValue := TValue.From<string>('52998224725');
    Assert.IsTrue(LValidator.Validate(LValue, nil), 'Valid CPF without formatting should pass');
  finally
    LValidator.Free;
  end;
end;

procedure TTestValidators.TestBRCPF_Invalid_ShouldFail;
var
  LValidator: MVCBRCPF;
  LValue: TValue;
begin
  LValidator := MVCBRCPF.Create;
  try
    LValue := TValue.From<string>('529.982.247-26');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'Invalid checksum should fail');

    LValue := TValue.From<string>('111.111.111-11');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'All same digits should fail');

    LValue := TValue.From<string>('000.000.000-00');
    Assert.IsFalse(LValidator.Validate(LValue, nil), 'All zeros should fail');

    LValue := TValue.From<string>('1234567890');
    Assert.IsFalse(LValidator.Validate(LValue, nil), '10 digit CPF should fail');
  finally
    LValidator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestValidators);

end.
