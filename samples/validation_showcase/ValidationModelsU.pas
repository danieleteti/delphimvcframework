// ***************************************************************************
//
// Delphi MVC Framework - Validation Showcase Sample
//
// ***************************************************************************

unit ValidationModelsU;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Serializer.Commons,
  MVCFramework.Validators,
  MVCFramework.Validators.CrossField,
  MVCFramework.ValidationEngine;

type
  /// <summary>
  /// Example: Simple user registration DTO
  /// Shows basic validators: Required, Email, MinLength, Range
  /// Also demonstrates CROSS-FIELD VALIDATION with password confirmation
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TUserRegistration = class
  private
    FUsername: string;
    FEmail: string;
    FPassword: string;
    FConfirmPassword: string;
    FAge: Integer;
    FWebsite: string;
  public
    [MVCRequired('Username is required')]
    [MVCMinLength(3, 'Username must be at least 3 characters')]
    [MVCMaxLength(20, 'Username must be at most 20 characters')]
    [MVCAlphaNumeric('Username must contain only letters and numbers')]
    property Username: string read FUsername write FUsername;

    [MVCRequired('Email is required')]
    [MVCEmail('Please provide a valid email address')]
    property Email: string read FEmail write FEmail;

    [MVCRequired('Password is required')]
    [MVCMinLength(8, 'Password must be at least 8 characters')]
    property Password: string read FPassword write FPassword;

    /// <summary>
    /// Password confirmation - MUST MATCH Password field (Cross-Field Validation)
    /// </summary>
    [MVCRequired('Please confirm your password')]
    [MVCCompareField('Password', 'Passwords do not match')]
    property ConfirmPassword: string read FConfirmPassword write FConfirmPassword;

    [MVCRange(18, 120, 'You must be at least 18 years old')]
    property Age: Integer read FAge write FAge;

    [MVCUrl('Please provide a valid URL')]
    property Website: string read FWebsite write FWebsite;
  end;

  /// <summary>
  /// Example: Address with postal code validation
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TAddress = class
  private
    FStreet: string;
    FCity: string;
    FPostalCode: string;
    FCountry: string;
  public
    [MVCRequired]
    [MVCMinLength(5, 'Street address is too short')]
    property Street: string read FStreet write FStreet;

    [MVCRequired]
    property City: string read FCity write FCity;

    [MVCRequired]
    [MVCPostalCode('IT', 'Invalid Italian postal code (CAP)')]
    property PostalCode: string read FPostalCode write FPostalCode;

    [MVCRequired]
    [MVCLength(2, 'Country code must be 2 characters')]
    property Country: string read FCountry write FCountry;
  end;

  /// <summary>
  /// Example: Customer with nested address (demonstrates recursive validation)
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TCustomer = class
  private
    FName: string;
    FEmail: string;
    FTaxId: string;
    FBillingAddress: TAddress;
    FShippingAddress: TAddress;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCRequired('Customer name is required')]
    [MVCMinLength(2)]
    property Name: string read FName write FName;

    [MVCRequired]
    [MVCEmail]
    property Email: string read FEmail write FEmail;

    [MVCITCodiceFiscale('Invalid Italian Tax ID (Codice Fiscale)')]
    property TaxId: string read FTaxId write FTaxId;

    /// <summary>
    /// Billing address - REQUIRED and VALIDATED RECURSIVELY
    /// </summary>
    [MVCRequired('Billing address is required')]
    property BillingAddress: TAddress read FBillingAddress write FBillingAddress;

    /// <summary>
    /// Shipping address - OPTIONAL but if provided, VALIDATED RECURSIVELY
    /// </summary>
    property ShippingAddress: TAddress read FShippingAddress write FShippingAddress;
  end;

  /// <summary>
  /// Example: Order item in a collection
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TOrderItem = class
  private
    FProductCode: string;
    FProductName: string;
    FQuantity: Integer;
    FUnitPrice: Currency;
  public
    [MVCRequired]
    [MVCPattern('^[A-Z]{3}-\d{4}$', 'Product code must be in format XXX-0000')]
    property ProductCode: string read FProductCode write FProductCode;

    [MVCRequired]
    [MVCMinLength(2)]
    property ProductName: string read FProductName write FProductName;

    [MVCRequired]
    [MVCPositive('Quantity must be positive')]
    property Quantity: Integer read FQuantity write FQuantity;

    [MVCRequired]
    [MVCPositiveOrZero('Unit price cannot be negative')]
    property UnitPrice: Currency read FUnitPrice write FUnitPrice;
  end;

  /// <summary>
  /// Example: Order with collection of items (demonstrates collection validation)
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TOrder = class
  private
    FOrderNumber: string;
    FCustomerEmail: string;
    FItems: TObjectList<TOrderItem>;
    FNotes: string;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCRequired]
    [MVCPattern('^ORD-\d{8}$', 'Order number must be in format ORD-00000000')]
    property OrderNumber: string read FOrderNumber write FOrderNumber;

    [MVCRequired]
    [MVCEmail]
    property CustomerEmail: string read FCustomerEmail write FCustomerEmail;

    /// <summary>
    /// Order items - Must have at least 1 item, max 100 items
    /// Each item is validated recursively!
    /// </summary>
    [MVCRequired]
    [MVCMinCount(1, 'Order must contain at least one item')]
    [MVCMaxCount(100, 'Order cannot contain more than 100 items')]
    property Items: TObjectList<TOrderItem> read FItems;

    [MVCMaxLength(500, 'Notes cannot exceed 500 characters')]
    property Notes: string read FNotes write FNotes;
  end;

  /// <summary>
  /// Example: Payment information with credit card validation
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TPaymentInfo = class
  private
    FCardNumber: string;
    FCardHolder: string;
    FExpiryMonth: Integer;
    FExpiryYear: Integer;
    FCVV: string;
    FIBAN: string;
  public
    [MVCRequired]
    [MVCCreditCard('Invalid credit card number')]
    property CardNumber: string read FCardNumber write FCardNumber;

    [MVCRequired]
    [MVCMinLength(2)]
    property CardHolder: string read FCardHolder write FCardHolder;

    [MVCRequired]
    [MVCRange(1, 12, 'Expiry month must be between 1 and 12')]
    property ExpiryMonth: Integer read FExpiryMonth write FExpiryMonth;

    [MVCRequired]
    [MVCRange(2024, 2050, 'Invalid expiry year')]
    property ExpiryYear: Integer read FExpiryYear write FExpiryYear;

    [MVCRequired]
    [MVCPattern('^\d{3,4}$', 'CVV must be 3 or 4 digits')]
    property CVV: string read FCVV write FCVV;

    [MVCIBAN('Invalid IBAN')]
    property IBAN: string read FIBAN write FIBAN;
  end;

  /// <summary>
  /// Example: Event booking with OBJECT-LEVEL VALIDATION (OnValidate)
  /// Demonstrates cross-field validation using OnValidate method.
  /// This approach is useful for complex business rules that involve multiple fields.
  /// </summary>
  [MVCNameCase(ncCamelCase)]
  TEventBooking = class
  private
    FEventName: string;
    FStartDate: TDate;
    FEndDate: TDate;
    FMinParticipants: Integer;
    FMaxParticipants: Integer;
    FBudgetMin: Currency;
    FBudgetMax: Currency;
  public
    [MVCRequired('Event name is required')]
    [MVCMinLength(3, 'Event name must be at least 3 characters')]
    property EventName: string read FEventName write FEventName;

    [MVCRequired('Start date is required')]
    property StartDate: TDate read FStartDate write FStartDate;

    [MVCRequired('End date is required')]
    property EndDate: TDate read FEndDate write FEndDate;

    [MVCPositiveOrZero('Min participants cannot be negative')]
    property MinParticipants: Integer read FMinParticipants write FMinParticipants;

    [MVCPositive('Max participants must be positive')]
    property MaxParticipants: Integer read FMaxParticipants write FMaxParticipants;

    [MVCPositiveOrZero('Budget min cannot be negative')]
    property BudgetMin: Currency read FBudgetMin write FBudgetMin;

    [MVCPositiveOrZero('Budget max cannot be negative')]
    property BudgetMax: Currency read FBudgetMax write FBudgetMax;

    /// <summary>
    /// Object-level validation for complex cross-field rules.
    /// Called automatically after property validators.
    /// </summary>
    procedure OnValidate(const AErrors: PMVCValidationErrors);
  end;

implementation

{ TCustomer }

constructor TCustomer.Create;
begin
  inherited;
  FBillingAddress := TAddress.Create;
  FShippingAddress := nil; // Optional
end;

destructor TCustomer.Destroy;
begin
  FBillingAddress.Free;
  FShippingAddress.Free;
  inherited;
end;

{ TOrder }

constructor TOrder.Create;
begin
  inherited;
  FItems := TObjectList<TOrderItem>.Create(True);
end;

destructor TOrder.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TEventBooking }

procedure TEventBooking.OnValidate(const AErrors: PMVCValidationErrors);
begin
  // Cross-field validation: End date must be after start date
  if FEndDate < FStartDate then
    AErrors.Add('EndDate', 'End date must be after or equal to start date');

  // Cross-field validation: Max participants must be >= Min participants
  if FMaxParticipants < FMinParticipants then
    AErrors.Add('MaxParticipants', 'Max participants must be greater than or equal to min participants');

  // Cross-field validation: Budget max must be >= Budget min
  if FBudgetMax < FBudgetMin then
    AErrors.Add('BudgetMax', 'Budget max must be greater than or equal to budget min');

  // Complex business rule: If event is longer than 7 days, require at least 10 participants
  if (FEndDate - FStartDate > 7) and (FMinParticipants < 10) then
    AErrors.Add('MinParticipants', 'Events longer than 7 days require at least 10 participants');
end;

end.
