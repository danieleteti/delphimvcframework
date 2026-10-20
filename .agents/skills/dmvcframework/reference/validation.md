# DMVCFramework — Validation

Declarative validators, when they fire, cross-field and object-level checks, storage validation, error format.

---

## Validation

DMVCFramework has a built-in declarative validation system. Validators are attributes placed on class properties.

A `[MVCFromBody]` parameter is **automatically validated before the action runs** when its class either
carries **at least one validator attribute** OR inherits from `TMVCValidatable`
(`TMVCValidationEngine.IsValidatableClass` = `HasValidators or IsValidatable`).
A plain `class` with `[MVCRequired]` on one property is therefore validated — inheriting `TMVCValidatable`
is required **only** to override `OnValidate` for cross-field/object-level checks.
On failure the action is **never executed**: HTTP 422 with a structured error body.

Nested objects and collection properties are validated **recursively**, with dot-path error keys.

Opt out per parameter: `[MVCFromBody(bvDoNotValidate)]` — the enum is `TMVCBodyValidation = (bvValidate, bvDoNotValidate)`, default `bvValidate`.

### Automatic validation — how it works

```
HTTP POST /people   →   deserialize body into TPersonDTO
                    →   class has validator attrs OR is TMVCValidatable?
                    →   run all property validators (recursive on nested objects/lists)
                    →   if any fail: return 422, action never called
                    →   if all pass: call the action
```

### Required units

```delphi
uses
  MVCFramework.Validation,          // TMVCValidatable, EMVCValidationException
  MVCFramework.Validators,          // ALL validator attributes, incl. cross-field MVCCompareField
  MVCFramework.ValidationEngine;    // TMVCValidationEngine (manual validation)
```

### Property validator attributes — full reference

```delphi
// Presence
[MVCRequired]
[MVCRequired('Custom message')]
[MVCNotEmpty('Field must not be blank')]     // allows null but rejects empty/whitespace

// String length
[MVCMinLength(3)]
[MVCMinLength(3, 'Must be at least 3 chars')]
[MVCMaxLength(100, 'Cannot exceed 100 chars')]
[MVCLength(2, 'Must be exactly 2 chars')]    // exact length

// Numeric range
[MVCRange(1, 120, 'Must be between 1 and 120')]
[MVCPositive('Must be > 0')]
[MVCPositiveOrZero('Must be >= 0')]

// String format
[MVCEmail]
[MVCEmail('Invalid email address')]
[MVCUrl('Invalid URL')]
[MVCPattern('^[A-Z]{3}-\d{4}$', 'Must match format XXX-0000')]  // regex
[MVCAlphaNumeric('Only letters and numbers')]

// Collection size
[MVCMinCount(1, 'At least one item required')]
[MVCMaxCount(100, 'Cannot exceed 100 items')]

// Domain-specific
[MVCCreditCard('Invalid credit card number')]  // Luhn check
[MVCIBAN('Invalid IBAN')]
[MVCPostalCode('IT', 'Invalid Italian postal code')]  // country code: IT, US, DE, ...
[MVCITCodiceFiscale('Invalid Italian Tax ID')]

// Cross-field (same unit: MVCFramework.Validators)
[MVCCompareField('Password', 'Passwords do not match')]  // value must equal Password field
```

### Validatable DTO

Inherit from `TMVCValidatable` for HTTP-boundary validation (never persisted directly):

```delphi
uses
  MVCFramework.Validation,
  MVCFramework.Validators,
  MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncCamelCase)]
  TUserRegistration = class(TMVCValidatable)
  private
    FUsername: string;
    FEmail: string;
    FPassword: string;
    FConfirmPassword: string;
    FAge: Integer;
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

    [MVCRequired('Please confirm your password')]
    [MVCCompareField('Password', 'Passwords do not match')]
    property ConfirmPassword: string read FConfirmPassword write FConfirmPassword;

    [MVCRange(18, 120, 'You must be at least 18 years old')]
    property Age: Integer read FAge write FAge;
  end;
```

The action receives `AUser` already validated — no extra code needed:

```delphi
function RegisterUser([MVCFromBody] const AUser: TUserRegistration): IMVCResponse;
begin
  // Reached only if ALL validators passed
  // Save AUser, return 201...
  Result := CreatedResponse('/users/' + newID.ToString);
end;
```

### Object-level cross-field validation (OnValidate)

When validation rules span multiple fields, override `OnValidate` in a `TMVCValidatable` subclass:

```delphi
type
  [MVCNameCase(ncCamelCase)]
  TEventBooking = class(TMVCValidatable)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FMinParticipants: Integer;
    FMaxParticipants: Integer;
  public
    [MVCRequired]
    property StartDate: TDate read FStartDate write FStartDate;
    [MVCRequired]
    property EndDate: TDate read FEndDate write FEndDate;
    [MVCPositiveOrZero]
    property MinParticipants: Integer read FMinParticipants write FMinParticipants;
    [MVCPositive]
    property MaxParticipants: Integer read FMaxParticipants write FMaxParticipants;

    procedure OnValidate(const AErrors: PMVCValidationErrors); override;
  end;

procedure TEventBooking.OnValidate(const AErrors: PMVCValidationErrors);
begin
  if FEndDate < FStartDate then
    AErrors.Add('endDate', 'End date must be after start date');
  if FMaxParticipants < FMinParticipants then
    AErrors.Add('maxParticipants', 'Max participants must be >= min participants');
  // Add any other cross-field business rules here
end;
```

`OnValidate` runs **after** all property validators. Add errors via `AErrors.Add('fieldName', 'message')`.

### Storage validation (ActiveRecord)

`TMVCActiveRecord` inherits `TMVCValidatable`. Override `OnStorageValidate` for rules that only apply at DB-write time (stricter business invariants, not HTTP-boundary shape checks):

```delphi
[MVCTable('people')]
TPerson = class(TMVCActiveRecord)
private
  [MVCTableField('name')]
  [MVCRequired]
  [MVCMinLength(2)]
  fName: NullableString;

  [MVCTableField('email')]
  [MVCRequired]
  [MVCEmail]
  fEmail: NullableString;
protected
  procedure OnStorageValidate(const AErrors: PMVCValidationErrors;
    const EntityAction: TMVCEntityAction); override;
public
  property Name: NullableString read fName write fName;
  property Email: NullableString read fEmail write fEmail;
end;

procedure TPerson.OnStorageValidate(const AErrors: PMVCValidationErrors;
  const EntityAction: TMVCEntityAction);
begin
  // Business rule enforced only at save time, not at HTTP boundary
  if fEmail.HasValue and (not fEmail.Value.EndsWith('@example.com')) then
    AErrors.Add('email', 'email domain must be @example.com');
end;
```

`OnStorageValidate` runs inside `Insert` / `Update`. Failure raises `EMVCStorageValidationException` (a subclass of `EMVCValidationException`) → automatically serialized as 422.

### DTO + ActiveRecord pattern (recommended two-layer approach)

```
HTTP boundary              Storage layer
TPersonDTO                 TPerson (ActiveRecord)
  [MVCFromBody]              .Insert / .Update
  TMVCValidatable            TMVCActiveRecord
  property validators        field validators
  OnValidate                 OnStorageValidate
  → EMVCValidationException  → EMVCStorageValidationException
  → 422 (before action)      → 422 (inside action)
```

```delphi
// DTO — loose shape check at HTTP boundary
[MVCNameCase(ncCamelCase)]
TPersonDTO = class(TMVCValidatable)
  [MVCRequired('name is required')]
  property Name: string ...;
  [MVCRequired('email is required')]
  property Email: string ...;
end;

// AR — strict business rules at storage
[MVCTable('people')]
TPerson = class(TMVCActiveRecord)
  [MVCTableField('name')]
  [MVCRequired][MVCMinLength(2)]
  fName: NullableString;
  [MVCTableField('email')]
  [MVCRequired][MVCEmail]
  fEmail: NullableString;
  procedure OnStorageValidate(...); override;
end;

// Controller — DTO at HTTP boundary, AR at storage
function CreatePerson([MVCFromBody] aDTO: TPersonDTO): TPerson;
begin
  // DTO validators already passed; map to AR and save
  Result := TPerson.Create;
  try
    Result.Name := aDTO.Name;
    Result.Email := aDTO.Email;
    Result.Insert; // AR validators + OnStorageValidate run here
  except
    Result.Free;
    raise;
  end;
end;
```

### Manual validation

When you need to validate an object not coming from `[MVCFromBody]`:

```delphi
uses MVCFramework.ValidationEngine;

var
  lErrors: TDictionary<string, string>;
begin
  if not TMVCValidationEngine.Validate(MyObject, lErrors) then
  begin
    try
      // lErrors: field name → error message
      for var lKey in lErrors.Keys do
        WriteLn(lKey + ': ' + lErrors[lKey]);
      Result := UnprocessableContentResponse('Validation failed');
    finally
      lErrors.Free;
    end;
  end
  else
  begin
    // lErrors is nil when validation passes
    Result := OKResponse;
  end;
end;

// Alternatively — validate and raise automatically:
TMVCValidationEngine.ValidateAndRaise(MyObject);
// raises EMVCValidationException → framework returns 422
```

### Validation error response format

`EMVCValidationException` is rendered as a standard `TMVCErrorResponse` — **there is no `errors` object**.
The message lists the failing fields, and each failure appears as a `"field: message"` string in `items`:

```json
{
  "statuscode": 422,
  "message": "Validation failed for fields: email, username",
  "classname": "EMVCValidationException",
  "items": [
    { "message": "email: Please provide a valid email address" },
    { "message": "username: Username must be at least 3 characters" }
  ]
}
```

If you need a different shape (RFC-7807, a field→message map), build it yourself in an exception handler —
do not expect the framework to emit it.

---
