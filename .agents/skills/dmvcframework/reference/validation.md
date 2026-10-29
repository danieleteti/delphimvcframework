# DMVCFramework — Validation

Declarative validators, when they fire, error keys, the 422 body (controller and Minimal API), manual
validation, HTML form posts, custom validators, cross-field rules, records, ActiveRecord storage validation.

---

## Units

```delphi
uses
  MVCFramework.Validation,          // TMVCValidatable, TMVCValidatorBase, PMVCValidationErrors,
                                    // EMVCValidationException, EMVCStorageValidationException
  MVCFramework.Validators,          // EVERY validator attribute, cross-field ones included
  MVCFramework.ValidationEngine;    // TMVCValidationEngine (manual validation)
```

There is no other validator unit (no `MVCFramework.Validators.CrossField`).

---

## How the engine reads a class — the rules that bite

`TMVCValidationEngine` (used by `[MVCFromBody]`, Minimal API class binding, `Validate`, `ValidateAndRaise`):

1. **Only public/published *properties* are validated.** Attributes on private fields are ignored by the
   engine (they are read only by `TMVCActiveRecord` at save time, see below). Put validators on properties.
2. **Every validator except `MVCRequired` passes an empty value.** `''`, a nil object, an empty `TValue` →
   `MVCEmail`, `MVCMinLength`, `MVCPattern`, `MVCRange`… all return True. An optional field = no
   `MVCRequired`; a mandatory one = `MVCRequired` plus the format validators.
3. **`MVCRequired` checks presence, not content.** Strings: not `''` (whitespace passes — add `MVCNotEmpty`).
   Objects/interfaces: not nil. `NullableXxx`: `HasValue`. **Integers, floats, dates, booleans: always pass**
   (0 is a value) — use `MVCRange`, `MVCPositive`, … or a `NullableXxx` property.
4. **`NullableXxx` properties: only `MVCRequired` understands them.** The engine passes the Nullable record
   as-is; string/numeric validators see a record and pass. `[MVCEmail]` on a `NullableString` *property*
   checks nothing. In an input DTO use plain `string`/`Integer` for any field with format/range rules.
   (ActiveRecord *fields* are different: the storage path unwraps Nullables — see below.)
5. **Nested objects and lists are validated recursively.** A class-typed property is followed; one with
   `Count` + `Items[]` (e.g. `TObjectList<T>`) has each item validated.
6. **Error keys are Delphi property names**, not the serialized JSON names: `Email`, `Address.Street`,
   `Items[1].ProductName`. **One message per key** — the first failing validator (declaration order) wins.
7. **`OnValidate` runs once, on the root object only**, after all attribute validators; it cannot overwrite a
   key that an attribute already set.
8. A class that declares `OnValidate` **without** inheriting `TMVCValidatable` raises `EMVCException` on
   first validation — inherit `TMVCValidatable`.
9. A class is "validatable" when it has ≥1 validator attribute **or** inherits `TMVCValidatable`
   (`TMVCValidationEngine.IsValidatableClass`). Plain classes with attributes work; `TMVCValidatable` is
   needed only for `OnValidate`.

---

## Validator attributes — complete list

All live in `MVCFramework.Validators`. Every constructor ends with an optional `AMessage: string = ''`;
empty means the default message shown. Messages are constant strings in the attribute: there is no built-in
localization.

### Presence, length, pattern, set

| Attribute | Parameters | Default message / notes |
|-----------|-----------|-------------------------|
| `MVCRequired` | `(AMessage)` | `Field is required` — see rule 3 |
| `MVCNotEmpty` | `(AMessage)` | `Field cannot be empty or contain only whitespace`; strings only |
| `MVCMinLength` | `(AMinLength: Integer; AMessage)` | `Must be at least %d characters` |
| `MVCMaxLength` | `(AMaxLength: Integer; AMessage)` | `Must be at most %d characters` |
| `MVCLength` | `(AExactLength: Integer; AMessage)` | `Must be exactly %d characters` |
| `MVCPattern` | `(APattern: string; AMessage)` | regex (`TRegEx.IsMatch`); `Value does not match the required pattern` |
| `MVCIn` | `(AAllowedValues: string; AMessage)` | comma-separated list, items trimmed; **case-insensitive** (`SameText`); works on strings, integers, enums (by name). `Value must be one of: …` |

### Numbers

| Attribute | Parameters | Notes |
|-----------|-----------|-------|
| `MVCRange` | `(AMinValue, AMaxValue: Int64; AMessage)` | inclusive; integer bounds, applies to integer and float properties |
| `MVCPositive` / `MVCPositiveOrZero` | `(AMessage)` | `> 0` / `>= 0` |
| `MVCNegative` / `MVCNegativeOrZero` | `(AMessage)` | `< 0` / `<= 0` |
| `MVCDecimalPrecision` | `(APrecision, AScale: Integer; AMessage)` | total digits / decimal places |
| `MVCDivisibleBy` | `(ADivisor: Integer; AMessage)` | |

### Dates (`TDate`/`TDateTime` properties)

| Attribute | Parameters | Notes |
|-----------|-----------|-------|
| `MVCPast` / `MVCPastOrPresent` | `(AMessage)` | |
| `MVCFuture` / `MVCFutureOrPresent` | `(AMessage)` | |
| `MVCAge` | `(AMinAge, AMaxAge: Integer; AMessage)` | on a birth date |

### String content

| Attribute | Parameters |
|-----------|-----------|
| `MVCAlpha`, `MVCAlphaNumeric`, `MVCLowercase`, `MVCUppercase`, `MVCSlug` | `(AMessage)` |
| `MVCContains`, `MVCNotContains` | `(const ASubstring: string; ACaseSensitive: Boolean = True; AMessage)` |
| `MVCStartsWith` | `(const APrefix: string; ACaseSensitive: Boolean = True; AMessage)` |
| `MVCEndsWith` | `(const ASuffix: string; ACaseSensitive: Boolean = True; AMessage)` |

### Collections

| Attribute | Parameters | Notes |
|-----------|-----------|-------|
| `MVCMinCount` | `(AMinCount: Integer; AMessage)` | nil list counts as 0 |
| `MVCMaxCount` | `(AMaxCount: Integer; AMessage)` | |
| `MVCDistinct` | `(AMessage)` | unique values |

### Formats

| Attribute | Parameters | Notes |
|-----------|-----------|-------|
| `MVCEmail`, `MVCUrl`, `MVCUUID`, `MVCBase64`, `MVCJson`, `MVCHexadecimal`, `MVCSemVer` | `(AMessage)` | |
| `MVCIPAddress` | `(AVersion: TIPVersion = ipAny; AMessage)` | `TIPVersion = (ipv4, ipv6, ipAny)` |
| `MVCIPv4`, `MVCMACAddress` | `(AMessage)` | |
| `MVCPhone` | `(const AFormat: string = 'international'; AMessage)` | `'it'`, `'us'`, anything else = international (`+` and 7–15 digits) |
| `MVCPostalCode` | `(const ACountryCode: string; AMessage)` | e.g. `'IT'`, `'US'`, `'UK'`, `'DE'`, `'FR'`, `'ES'`, `'CH'`, `'AT'`… |
| `MVCLatitude`, `MVCLongitude`, `MVCCountryCode` | `(AMessage)` | country = ISO 3166-1 alpha-2 |
| `MVCCreditCard` (Luhn), `MVCIBAN`, `MVCBic` | `(AMessage)` | |
| `MVCEAN13`, `MVCISBN`, `MVCVIN` | `(AMessage)` | |
| `MVCStrongPassword` | `(AMinLength = 8; AMinUppercase = 1; AMinLowercase = 1; AMinDigits = 1; AMinSpecial = 1; AMessage)` | all `Integer` |
| `MVCEUVatNumber`, `MVCITCodiceFiscale`, `MVCITPartitaIVA`, `MVCUSSSN`, `MVCUSAbaRouting`, `MVCBRCPF` | `(AMessage)` | national IDs |

### Cross-field (other field named by its Delphi property name)

| Attribute | Parameters | Meaning |
|-----------|-----------|---------|
| `MVCCompareField` | `(const AOtherFieldName: string; AMessage)` | equal to the other field (password confirmation) |
| `MVCDifferentFrom` | `(const AOtherFieldName: string; AMessage)` | different from it |
| `MVCLessThanField` / `MVCGreaterThanField` | `(const AOtherFieldName: string; AOrEqual: Boolean = False; AMessage)` | numeric |
| `MVCDateBefore` / `MVCDateAfter` | `(const AOtherFieldName: string; AOrEqual: Boolean = False; AMessage)` | dates |
| `MVCRequiredIf` | `(const AOtherFieldName: string; AOperator: TRequiredIfOperator = roIsNotEmpty; const AExpectedValue: string = ''; AMessage)` | required when the condition on the other field holds |
| `MVCProhibitedIf` | same as `MVCRequiredIf` | must be empty when the condition holds |

`TRequiredIfOperator = (roEquals, roNotEquals, roIsNotEmpty, roIsEmpty, roGreaterThan, roLessThan, roIn, roNotIn)`
— `roIn`/`roNotIn` take a comma-separated `AExpectedValue`.

Cross-field validators need the owning object: they work on classes, **not on records** (see Records).

---

## Where validation runs automatically

| Input | Validated? | On failure |
|-------|-----------|------------|
| Controller `[MVCFromBody]` class param | Yes, if validatable (rule 9), before the action | `EMVCValidationException` → 422 |
| Controller `[MVCFromBody]` list param (`TObjectList<T>`) | **No** — the list class is not validatable. Wrap it: a DTO with a `TObjectList<T>` property (+ `MVCMinCount`) validates every item | — |
| Controller `[MVCFromQueryString]` / `[MVCFromContentField]` / route params | **No** (scalars) — check them in the action, or validate a model manually | — |
| Minimal API class arg (body on POST/PUT/PATCH, query string on GET/DELETE) | Yes, if validatable | 422 |
| Minimal API record arg (fields bound from route/query/`[MVCFromQueryString]`/`[MVCFromContentField]`) | Yes, fields with validator attributes | 422 |
| HTML form post read from `Context.Request.ContentFields` | **No** — validate manually (below) | your choice |
| `TMVCActiveRecord.Insert` / `Update` (and `Store`, which calls them) | Yes — field validators + `OnStorageValidate` | `EMVCStorageValidationException` → 422 |

Opt out per parameter: `[MVCFromBody(bvDoNotValidate)]` — `TMVCBodyValidation = (bvValidate, bvDoNotValidate)`,
default `bvValidate`.

---

## Input DTO (controller)

```delphi
uses
  MVCFramework.Validation, MVCFramework.Validators, MVCFramework.Serializer.Commons;

type
  [MVCNameCase(ncCamelCase)]
  TUserRegistration = class(TMVCValidatable)   // TMVCValidatable only because of OnValidate below
  private
    FUsername: string;
    FEmail: string;
    FPassword: string;
    FConfirmPassword: string;
    FAge: Integer;
  public
    [MVCRequired('Username is required')]
    [MVCMinLength(3)]
    [MVCMaxLength(20)]
    [MVCAlphaNumeric]
    property Username: string read FUsername write FUsername;

    [MVCRequired('Email is required')]
    [MVCEmail('Please provide a valid email address')]
    property Email: string read FEmail write FEmail;

    [MVCRequired]
    [MVCStrongPassword(10)]
    property Password: string read FPassword write FPassword;

    [MVCRequired('Please confirm your password')]
    [MVCCompareField('Password', 'Passwords do not match')]
    property ConfirmPassword: string read FConfirmPassword write FConfirmPassword;

    [MVCRange(18, 120, 'You must be at least 18 years old')]   // MVCRequired would be useless on Integer
    property Age: Integer read FAge write FAge;

    procedure OnValidate(const AErrors: PMVCValidationErrors); override;
  end;

procedure TUserRegistration.OnValidate(const AErrors: PMVCValidationErrors);
begin
  if SameText(FUsername, 'admin') then
    AErrors.Add('Username', 'This username is reserved');   // key = property name, like the attributes
end;
```

```delphi
[MVCPath('/users')]
[MVCHTTPMethod([httpPOST])]
function RegisterUser([MVCFromBody] const AUser: TUserRegistration): IMVCResponse;
begin
  // reached only when every validator and OnValidate passed; the framework frees AUser
  ...
  Result := CreatedResponse('/users/' + lNewID.ToString);
end;
```

`PMVCValidationErrors` is a pointer to the `TMVCValidationErrors` record: `Add(AFieldPath, AMessage)`,
`HasErrors`. The keys you pass are stored verbatim — use the property name so all keys look alike (the
samples do: `AErrors.Add('EndDate', ...)` in `samples/validation_showcase/ValidationModelsU.pas`).

---

## The 422 response

### Controllers — `TMVCErrorResponse`

Nothing to write: let `EMVCValidationException` propagate. It carries 422; the engine renders it as a
`TMVCErrorResponse`. **There is no `errors` object** — each failure is one `"Key: message"` string in `items`:

```json
{
  "statuscode": 422,
  "message": "Validation failed for fields: Email, Username",
  "classname": "EMVCValidationException",
  "items": [
    { "message": "Email: Please provide a valid email address" },
    { "message": "Username: Must be at least 3 characters" }
  ]
}
```

`classname` is filled only in DEBUG builds (empty in release). The key order follows a dictionary: do not
rely on it.

### Minimal API — RFC 7807 ProblemDetails with an `errors` member

The Minimal API dispatcher renders any `EMVCException` through `ProblemDetails(...)`; for an
`EMVCValidationException` it adds the RFC 7807 extension member `errors` (property name → message):

```json
{
  "type": "about:blank",
  "title": "Unprocessable Content",
  "status": 422,
  "detail": "Validation failed for fields: Email, Username",
  "instance": "/api/users",
  "errors": {
    "Email": "Please provide a valid email address",
    "Username": "Must be at least 3 characters"
  }
}
```

`errors` arrived after 3.5.0-rc7. On rc7 and earlier the body stops at `instance`: the client learns *which*
fields failed, not *why* — validate manually and return your own body (next section) when that matters.
Binding failures are **400**, from two different exceptions: `EMVCMinimalAPI` for a route or query value
that does not convert (a non-numeric segment), `EMVCException` from the serializer for a malformed JSON
body. An endpoint filter that wants both catches `EMVCException` and checks `HTTPStatusCode`.

### Need a field → message map? Build it

```delphi
uses
  System.Generics.Collections, JsonDataObjects, MVCFramework.ValidationEngine;

function ValidationErrorsResponse(const AErrors: TDictionary<string, string>): IMVCResponse;
var
  lJSON: TJsonObject;
  lPair: TPair<string, string>;
begin
  lJSON := TJsonObject.Create;
  for lPair in AErrors do
    lJSON.O['errors'].S[lPair.Key] := lPair.Value;
  Result := UnprocessableEntity(lJSON);   // MVCFramework.pas; Owns = True: the response frees lJSON
end;
```

An `IMVCResponse` wraps its body in `data`: the client receives `{"data": {"errors": {...}}}`, not the
flat `errors` of the Minimal API ProblemDetails above.

---

## Manual validation

```delphi
class function TMVCValidationEngine.Validate(const AObject: TObject;
  out AErrors: TDictionary<string, string>): Boolean;     // True = valid, AErrors = nil
class procedure TMVCValidationEngine.ValidateAndRaise(const AObject: TObject);  // raises EMVCValidationException
```

`Validate` allocates the dictionary **only on failure**; the caller frees it.

```delphi
var
  lErrors: TDictionary<string, string>;
begin
  if not TMVCValidationEngine.Validate(lOrder, lErrors) then
  try
    Exit(ValidationErrorsResponse(lErrors));    // or log, or copy into a form error map
  finally
    lErrors.Free;
  end;
  // valid: lErrors is nil, nothing to free
end;
```

Use it when the object did not come from `[MVCFromBody]`: assembled from form fields, query parameters,
several sources, or when you passed `bvDoNotValidate` to patch the object first. `ValidateAndRaise` is the
one-liner when the standard 422 body is fine. You can also raise directly:
`raise EMVCValidationException.Create('Email', 'Already registered');` (field, message) or
`EMVCValidationException.Create(ADictionary)` (copies the dictionary).

---

## HTML form post — validate and re-render with errors

Form fields are not bound to a model: copy them into an object, run the engine, re-render the same page with
what was typed and one message per field. The framework sample `samples/webapp_htmx_forms` does exactly this
(`ReadPerson` in `PeopleSampleU.pas`, called by `Controllers.PeoplePagesU`).

Conventions of the wizard's forms library (`lib/forms_bootstrap5.tpro`, macros `f.input`, `f.select`, …):
the template reads the values from `formModel` and the messages from `formErrors`, both looked up by the
control's `name` — **lowercase** — so the error keys must be lowercase too. The engine returns property
names (`Email`), hence the `LowerCase` below. `Context.Request.ContentFields` keys are already lowercase.

```delphi
type
  TContactForm = class
  private
    fName, fEmail: string;
    fQuantity: Integer;
  public
    [MVCRequired('Enter the name')]
    [MVCMaxLength(60, 'Keep the name under 60 characters')]
    property Name: string read fName write fName;
    [MVCRequired('Enter the email address')]
    [MVCEmail('Enter a valid email address')]
    property Email: string read fEmail write fEmail;
    [MVCRange(1, 99, 'Enter a number from 1 to 99')]
    property Quantity: Integer read fQuantity write fQuantity;
  end;

// Copies the posted fields into AModel; fills AErrors (lowercase field name -> message)
procedure ReadContact(AModel: TContactForm; const AFields, AErrors: TDictionary<string, string>);
var
  lText: string;
  lQuantity: Integer;
  lErrors: TDictionary<string, string>;
  lPair: TPair<string, string>;
begin
  if AFields.TryGetValue('name', lText) then AModel.Name := lText.Trim;
  if AFields.TryGetValue('email', lText) then AModel.Email := lText.Trim;
  // a value that does not even parse gets its own message; the validator would only see 0
  if AFields.TryGetValue('quantity', lText) and TryStrToInt(lText.Trim, lQuantity) then
    AModel.Quantity := lQuantity
  else
    AErrors.AddOrSetValue('quantity', 'Enter a whole number');
  if not TMVCValidationEngine.Validate(AModel, lErrors) then
  try
    for lPair in lErrors do
      if not AErrors.ContainsKey(LowerCase(lPair.Key)) then   // a parse error keeps its message
        AErrors.Add(LowerCase(lPair.Key), lPair.Value);
  finally
    lErrors.Free;
  end;
end;
```

Controller action (`TMVCHTMLResponse`, as the generated People controller does):

```delphi
type
  TContactPagesController = class(TMVCController)
  public
    [MVCPath('/contacts/new')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function PostContact: IMVCResponse;
  end;

function TContactPagesController.PostContact: IMVCResponse;
var
  lModel: TContactForm;
  lErrors: TDictionary<string, string>;
  lPage: TMVCHTMLResponse;
begin
  lModel := TContactForm.Create;
  lErrors := TDictionary<string, string>.Create;
  try
    ReadContact(lModel, Context.Request.ContentFields, lErrors);
    if lErrors.Count = 0 then
    begin
      // save, then Post/Redirect/Get
      Exit(RedirectResponse('/web/contacts'));
    end;
    lPage := TMVCHTMLResponse.Create;
    Result := lPage;                         // owned by the interface from here
    lPage.StatusCode := HTTP_STATUS.UnprocessableEntity;
    ViewData['formModel'] := Context.Request.ContentFields;   // re-show exactly what was typed
    ViewData['formErrors'] := lErrors;
    lPage.HTMLBody := RenderView('contacts/edit');
  finally
    lErrors.Free;                            // ViewData owns nothing; rendering is already done
    lModel.Free;
  end;
end;
```

Minimal API (`.AsWeb` group):

```delphi
lContacts.MapPost<TWebContext>('/new',
  function (Ctx: TWebContext): IMVCResponse
  var
    lModel: TContactForm;
    lErrors: TDictionary<string, string>;
  begin
    lModel := TContactForm.Create;
    lErrors := TDictionary<string, string>.Create;
    try
      ReadContact(lModel, Ctx.Request.ContentFields, lErrors);
      if lErrors.Count = 0 then
        Exit(Redirect('/web/contacts'));
      ViewData['formModel'] := Ctx.Request.ContentFields;
      ViewData['formErrors'] := lErrors;
      Result := RenderView('contacts/edit');
      Result.StatusCode := HTTP_STATUS.UnprocessableEntity;
    finally
      lErrors.Free;
      lModel.Free;
    end;
  end);
```

Why `formModel := ContentFields` on failure: an invalid value that did not parse (`"abc"` in a number field)
is shown back as typed, instead of the model's `0`. Put `novalidate` on the `<form>` if you want the server's
messages rather than the browser's.

---

## Custom validator

Subclass `TMVCValidatorBase` (a `TCustomAttribute`), override `Validate`, set `FErrorMessage` in the
constructor. **No registration**: the engine picks up any attribute that `is TMVCValidatorBase`, on DTO
properties and on ActiveRecord fields alike.

```delphi
uses
  System.Rtti, System.SysUtils, System.TypInfo, MVCFramework.Validation;

type
  SkuCodeAttribute = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

constructor SkuCodeAttribute.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid SKU';
end;

function SkuCodeAttribute.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  // follow the built-in convention: empty / non-string values pass (MVCRequired handles presence)
  if AValue.IsEmpty or not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);
  Result := AValue.AsString.StartsWith('SKU-');
end;
```

Use it as `[SkuCode]` or `[SkuCode('Use the SKU-nnnn format')]` (Delphi drops the `Attribute` suffix).
`AObject` is the owning object (nil on records) — read another property through RTTI for a reusable
cross-field validator. For a one-off cross-field rule, `OnValidate` is simpler.

---

## Records (Minimal API)

```delphi
type
  TCreateCustomerReq = record
    [MVCRequired] [MVCMinLength(2)]  FirstName: string;
    [MVCRequired]                    LastName: string;
    [MVCEmail]                       Email: string;
  end;
```

`TMVCValidationEngine.ValidateRecord` checks each **field**; keys are field names. No `OnValidate`, no
nesting, and cross-field validators receive `nil` as the owner (`MVCCompareField` then always passes) — when
a rule involves two fields, use a class.

---

## ActiveRecord (storage validation)

`TMVCActiveRecord` inherits `TMVCValidatable`. At save time `Insert` and `Update` call
`Validate(EntityAction)` which:

- runs the validator attributes on the **mapped fields** (`[MVCTableField]` fields, incl. private ones),
  **unwrapping `NullableXxx`** — so `[MVCEmail]` on a `NullableString` field works here, and
  `[MVCRequired]` fails on a null one;
- skips, on `eaCreate`, the validators on the primary key and on `foAutoGenerated`/`foReadOnly`/`foDoNotInsert` fields;
- calls `OnStorageValidate(AErrors, EntityAction)` (only from `Insert`/`Update`, never at the HTTP boundary);
- raises `EMVCStorageValidationException` (a subclass of `EMVCValidationException`, 422) with all the errors.

Error key = field name **without the leading `f`/`F`** (`fEmail` → `Email`).

Order in `Insert`: `Validate` → `OnValidation(eaCreate)` → audit fields filled → `OnBeforeInsert` →
`OnBeforeInsertOrUpdate`. **Validators see the entity before the `OnBefore*` hooks run** — a value computed
there (a slug, a default) is not there yet: set it before calling `Insert`, or do not put a validator on it.
`Validate` is public: call `lEntity.Validate(eaUpdate)` to check without saving.

`OnValidation(const EntityAction: TMVCEntityAction)` is the older hook (also called on delete with
`eaDelete`); to reject, raise. `EMVCActiveRecordValidationError.Create(PropertyName, Message)` is a **400**,
not a 422 — prefer `OnStorageValidate` + `AErrors.Add` for new code.

```delphi
[MVCNameCase(ncCamelCase)]
[MVCTable('people')]
TPerson = class(TMVCActiveRecord)
private
  [MVCTableField('id', [foPrimaryKey, foAutoGenerated])]
  fID: NullableInt32;
  [MVCTableField('name')]
  [MVCRequired('name is required')]
  [MVCMinLength(2, 'name must be at least 2 chars')]
  fName: NullableString;
  [MVCTableField('email')]
  [MVCRequired('email is required')]
  [MVCEmail('email must be a valid address')]
  fEmail: NullableString;
protected
  procedure OnStorageValidate(const AErrors: PMVCValidationErrors;
    const EntityAction: TMVCEntityAction); override;
public
  property ID: NullableInt32 read fID write fID;
  property Name: NullableString read fName write fName;
  property Email: NullableString read fEmail write fEmail;
end;

procedure TPerson.OnStorageValidate(const AErrors: PMVCValidationErrors;
  const EntityAction: TMVCEntityAction);
begin
  if fEmail.HasValue and (not LowerCase(fEmail.Value).EndsWith('@example.com')) then
    AErrors.Add('email', 'email domain must be @example.com (storage policy)');
end;
```

(From `samples/validation_vs_storage_demo/ModelsU.pas`.)

### Handling the failure

In a controller or a Minimal API handler, **let it propagate**: the 422 is rendered as above. Catch it only to do something else — e.g. a web
form that re-renders with the messages:

```delphi
try
  lPerson.Insert;
except
  on E: EMVCStorageValidationException do
  begin
    for lPair in E.ValidationErrors do        // TDictionary<string, string>, owned by the exception
      lFormErrors.AddOrSetValue(LowerCase(lPair.Key), lPair.Value);
  end;
end;
```

`on E: EMVCValidationException` catches both layers; `EMVCStorageValidationException` only the storage one.

### An entity as `[MVCFromBody]`

Allowed (`samples/validation_vs_storage_demo/ControllerU.pas`, `CreatePersonAR`), but the boundary check
only sees **property** attributes and `OnValidate` — the field validators of the entity fire later, inside
`Insert`, still as a 422. Prefer the two-layer pattern for public APIs (it also prevents mass assignment —
see the `dmvcframework-security` skill):

```delphi
function CreatePerson([MVCFromBody] aDTO: TPersonDTO): TPerson;   // DTO: shape, at the boundary
begin
  Result := TPerson.Create;
  try
    Result.Name := aDTO.Name;
    Result.Email := aDTO.Email;
    Result.Insert;                // entity: field validators + OnStorageValidate
  except
    Result.Free;
    raise;
  end;
end;
```

---

## Testing validation

Assert the status code and, for a controller, the `items` messages (`HTTP_STATUS.UnprocessableEntity`).
The engine is plain code: `TMVCValidationEngine.Validate` on an object in a DUnitX test needs no server
(`unittests/general/TestClient/ValidationTestsU.pas` does exactly this, asserting keys such as
`Address.Street` and `Items[1].ProductName`). See the `dmvcframework-testing` skill.
