// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit FormsDemoU;

// The TemplatePro forms library, end to end.
//
//   GET  /forms   renders templates/pages/forms.html with a default TFormsDemo
//   POST /forms   copies the posted fields into a TFormsDemo, validates it with
//                 the validator attributes on its properties
//                 (TMVCValidationEngine) and either
//                 - re-renders the page with what was typed + one message per
//                   invalid field (HTTP 422), or
//                 - shows the saved values with a success message (HTTP 200)
//
// The forms library (templates/lib/forms_bootstrap5.tpro) reads two variables
// by default:
//   formModel   the values shown in the controls: any object, dataset, JSON
//               object, TStrings or TDictionary<string, ...>
//   formErrors  field name -> message; a non-empty message marks the control
//               is-invalid and prints the message under it
// So the Delphi side only has to put the right objects in ViewData.

interface

uses
  System.Generics.Collections,
  MVCFramework.MinimalAPI,
  MVCFramework.Validators;

const
  COUNTRY_CODES = 'IT,DE,FR,ES,BR'; // the codes of Countries, for MVCIn

type
  // One option of the "country" select: the template picks the members to use
  // with valueprop="Code" textprop="Name".
  TCountry = class
  private
    FCode: string;
    FName: string;
  public
    constructor Create(const ACode, AName: string);
    property Code: string read FCode;
    property Name: string read FName;
  end;

  // The model behind the form: one property per kind of control, and the
  // rules as validator attributes (MVCFramework.Validators).
  // Property names are matched case-insensitively by the macros, so the
  // template can use the lowercase names that Request.ContentFields uses.
  TFormsDemo = class
  private
    FId: Integer;
    FFullName: string;
    FEmail: string;
    FBirthDate: TDate;
    FQuantity: Integer;
    FCountry: string;
    FSubscribed: Boolean;
    FNotes: string;
  public
    constructor Create;
    // No setter: f.auto() marks it ReadOnly, and the POST never changes it
    property Id: Integer read FId;
    [MVCRequired('Enter between 2 and 60 characters')]
    [MVCMinLength(2, 'Enter between 2 and 60 characters')]
    [MVCMaxLength(60, 'Enter between 2 and 60 characters')]
    property FullName: string read FFullName write FFullName;     // text
    [MVCRequired('Enter the email address')]
    [MVCEmail('Enter a valid email address')]
    [MVCMaxLength(254)]
    property Email: string read FEmail write FEmail;              // email
    [MVCPastOrPresent('Enter a date that is not in the future')]
    property BirthDate: TDate read FBirthDate write FBirthDate;   // date (TDate -> ftDate)
    [MVCRange(1, 99, 'Enter a whole number between 1 and 99')]
    property Quantity: Integer read FQuantity write FQuantity;    // number
    // A select can be tampered with: accept only one of the offered values
    [MVCRequired('Choose a country')]
    [MVCIn(COUNTRY_CODES, 'Choose a country')]
    property Country: string read FCountry write FCountry;        // select
    property Subscribed: Boolean read FSubscribed write FSubscribed; // checkbox
    [MVCMaxLength(500, 'Keep the notes under 500 characters')]
    property Notes: string read FNotes write FNotes;              // textarea
  end;

function Countries: TObjectList<TCountry>;

/// Copies the posted fields into AModel, validates it and adds one message per
/// invalid field to AErrors. Keys are lowercase, as in Request.ContentFields.
procedure ReadFormsDemo(AModel: TFormsDemo;
  const AFields, AErrors: TDictionary<string, string>);

procedure MapFormsRoutes(const AWeb: TMVCRouteGroup<TObject>);

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Defaults,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.ValidationEngine;

var
  GCountries: TObjectList<TCountry>;

function Countries: TObjectList<TCountry>;
begin
  Result := GCountries;
end;

{ TCountry }

constructor TCountry.Create(const ACode, AName: string);
begin
  inherited Create;
  FCode := ACode;
  FName := AName;
end;

{ TFormsDemo }

constructor TFormsDemo.Create;
begin
  inherited Create;
  FId := 42;
  FFullName := 'Ada Lovelace';
  FEmail := 'ada@example.com';
  FBirthDate := EncodeDate(1990, 12, 10);
  FQuantity := 3;
  FCountry := 'IT';
  FSubscribed := True;
  FNotes := 'Edit any field and submit: the server validates every value.';
end;

procedure ReadFormsDemo(AModel: TFormsDemo;
  const AFields, AErrors: TDictionary<string, string>);

  function Field(const AName: string): string;
  begin
    if not AFields.TryGetValue(AName, Result) then
      Result := '';
  end;

  // Decimal digits only: StrToInt would also take "$1F" or "0x0A"
  function IsWholeNumber(const AText: string): Boolean;
  var
    C: Char;
  begin
    Result := (AText.Length > 0) and (AText.Length <= 9);
    for C in AText do
      if not CharInSet(C, ['0'..'9']) then
        Exit(False);
  end;

var
  lDate: TDateTime;
  lText: string;
  lErrors: TDictionary<string, string>;
  lPair: TPair<string, string>;
begin
  // 1. Copy: only what does not parse needs a message here
  AModel.FullName := Field('fullname').Trim;
  AModel.Email := Field('email').Trim;
  // <input type="date"> posts yyyy-mm-dd, a local date: True reads it as is
  if TryISO8601ToDate(Field('birthdate'), lDate, True) then
    AModel.BirthDate := DateOf(lDate)
  else
    AErrors.AddOrSetValue('birthdate', 'Enter a valid date');
  lText := Field('quantity').Trim;
  if IsWholeNumber(lText) then
    AModel.Quantity := lText.ToInteger
  else
    AErrors.AddOrSetValue('quantity', 'Enter a whole number between 1 and 99');
  AModel.Country := Field('country');
  // The checkbox macro posts "false" from a hidden input, then "true" when
  // the box is ticked: ContentFields keeps the last value of a repeated field
  AModel.Subscribed := SameText(Field('subscribed'), 'true');
  AModel.Notes := Field('notes').Trim;

  // 2. Validate: the engine returns property names ("FullName"), the form
  // uses the posted names ("fullname"). A field that did not parse keeps
  // its message.
  if not TMVCValidationEngine.Validate(AModel, lErrors) then
    try
      for lPair in lErrors do
        if not AErrors.ContainsKey(LowerCase(lPair.Key)) then
          AErrors.Add(LowerCase(lPair.Key), lPair.Value);
    finally
      lErrors.Free;
    end;
end;

// AModel feeds the hand-written form, ADemo feeds the f.auto() section.
// After an invalid POST AModel is Request.ContentFields (exactly what was
// typed, even "abc" in a number field), while ADemo is the object.
function RenderFormsPage(const AModel: TObject; const ADemo: TFormsDemo;
  const AErrors: TObject; const ASaved: Boolean): IMVCResponse;
begin
  ViewData['ispage'] := True;
  ViewData['formModel'] := AModel;
  if AErrors <> nil then
    ViewData['formErrors'] := AErrors;
  ViewData['demo'] := ADemo;
  ViewData['countries'] := GCountries;
  ViewData['saved'] := ASaved;
  Result := RenderView('pages/forms');
end;

procedure MapFormsRoutes(const AWeb: TMVCRouteGroup<TObject>);
begin
  AWeb.MapGet('/forms',
    function: IMVCResponse
    var
      lDemo: TFormsDemo;
    begin
      lDemo := TFormsDemo.Create;
      try
        Result := RenderFormsPage(lDemo, lDemo, nil, False);
      finally
        lDemo.Free; // ViewData owns nothing: the view is rendered by now
      end;
    end);

  AWeb.MapPost<TWebContext>('/forms',
    function (Ctx: TWebContext): IMVCResponse
    var
      lDemo: TFormsDemo;
      lErrors: TDictionary<string, string>;
    begin
      lDemo := TFormsDemo.Create;
      // Case-insensitive keys: f.auto() looks errors up by property name
      // ("FullName"), the hand-written controls by the posted name ("fullname")
      lErrors := TDictionary<string, string>.Create(TIStringComparer.Ordinal);
      try
        ReadFormsDemo(lDemo, Ctx.Request.ContentFields, lErrors);
        if lErrors.Count = 0 then
          // A real application would store the data and answer with
          // Post/Redirect/Get; the showcase keeps nothing, so it shows the
          // values that passed validation right away.
          Exit(RenderFormsPage(lDemo, lDemo, nil, True));
        Result := RenderFormsPage(Ctx.Request.ContentFields, lDemo, lErrors, False);
        Result.StatusCode := HTTP_STATUS.UnprocessableEntity;
      finally
        lErrors.Free;
        lDemo.Free;
      end;
    end);
end;

initialization

GCountries := TObjectList<TCountry>.Create(True);
GCountries.Add(TCountry.Create('IT', 'Italy'));
GCountries.Add(TCountry.Create('DE', 'Germany'));
GCountries.Add(TCountry.Create('FR', 'France'));
GCountries.Add(TCountry.Create('ES', 'Spain'));
GCountries.Add(TCountry.Create('BR', 'Brazil'));

finalization

GCountries.Free;

end.
