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

unit MVCFramework.Validators;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.DateUtils,
  System.RegularExpressions,
  System.Generics.Collections,
  System.Math,
  MVCFramework.Validation;

type
  // ===========================================================================
  // ENUMERATIONS
  // ===========================================================================

  TIPVersion = (ipv4, ipv6, ipAny);

  // ===========================================================================
  // BASIC VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a value is not empty/null.
  /// For strings: checks not empty
  /// For objects: checks not nil
  /// For integers: always passes (0 is a valid value)
  /// For Nullable types: checks HasValue = True
  /// </summary>
  MVCRequired = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string value is not empty or contains only whitespace.
  /// Unlike MVCRequired, this accepts null but rejects empty strings and whitespace-only strings.
  /// </summary>
  MVCNotEmpty = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string has a minimum length.
  /// </summary>
  MVCMinLength = class(TMVCValidatorBase)
  private
    FMinLength: Integer;
  public
    constructor Create(AMinLength: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MinLength: Integer read FMinLength;
  end;

  /// <summary>
  /// Validates that a string has a maximum length.
  /// </summary>
  MVCMaxLength = class(TMVCValidatorBase)
  private
    FMaxLength: Integer;
  public
    constructor Create(AMaxLength: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MaxLength: Integer read FMaxLength;
  end;

  /// <summary>
  /// Validates that a string has an exact length.
  /// </summary>
  MVCLength = class(TMVCValidatorBase)
  private
    FExactLength: Integer;
  public
    constructor Create(AExactLength: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property ExactLength: Integer read FExactLength;
  end;

  /// <summary>
  /// Validates that a numeric value is within a specified range.
  /// </summary>
  MVCRange = class(TMVCValidatorBase)
  private
    FMinValue: Int64;
    FMaxValue: Int64;
  public
    constructor Create(AMinValue, AMaxValue: Int64; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MinValue: Int64 read FMinValue;
    property MaxValue: Int64 read FMaxValue;
  end;

  /// <summary>
  /// Validates that a string matches a regular expression pattern.
  /// </summary>
  MVCPattern = class(TMVCValidatorBase)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Pattern: string read FPattern;
  end;

  /// <summary>
  /// Validates that a value is one of the allowed values.
  /// Works with strings (comma-separated list) and enums.
  /// </summary>
  MVCIn = class(TMVCValidatorBase)
  private
    FAllowedValues: TArray<string>;
  public
    constructor Create(const AAllowedValues: string; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property AllowedValues: TArray<string> read FAllowedValues;
  end;

  // ===========================================================================
  // NUMERIC VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a numeric value is positive (> 0).
  /// </summary>
  MVCPositive = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a numeric value is positive or zero (>= 0).
  /// </summary>
  MVCPositiveOrZero = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a numeric value is negative (< 0).
  /// </summary>
  MVCNegative = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a numeric value is negative or zero (<= 0).
  /// </summary>
  MVCNegativeOrZero = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates decimal precision (total digits and decimal places).
  /// Example: MVCDecimalPrecision(10, 2) allows max 10 total digits with 2 decimal places.
  /// </summary>
  MVCDecimalPrecision = class(TMVCValidatorBase)
  private
    FPrecision: Integer;
    FScale: Integer;
  public
    constructor Create(APrecision, AScale: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
  end;

  /// <summary>
  /// Validates that a numeric value is divisible by a specific number.
  /// Example: MVCDivisibleBy(5) ensures value is multiple of 5.
  /// </summary>
  MVCDivisibleBy = class(TMVCValidatorBase)
  private
    FDivisor: Integer;
  public
    constructor Create(ADivisor: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Divisor: Integer read FDivisor;
  end;

  // ===========================================================================
  // DATE VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a date is in the past.
  /// </summary>
  MVCPast = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a date is in the past or is today.
  /// </summary>
  MVCPastOrPresent = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a date is in the future.
  /// </summary>
  MVCFuture = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a date is in the future or is today.
  /// </summary>
  MVCFutureOrPresent = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a birth date results in an age within the specified range.
  /// Example: MVCAge(18, 65) validates that person is between 18 and 65 years old.
  /// </summary>
  MVCAge = class(TMVCValidatorBase)
  private
    FMinAge: Integer;
    FMaxAge: Integer;
  public
    constructor Create(AMinAge, AMaxAge: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MinAge: Integer read FMinAge;
    property MaxAge: Integer read FMaxAge;
  end;

  // ===========================================================================
  // STRING VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a string contains only letters (a-z, A-Z).
  /// </summary>
  MVCAlpha = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string contains only letters and numbers.
  /// </summary>
  MVCAlphaNumeric = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is entirely lowercase.
  /// </summary>
  MVCLowercase = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is entirely uppercase.
  /// </summary>
  MVCUppercase = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string contains a specific substring.
  /// </summary>
  MVCContains = class(TMVCValidatorBase)
  private
    FSubstring: string;
    FCaseSensitive: Boolean;
  public
    constructor Create(const ASubstring: string; ACaseSensitive: Boolean = True;
      const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Substring: string read FSubstring;
    property CaseSensitive: Boolean read FCaseSensitive;
  end;

  /// <summary>
  /// Validates that a string does NOT contain a specific substring.
  /// </summary>
  MVCNotContains = class(TMVCValidatorBase)
  private
    FSubstring: string;
    FCaseSensitive: Boolean;
  public
    constructor Create(const ASubstring: string; ACaseSensitive: Boolean = True;
      const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string starts with a specific prefix.
  /// </summary>
  MVCStartsWith = class(TMVCValidatorBase)
  private
    FPrefix: string;
    FCaseSensitive: Boolean;
  public
    constructor Create(const APrefix: string; ACaseSensitive: Boolean = True;
      const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string ends with a specific suffix.
  /// </summary>
  MVCEndsWith = class(TMVCValidatorBase)
  private
    FSuffix: string;
    FCaseSensitive: Boolean;
  public
    constructor Create(const ASuffix: string; ACaseSensitive: Boolean = True;
      const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid URL slug (lowercase letters, numbers, hyphens).
  /// Example: "my-article-title-123"
  /// </summary>
  MVCSlug = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // COLLECTION VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a collection (array, TList, TObjectList) has at least N elements.
  /// </summary>
  MVCMinCount = class(TMVCValidatorBase)
  private
    FMinCount: Integer;
  public
    constructor Create(AMinCount: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MinCount: Integer read FMinCount;
  end;

  /// <summary>
  /// Validates that a collection has at most N elements.
  /// </summary>
  MVCMaxCount = class(TMVCValidatorBase)
  private
    FMaxCount: Integer;
  public
    constructor Create(AMaxCount: Integer; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property MaxCount: Integer read FMaxCount;
  end;

  /// <summary>
  /// Validates that all elements in a collection are unique (no duplicates).
  /// Works with string arrays and lists.
  /// </summary>
  MVCDistinct = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - INTERNET
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid email address.
  /// </summary>
  MVCEmail = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid URL.
  /// </summary>
  MVCUrl = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates IP address (IPv4, IPv6, or both).
  /// </summary>
  MVCIPAddress = class(TMVCValidatorBase)
  private
    FVersion: TIPVersion;
  public
    constructor Create(AVersion: TIPVersion = ipAny; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Version: TIPVersion read FVersion;
  end;

  /// <summary>
  /// Validates that a string is a valid IPv4 address.
  /// Example: "192.168.1.1"
  /// </summary>
  MVCIPv4 = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid MAC address.
  /// Supports formats: XX:XX:XX:XX:XX:XX, XX-XX-XX-XX-XX-XX, XXXXXXXXXXXX
  /// </summary>
  MVCMACAddress = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - ENCODING
  // ===========================================================================

  /// <summary>
  /// Validates UUID/GUID format.
  /// </summary>
  MVCUUID = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates Base64 encoded string.
  /// </summary>
  MVCBase64 = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates JSON string format.
  /// </summary>
  MVCJson = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates hexadecimal string.
  /// </summary>
  MVCHexadecimal = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid Semantic Version (SemVer 2.0).
  /// Example: "1.2.3", "1.0.0-alpha+build.123"
  /// </summary>
  MVCSemVer = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - PHONE & POSTAL
  // ===========================================================================

  /// <summary>
  /// Validates phone number format.
  /// Supports country-specific formats or international format.
  /// </summary>
  MVCPhone = class(TMVCValidatorBase)
  private
    FFormat: string;
  public
    constructor Create(const AFormat: string = 'international'; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property Format: string read FFormat;
  end;

  /// <summary>
  /// Validates postal code based on country.
  /// Example: MVCPostalCode('IT') for Italian CAP, MVCPostalCode('US') for ZIP.
  /// </summary>
  MVCPostalCode = class(TMVCValidatorBase)
  private
    FCountryCode: string;
  public
    constructor Create(const ACountryCode: string; const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
    property CountryCode: string read FCountryCode;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - FINANCIAL
  // ===========================================================================

  /// <summary>
  /// Validates credit card number using Luhn algorithm.
  /// </summary>
  MVCCreditCard = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates IBAN (International Bank Account Number).
  /// </summary>
  MVCIBAN = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid BIC/SWIFT code.
  /// Format: 8 or 11 characters (AAAABBCCXXX)
  /// </summary>
  MVCBic = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - GEOGRAPHIC
  // ===========================================================================

  /// <summary>
  /// Validates that a value is a valid latitude (-90 to +90).
  /// </summary>
  MVCLatitude = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a value is a valid longitude (-180 to +180).
  /// </summary>
  MVCLongitude = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid ISO 3166-1 alpha-2 country code.
  /// Example: "IT", "US", "BR", "DE"
  /// </summary>
  MVCCountryCode = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // FORMAT VALIDATORS - BARCODES
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid EAN-13 barcode.
  /// Uses mod 10 checksum algorithm.
  /// </summary>
  MVCEAN13 = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid ISBN (ISBN-10 or ISBN-13).
  /// Automatically detects format and validates checksum.
  /// </summary>
  MVCISBN = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid Vehicle Identification Number (VIN).
  /// 17 characters with transliteration and check digit validation.
  /// </summary>
  MVCVIN = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // SECURITY VALIDATORS
  // ===========================================================================

  /// <summary>
  /// Validates that a password meets strength requirements.
  /// Parameters: MinLength, MinUppercase, MinLowercase, MinDigits, MinSpecial
  /// </summary>
  MVCStrongPassword = class(TMVCValidatorBase)
  private
    FMinLength: Integer;
    FMinUppercase: Integer;
    FMinLowercase: Integer;
    FMinDigits: Integer;
    FMinSpecial: Integer;
  public
    constructor Create(const AMinLength: Integer = 8;
      const AMinUppercase: Integer = 1; const AMinLowercase: Integer = 1;
      const AMinDigits: Integer = 1; const AMinSpecial: Integer = 1;
      const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // TAX ID VALIDATORS - EUROPEAN UNION
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid EU VAT number.
  /// Supports all 27 EU member states with country-specific algorithms.
  /// </summary>
  MVCEUVatNumber = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // TAX ID VALIDATORS - ITALY (IT)
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid Italian Codice Fiscale.
  /// 16 characters with check character validation.
  /// </summary>
  MVCITCodiceFiscale = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid Italian Partita IVA.
  /// 11 digits with Luhn-based checksum.
  /// </summary>
  MVCITPartitaIVA = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // TAX ID VALIDATORS - USA (US)
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid US Social Security Number.
  /// Format: XXX-XX-XXXX with area number validation.
  /// </summary>
  MVCUSSSN = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  /// <summary>
  /// Validates that a string is a valid US ABA Routing Number.
  /// 9 digits with checksum validation.
  /// </summary>
  MVCUSAbaRouting = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

  // ===========================================================================
  // TAX ID VALIDATORS - BRAZIL (BR)
  // ===========================================================================

  /// <summary>
  /// Validates that a string is a valid Brazilian CPF.
  /// 11 digits with two check digits (mod 11 algorithm).
  /// </summary>
  MVCBRCPF = class(TMVCValidatorBase)
  public
    constructor Create(const AMessage: string = '');
    function Validate(const AValue: TValue; const AObject: TObject): Boolean; override;
  end;

implementation

uses
  System.Character,
  System.StrUtils,
  System.NetEncoding,
  JsonDataObjects;

const
  // ISO 3166-1 alpha-2 country codes (249 countries)
  ISO_COUNTRY_CODES: array[0..248] of string = (
    'AD', 'AE', 'AF', 'AG', 'AI', 'AL', 'AM', 'AO', 'AQ', 'AR',
    'AS', 'AT', 'AU', 'AW', 'AX', 'AZ', 'BA', 'BB', 'BD', 'BE',
    'BF', 'BG', 'BH', 'BI', 'BJ', 'BL', 'BM', 'BN', 'BO', 'BQ',
    'BR', 'BS', 'BT', 'BV', 'BW', 'BY', 'BZ', 'CA', 'CC', 'CD',
    'CF', 'CG', 'CH', 'CI', 'CK', 'CL', 'CM', 'CN', 'CO', 'CR',
    'CU', 'CV', 'CW', 'CX', 'CY', 'CZ', 'DE', 'DJ', 'DK', 'DM',
    'DO', 'DZ', 'EC', 'EE', 'EG', 'EH', 'ER', 'ES', 'ET', 'FI',
    'FJ', 'FK', 'FM', 'FO', 'FR', 'GA', 'GB', 'GD', 'GE', 'GF',
    'GG', 'GH', 'GI', 'GL', 'GM', 'GN', 'GP', 'GQ', 'GR', 'GS',
    'GT', 'GU', 'GW', 'GY', 'HK', 'HM', 'HN', 'HR', 'HT', 'HU',
    'ID', 'IE', 'IL', 'IM', 'IN', 'IO', 'IQ', 'IR', 'IS', 'IT',
    'JE', 'JM', 'JO', 'JP', 'KE', 'KG', 'KH', 'KI', 'KM', 'KN',
    'KP', 'KR', 'KW', 'KY', 'KZ', 'LA', 'LB', 'LC', 'LI', 'LK',
    'LR', 'LS', 'LT', 'LU', 'LV', 'LY', 'MA', 'MC', 'MD', 'ME',
    'MF', 'MG', 'MH', 'MK', 'ML', 'MM', 'MN', 'MO', 'MP', 'MQ',
    'MR', 'MS', 'MT', 'MU', 'MV', 'MW', 'MX', 'MY', 'MZ', 'NA',
    'NC', 'NE', 'NF', 'NG', 'NI', 'NL', 'NO', 'NP', 'NR', 'NU',
    'NZ', 'OM', 'PA', 'PE', 'PF', 'PG', 'PH', 'PK', 'PL', 'PM',
    'PN', 'PR', 'PS', 'PT', 'PW', 'PY', 'QA', 'RE', 'RO', 'RS',
    'RU', 'RW', 'SA', 'SB', 'SC', 'SD', 'SE', 'SG', 'SH', 'SI',
    'SJ', 'SK', 'SL', 'SM', 'SN', 'SO', 'SR', 'SS', 'ST', 'SV',
    'SX', 'SY', 'SZ', 'TC', 'TD', 'TF', 'TG', 'TH', 'TJ', 'TK',
    'TL', 'TM', 'TN', 'TO', 'TR', 'TT', 'TV', 'TW', 'TZ', 'UA',
    'UG', 'UM', 'US', 'UY', 'UZ', 'VA', 'VC', 'VE', 'VG', 'VI',
    'VN', 'VU', 'WF', 'WS', 'YE', 'YT', 'ZA', 'ZM', 'ZW'
  );

  // Codice Fiscale character mappings
  CF_ODD_VALUES: array[0..35] of Integer = (
    1, 0, 5, 7, 9, 13, 15, 17, 19, 21,
    1, 0, 5, 7, 9, 13, 15, 17, 19, 21,
    2, 4, 18, 20, 11, 3, 6, 8, 12, 14,
    16, 10, 22, 25, 24, 23
  );

  CF_EVEN_VALUES: array[0..35] of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25
  );

// ===========================================================================
// HELPER FUNCTIONS
// ===========================================================================

function GetCollectionCount(const AValue: TValue): Integer;
var
  LObj: TObject;
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LCountProp: TRttiProperty;
begin
  Result := -1;
  if AValue.IsEmpty then
    Exit(0);

  if AValue.IsObject then
  begin
    LObj := AValue.AsObject;
    if LObj = nil then
      Exit(0);

    LRttiContext := TRttiContext.Create;
    try
      LRttiType := LRttiContext.GetType(LObj.ClassType);
      LCountProp := LRttiType.GetProperty('Count');
      if Assigned(LCountProp) then
        Result := LCountProp.GetValue(LObj).AsInteger;
    finally
      LRttiContext.Free;
    end;
  end
  else if AValue.IsArray then
    Result := AValue.GetArrayLength;
end;

function ValidateEANChecksum(const AValue: string): Boolean;
var
  I, LSum, LDigit: Integer;
begin
  Result := False;
  if Length(AValue) <> 13 then
    Exit;

  LSum := 0;
  for I := 1 to 12 do
  begin
    if not AValue[I].IsDigit then
      Exit;
    LDigit := StrToInt(AValue[I]);
    if Odd(I) then
      LSum := LSum + LDigit
    else
      LSum := LSum + LDigit * 3;
  end;

  if not AValue[13].IsDigit then
    Exit;

  Result := (10 - (LSum mod 10)) mod 10 = StrToInt(AValue[13]);
end;

function ValidateISBN10(const AValue: string): Boolean;
var
  I, LSum, LDigit: Integer;
  LClean: string;
begin
  Result := False;
  LClean := StringReplace(AValue, '-', '', [rfReplaceAll]);
  LClean := StringReplace(LClean, ' ', '', [rfReplaceAll]);

  if Length(LClean) <> 10 then
    Exit;

  LSum := 0;
  for I := 1 to 9 do
  begin
    if not LClean[I].IsDigit then
      Exit;
    LSum := LSum + StrToInt(LClean[I]) * (11 - I);
  end;

  if (LClean[10] = 'X') or (LClean[10] = 'x') then
    LDigit := 10
  else if LClean[10].IsDigit then
    LDigit := StrToInt(LClean[10])
  else
    Exit;

  LSum := LSum + LDigit;
  Result := LSum mod 11 = 0;
end;

function ValidateISBN13(const AValue: string): Boolean;
var
  LClean: string;
begin
  LClean := StringReplace(AValue, '-', '', [rfReplaceAll]);
  LClean := StringReplace(LClean, ' ', '', [rfReplaceAll]);
  Result := ValidateEANChecksum(LClean);
end;

function ValidateVINCheckDigit(const AValue: string): Boolean;
const
  TRANSLITERATION: array['A'..'Z'] of Integer = (
    1, 2, 3, 4, 5, 6, 7, 8, 0, 1,
    2, 3, 4, 5, 0, 7, 0, 9, 2, 3,
    4, 5, 6, 7, 8, 9
  );
  WEIGHTS: array[1..17] of Integer = (8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2);
var
  I, LSum, LValue: Integer;
  LCheck: Char;
  C: Char;
begin
  Result := False;
  if Length(AValue) <> 17 then
    Exit;

  LSum := 0;
  for I := 1 to 17 do
  begin
    C := UpCase(AValue[I]);

    if I = 9 then
    begin
      if (C = 'X') then
        LValue := 10
      else if C.IsDigit then
        LValue := StrToInt(C)
      else
        Exit;
    end
    else if C.IsDigit then
      LValue := StrToInt(C)
    else if (C >= 'A') and (C <= 'Z') then
    begin
      if (C = 'I') or (C = 'O') or (C = 'Q') then
        Exit;
      LValue := TRANSLITERATION[C];
    end
    else
      Exit;

    LSum := LSum + LValue * WEIGHTS[I];
  end;

  LCheck := AValue[9];
  if LCheck = 'X' then
    Result := LSum mod 11 = 10
  else if LCheck.IsDigit then
    Result := LSum mod 11 = StrToInt(LCheck)
  else
    Result := False;
end;

function ValidateCodiceFiscale(const AValue: string): Boolean;
var
  I, LSum, LCharIndex: Integer;
  LUpper: string;
  C: Char;
begin
  Result := False;
  if Length(AValue) <> 16 then
    Exit;

  LUpper := UpperCase(AValue);

  for I := 1 to 6 do
    if not LUpper[I].IsLetter then Exit;
  for I := 7 to 8 do
    if not LUpper[I].IsDigit then Exit;
  if not LUpper[9].IsLetter then Exit;
  for I := 10 to 11 do
    if not LUpper[I].IsDigit then Exit;
  if not LUpper[12].IsLetter then Exit;
  for I := 13 to 15 do
    if not LUpper[I].IsDigit then Exit;

  LSum := 0;
  for I := 1 to 15 do
  begin
    C := LUpper[I];
    if C.IsDigit then
      LCharIndex := Ord(C) - Ord('0')
    else
      LCharIndex := Ord(C) - Ord('A') + 10;

    if Odd(I) then
      LSum := LSum + CF_ODD_VALUES[LCharIndex]
    else
      LSum := LSum + CF_EVEN_VALUES[LCharIndex];
  end;

  Result := Chr(Ord('A') + (LSum mod 26)) = LUpper[16];
end;

function ValidatePartitaIVA(const AValue: string): Boolean;
var
  I, LSum, LDigit, LCheck: Integer;
begin
  Result := False;
  if Length(AValue) <> 11 then
    Exit;

  for I := 1 to 11 do
    if not AValue[I].IsDigit then
      Exit;

  LSum := 0;
  for I := 1 to 10 do
  begin
    LDigit := StrToInt(AValue[I]);
    if Odd(I) then
      LSum := LSum + LDigit
    else
    begin
      LDigit := LDigit * 2;
      if LDigit > 9 then
        LDigit := LDigit - 9;
      LSum := LSum + LDigit;
    end;
  end;

  LCheck := (10 - (LSum mod 10)) mod 10;
  Result := LCheck = StrToInt(AValue[11]);
end;

function ValidateCPF(const AValue: string): Boolean;
var
  I, LSum, LRemainder, LDigit1, LDigit2: Integer;
  LClean: string;
begin
  Result := False;

  LClean := StringReplace(AValue, '.', '', [rfReplaceAll]);
  LClean := StringReplace(LClean, '-', '', [rfReplaceAll]);

  if Length(LClean) <> 11 then
    Exit;

  if TRegEx.IsMatch(LClean, '^(\d)\1{10}$') then
    Exit;

  for I := 1 to 11 do
    if not LClean[I].IsDigit then
      Exit;

  LSum := 0;
  for I := 1 to 9 do
    LSum := LSum + StrToInt(LClean[I]) * (11 - I);

  LRemainder := LSum mod 11;
  if LRemainder < 2 then
    LDigit1 := 0
  else
    LDigit1 := 11 - LRemainder;

  if StrToInt(LClean[10]) <> LDigit1 then
    Exit;

  LSum := 0;
  for I := 1 to 10 do
    LSum := LSum + StrToInt(LClean[I]) * (12 - I);

  LRemainder := LSum mod 11;
  if LRemainder < 2 then
    LDigit2 := 0
  else
    LDigit2 := 11 - LRemainder;

  Result := StrToInt(LClean[11]) = LDigit2;
end;

function ValidateABARouting(const AValue: string): Boolean;
var
  I, LSum: Integer;
begin
  Result := False;
  if Length(AValue) <> 9 then
    Exit;

  for I := 1 to 9 do
    if not AValue[I].IsDigit then
      Exit;

  LSum := 3 * (StrToInt(AValue[1]) + StrToInt(AValue[4]) + StrToInt(AValue[7])) +
          7 * (StrToInt(AValue[2]) + StrToInt(AValue[5]) + StrToInt(AValue[8])) +
          (StrToInt(AValue[3]) + StrToInt(AValue[6]) + StrToInt(AValue[9]));

  Result := LSum mod 10 = 0;
end;

// ===========================================================================
// BASIC VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCRequired }

constructor MVCRequired.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Field is required';
end;

function MVCRequired.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LTypeName: string;
  LCtx: TRttiContext;
  LType: TRttiType;
  LHasValueProp: TRttiProperty;
begin
  if AValue.TypeInfo <> nil then
  begin
    LTypeName := string(AValue.TypeInfo.Name);
    if LTypeName.StartsWith('Nullable', True) then
    begin
      LCtx := TRttiContext.Create;
      try
        LType := LCtx.GetType(AValue.TypeInfo);
        LHasValueProp := LType.GetProperty('HasValue');
        if Assigned(LHasValueProp) then
        begin
          Result := LHasValueProp.GetValue(AValue.GetReferenceToRawData).AsBoolean;
          Exit;
        end;
      finally
        LCtx.Free;
      end;
    end;
  end;

  if AValue.IsEmpty then
    Exit(False);

  case AValue.Kind of
    tkUString, tkString, tkLString, tkWString:
      Result := not AValue.AsString.IsEmpty;
    tkClass:
      Result := AValue.AsObject <> nil;
    tkInterface:
      Result := AValue.AsInterface <> nil;
  else
    Result := True;
  end;
end;

{ MVCNotEmpty }

constructor MVCNotEmpty.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Field cannot be empty or contain only whitespace';
end;

function MVCNotEmpty.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString.Trim;
  Result := not LStr.IsEmpty;
end;

{ MVCMinLength }

constructor MVCMinLength.Create(AMinLength: Integer; const AMessage: string);
begin
  inherited Create;
  FMinLength := AMinLength;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Must be at least %d characters', [FMinLength]);
end;

function MVCMinLength.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  Result := AValue.AsString.Length >= FMinLength;
end;

{ MVCMaxLength }

constructor MVCMaxLength.Create(AMaxLength: Integer; const AMessage: string);
begin
  inherited Create;
  FMaxLength := AMaxLength;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Must be at most %d characters', [FMaxLength]);
end;

function MVCMaxLength.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  Result := AValue.AsString.Length <= FMaxLength;
end;

{ MVCLength }

constructor MVCLength.Create(AExactLength: Integer; const AMessage: string);
begin
  inherited Create;
  FExactLength := AExactLength;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Must be exactly %d characters', [FExactLength]);
end;

function MVCLength.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  Result := AValue.AsString.Length = FExactLength;
end;

{ MVCRange }

constructor MVCRange.Create(AMinValue, AMaxValue: Int64; const AMessage: string);
begin
  inherited Create;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must be between %d and %d', [FMinValue, FMaxValue]);
end;

function MVCRange.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LIntValue: Int64;
  LFloatValue: Extended;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      begin
        LIntValue := AValue.AsInteger;
        Result := (LIntValue >= FMinValue) and (LIntValue <= FMaxValue);
      end;
    tkInt64:
      begin
        LIntValue := AValue.AsInt64;
        Result := (LIntValue >= FMinValue) and (LIntValue <= FMaxValue);
      end;
    tkFloat:
      begin
        LFloatValue := AValue.AsExtended;
        Result := (LFloatValue >= FMinValue) and (LFloatValue <= FMaxValue);
      end;
  else
    Result := True;
  end;
end;

{ MVCPattern }

constructor MVCPattern.Create(const APattern: string; const AMessage: string);
begin
  inherited Create;
  FPattern := APattern;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value does not match the required pattern';
end;

function MVCPattern.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, FPattern);
end;

{ MVCIn }

constructor MVCIn.Create(const AAllowedValues: string; const AMessage: string);
var
  LValues: TArray<string>;
  I: Integer;
begin
  inherited Create;
  LValues := AAllowedValues.Split([',']);
  SetLength(FAllowedValues, Length(LValues));
  for I := 0 to High(LValues) do
    FAllowedValues[I] := LValues[I].Trim;

  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must be one of: %s', [AAllowedValues]);
end;

function MVCIn.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStrValue: string;
  LAllowed: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkUString, tkString, tkLString, tkWString:
      LStrValue := AValue.AsString;
    tkInteger:
      LStrValue := IntToStr(AValue.AsInteger);
    tkInt64:
      LStrValue := IntToStr(AValue.AsInt64);
    tkEnumeration:
      LStrValue := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);
  else
    Exit(True);
  end;

  for LAllowed in FAllowedValues do
  begin
    if SameText(LStrValue, LAllowed) then
      Exit(True);
  end;

  Result := False;
end;

// ===========================================================================
// NUMERIC VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCPositive }

constructor MVCPositive.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be positive';
end;

function MVCPositive.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      Result := AValue.AsInteger > 0;
    tkInt64:
      Result := AValue.AsInt64 > 0;
    tkFloat:
      Result := AValue.AsExtended > 0;
  else
    Result := True;
  end;
end;

{ MVCPositiveOrZero }

constructor MVCPositiveOrZero.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be positive or zero';
end;

function MVCPositiveOrZero.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      Result := AValue.AsInteger >= 0;
    tkInt64:
      Result := AValue.AsInt64 >= 0;
    tkFloat:
      Result := AValue.AsExtended >= 0;
  else
    Result := True;
  end;
end;

{ MVCNegative }

constructor MVCNegative.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be negative';
end;

function MVCNegative.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      Result := AValue.AsInteger < 0;
    tkInt64:
      Result := AValue.AsInt64 < 0;
    tkFloat:
      Result := AValue.AsExtended < 0;
  else
    Result := True;
  end;
end;

{ MVCNegativeOrZero }

constructor MVCNegativeOrZero.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be negative or zero';
end;

function MVCNegativeOrZero.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      Result := AValue.AsInteger <= 0;
    tkInt64:
      Result := AValue.AsInt64 <= 0;
    tkFloat:
      Result := AValue.AsExtended <= 0;
  else
    Result := True;
  end;
end;

{ MVCDecimalPrecision }

constructor MVCDecimalPrecision.Create(APrecision, AScale: Integer; const AMessage: string);
begin
  inherited Create;
  FPrecision := APrecision;
  FScale := AScale;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must have at most %d digits with %d decimal places',
      [FPrecision, FScale]);
end;

function MVCDecimalPrecision.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LValue: Extended;
  LStrValue: string;
  LIntPart, LDecPart: string;
  LDecimalPos: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind <> tkFloat then
    Exit(True);

  LValue := AValue.AsExtended;
  LStrValue := FloatToStr(Abs(LValue));

  LDecimalPos := Pos(FormatSettings.DecimalSeparator, LStrValue);
  if LDecimalPos > 0 then
  begin
    LIntPart := Copy(LStrValue, 1, LDecimalPos - 1);
    LDecPart := Copy(LStrValue, LDecimalPos + 1, Length(LStrValue));

    if Length(LDecPart) > FScale then
      Exit(False);

    Result := (Length(LIntPart) + Length(LDecPart)) <= FPrecision;
  end
  else
  begin
    Result := Length(LStrValue) <= FPrecision;
  end;
end;

{ MVCDivisibleBy }

constructor MVCDivisibleBy.Create(ADivisor: Integer; const AMessage: string);
begin
  inherited Create;
  FDivisor := ADivisor;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must be divisible by %d', [FDivisor]);
end;

function MVCDivisibleBy.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LValue: Int64;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkInteger:
      LValue := AValue.AsInteger;
    tkInt64:
      LValue := AValue.AsInt64;
  else
    Exit(True);
  end;

  if FDivisor = 0 then
    Exit(False);

  Result := (LValue mod FDivisor) = 0;
end;

// ===========================================================================
// DATE VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCPast }

constructor MVCPast.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Date must be in the past';
end;

function MVCPast.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind = tkFloat then
    Result := AValue.AsExtended < Now
  else
    Result := True;
end;

{ MVCPastOrPresent }

constructor MVCPastOrPresent.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Date must be in the past or present';
end;

function MVCPastOrPresent.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind = tkFloat then
    Result := AValue.AsExtended <= Now
  else
    Result := True;
end;

{ MVCFuture }

constructor MVCFuture.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Date must be in the future';
end;

function MVCFuture.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind = tkFloat then
    Result := AValue.AsExtended > Now
  else
    Result := True;
end;

{ MVCFutureOrPresent }

constructor MVCFutureOrPresent.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Date must be in the future or present';
end;

function MVCFutureOrPresent.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind = tkFloat then
    Result := AValue.AsExtended >= Now
  else
    Result := True;
end;

{ MVCAge }

constructor MVCAge.Create(AMinAge, AMaxAge: Integer; const AMessage: string);
begin
  inherited Create;
  FMinAge := AMinAge;
  FMaxAge := AMaxAge;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Age must be between %d and %d', [FMinAge, FMaxAge]);
end;

function MVCAge.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LBirthDate: TDateTime;
  LAge: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  if AValue.Kind <> tkFloat then
    Exit(True);

  LBirthDate := AValue.AsExtended;
  LAge := YearsBetween(Now, LBirthDate);

  Result := (LAge >= FMinAge) and (LAge <= FMaxAge);
end;

// ===========================================================================
// STRING VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCAlpha }

constructor MVCAlpha.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must contain only letters';
end;

function MVCAlpha.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  Result := TRegEx.IsMatch(AValue.AsString, '^[a-zA-Z]+$');
end;

{ MVCAlphaNumeric }

constructor MVCAlphaNumeric.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must contain only letters and numbers';
end;

function MVCAlphaNumeric.Validate(const AValue: TValue; const AObject: TObject): Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  Result := TRegEx.IsMatch(AValue.AsString, '^[a-zA-Z0-9]+$');
end;

{ MVCLowercase }

constructor MVCLowercase.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be entirely lowercase';
end;

function MVCLowercase.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  Result := LStr = LowerCase(LStr);
end;

{ MVCUppercase }

constructor MVCUppercase.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be entirely uppercase';
end;

function MVCUppercase.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  Result := LStr = UpperCase(LStr);
end;

{ MVCContains }

constructor MVCContains.Create(const ASubstring: string; ACaseSensitive: Boolean;
  const AMessage: string);
begin
  inherited Create;
  FSubstring := ASubstring;
  FCaseSensitive := ACaseSensitive;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must contain "%s"', [FSubstring]);
end;

function MVCContains.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if FCaseSensitive then
    Result := LStr.Contains(FSubstring)
  else
    Result := LStr.ToLower.Contains(FSubstring.ToLower);
end;

{ MVCNotContains }

constructor MVCNotContains.Create(const ASubstring: string; ACaseSensitive: Boolean;
  const AMessage: string);
begin
  inherited Create;
  FSubstring := ASubstring;
  FCaseSensitive := ACaseSensitive;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must not contain "%s"', [FSubstring]);
end;

function MVCNotContains.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if FCaseSensitive then
    Result := not LStr.Contains(FSubstring)
  else
    Result := not LStr.ToLower.Contains(FSubstring.ToLower);
end;

{ MVCStartsWith }

constructor MVCStartsWith.Create(const APrefix: string; ACaseSensitive: Boolean;
  const AMessage: string);
begin
  inherited Create;
  FPrefix := APrefix;
  FCaseSensitive := ACaseSensitive;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must start with "%s"', [FPrefix]);
end;

function MVCStartsWith.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if FCaseSensitive then
    Result := LStr.StartsWith(FPrefix)
  else
    Result := LStr.ToLower.StartsWith(FPrefix.ToLower);
end;

{ MVCEndsWith }

constructor MVCEndsWith.Create(const ASuffix: string; ACaseSensitive: Boolean;
  const AMessage: string);
begin
  inherited Create;
  FSuffix := ASuffix;
  FCaseSensitive := ACaseSensitive;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Value must end with "%s"', [FSuffix]);
end;

function MVCEndsWith.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if FCaseSensitive then
    Result := LStr.EndsWith(FSuffix)
  else
    Result := LStr.ToLower.EndsWith(FSuffix.ToLower);
end;

{ MVCSlug }

constructor MVCSlug.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid URL slug (lowercase letters, numbers, and hyphens)'
  else
    FErrorMessage := AMessage;
end;

function MVCSlug.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, '^[a-z0-9]+(?:-[a-z0-9]+)*$');
end;

// ===========================================================================
// COLLECTION VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCMinCount }

constructor MVCMinCount.Create(AMinCount: Integer; const AMessage: string);
begin
  inherited Create;
  FMinCount := AMinCount;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Collection must have at least %d items', [FMinCount]);
end;

function MVCMinCount.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LCount: Integer;
begin
  LCount := GetCollectionCount(AValue);
  if LCount < 0 then
    Exit(True);

  Result := LCount >= FMinCount;
end;

{ MVCMaxCount }

constructor MVCMaxCount.Create(AMaxCount: Integer; const AMessage: string);
begin
  inherited Create;
  FMaxCount := AMaxCount;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := Format('Collection must have at most %d items', [FMaxCount]);
end;

function MVCMaxCount.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LCount: Integer;
begin
  LCount := GetCollectionCount(AValue);
  if LCount < 0 then
    Exit(True);

  Result := LCount <= FMaxCount;
end;

{ MVCDistinct }

constructor MVCDistinct.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Collection must contain only unique values';
end;

function MVCDistinct.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LSet: TDictionary<string, Boolean>;
  I: Integer;
  LItem: TValue;
  LItemStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not AValue.IsArray then
    Exit(True);

  LSet := TDictionary<string, Boolean>.Create;
  try
    for I := 0 to AValue.GetArrayLength - 1 do
    begin
      LItem := AValue.GetArrayElement(I);
      LItemStr := LItem.ToString;
      if LSet.ContainsKey(LItemStr) then
        Exit(False);
      LSet.Add(LItemStr, True);
    end;
    Result := True;
  finally
    LSet.Free;
  end;
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - INTERNET
// ===========================================================================

{ MVCEmail }

constructor MVCEmail.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid email address';
end;

function MVCEmail.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  EMAIL_PATTERN = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, EMAIL_PATTERN);
end;

{ MVCUrl }

constructor MVCUrl.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid URL';
end;

function MVCUrl.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  URL_PATTERN = '^(https?|ftp)://[^\s/$.?#].[^\s]*$';
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, URL_PATTERN, [roIgnoreCase]);
end;

{ MVCIPAddress }

constructor MVCIPAddress.Create(AVersion: TIPVersion; const AMessage: string);
begin
  inherited Create;
  FVersion := AVersion;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid IP address';
end;

function MVCIPAddress.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  IPV4_PATTERN = '^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$';
  IPV6_PATTERN = '^(([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|' +
    '([0-9a-fA-F]{1,4}:){1,7}:|' +
    '([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|' +
    '([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|' +
    '([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|' +
    '([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|' +
    '([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|' +
    '[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|' +
    ':((:[0-9a-fA-F]{1,4}){1,7}|:))$';
var
  LIP: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LIP := AValue.AsString;
  if LIP.IsEmpty then
    Exit(True);

  case FVersion of
    ipv4:
      Result := TRegEx.IsMatch(LIP, IPV4_PATTERN);
    ipv6:
      Result := TRegEx.IsMatch(LIP, IPV6_PATTERN);
    ipAny:
      Result := TRegEx.IsMatch(LIP, IPV4_PATTERN) or TRegEx.IsMatch(LIP, IPV6_PATTERN);
  else
    Result := False;
  end;
end;

{ MVCIPv4 }

constructor MVCIPv4.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid IPv4 address'
  else
    FErrorMessage := AMessage;
end;

function MVCIPv4.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
  LParts: TArray<string>;
  I, LPart: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  LParts := LStr.Split(['.']);
  if Length(LParts) <> 4 then
    Exit(False);

  for I := 0 to 3 do
  begin
    if not TryStrToInt(LParts[I], LPart) then
      Exit(False);
    if (LPart < 0) or (LPart > 255) then
      Exit(False);
    if (Length(LParts[I]) > 1) and (LParts[I][1] = '0') then
      Exit(False);
  end;

  Result := True;
end;

{ MVCMACAddress }

constructor MVCMACAddress.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid MAC address'
  else
    FErrorMessage := AMessage;
end;

function MVCMACAddress.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr,
    '^([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}$|^[0-9A-Fa-f]{12}$',
    [roIgnoreCase]);
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - ENCODING
// ===========================================================================

{ MVCUUID }

constructor MVCUUID.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid UUID';
end;

function MVCUUID.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  UUID_PATTERN = '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$';
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, UUID_PATTERN);
end;

{ MVCBase64 }

constructor MVCBase64.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid Base64 string';
end;

function MVCBase64.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  BASE64_PATTERN = '^[A-Za-z0-9+/]*={0,2}$';
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  if (Length(LStr) mod 4) <> 0 then
    Exit(False);

  Result := TRegEx.IsMatch(LStr, BASE64_PATTERN);
end;

{ MVCJson }

constructor MVCJson.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid JSON format';
end;

function MVCJson.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LJson: TJsonBaseObject;
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  try
    LJson := TJsonBaseObject.Parse(LStr);
    try
      Result := Assigned(LJson);
    finally
      LJson.Free;
    end;
  except
    Result := False;
  end;
end;

{ MVCHexadecimal }

constructor MVCHexadecimal.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid hexadecimal string';
end;

function MVCHexadecimal.Validate(const AValue: TValue; const AObject: TObject): Boolean;
const
  HEX_PATTERN = '^[0-9a-fA-F]+$';
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, HEX_PATTERN);
end;

{ MVCSemVer }

constructor MVCSemVer.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid semantic version (e.g., 1.2.3 or 1.0.0-alpha+build)'
  else
    FErrorMessage := AMessage;
end;

function MVCSemVer.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr,
    '^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)' +
    '(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?' +
    '(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$');
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - PHONE & POSTAL
// ===========================================================================

{ MVCPhone }

constructor MVCPhone.Create(const AFormat: string; const AMessage: string);
begin
  inherited Create;
  FFormat := AFormat.ToLower;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid phone number';
end;

function MVCPhone.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LPhone: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LPhone := AValue.AsString;
  if LPhone.IsEmpty then
    Exit(True);

  LPhone := LPhone.Replace(' ', '').Replace('-', '').Replace('(', '').Replace(')', '');

  if FFormat = 'it' then
    Result := TRegEx.IsMatch(LPhone, '^(\+39)?[0-9]{9,10}$')
  else if FFormat = 'us' then
    Result := TRegEx.IsMatch(LPhone, '^(\+1)?[0-9]{10}$')
  else
    Result := TRegEx.IsMatch(LPhone, '^\+?[0-9]{7,15}$');
end;

{ MVCPostalCode }

constructor MVCPostalCode.Create(const ACountryCode: string; const AMessage: string);
begin
  inherited Create;
  FCountryCode := ACountryCode.ToUpper;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid postal code';
end;

function MVCPostalCode.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LCode: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LCode := AValue.AsString;
  if LCode.IsEmpty then
    Exit(True);

  LCode := LCode.Replace(' ', '');

  if FCountryCode = 'IT' then
    Result := TRegEx.IsMatch(LCode, '^\d{5}$')
  else if FCountryCode = 'US' then
    Result := TRegEx.IsMatch(LCode, '^\d{5}(-\d{4})?$')
  else if FCountryCode = 'UK' then
    Result := TRegEx.IsMatch(LCode, '^[A-Z]{1,2}\d{1,2}[A-Z]?\s?\d[A-Z]{2}$', [roIgnoreCase])
  else if FCountryCode = 'DE' then
    Result := TRegEx.IsMatch(LCode, '^\d{5}$')
  else if FCountryCode = 'FR' then
    Result := TRegEx.IsMatch(LCode, '^\d{5}$')
  else if FCountryCode = 'ES' then
    Result := TRegEx.IsMatch(LCode, '^\d{5}$')
  else if FCountryCode = 'CH' then
    Result := TRegEx.IsMatch(LCode, '^\d{4}$')
  else if FCountryCode = 'AT' then
    Result := TRegEx.IsMatch(LCode, '^\d{4}$')
  else
    Result := True;
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - FINANCIAL
// ===========================================================================

{ MVCCreditCard }

constructor MVCCreditCard.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid credit card number';
end;

function MVCCreditCard.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LNumber: string;
  I, LSum, LDigit: Integer;
  LDouble: Boolean;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LNumber := AValue.AsString;
  if LNumber.IsEmpty then
    Exit(True);

  LNumber := LNumber.Replace(' ', '').Replace('-', '');

  if not TRegEx.IsMatch(LNumber, '^\d{13,19}$') then
    Exit(False);

  LSum := 0;
  LDouble := False;

  for I := Length(LNumber) downto 1 do
  begin
    LDigit := StrToInt(LNumber[I]);

    if LDouble then
    begin
      LDigit := LDigit * 2;
      if LDigit > 9 then
        LDigit := LDigit - 9;
    end;

    LSum := LSum + LDigit;
    LDouble := not LDouble;
  end;

  Result := (LSum mod 10) = 0;
end;

{ MVCIBAN }

constructor MVCIBAN.Create(const AMessage: string);
begin
  inherited Create;
  FErrorMessage := AMessage;
  if FErrorMessage.IsEmpty then
    FErrorMessage := 'Invalid IBAN';
end;

function MVCIBAN.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LIBAN, LRearranged, LNumeric: string;
  I: Integer;
  LChar: Char;
  LChecksum: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  if not (AValue.Kind in [tkUString, tkString, tkLString, tkWString]) then
    Exit(True);

  LIBAN := AValue.AsString;
  if LIBAN.IsEmpty then
    Exit(True);

  LIBAN := LIBAN.Replace(' ', '').ToUpper;

  if not TRegEx.IsMatch(LIBAN, '^[A-Z]{2}\d{2}[A-Z0-9]{1,30}$') then
    Exit(False);

  LRearranged := Copy(LIBAN, 5, Length(LIBAN)) + Copy(LIBAN, 1, 4);

  LNumeric := '';
  for I := 1 to Length(LRearranged) do
  begin
    LChar := LRearranged[I];
    if CharInSet(LChar, ['A'..'Z']) then
      LNumeric := LNumeric + IntToStr(Ord(LChar) - Ord('A') + 10)
    else
      LNumeric := LNumeric + LChar;
  end;

  LChecksum := 0;
  for I := 1 to Length(LNumeric) do
  begin
    LChecksum := (LChecksum * 10 + StrToInt(LNumeric[I])) mod 97;
  end;

  Result := LChecksum = 1;
end;

{ MVCBic }

constructor MVCBic.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid BIC/SWIFT code'
  else
    FErrorMessage := AMessage;
end;

function MVCBic.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := UpperCase(AValue.AsString);
  if LStr.IsEmpty then
    Exit(True);

  Result := TRegEx.IsMatch(LStr, '^[A-Z]{4}[A-Z]{2}[A-Z0-9]{2}([A-Z0-9]{3})?$');
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - GEOGRAPHIC
// ===========================================================================

{ MVCLatitude }

constructor MVCLatitude.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid latitude (-90 to +90)'
  else
    FErrorMessage := AMessage;
end;

function MVCLatitude.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LDouble: Double;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkFloat:
      LDouble := AValue.AsExtended;
    tkInteger, tkInt64:
      LDouble := AValue.AsInt64;
    tkString, tkUString, tkLString, tkWString:
      if not TryStrToFloat(AValue.AsString, LDouble) then
        Exit(False);
  else
    Exit(True);
  end;

  Result := (LDouble >= -90) and (LDouble <= 90);
end;

{ MVCLongitude }

constructor MVCLongitude.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid longitude (-180 to +180)'
  else
    FErrorMessage := AMessage;
end;

function MVCLongitude.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LDouble: Double;
begin
  if AValue.IsEmpty then
    Exit(True);

  case AValue.Kind of
    tkFloat:
      LDouble := AValue.AsExtended;
    tkInteger, tkInt64:
      LDouble := AValue.AsInt64;
    tkString, tkUString, tkLString, tkWString:
      if not TryStrToFloat(AValue.AsString, LDouble) then
        Exit(False);
  else
    Exit(True);
  end;

  Result := (LDouble >= -180) and (LDouble <= 180);
end;

{ MVCCountryCode }

constructor MVCCountryCode.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid ISO 3166-1 alpha-2 country code'
  else
    FErrorMessage := AMessage;
end;

function MVCCountryCode.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
  I: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := UpperCase(AValue.AsString);
  if LStr.IsEmpty then
    Exit(True);

  if Length(LStr) <> 2 then
    Exit(False);

  for I := Low(ISO_COUNTRY_CODES) to High(ISO_COUNTRY_CODES) do
    if ISO_COUNTRY_CODES[I] = LStr then
      Exit(True);

  Result := False;
end;

// ===========================================================================
// FORMAT VALIDATORS IMPLEMENTATION - BARCODES
// ===========================================================================

{ MVCEAN13 }

constructor MVCEAN13.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid EAN-13 barcode'
  else
    FErrorMessage := AMessage;
end;

function MVCEAN13.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidateEANChecksum(LStr);
end;

{ MVCISBN }

constructor MVCISBN.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid ISBN (ISBN-10 or ISBN-13)'
  else
    FErrorMessage := AMessage;
end;

function MVCISBN.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr, LClean: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  LClean := StringReplace(LStr, '-', '', [rfReplaceAll]);
  LClean := StringReplace(LClean, ' ', '', [rfReplaceAll]);

  case Length(LClean) of
    10: Result := ValidateISBN10(LStr);
    13: Result := ValidateISBN13(LStr);
  else
    Result := False;
  end;
end;

{ MVCVIN }

constructor MVCVIN.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid Vehicle Identification Number (VIN)'
  else
    FErrorMessage := AMessage;
end;

function MVCVIN.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidateVINCheckDigit(LStr);
end;

// ===========================================================================
// SECURITY VALIDATORS IMPLEMENTATION
// ===========================================================================

{ MVCStrongPassword }

constructor MVCStrongPassword.Create(const AMinLength, AMinUppercase,
  AMinLowercase, AMinDigits, AMinSpecial: Integer; const AMessage: string);
begin
  inherited Create;
  FMinLength := AMinLength;
  FMinUppercase := AMinUppercase;
  FMinLowercase := AMinLowercase;
  FMinDigits := AMinDigits;
  FMinSpecial := AMinSpecial;
  if AMessage.IsEmpty then
    FErrorMessage := Format('Password must be at least %d characters with %d uppercase, ' +
      '%d lowercase, %d digits and %d special characters',
      [FMinLength, FMinUppercase, FMinLowercase, FMinDigits, FMinSpecial])
  else
    FErrorMessage := AMessage;
end;

function MVCStrongPassword.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
  I, LUpper, LLower, LDigit, LSpecial: Integer;
  C: Char;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if Length(LStr) < FMinLength then
    Exit(False);

  LUpper := 0;
  LLower := 0;
  LDigit := 0;
  LSpecial := 0;

  for I := 1 to Length(LStr) do
  begin
    C := LStr[I];
    if C.IsUpper then
      Inc(LUpper)
    else if C.IsLower then
      Inc(LLower)
    else if C.IsDigit then
      Inc(LDigit)
    else if not C.IsWhiteSpace then
      Inc(LSpecial);
  end;

  Result := (LUpper >= FMinUppercase) and (LLower >= FMinLowercase) and
            (LDigit >= FMinDigits) and (LSpecial >= FMinSpecial);
end;

// ===========================================================================
// TAX ID VALIDATORS IMPLEMENTATION - EU
// ===========================================================================

{ MVCEUVatNumber }

constructor MVCEUVatNumber.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid EU VAT number'
  else
    FErrorMessage := AMessage;
end;

function MVCEUVatNumber.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr, LCountry, LNumber: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := UpperCase(StringReplace(AValue.AsString, ' ', '', [rfReplaceAll]));
  if LStr.IsEmpty then
    Exit(True);

  if Length(LStr) < 4 then
    Exit(False);

  LCountry := Copy(LStr, 1, 2);
  LNumber := Copy(LStr, 3, Length(LStr) - 2);

  case IndexStr(LCountry, ['AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'EL', 'ES',
                           'FI', 'FR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LU', 'LV', 'MT',
                           'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK']) of
    0: Result := TRegEx.IsMatch(LNumber, '^U\d{8}$');
    1: Result := TRegEx.IsMatch(LNumber, '^0\d{9}$');
    2: Result := TRegEx.IsMatch(LNumber, '^\d{9,10}$');
    3: Result := TRegEx.IsMatch(LNumber, '^\d{8}[A-Z]$');
    4: Result := TRegEx.IsMatch(LNumber, '^\d{8,10}$');
    5: Result := TRegEx.IsMatch(LNumber, '^\d{9}$');
    6: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    7: Result := TRegEx.IsMatch(LNumber, '^\d{9}$');
    8: Result := TRegEx.IsMatch(LNumber, '^\d{9}$');
    9: Result := TRegEx.IsMatch(LNumber, '^[A-Z0-9]\d{7}[A-Z0-9]$');
    10: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    11: Result := TRegEx.IsMatch(LNumber, '^[A-Z0-9]{2}\d{9}$');
    12: Result := TRegEx.IsMatch(LNumber, '^\d{11}$');
    13: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    14: Result := TRegEx.IsMatch(LNumber, '^\d{7}[A-Z]{1,2}$');
    15: Result := TRegEx.IsMatch(LNumber, '^\d{11}$');
    16: Result := TRegEx.IsMatch(LNumber, '^\d{9,12}$');
    17: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    18: Result := TRegEx.IsMatch(LNumber, '^\d{11}$');
    19: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    20: Result := TRegEx.IsMatch(LNumber, '^\d{9}B\d{2}$');
    21: Result := TRegEx.IsMatch(LNumber, '^\d{10}$');
    22: Result := TRegEx.IsMatch(LNumber, '^\d{9}$');
    23: Result := TRegEx.IsMatch(LNumber, '^\d{2,10}$');
    24: Result := TRegEx.IsMatch(LNumber, '^\d{12}$');
    25: Result := TRegEx.IsMatch(LNumber, '^\d{8}$');
    26: Result := TRegEx.IsMatch(LNumber, '^\d{10}$');
  else
    Result := False;
  end;
end;

// ===========================================================================
// TAX ID VALIDATORS IMPLEMENTATION - ITALY
// ===========================================================================

{ MVCITCodiceFiscale }

constructor MVCITCodiceFiscale.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid Italian Codice Fiscale'
  else
    FErrorMessage := AMessage;
end;

function MVCITCodiceFiscale.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidateCodiceFiscale(LStr);
end;

{ MVCITPartitaIVA }

constructor MVCITPartitaIVA.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid Italian Partita IVA'
  else
    FErrorMessage := AMessage;
end;

function MVCITPartitaIVA.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidatePartitaIVA(LStr);
end;

// ===========================================================================
// TAX ID VALIDATORS IMPLEMENTATION - USA
// ===========================================================================

{ MVCUSSSN }

constructor MVCUSSSN.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid US Social Security Number'
  else
    FErrorMessage := AMessage;
end;

function MVCUSSSN.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
  LArea, LGroup, LSerial: Integer;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := StringReplace(AValue.AsString, '-', '', [rfReplaceAll]);
  LStr := StringReplace(LStr, ' ', '', [rfReplaceAll]);

  if LStr.IsEmpty then
    Exit(True);

  if not TRegEx.IsMatch(LStr, '^\d{9}$') then
    Exit(False);

  LArea := StrToInt(Copy(LStr, 1, 3));
  LGroup := StrToInt(Copy(LStr, 4, 2));
  LSerial := StrToInt(Copy(LStr, 6, 4));

  if (LArea = 0) or (LArea = 666) or (LArea >= 900) then
    Exit(False);

  if (LGroup = 0) or (LSerial = 0) then
    Exit(False);

  Result := True;
end;

{ MVCUSAbaRouting }

constructor MVCUSAbaRouting.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid US ABA Routing Number'
  else
    FErrorMessage := AMessage;
end;

function MVCUSAbaRouting.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidateABARouting(LStr);
end;

// ===========================================================================
// TAX ID VALIDATORS IMPLEMENTATION - BRAZIL
// ===========================================================================

{ MVCBRCPF }

constructor MVCBRCPF.Create(const AMessage: string);
begin
  inherited Create;
  if AMessage.IsEmpty then
    FErrorMessage := 'Value must be a valid Brazilian CPF'
  else
    FErrorMessage := AMessage;
end;

function MVCBRCPF.Validate(const AValue: TValue; const AObject: TObject): Boolean;
var
  LStr: string;
begin
  if AValue.IsEmpty then
    Exit(True);

  LStr := AValue.AsString;
  if LStr.IsEmpty then
    Exit(True);

  Result := ValidateCPF(LStr);
end;

end.
