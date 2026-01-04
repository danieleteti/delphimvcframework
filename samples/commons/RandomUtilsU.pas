// ***************************************************************************
//
// Delphi Fake Data Utils
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphi_fake_data_utils
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

{$DEFINE GENERATE_DATASETS}

unit RandomUtilsU;

interface

{$IF Defined(GENERATE_DATASETS)}
uses
  Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  System.Generics.Collections;
{$ELSE}
uses
  System.Generics.Collections;
{$ENDIF}

// ============================================================================
// INTERNATIONALIZATION CONSTANTS
// ============================================================================

const
  // === ITALIAN NAMES ===
  FirstNamesIT: array [0 .. 29] of string = (
    'Daniele', 'Marco', 'Andrea', 'Francesco', 'Alessandro', 'Matteo', 'Lorenzo',
    'Gabriele', 'Mattia', 'Luca', 'Davide', 'Riccardo', 'Federico', 'Simone',
    'Giulia', 'Francesca', 'Chiara', 'Federica', 'Sara', 'Martina', 'Valentina',
    'Alessandra', 'Silvia', 'Elisa', 'Debora', 'Roberta', 'Laura', 'Paola',
    'Cristina', 'Monica'
  );

  LastNamesIT: array [0 .. 19] of string = (
    'Rossi', 'Ferrari', 'Russo', 'Bianchi', 'Romano', 'Colombo', 'Ricci',
    'Marino', 'Greco', 'Bruno', 'Gallo', 'Conti', 'De Luca', 'Mancini',
    'Costa', 'Giordano', 'Rizzo', 'Lombardi', 'Moretti', 'Teti'
  );

  // === ENGLISH NAMES ===
  FirstNamesEN: array [0 .. 29] of string = (
    'James', 'William', 'Joseph', 'David', 'Charles', 'Thomas', 'Christopher',
    'Daniel', 'Matthew', 'Anthony', 'Donald', 'Steven', 'Paul', 'Andrew',
    'Mary', 'Patricia', 'Jennifer', 'Linda', 'Elizabeth', 'Barbara', 'Susan',
    'Jessica', 'Sarah', 'Karen', 'Nancy', 'Lisa', 'Betty', 'Helen',
    'Sandra', 'Donna'
  );

  LastNamesEN: array [0 .. 19] of string = (
    'Smith', 'Johnson', 'Williams', 'Brown', 'Jones', 'Garcia', 'Miller',
    'Davis', 'Rodriguez', 'Martinez', 'Hernandez', 'Lopez', 'Gonzalez',
    'Wilson', 'Anderson', 'Thomas', 'Taylor', 'Moore', 'Jackson', 'Martin'
  );

  // === LOCATIONS ===

  // Italian Cities and Streets
  CitiesIT: array [0 .. 24] of string = (
    'Roma', 'Milano', 'Napoli', 'Torino', 'Palermo', 'Genova', 'Bologna',
    'Firenze', 'Bari', 'Catania', 'Venezia', 'Verona', 'Messina', 'Padova',
    'Trieste', 'Brescia', 'Parma', 'Prato', 'Modena', 'Reggio Calabria',
    'Reggio Emilia', 'Perugia', 'Livorno', 'Ravenna', 'Cagliari'
  );

  StreetsIT: array [0 .. 19] of string = (
    'Via Roma', 'Corso Italia', 'Via Nazionale', 'Piazza San Giovanni',
    'Via del Corso', 'Via Veneto', 'Largo Argentina', 'Via Appia',
    'Viale Trastevere', 'Via Sistina', 'Piazza Navona', 'Via Condotti',
    'Via Frattina', 'Via del Tritone', 'Corso Vittorio Emanuele',
    'Via Giulia', 'Via Margutta', 'Borgo Pio', 'Via Ottaviano', 'Via Cola di Rienzo'
  );

  // US Cities and Streets
  CitiesUS: array [0 .. 19] of string = (
    'New York', 'Los Angeles', 'Chicago', 'Houston', 'Phoenix', 'Philadelphia',
    'San Antonio', 'San Diego', 'Dallas', 'San Jose', 'Austin', 'Jacksonville',
    'Fort Worth', 'Columbus', 'Charlotte', 'San Francisco', 'Indianapolis',
    'Seattle', 'Denver', 'Washington DC'
  );

  StreetsUS: array [0 .. 19] of string = (
    'Main Street', 'First Street', 'Second Street', 'Third Street', 'Park Avenue',
    'Oak Street', 'Pine Street', 'Maple Avenue', 'Cedar Street', 'Elm Street',
    'Washington Street', 'Lake Street', 'Hill Street', 'Church Street',
    'Spring Street', 'State Street', 'High Street', 'School Street',
    'Union Street', 'Water Street'
  );

  // === BUSINESS DATA ===
  CompanyNames: array [0 .. 29] of string = (
    'TechSolutions', 'DataCorp', 'InnovateLab', 'DigitalPro', 'WebMaster',
    'CodeFactory', 'SmartSystems', 'NetWorking Plus', 'CloudTech', 'DevStudio',
    'AppBuilder', 'SoftwareHouse', 'TechInnovation', 'DigitalCraft',
    'WebDev Solutions', 'DataMining Corp', 'CloudSolutions', 'TechHub',
    'DevLab', 'CodeBase', 'CyberTech', 'InfoSystems', 'TechAdvanced',
    'GlobalTech', 'MegaSoft', 'UltraTech', 'PowerSoft', 'NextGen Tech',
    'FutureTech', 'InnovaTech'
  );

  Industries: array [0 .. 19] of string = (
    'Information Technology', 'Healthcare', 'Finance', 'Manufacturing',
    'Retail', 'Education', 'Automotive', 'Real Estate', 'Media',
    'Telecommunications', 'Energy', 'Transportation', 'Agriculture',
    'Construction', 'Hospitality', 'Banking', 'Insurance', 'Consulting',
    'Aerospace', 'Biotechnology'
  );

  JobTitles: array [0 .. 29] of string = (
    'Software Developer', 'Project Manager', 'System Administrator',
    'Database Administrator', 'Web Developer', 'DevOps Engineer',
    'Software Architect', 'Business Analyst', 'Quality Assurance',
    'Technical Lead', 'Full Stack Developer', 'Backend Developer',
    'Frontend Developer', 'Data Scientist', 'Product Manager',
    'UI/UX Designer', 'Security Specialist', 'Cloud Engineer',
    'Mobile Developer', 'Integration Specialist', 'Senior Developer',
    'Junior Developer', 'Team Lead', 'Engineering Manager',
    'Principal Engineer', 'Director', 'VP Engineering', 'CTO',
    'Chief Architect', 'Consultant'
  );

  // === PRODUCT DATA ===
  ProductNames: array [0 .. 29] of string = (
    'UltraTech Pro', 'SmartDevice X1', 'PowerTool 2024', 'EcoSolution',
    'QuickStart Kit', 'MegaBoost Plus', 'FlexiWork Station', 'TurboCharge',
    'SecureVault', 'DataStream Pro', 'CloudSync Elite', 'FastTrack',
    'SmartHome Hub', 'PowerGrid Max', 'EcoFriendly Pack', 'TechBooster',
    'QuickFix Tool', 'MegaPower Unit', 'FlexiSuite', 'TurboMax Pro',
    'CyberShield', 'DataGuard', 'CloudMaster', 'TechPilot', 'SmartCore',
    'PowerLink', 'UltraSync', 'MegaFlow', 'TechFusion', 'ProActive'
  );

  ProductCategories: array [0 .. 14] of string = (
    'Electronics', 'Software', 'Hardware', 'Accessories', 'Tools',
    'Services', 'Components', 'Devices', 'Systems', 'Solutions',
    'Security', 'Networking', 'Storage', 'Computing', 'Mobile'
  );

  Colors: array [0 .. 24] of string = (
    'Red', 'Blue', 'Green', 'Yellow', 'Orange', 'Purple', 'Pink',
    'Brown', 'Black', 'White', 'Gray', 'Cyan', 'Magenta', 'Lime',
    'Maroon', 'Navy', 'Olive', 'Teal', 'Silver', 'Gold',
    'Crimson', 'Azure', 'Beige', 'Coral', 'Indigo'
  );

  ColorHex: array [0 .. 24] of string = (
    '#FF0000', '#0000FF', '#008000', '#FFFF00', '#FFA500', '#800080',
    '#FFC0CB', '#A52A2A', '#000000', '#FFFFFF', '#808080', '#00FFFF',
    '#FF00FF', '#00FF00', '#800000', '#000080', '#808000', '#008080',
    '#C0C0C0', '#FFD700', '#DC143C', '#F0FFFF', '#F5F5DC', '#FF7F50', '#4B0082'
  );

  // === ORIGINAL CONSTANTS FOR BACKWARD COMPATIBILITY ===
  FirstNames: array [0 .. 29] of string = (
    'Daniele', 'Debora', 'Mattia', 'Jack', 'James', 'William', 'Joseph', 'David',
    'Charles', 'Thomas', 'Ethan', 'Liam', 'Noah', 'Logan', 'Lucas', 'Mason',
    'Benjamin', 'Alexander', 'Elijah', 'Jordan', 'Alexander', 'Jamie', 'Tyler',
    'Caleb', 'Kieran', 'Ryan', 'Colton', 'Jaxon', 'Gavin', 'Ryder'
  );

  LastNames: array [0 .. 13] of string = (
    'Smith', 'Johnson', 'Williams', 'Brown', 'Black', 'Red', 'Green', 'Willis',
    'Jones', 'Miller', 'Davis', 'Wilson', 'Martinez', 'Anderson'
  );

  Countries: array [0 .. 24] of string = (
    'italy', 'new york', 'illinois', 'arizona', 'nevada', 'uk', 'france',
    'georgia', 'spain', 'portugal', 'germany', 'norway', 'california', 'usa',
    'japan', 'australia', 'singapore', 'hong kong', 'taiwan', 'south africa',
    'canada', 'switzerland', 'sweden', 'netherlands', 'belgium'
  );

  LOREM_IPSUM =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.' +
    'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.' +
    'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.' +
    'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';

  WORDS: array [0 .. 199] of string = (
    'bite', 'mate', 'quill', 'back', 'church', 'pear', 'knit', 'bent', 'wrench', 'crack',
    'heavenly', 'deceive', 'maddening', 'plain', 'writer', 'rapid', 'acidic', 'decide', 'hat', 'paint',
    'cow', 'dysfunctional', 'pet', 'giraffe', 'connection', 'sour', 'voracious', 'cloudy', 'wry', 'curve',
    'agree', 'eggnog', 'flaky', 'painstaking', 'warm', 'silk', 'icy', 'hellish', 'toy', 'milky',
    'skirt', 'test', 'daffy', 'questionable', 'gamy', 'aware', 'berry', 'throne', 'oven', 'subtract',
    'cool', 'care', 'charge', 'smash', 'curve', 'comfortable', 'narrow', 'merciful', 'material', 'fear',
    'exercise', 'skinny', 'fire', 'rainstorm', 'tail', 'nondescript', 'calculating', 'pack', 'steel', 'marvelous',
    'baseball', 'furtive', 'stitch', 'abiding', 'empty', 'bushes', 'painful', 'tense', 'verse', 'unwritten',
    'reproduce', 'receptive', 'bottle', 'silky', 'alleged', 'stingy', 'irritate', 'expand', 'cap', 'unsuitable',
    'gigantic', 'exist', 'damp', 'scrub', 'disgusted', 'sun', 'ink', 'detailed', 'defeated', 'economic',
    'chunky', 'stop', 'overflow', 'numerous', 'joyous', 'wipe', 'drink', 'error', 'branch', 'male',
    'proud', 'soggy', 'ship', 'excite', 'industry', 'wistful', 'man', 'vacation', 'doctor', 'naughty',
    'plane', 'ignore', 'open', 'act', 'earthquake', 'inconclusive', 'reflect', 'force', 'funny', 'wonder',
    'magenta', 'near', 'dam', 'windy', 'maid', 'wacky', 'release', 'birthday', 'statement', 'psychotic',
    'quicksand', 'things', 'planes', 'boundary', 'nod', 'touch', 'argue', 'sin', 'train', 'adhoc',
    'needle', 'regret', 'stroke', 'strengthen', 'bruise', 'mine', 'rod', 'tax', 'twig', 'advise',
    'stamp', 'rhyme', 'obnoxious', 'few', 'inform', 'fixed', 'mailbox', 'bells', 'grade', 'machine',
    'yarn', 'lighten', 'tub', 'guiltless', 'hot', 'misty', 'van', 'flap', 'nosy', 'neighborly',
    'crime', 'nifty', 'uninterested', 'noisy', 'oafish', 'squeal', 'page', 'wet', 'embarrassed', 'long-term',
    'closed', 'language', 'argument', 'elite', 'ban', 'trip', 'tour', 'wine', 'profit', 'envious'
  );

// ============================================================================
// TYPES AND ENUMS
// ============================================================================

type
  TJobLevel = (jlJunior, jlMid, jlSenior);
  TLocale = (lcIT, lcUS, lcUK, lcDE, lcFR);

  TPerson = class
  private
    FID: Integer;
    FName: String;
    FEMail: String;
    FSurname: String;
    FDOB: TDate;
    FJobTitle: String;
    FSalary: Currency;
    FAddress: String;
    FCity: String;
    FCountry: String;
    FPhoneNumber: String;
  public
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Surname: String read FSurname write FSurname;
    property DOB: TDate read FDOB write FDOB;
    property EMail: String read FEMail write FEMail;
    property JobTitle: String read FJobTitle write FJobTitle;
    property Salary: Currency read FSalary write FSalary;
    property Address: String read FAddress write FAddress;
    property City: String read FCity write FCity;
    property Country: String read FCountry write FCountry;
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
    constructor Create; overload;
    constructor Create(const ID: Integer; const Name, Surname: String; const DOB: TDate; const EMail: String); overload;
  end;

  TCompany = class
  private
    FID: Integer;
    FName: String;
    FIndustry: String;
    FAddress: String;
    FWebsite: String;
    FFoundedYear: Integer;
    FEmployeeCount: Integer;
  public
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Industry: String read FIndustry write FIndustry;
    property Address: String read FAddress write FAddress;
    property Website: String read FWebsite write FWebsite;
    property FoundedYear: Integer read FFoundedYear write FFoundedYear;
    property EmployeeCount: Integer read FEmployeeCount write FEmployeeCount;
    constructor Create;
  end;

  TProduct = class
  private
    FID: Integer;
    FName: String;
    FCategory: String;
    FPrice: Currency;
    FColor: String;
    FDescription: String;
    FSKU: String;
  public
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Category: String read FCategory write FCategory;
    property Price: Currency read FPrice write FPrice;
    property Color: String read FColor write FColor;
    property Description: String read FDescription write FDescription;
    property SKU: String read FSKU write FSKU;
    constructor Create;
  end;

// ============================================================================
// BACKWARD COMPATIBILITY FUNCTIONS (Original API)
// ============================================================================

function GetRndFirstName: String; overload;
function GetRndLastName: String; overload;
function GetRndFullName: String; overload;
function GetRndCountry: String;
function GetRndEMailAddress: String; overload;
function GetRndFrom(const ArrayOfString: TArray<String>): String; overload;
function GetRndFrom(const ArrayOfInteger: TArray<Integer>): Integer; overload;
function GetRndDate(const InitialYear: Word = 1980; YearsSpan: Word = 40): TDate; overload;
function GetRndDate(const InitialDate: TDate; const DaysSpan: Word = 365): TDate; overload;
function GetRndInteger(const aFrom: Integer = 0; aTo: Integer = 1000): Integer;
function GetRndFloat(const aFrom: Extended = 0; aTo: Extended = 1000): Extended;
function GetRndWord: String;
function GetRndPhrase(const aFrom: Integer = 0; aTo: Integer = 1000): String;
function GetPeopleObjectList(const Count: Integer): TObjectList<TPerson>;

// ============================================================================
// ENHANCED INTERNATIONALIZATION FUNCTIONS
// ============================================================================

function GetRndFirstName(const Locale: TLocale): String; overload;
function GetRndLastName(const Locale: TLocale): String; overload;
function GetRndFullName(const Locale: TLocale): String; overload;
function GetRndEMailAddress(const Locale: TLocale): String; overload;
function GetRndEMailAddress(const FirstName, LastName: String; const Locale: TLocale = lcIT): String; overload;

// ============================================================================
// LOCATION FUNCTIONS
// ============================================================================

function GetRndCity(const Locale: TLocale = lcIT): String;
function GetRndStreet(const Locale: TLocale = lcIT): String;
function GetRndZipCode(const Locale: TLocale = lcIT): String;
function GetRndAddress(const Locale: TLocale = lcIT): String; overload;
function GetRndAddress(const Street, City: String; const Locale: TLocale = lcIT): String; overload;
function GetRndPhoneNumber(const Locale: TLocale = lcIT): String;

// ============================================================================
// BUSINESS FUNCTIONS
// ============================================================================

function GetRndCompanyName: String;
function GetRndIndustry: String;
function GetRndJobTitle: String;
function GetRndSalary(const MinSalary: Currency = 25000; MaxSalary: Currency = 150000): Currency;

// ============================================================================
// PRODUCT FUNCTIONS
// ============================================================================

function GetRndProductName: String;
function GetRndProductCategory: String;
function GetRndSKU: String;
function GetRndPrice(const MinPrice: Currency = 9.99; MaxPrice: Currency = 999.99): Currency;

// ============================================================================
// COLOR AND VISUAL FUNCTIONS
// ============================================================================

function GetRndColor: String;
function GetRndColorHex: String;
function GetRndColorPair: String; // Returns "ColorName (#HEX)"

// ============================================================================
// TECHNICAL DATA FUNCTIONS
// ============================================================================

function GetRndURL: String; overload;
function GetRndURL(const Domain: String; const Secure: Boolean = True): String; overload;
function GetRndIPAddress: String; overload;
function GetRndIPAddress(const LocalNetwork: Boolean): String; overload;
function GetRndMacAddress: String;

// ============================================================================
// FINANCIAL DATA FUNCTIONS (FAKE FOR TESTING)
// ============================================================================

function GetRndCreditCardNumber(const CardType: String = 'VISA'): String;
function GetRndIBAN(const CountryCode: String = 'IT'): String;
function GetRndBankAccount: String;

// ============================================================================
// VALIDATORS
// ============================================================================

function IsValidFakeEmail(const Email: String): Boolean;
function IsValidFakeIBAN(const IBAN: String): Boolean;
function IsValidFakeCreditCard(const CreditCard: String): Boolean;

// ============================================================================
// DATASET GENERATION FUNCTIONS
// ============================================================================

{$IF Defined(GENERATE_DATASETS)}
function GetPeople(const Count: Integer = 20): TDataSet; overload;
function GetPeople(const Count: Integer; const Locale: TLocale): TDataSet; overload;
function GetUsers(const Count: Integer = 10): TDataSet; overload;
function GetUsers(const Count: Integer; const Locale: TLocale): TDataSet; overload;
function GetPosts(const Count: Integer = 10): TDataSet;
function GetCompanies(const Count: Integer = 10; const Locale: TLocale = lcIT): TDataSet;
function GetProducts(const Count: Integer = 20): TDataSet;
function GetEmployees(const Count: Integer = 50; const Locale: TLocale = lcIT): TDataSet;
{$ENDIF}

implementation

uses
  System.SysUtils, System.DateUtils, System.Math, System.StrUtils, System.RegularExpressions;

const
  OneDay = OneHour * 24;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

function GetLocaleFirstNames(const Locale: TLocale): TArray<String>;
begin
  case Locale of
    lcIT:
    begin
      SetLength(Result, Length(FirstNamesIT));
      Move(FirstNamesIT[0], Result[0], Length(FirstNamesIT) * SizeOf(String));
    end;
    lcUS, lcUK:
    begin
      SetLength(Result, Length(FirstNamesEN));
      Move(FirstNamesEN[0], Result[0], Length(FirstNamesEN) * SizeOf(String));
    end;
  else
    begin
      SetLength(Result, Length(FirstNamesIT));
      Move(FirstNamesIT[0], Result[0], Length(FirstNamesIT) * SizeOf(String));
    end;
  end;
end;

function GetLocaleLastNames(const Locale: TLocale): TArray<String>;
begin
  case Locale of
    lcIT:
    begin
      SetLength(Result, Length(LastNamesIT));
      Move(LastNamesIT[0], Result[0], Length(LastNamesIT) * SizeOf(String));
    end;
    lcUS, lcUK:
    begin
      SetLength(Result, Length(LastNamesEN));
      Move(LastNamesEN[0], Result[0], Length(LastNamesEN) * SizeOf(String));
    end;
  else
    begin
      SetLength(Result, Length(LastNamesIT));
      Move(LastNamesIT[0], Result[0], Length(LastNamesIT) * SizeOf(String));
    end;
  end;
end;

// ============================================================================
// BACKWARD COMPATIBILITY IMPLEMENTATION
// ============================================================================

function GetRndDate(const InitialDate: TDate; const DaysSpan: Word = 365): TDate;
begin
  Result := InitialDate + (OneDay * Random(DaysSpan));
end;

function GetRndDate(const InitialYear: Word; YearsSpan: Word): TDate;
begin
  Result := EncodeDate(InitialYear + Random(YearsSpan), 1, 1) + (OneDay * Random(365));
end;

function GetRndEMailAddress: String;
begin
  Result := GetRndEMailAddress(lcIT);
end;

function GetRndFrom(const ArrayOfString: TArray<String>): String;
begin
  if Length(ArrayOfString) = 0 then
    raise Exception.Create('Array cannot be empty');
  Result := ArrayOfString[Random(Length(ArrayOfString))];
end;

function GetRndFrom(const ArrayOfInteger: TArray<Integer>): Integer;
begin
  if Length(ArrayOfInteger) = 0 then
    raise Exception.Create('Array cannot be empty');
  Result := ArrayOfInteger[Random(Length(ArrayOfInteger))];
end;

function GetRndCountry: String;
begin
  Result := Countries[Random(High(Countries) + 1)];
end;

function GetRndFirstName: String;
begin
  Result := FirstNames[Random(High(FirstNames) + 1)];
end;

function GetRndFullName: String;
begin
  Result := GetRndFirstName + ' ' + GetRndLastName;
end;

function GetRndLastName: String;
begin
  Result := LastNames[Random(High(LastNames) + 1)];
end;

function GetRndWord: String;
begin
  Result := WORDS[Random(Length(WORDS))];
end;

function GetRndPhrase(const aFrom: Integer = 0; aTo: Integer = 1000): String;
var
  WordCount: Integer;
  I: Integer;
begin
  if aFrom >= aTo then
    raise Exception.Create('FROM cannot be greater than or equal to TO');

  Result := '';
  WordCount := RandomRange(aFrom, aTo);
  for I := 1 to WordCount do
  begin
    Result := Result + GetRndWord + ' ';
  end;
  Result := Result.Trim;
  if Result <> '' then
  begin
    Result := UpCase(Result.Chars[0]) + Result.Substring(1) + '.';
  end;
end;

function GetRndFloat(const aFrom: Extended = 0; aTo: Extended = 1000): Extended;
begin
  if aFrom >= aTo then
    raise Exception.Create('FROM cannot be greater than or equal to TO');
  Result := aFrom + (Random * (aTo - aFrom));
end;

function GetRndInteger(const aFrom: Integer; aTo: Integer): Integer;
begin
  if aFrom >= aTo then
    raise Exception.Create('FROM cannot be greater than or equal to TO');
  Result := RandomRange(aFrom, aTo);
end;

// ============================================================================
// ENHANCED INTERNATIONALIZATION IMPLEMENTATION
// ============================================================================

function GetRndFirstName(const Locale: TLocale): String;
var
  Names: TArray<String>;
begin
  Names := GetLocaleFirstNames(Locale);
  Result := Names[Random(Length(Names))];
end;

function GetRndLastName(const Locale: TLocale): String;
var
  Names: TArray<String>;
begin
  Names := GetLocaleLastNames(Locale);
  Result := Names[Random(Length(Names))];
end;

function GetRndFullName(const Locale: TLocale): String;
begin
  Result := GetRndFirstName(Locale) + ' ' + GetRndLastName(Locale);
end;

function GetRndEMailAddress(const Locale: TLocale): String;
var
  FirstName, LastName: String;
begin
  FirstName := GetRndFirstName(Locale).ToLower;
  LastName := GetRndLastName(Locale).ToLower;
  Result := GetRndEMailAddress(FirstName, LastName, Locale);
end;

function GetRndEMailAddress(const FirstName, LastName: String; const Locale: TLocale = lcIT): String;
var
  Domains: TArray<String>;
  CleanFirstName, CleanLastName: String;
begin
  // Clean names for email compatibility
  CleanFirstName := FirstName.ToLower.Replace(' ', '').Replace('''', '');
  CleanLastName := LastName.ToLower.Replace(' ', '').Replace('''', '');

  case Locale of
    lcIT: Domains := TArray<String>.Create('libero.it', 'gmail.com', 'outlook.it', 'tiscali.it', 'alice.it');
    lcUS: Domains := TArray<String>.Create('gmail.com', 'yahoo.com', 'hotmail.com', 'outlook.com', 'aol.com');
    lcUK: Domains := TArray<String>.Create('gmail.co.uk', 'yahoo.co.uk', 'hotmail.co.uk', 'outlook.co.uk', 'btinternet.com');
  else
    Domains := TArray<String>.Create('gmail.com', 'yahoo.com', 'hotmail.com');
  end;

  case Random(4) of
    0: Result := Format('%s.%s@%s', [CleanFirstName, CleanLastName, GetRndFrom(Domains)]);
    1: Result := Format('%s%s@%s', [CleanFirstName, CleanLastName, GetRndFrom(Domains)]);
    2: Result := Format('%s_%s@%s', [CleanFirstName, CleanLastName, GetRndFrom(Domains)]);
    3: Result := Format('%s%d@%s', [CleanFirstName, GetRndInteger(1, 999), GetRndFrom(Domains)]);
  end;
end;

// ============================================================================
// LOCATION IMPLEMENTATION
// ============================================================================

function GetRndCity(const Locale: TLocale = lcIT): String;
begin
  case Locale of
    lcIT: Result := CitiesIT[Random(Length(CitiesIT))];
    lcUS: Result := CitiesUS[Random(Length(CitiesUS))];
  else
    Result := CitiesIT[Random(Length(CitiesIT))]; // Fallback
  end;
end;

function GetRndStreet(const Locale: TLocale = lcIT): String;
begin
  case Locale of
    lcIT: Result := StreetsIT[Random(Length(StreetsIT))];
    lcUS: Result := StreetsUS[Random(Length(StreetsUS))];
  else
    Result := StreetsIT[Random(Length(StreetsIT))]; // Fallback
  end;
end;

function GetRndZipCode(const Locale: TLocale = lcIT): String;
begin
  case Locale of
    lcIT: Result := Format('%05d', [GetRndInteger(10000, 99999)]);
    lcUS: Result := Format('%05d', [GetRndInteger(10000, 99999)]);
    lcUK: Result := Format('%s%d %d%s%s', [Chr(Ord('A') + Random(26)), Random(10), Random(10), Chr(Ord('A') + Random(26)), Chr(Ord('A') + Random(26))]);
  else
    Result := Format('%05d', [GetRndInteger(10000, 99999)]);
  end;
end;

function GetRndAddress(const Locale: TLocale = lcIT): String;
begin
  Result := Format('%s %d, %s %s', [
    GetRndStreet(Locale),
    GetRndInteger(1, 999),
    GetRndZipCode(Locale),
    GetRndCity(Locale)
  ]);
end;

function GetRndAddress(const Street, City: String; const Locale: TLocale = lcIT): String;
begin
  Result := Format('%s %d, %s %s', [
    Street,
    GetRndInteger(1, 999),
    GetRndZipCode(Locale),
    City
  ]);
end;

function GetRndPhoneNumber(const Locale: TLocale = lcIT): String;
begin
  case Locale of
    lcIT: Result := Format('+39 %03d %03d %04d', [GetRndInteger(300, 399), GetRndInteger(100, 999), GetRndInteger(1000, 9999)]);
    lcUS: Result := Format('+1 (%03d) %03d-%04d', [GetRndInteger(200, 999), GetRndInteger(100, 999), GetRndInteger(1000, 9999)]);
    lcUK: Result := Format('+44 %d %04d %04d', [GetRndInteger(10, 99), GetRndInteger(1000, 9999), GetRndInteger(1000, 9999)]);
  else
    Result := Format('+39 %03d %03d %04d', [GetRndInteger(300, 399), GetRndInteger(100, 999), GetRndInteger(1000, 9999)]);
  end;
end;

// ============================================================================
// BUSINESS IMPLEMENTATION
// ============================================================================

function GetRndCompanyName: String;
begin
  Result := CompanyNames[Random(Length(CompanyNames))];
end;

function GetRndIndustry: String;
begin
  Result := Industries[Random(Length(Industries))];
end;

function GetRndJobTitle: String;
begin
  Result := JobTitles[Random(Length(JobTitles))];
end;

function GetRndSalary(const MinSalary: Currency = 25000; MaxSalary: Currency = 150000): Currency;
begin
  if MinSalary >= MaxSalary then
    raise Exception.Create('MinSalary cannot be greater than or equal to MaxSalary');
  Result := MinSalary + (Random * (MaxSalary - MinSalary));
  Result := Round(Result / 1000) * 1000; // Round to nearest thousand
end;

// ============================================================================
// PRODUCT IMPLEMENTATION
// ============================================================================

function GetRndProductName: String;
begin
  Result := ProductNames[Random(Length(ProductNames))];
end;

function GetRndProductCategory: String;
begin
  Result := ProductCategories[Random(Length(ProductCategories))];
end;

function GetRndSKU: String;
begin
  Result := Format('%s-%04d-%s', [
    Chr(Ord('A') + Random(26)) + Chr(Ord('A') + Random(26)),
    GetRndInteger(1000, 9999),
    Chr(Ord('A') + Random(26)) + Chr(Ord('A') + Random(26))
  ]);
end;

function GetRndPrice(const MinPrice: Currency = 9.99; MaxPrice: Currency = 999.99): Currency;
begin
  if MinPrice >= MaxPrice then
    raise Exception.Create('MinPrice cannot be greater than or equal to MaxPrice');
  Result := MinPrice + (Random * (MaxPrice - MinPrice));
  Result := Round(Result * 100) / 100; // Round to 2 decimal places
end;

// ============================================================================
// COLOR AND VISUAL IMPLEMENTATION
// ============================================================================

function GetRndColor: String;
begin
  Result := Colors[Random(Length(Colors))];
end;

function GetRndColorHex: String;
begin
  Result := ColorHex[Random(Length(ColorHex))];
end;

function GetRndColorPair: String;
var
  Index: Integer;
begin
  Index := Random(Length(Colors));
  Result := Format('%s (%s)', [Colors[Index], ColorHex[Index]]);
end;

// ============================================================================
// TECHNICAL DATA IMPLEMENTATION
// ============================================================================

function GetRndURL: String;
var
  Protocols: TArray<String>;
  TLDs: TArray<String>;
begin
  Protocols := TArray<String>.Create('http', 'https');
  TLDs := TArray<String>.Create('com', 'org', 'net', 'it', 'eu');
  Result := Format('%s://www.%s%d.%s', [
    GetRndFrom(Protocols),
    GetRndWord.ToLower,
    GetRndInteger(1, 999),
    GetRndFrom(TLDs)
  ]);
end;

function GetRndURL(const Domain: String; const Secure: Boolean = True): String;
var
  Protocol: String;
begin
  if Secure then
    Protocol := 'https'
  else
    Protocol := 'http';
  Result := Format('%s://%s', [Protocol, Domain]);
end;

function GetRndIPAddress: String;
begin
  Result := GetRndIPAddress(False);
end;

function GetRndIPAddress(const LocalNetwork: Boolean): String;
begin
  if LocalNetwork then
  begin
    case Random(3) of
      0: Result := Format('192.168.%d.%d', [GetRndInteger(1, 255), GetRndInteger(1, 254)]);
      1: Result := Format('10.%d.%d.%d', [GetRndInteger(0, 255), GetRndInteger(0, 255), GetRndInteger(1, 254)]);
      2: Result := Format('172.%d.%d.%d', [GetRndInteger(16, 31), GetRndInteger(0, 255), GetRndInteger(1, 254)]);
    end;
  end
  else
  begin
    Result := Format('%d.%d.%d.%d', [
      GetRndInteger(1, 255),
      GetRndInteger(0, 255),
      GetRndInteger(0, 255),
      GetRndInteger(1, 255)
    ]);
  end;
end;

function GetRndMacAddress: String;
var
  I: Integer;
  MacBytes: array[0..5] of String;
begin
  for I := 0 to 5 do
    MacBytes[I] := Format('%02X', [GetRndInteger(0, 255)]);
  Result := String.Join(':', MacBytes);
end;

// ============================================================================
// FINANCIAL DATA IMPLEMENTATION (FAKE FOR TESTING)
// ============================================================================

function GetRndCreditCardNumber(const CardType: String = 'VISA'): String;
var
  I: Integer;
  CC: String;
  CardTypeUpper: String;
begin
  // Generate fake numbers for testing - NOT VALID for real transactions
  CardTypeUpper := UpperCase(CardType);

  if CardTypeUpper = 'MASTERCARD' then
    CC := '5' + StringOfChar('0', 15)
  else if CardTypeUpper = 'AMEX' then
    CC := '3' + StringOfChar('0', 14)
  else
    CC := '4' + StringOfChar('0', 15); // Default to VISA format

  // Fill with random digits (except first digit)
  for I := 2 to Length(CC) do
    CC[I] := Chr(Ord('0') + Random(10));

  // Format based on card type
  if CardTypeUpper = 'AMEX' then
    Result := Format('%s-%s-%s', [CC.Substring(0, 4), CC.Substring(4, 6), CC.Substring(10, 5)])
  else
    Result := Format('%s-%s-%s-%s', [CC.Substring(0, 4), CC.Substring(4, 4), CC.Substring(8, 4), CC.Substring(12, 4)]);
end;

function GetRndIBAN(const CountryCode: String = 'IT'): String;
var
  CheckDigits: String;
  BankCode, AccountNumber: String;
  CountryUpper: String;
  BankLetter: Char;
begin
  // Generate fake IBAN for testing purposes
  CheckDigits := Format('%02d', [GetRndInteger(10, 99)]);
  CountryUpper := UpperCase(CountryCode);
  BankLetter := Chr(Ord('A') + Random(26));

  if CountryUpper = 'IT' then
  begin
    BankCode := Format('%s %04d %04d', [BankLetter, GetRndInteger(1000, 9999), GetRndInteger(1000, 9999)]);
    AccountNumber := Format('%012d', [Int64(GetRndInteger(100000000, 999999999)) * 1000 + GetRndInteger(100, 999)]);
    Result := Format('IT%s %s %s', [CheckDigits, BankCode, AccountNumber]);
  end
  else
  begin
    // Default format
    BankCode := Format('%s %04d %04d', [BankLetter, GetRndInteger(1000, 9999), GetRndInteger(1000, 9999)]);
    AccountNumber := Format('%012d', [Int64(GetRndInteger(100000000, 999999999)) * 1000 + GetRndInteger(100, 999)]);
    Result := Format('%s%s %s %s', [CountryCode, CheckDigits, BankCode, AccountNumber]);
  end;
end;

function GetRndBankAccount: String;
begin
  Result := Format('%04d-%04d-%04d-%04d', [
    GetRndInteger(1000, 9999),
    GetRndInteger(1000, 9999),
    GetRndInteger(1000, 9999),
    GetRndInteger(1000, 9999)
  ]);
end;

// ============================================================================
// VALIDATORS IMPLEMENTATION
// ============================================================================

function IsValidFakeEmail(const Email: String): Boolean;
begin
  Result := TRegEx.IsMatch(Email, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

function IsValidFakeIBAN(const IBAN: String): Boolean;
var
  CleanIBAN: String;
begin
  CleanIBAN := IBAN.Replace(' ', '');
  Result := (Length(CleanIBAN) >= 15) and (Length(CleanIBAN) <= 34) and
            TRegEx.IsMatch(CleanIBAN, '^[A-Z]{2}[0-9]{2}[A-Z0-9]+$');
end;

function IsValidFakeCreditCard(const CreditCard: String): Boolean;
var
  CleanCC: String;
begin
  CleanCC := CreditCard.Replace('-', '').Replace(' ', '');
  Result := TRegEx.IsMatch(CleanCC, '^[0-9]{13,19}$');
end;

// ============================================================================
// CLASSES IMPLEMENTATION
// ============================================================================

{ TPerson }

constructor TPerson.Create;
begin
  inherited Create;
  FID := 0;
  FName := '';
  FSurname := '';
  FDOB := 0;
  FEMail := '';
  FJobTitle := '';
  FSalary := 0;
  FAddress := '';
  FCity := '';
  FCountry := '';
  FPhoneNumber := '';
end;

constructor TPerson.Create(const ID: Integer; const Name, Surname: String; const DOB: TDate; const EMail: String);
begin
  inherited Create;
  FID := ID;
  FName := Name;
  FSurname := Surname;
  FDOB := DOB;
  FEMail := EMail;
  FJobTitle := '';
  FSalary := 0;
  FAddress := '';
  FCity := '';
  FCountry := '';
  FPhoneNumber := '';
end;

{ TCompany }

constructor TCompany.Create;
begin
  inherited Create;
  FID := 0;
  FName := '';
  FIndustry := '';
  FAddress := '';
  FWebsite := '';
  FFoundedYear := 0;
  FEmployeeCount := 0;
end;

{ TProduct }

constructor TProduct.Create;
begin
  inherited Create;
  FID := 0;
  FName := '';
  FCategory := '';
  FPrice := 0;
  FColor := '';
  FDescription := '';
  FSKU := '';
end;

// ============================================================================
// OBJECT LIST FUNCTIONS
// ============================================================================

function GetPeopleObjectList(const Count: Integer): TObjectList<TPerson>;
var
  I: Integer;
begin
  Result := TObjectList<TPerson>.Create;
  try
    for I := 1 to Count do
    begin
      Result.Add(TPerson.Create(I, GetRndFirstName, GetRndLastName, GetRndDate, GetRndEMailAddress));
    end;
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// DATASET GENERATION IMPLEMENTATION
// ============================================================================

{$IF Defined(GENERATE_DATASETS)}

function GetPeople(const Count: Integer = 20): TDataSet;
begin
  Result := GetPeople(Count, lcIT);
end;

function GetPeople(const Count: Integer; const Locale: TLocale): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('code', ftInteger);
    lMT.FieldDefs.Add('first_name', ftString, 50);
    lMT.FieldDefs.Add('last_name', ftString, 50);
    lMT.FieldDefs.Add('email', ftString, 100);
    lMT.FieldDefs.Add('country', ftString, 50);
    lMT.FieldDefs.Add('city', ftString, 50);
    lMT.FieldDefs.Add('address', ftString, 200);
    lMT.FieldDefs.Add('phone', ftString, 20);
    lMT.FieldDefs.Add('dob', ftDate);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        I,
        GetRndFirstName(Locale),
        GetRndLastName(Locale),
        GetRndEMailAddress(Locale),
        GetRndCountry,
        GetRndCity(Locale),
        GetRndAddress(Locale),
        GetRndPhoneNumber(Locale),
        GetRndDate
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetUsers(const Count: Integer = 10): TDataSet;
begin
  Result := GetUsers(Count, lcIT);
end;

function GetUsers(const Count: Integer; const Locale: TLocale): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('first_name', ftString, 100);
    lMT.FieldDefs.Add('last_name', ftString, 100);
    lMT.FieldDefs.Add('email', ftString, 100);
    lMT.FieldDefs.Add('phone', ftString, 20);
    lMT.FieldDefs.Add('address', ftString, 200);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        GetRndFirstName(Locale),
        GetRndLastName(Locale),
        GetRndEMailAddress(Locale),
        GetRndPhoneNumber(Locale),
        GetRndAddress(Locale)
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetPosts(const Count: Integer = 10): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('title', ftString, 100);
    lMT.FieldDefs.Add('abstract', ftString, 500);
    lMT.FieldDefs.Add('word_count', ftInteger);
    lMT.FieldDefs.Add('comments', ftInteger);
    lMT.FieldDefs.Add('post_date', ftDate);
    lMT.FieldDefs.Add('author', ftString, 100);
    lMT.FieldDefs.Add('category', ftString, 50);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        GetRndPhrase(3, 8),
        GetRndPhrase(30, 50),
        GetRndInteger(20, 5000),
        GetRndInteger(0, 20),
        GetRndDate(2020, 4),
        GetRndFullName,
        GetRndProductCategory
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetCompanies(const Count: Integer = 10; const Locale: TLocale = lcIT): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('id', ftInteger);
    lMT.FieldDefs.Add('name', ftString, 100);
    lMT.FieldDefs.Add('industry', ftString, 100);
    lMT.FieldDefs.Add('address', ftString, 200);
    lMT.FieldDefs.Add('website', ftString, 100);
    lMT.FieldDefs.Add('founded_year', ftInteger);
    lMT.FieldDefs.Add('employee_count', ftInteger);
    lMT.FieldDefs.Add('country', ftString, 50);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        I,
        GetRndCompanyName,
        GetRndIndustry,
        GetRndAddress(Locale),
        GetRndURL,
        GetRndInteger(1950, YearOf(Now) - 1),
        GetRndInteger(10, 5000),
        GetRndCountry
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetProducts(const Count: Integer = 20): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('id', ftInteger);
    lMT.FieldDefs.Add('sku', ftString, 20);
    lMT.FieldDefs.Add('name', ftString, 100);
    lMT.FieldDefs.Add('category', ftString, 50);
    lMT.FieldDefs.Add('price', ftCurrency);
    lMT.FieldDefs.Add('color', ftString, 30);
    lMT.FieldDefs.Add('description', ftString, 500);
    lMT.FieldDefs.Add('in_stock', ftBoolean);
    lMT.FieldDefs.Add('stock_quantity', ftInteger);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        I,
        GetRndSKU,
        GetRndProductName,
        GetRndProductCategory,
        GetRndPrice(9.99, 999.99),
        GetRndColor,
        GetRndPhrase(10, 30),
        Random(10) > 1, // 90% in stock
        GetRndInteger(0, 1000)
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

function GetEmployees(const Count: Integer = 50; const Locale: TLocale = lcIT): TDataSet;
var
  lMT: TFDMemTable;
  I: Integer;
begin
  lMT := TFDMemTable.Create(nil);
  try
    lMT.FieldDefs.Clear;
    lMT.FieldDefs.Add('id', ftInteger);
    lMT.FieldDefs.Add('employee_code', ftString, 10);
    lMT.FieldDefs.Add('first_name', ftString, 50);
    lMT.FieldDefs.Add('last_name', ftString, 50);
    lMT.FieldDefs.Add('email', ftString, 100);
    lMT.FieldDefs.Add('phone', ftString, 20);
    lMT.FieldDefs.Add('job_title', ftString, 100);
    lMT.FieldDefs.Add('salary', ftCurrency);
    lMT.FieldDefs.Add('hire_date', ftDate);
    lMT.FieldDefs.Add('address', ftString, 200);
    lMT.FieldDefs.Add('department', ftString, 50);
    lMT.Active := True;

    for I := 1 to Count do
    begin
      lMT.AppendRecord([
        I,
        Format('EMP%04d', [I]),
        GetRndFirstName(Locale),
        GetRndLastName(Locale),
        GetRndEMailAddress(Locale),
        GetRndPhoneNumber(Locale),
        GetRndJobTitle,
        GetRndSalary(25000, 150000),
        GetRndDate(2015, 9),
        GetRndAddress(Locale),
        GetRndIndustry
      ]);
    end;
    lMT.First;
    Result := lMT;
  except
    lMT.Free;
    raise;
  end;
end;

{$ENDIF}

initialization

Randomize;

end.
