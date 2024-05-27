unit sqids;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  SysUtils, StrUtils
  {$ELSE}
  System.SysUtils, System.StrUtils
  {$ENDIF}
  ;

const
  DEFAULT_ALPHABET    = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  DEFAULT_MIN_LENGTH  = 0;
  MIN_ALPHABET_LENGTH = 3;
  MAX_ALPHABET_LENGTH = High(Byte);
  {$I blocklist.inc}

type
  TNumber = UInt64;
  TNumbers = TArray<TNumber>;

  { TSqids }

  TSqids = class
  private
    FAlphabet: string;
    FMinLength: Byte;
    FBlockList: TArray<string>;
    function Shuffle(AAlphabet: string): string;
    function ToId(ANum: TNumber; AAlphabet: string): string;
    function ToNumber(AId: string; AAlphabet: string): TNumber;
    function IsBlocked(AId: string): Boolean;
    function EncodeNumbers(ANumbers: TNumbers; AIncrement: Word = 0): string;
  public
    constructor Create(AAlphabet: string; AMinLength: Byte;
      ABlockList: TArray<string>); overload;
    constructor Create(AAlphabet: string = DEFAULT_ALPHABET;
      AMinLength: Byte = DEFAULT_MIN_LENGTH); overload;
    constructor Create(AMinLength: Byte); overload;
    constructor Create(ABlockList: TArray<string>); overload;

    function Encode(ANumbers: TNumbers): string;
    function EncodeSingle(ANumber: TNumber): string;
    function Decode(AId: string): TNumbers;
    function DecodeSingle(AId: string): TNumber;
  end;

  ESqidsException = class(Exception);

implementation

{ TSqids }

constructor TSqids.Create(AAlphabet: string; AMinLength: Byte; ABlockList: TArray<string>);
var
  I: Integer;
  LFilteredBlockList: TArray<string>;
  LAlphabetLower: string;
  C: Char;
  LCharsInAlphabet: Boolean;
begin
  inherited Create;

  if AAlphabet = '' then
    AAlphabet := DEFAULT_ALPHABET;

  for C in AAlphabet do
    if Ord(C) > 127 then
      raise ESqidsException.Create('Alphabet cannot contain multibyte characters');

  if Length(AAlphabet) < MIN_ALPHABET_LENGTH then
    raise ESqidsException.CreateFmt('Alphabet length must be at least %d', [MIN_ALPHABET_LENGTH]);

  if Length(AAlphabet) > MAX_ALPHABET_LENGTH then
    raise ESqidsException.CreateFmt('Alphabet length must not be longer than %d', [MAX_ALPHABET_LENGTH]);

  if Pos(' ', AAlphabet) > 0 then
    raise ESqidsException.Create('Alphabet must not contain spaces');

  for I := 1 to Length(AAlphabet) do
    if Pos(AAlphabet[I], AAlphabet, I+1) > 0 then
      raise ESqidsException.Create('Alphabet must contain unique characters');

  // clean up blocklist:
  // 1. all blocklist words should be lowercase
  // 2. no words less than 3 chars
  // 3. if some words contain chars that are not in the alphabet, remove those
  LFilteredBlockList := Copy(ABlockList, 0);
  LAlphabetLower := Lowercase(AAlphabet);
  I := 0;
  while I < Length(LFilteredBlockList) do
    if Length(LFilteredBlockList[I]) < 3 then
      Delete(LFilteredBlockList, I, 1)
    else
    begin
      LFilteredBlockList[I] := Lowercase(LFilteredBlockList[I]);

      LCharsInAlphabet := True;
      for C in LFilteredBlockList[I] do
        LCharsInAlphabet := LCharsInAlphabet and LAlphabetLower.Contains(C);

      if not LCharsInAlphabet then
        Delete(LFilteredBlockList, I, 1)
      else
        Inc(I);
    end;

  FAlphabet := Shuffle(AAlphabet);
  FMinLength := AMinLength;
  FBlockList := LFilteredBlockList;
end;

constructor TSqids.Create(AAlphabet: string; AMinLength: Byte);
begin
  Create(AAlphabet, AMinLength, DEFAULT_BLOCKLIST);
end;

constructor TSqids.Create(AMinLength: Byte);
begin
  Create(DEFAULT_ALPHABET, AMinLength, DEFAULT_BLOCKLIST);
end;

constructor TSqids.Create(ABlockList: TArray<string>);
begin
  Create(DEFAULT_ALPHABET, DEFAULT_MIN_LENGTH, ABlockList);
end;

// consistent shuffle (always produces the same result given the input)
function TSqids.Shuffle(AAlphabet: string): string;
var
  I: Integer;
  J: Integer;
  L: Integer;
  R: Integer;
  C: Char;
begin
  Result := AAlphabet;

  L := Length(Result);
  I := 0;
  J := L - 1;
  while J > 0 do
  begin
    // In Pascal, string index is 1-based
    // So when accessing string chars, it is needed to add 1, e.g. [I+1]
    R := (I * J + Ord(Result[I+1]) + Ord(Result[J+1])) mod L;

    // swap characters at position I+1 and R+1
    C := Result[I+1];
    Result[I+1] := Result[R+1];
    Result[R+1] := C;

    Inc(I);
    Dec(J);
  end;
end;

function TSqids.ToId(ANum: TNumber; AAlphabet: string): string;
var
  L: Byte;
  LNumResult: TNumber;
begin
  Result := '';
  L := Length(AAlphabet);
  LNumResult := ANum;

  repeat
    Result := AAlphabet[(LNumResult mod L) + 1] + Result;
    LNumResult := LNumResult div L;
  until LNumResult = 0;
end;

function TSqids.ToNumber(AId: string; AAlphabet: string): TNumber;
var
  C: Char;
  L: Byte;
begin
  Result := 0;
  L := Length(AAlphabet);

  for C in AId do
    Result := Result * L + (TNumber(Pos(C, AAlphabet)) - 1);
end;

function TSqids.IsBlocked(AId: string): Boolean;

  function ContainsDigits(S: string): Boolean;
  var
    C: Char;
  begin
    for C in S do
      if CharInSet(C, ['0'..'9']) then
        Exit(True);

    Result := False;
  end;

var
  LWord: string;
begin
  AId := AId.ToLower;

  for LWord in FBlockList do
    // no point in checking words that are longer than the ID
    if Length(LWord) <= Length(AId) then
      if (Length(AId) <= 3) or (Length(LWord) <= 3) then
      begin
        // short words have to match completely; otherwise, too many matches
        if AId = LWord then
          Exit(True);
      end
      else if ContainsDigits(LWord) then
      begin
        // if blocklist words contain numbers (leetspeak),
        // they will only trigger a match if they're at
        // the beginning or the end of the ID.
        if AId.StartsWith(LWord) or AId.EndsWith(LWord) then
          Exit(True);
      end
      else if AId.Contains(LWord) then
        Exit(True);

  Result := False;
end;

function TSqids.Encode(ANumbers: TNumbers): string;
begin
  if Length(ANumbers) = 0 then
    Exit('');

  // no range checking implemented; compiler enforces numbers are within
  // range of TNumber type

  Result := EncodeNumbers(ANumbers);
end;

function TSqids.EncodeSingle(ANumber: TNumber): string;
begin
  Result := Encode([ANumber]);
end;

function TSqids.EncodeNumbers(ANumbers: TNumbers; AIncrement: Word = 0): string;
var
  LOffset: TNumber;
  I: TNumber;
  L: Byte;
  LAlphabet: string;
  LAlphabetWithoutSeparator: string;
  LPrefix: Char;
  LNum: TNumber;
begin
  // if increment is greater than alphabet length, we've reached max attempts
  if AIncrement > Length(FAlphabet) then
    raise ESqidsException.Create('Reached max attempts to re-generate the ID');

  // get a semi-random LOffset from input numbers
  LOffset := Length(ANumbers);
  L := Length(FAlphabet);
  for I := 0 to Length(ANumbers) - 1 do
    LOffset := LOffset + Ord(FAlphabet[(ANumbers[I] mod L) + 1]) + I;
  LOffset := LOffset mod L;

  // if there is a non-zero `increment`, it's an internal attempt to re-generate the ID
  LOffset := (LOffset + AIncrement) mod L;

  // re-arrange alphabet so that second-half goes in front of the first-half
  LAlphabet := Copy(FAlphabet, LOffset + 1) + Copy(FAlphabet, 1, LOffset);

  // `prefix` is the first character in the generated ID, used for randomization
  LPrefix := LAlphabet[1];

  // reverse alphabet (otherwise for [0, x] `LOffset` and `separator` will be the same char)
  LAlphabet := ReverseString(LAlphabet);

  // final ID will always have the `prefix` character at the beginning
  Result := LPrefix;

  // encode input array
  for I := 0 to Length(ANumbers) - 1 do
  begin
    LNum := ANumbers[I];

    // the first character of the alphabet is going to be reserved for the `separator`
    LAlphabetWithoutSeparator := Copy(LAlphabet, 2);
    Result := Result + ToId(LNum, LAlphabetWithoutSeparator);

    // if not the last number
    if I < TNumber(Length(ANumbers)) - 1 then
    begin
      // `separator` character is used to isolate numbers within the ID
      Result := Result + LAlphabet[1];

      // shuffle on every iteration
      LAlphabet := Shuffle(LAlphabet);
    end;
  end;

  // handle `minLength` requirement, if the ID is too short
  if FMinLength > Length(Result) then
  begin
    // append a separator
    Result := Result + LAlphabet[1];

    // keep appending `separator` + however much alphabet is needed
    // for decoding: two separators next to each other is what tells us the rest are junk characters
    while FMinLength - Length(Result) > 0 do
    begin
      LAlphabet := Shuffle(LAlphabet);
      Result := Result + Copy(LAlphabet, 1, FMinLength - Length(Result));
    end;
  end;

  // if ID has a blocked word anywhere, restart with a +1 increment
  if IsBlocked(Result) then
    Result := EncodeNumbers(ANumbers, AIncrement + 1);
end;

function TSqids.Decode(AId: string): TNumbers;
var
  C: Char;
  LPrefix: Char;
  LSeparator: Char;
  LOffset: TNumber;
  LAlphabet: string;
  LAlphabetWithoutSeparator: string;
  LChunks: TArray<string>;
begin
  Result := [];
  if AId = '' then Exit;

  // if a character is not in the alphabet, return an empty array
  for C in AId do
    if not FAlphabet.Contains(C) then
      Exit;

  // first character is always the `prefix`
  LPrefix := AId[1];

  // `offset` is the semi-random position that was generated during encoding
  LOffset := Pos(LPrefix, FAlphabet) - 1;

  // re-arrange alphabet back into its original form
  LAlphabet := Copy(FAlphabet, LOffset + 1) + Copy(FAlphabet, 1, LOffset);

  // reverse alphabet
  LAlphabet := ReverseString(LAlphabet);

  // now it's safe to remove the prefix character from ID, it's not needed anymore
  AId := Copy(AId, 2);

  // decode
  while not AId.IsEmpty do
  begin
    LSeparator := LAlphabet[1];

    // we need the first part to the left of the separator to decode the number
    LChunks := AId.Split([LSeparator]);
    if Length(LChunks) > 0 then
    begin
      // if chunk is empty, we are done (the rest are junk characters)
      if LChunks[0] = '' then
        Exit;

      // decode the number without using the `separator` character
      LAlphabetWithoutSeparator := Copy(LAlphabet, 2);
      Result := Result + [ToNumber(LChunks[0], LAlphabetWithoutSeparator)];

      // if this ID has multiple numbers, shuffle the alphabet because that's what encoding function did
      if Length(LChunks) > 1 then
        LAlphabet := Shuffle(LAlphabet);
    end;

    // `id` is now going to be everything to the right of the `separator`
    Delete(LChunks, 0, 1);
    AId := string.Join(LSeparator, LChunks);
  end;
end;

function TSqids.DecodeSingle(AId: string): TNumber;
var
  LNumbers: TNumbers;
begin
  LNumbers := Decode(AId);
  if Length(LNumbers) = 1 then
    Result := LNumbers[0]
  else
    raise ESqidsException.CreateFmt('%d numbers in Id, expected: 1', [Length(LNumbers)]);
end;

end.
