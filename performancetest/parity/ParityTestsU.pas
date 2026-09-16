unit ParityTestsU;

{
  Parity tests for the streaming JSON serializer.

  Every test instantiates an object, serializes it twice - once via the
  legacy TMVCJsonDataObjectsSerializer, once via the streaming
  TMVCStreamingJsonSerializer - and asserts the two outputs are
  byte-for-byte identical. Divergence is the bug.

  The helper harness prints a concise report so the tool can run in a
  console and iterate fast. Once the matrix is stable this unit can be
  folded into the DUnitX suite.
}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Data.DB, Datasnap.DBClient,
  MVCFramework.Serializer.Commons, MVCFramework.Nullables;

type
  { --- Test DTOs -----------------------------------------------------------
    One class per property type so divergences are easy to attribute.
    Using 'public' properties to match the bench DTO style. }
  TDtoInts = class
  private
    FI32: Integer;
    FI64: Int64;
  public
    property I32: Integer read FI32 write FI32;
    property I64: Int64   read FI64 write FI64;
  end;

  TDtoStr = class
  private
    FS: string;
  public
    property S: string read FS write FS;
  end;

  TDtoFloats = class
  private
    FF: Double;
    FG: Single;
  public
    property F: Double  read FF  write FF;
    property G: Single  read FG  write FG;
  end;

  TDtoBool = class
  private
    FB: Boolean;
  public
    property B: Boolean read FB write FB;
  end;

  TDtoDate = class
  private
    FD: TDateTime;
  public
    property D: TDateTime read FD write FD;
  end;

  TDtoMixed = class
  private
    FId: Integer;
    FName: string;
    FPrice: Double;
    FActive: Boolean;
  public
    property Id: Integer  read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
  end;

  TColor = (clRed, clGreen, clBlue);

  TDtoEnum = class
  private
    FC: TColor;
  public
    property C: TColor read FC write FC;
  end;

  TDtoGuid = class
  private
    FG: TGUID;
  public
    property G: TGUID read FG write FG;
  end;

  TDtoCurrency = class
  private
    FAmt: Currency;
  public
    property Amt: Currency read FAmt write FAmt;
  end;

  TDtoWideChars = class
  private
    FS: string;
  public
    property S: string read FS write FS;
  end;

  { --- Attribute tests --- }

  TDtoNameAs = class
  private
    FOne: Integer;
    FTwo: string;
  public
    [MVCNameAs('custom_one')]
    property One: Integer read FOne write FOne;
    [MVCNameAs('CustomTwo', True {fixed})]
    property Two: string read FTwo write FTwo;
  end;

  [MVCNameCase(ncSnakeCase)]
  TDtoNameCaseSnake = class
  private
    FFirstName: string;
    FSomeValue: Integer;
  public
    property FirstName: string read FFirstName write FFirstName;
    property SomeValue: Integer read FSomeValue write FSomeValue;
  end;

  [MVCNameCase(ncUpperCase)]
  TDtoNameCaseUpper = class
  private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
  end;

  TDtoDoNotSer = class
  private
    FPublic: Integer;
    FHidden: string;
  public
    property Public_: Integer read FPublic write FPublic;
    [MVCDoNotSerialize]
    property Hidden: string read FHidden write FHidden;
  end;

  TDtoNullables = class
  private
    FS: NullableString;
    FI32: NullableInt32;
    FI64: NullableInt64;
    FD: NullableDouble;
    FB: NullableBoolean;
    FDT: NullableTDateTime;
    FG: NullableTGUID;
  public
    property S: NullableString read FS write FS;
    property I32: NullableInt32 read FI32 write FI32;
    property I64: NullableInt64 read FI64 write FI64;
    property D: NullableDouble read FD write FD;
    property B: NullableBoolean read FB write FB;
    property DT: NullableTDateTime read FDT write FDT;
    property G: NullableTGUID read FG write FG;
  end;

  { --- Nested class property --- }

  TDtoAddress = class
  private
    FStreet: string;
    FCity: string;
    FZip: Integer;
  public
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    property Zip: Integer read FZip write FZip;
  end;

  TDtoPerson = class
  private
    FName: string;
    FAddress: TDtoAddress;
  public
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Address: TDtoAddress read FAddress write FAddress;
  end;

  TDtoLineItem = class
  private
    FSku: string;
    FQty: Integer;
    FPrice: Double;
  public
    property Sku: string read FSku write FSku;
    property Qty: Integer read FQty write FQty;
    property Price: Double read FPrice write FPrice;
  end;

  TDtoOrder = class
  private
    FOrderId: Integer;
    FItems: TObjectList<TDtoLineItem>;
  public
    constructor Create;
    destructor Destroy; override;
    property OrderId: Integer read FOrderId write FOrderId;
    property Items: TObjectList<TDtoLineItem> read FItems;
  end;

  { --- TArray<T> --- }

  TDtoArrayInts = class
  private
    FValues: TArray<Integer>;
  public
    property Values: TArray<Integer> read FValues write FValues;
  end;

  TDtoArrayStrings = class
  private
    FTags: TArray<string>;
  public
    property Tags: TArray<string> read FTags write FTags;
  end;

  TDtoArrayDoubles = class
  private
    FMeasures: TArray<Double>;
  public
    property Measures: TArray<Double> read FMeasures write FMeasures;
  end;

  TDtoArrayObjects = class
  private
    FChildren: TArray<TDtoAddress>;
  public
    destructor Destroy; override;
    property Children: TArray<TDtoAddress> read FChildren write FChildren;
  end;

  { --- TStream property --- }
  TDtoStream = class
  private
    FData: TStream;
  public
    destructor Destroy; override;
    property Data: TStream read FData write FData;
  end;

  { --- TDataSet property --- }
  TDtoDataSet = class
  private
    FRows: TClientDataSet;
  public
    destructor Destroy; override;
    property Rows: TClientDataSet read FRows write FRows;
  end;

  { --- stFields marker - streaming must fall back --- }
  [MVCSerialize(stFields)]
  TDtoStFields = class
  private
    FValue: Integer;
    FName: string;
  public
    property Value: Integer read FValue write FValue;
    property Name: string read FName write FName;
  end;

function RunParityTests: Integer;

implementation

uses
  System.Math,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Streaming;

destructor TDtoPerson.Destroy;
begin
  FAddress.Free;
  inherited;
end;

constructor TDtoOrder.Create;
begin
  inherited Create;
  FItems := TObjectList<TDtoLineItem>.Create(True);
end;

destructor TDtoOrder.Destroy;
begin
  FItems.Free;
  inherited;
end;

destructor TDtoArrayObjects.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FChildren) do
    FChildren[I].Free;
  inherited;
end;

destructor TDtoStream.Destroy;
begin
  FData.Free;
  inherited;
end;

destructor TDtoDataSet.Destroy;
begin
  FRows.Free;
  inherited;
end;

type
  TTestCase = record
    Name: string;
    Obj: TObject;
    OwnsObj: Boolean;
    IsList: Boolean;
    ExpectedDiverge: Boolean;    { Legacy has an acknowledged defect; the
                                   streaming output is the intentionally-
                                   improved one. Parity failure is expected
                                   and reported as "FIX" rather than FAIL. }
    DivergeNote: string;
  end;

function SerializeLegacy(AObj: TObject; AIsList: Boolean): string;
var
  Ser: TMVCJsonDataObjectsSerializer;
begin
  Ser := TMVCJsonDataObjectsSerializer.Create;
  try
    if AIsList then
      Result := Ser.SerializeCollection(AObj)
    else
      Result := Ser.SerializeObject(AObj);
  finally
    Ser.Free;
  end;
end;

function SerializeStreaming(AObj: TObject; AIsList: Boolean): string;
var
  Buf: TBytesStream;
  OK: Boolean;
begin
  Buf := TBytesStream.Create;
  try
    if AIsList then
      OK := TMVCStreamingJsonSerializer.TryWriteList(AObj, Buf)
    else
      OK := TMVCStreamingJsonSerializer.TryWriteObject(AObj, Buf);
    if not OK then
      Exit('<STREAMING UNSUPPORTED>');
    Buf.Position := 0;
    Result := TEncoding.UTF8.GetString(Buf.Bytes, 0, Buf.Size);
  finally
    Buf.Free;
  end;
end;

function HexDump(const S: string; MaxBytes: Integer = 16): string;
var
  Bytes: TBytes;
  I: Integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(S);
  Result := '';
  for I := 0 to Min(High(Bytes), MaxBytes - 1) do
    Result := Result + IntToHex(Bytes[I], 2) + ' ';
  if Length(Bytes) > MaxBytes then
    Result := Result + '...';
end;

function RunOne(const ACase: TTestCase): Boolean;
var
  Leg, Str: string;
  Equal, StreamingSkipped: Boolean;
begin
  Leg := SerializeLegacy(ACase.Obj, ACase.IsList);
  Str := SerializeStreaming(ACase.Obj, ACase.IsList);
  StreamingSkipped := Str = '<STREAMING UNSUPPORTED>';
  Equal := Leg = Str;
  Writeln('--- ', ACase.Name, ' ---');
  Writeln('  legacy:    ', Leg);
  Writeln('  streaming: ', Str);
  if StreamingSkipped then
  begin
    Writeln('  SKIP (streaming shape unsupported, legacy fallback)');
    Result := True;
    Writeln;
    Exit;
  end;
  if not Equal then
  begin
    Writeln('  legacy hex:    ', HexDump(Leg));
    Writeln('  streaming hex: ', HexDump(Str));
  end;
  if ACase.ExpectedDiverge then
  begin
    if Equal then
    begin
      Writeln('  UNEXPECTED PARITY (test expected divergence)');
      Result := False;
    end
    else
    begin
      Writeln('  FIX - ', ACase.DivergeNote);
      Result := True;
    end;
  end
  else if Equal then
  begin
    Writeln('  PASS');
    Result := True;
  end
  else
  begin
    Writeln('  FAIL (bytes differ)');
    Result := False;
  end;
  Writeln;
end;

procedure FillCases(Cases: TList<TTestCase>);

  procedure Add(const AName: string; AObj: TObject; AIsList: Boolean = False);
  var TC: TTestCase;
  begin
    TC.Name := AName;
    TC.Obj := AObj;
    TC.OwnsObj := True;
    TC.IsList := AIsList;
    TC.ExpectedDiverge := False;
    TC.DivergeNote := '';
    Cases.Add(TC);
  end;

  procedure AddDiverge(const AName: string; AObj: TObject; const ANote: string; AIsList: Boolean = False);
  var TC: TTestCase;
  begin
    TC.Name := AName;
    TC.Obj := AObj;
    TC.OwnsObj := True;
    TC.IsList := AIsList;
    TC.ExpectedDiverge := True;
    TC.DivergeNote := ANote;
    Cases.Add(TC);
  end;

var
  LInts: TDtoInts;
  LStr:  TDtoStr;
  LFlt:  TDtoFloats;
  LBool: TDtoBool;
  LDate: TDtoDate;
  LMix:  TDtoMixed;
  LList: TObjectList<TDtoMixed>;
  I: Integer;
begin
  { --- Integers --- }
  LInts := TDtoInts.Create; LInts.I32 := 42;       LInts.I64 := 1234567890123; Add('ints/small',   LInts);
  LInts := TDtoInts.Create; LInts.I32 := -1;       LInts.I64 := 0;             Add('ints/neg',     LInts);
  LInts := TDtoInts.Create; LInts.I32 := MaxInt;   LInts.I64 := High(Int64);   Add('ints/maxvals', LInts);

  { --- Strings --- }
  LStr := TDtoStr.Create; LStr.S := 'hello';                    Add('string/ascii',      LStr);
  LStr := TDtoStr.Create; LStr.S := '';                         Add('string/empty',      LStr);
  LStr := TDtoStr.Create; LStr.S := 'caffè';                    Add('string/accented',   LStr);
  LStr := TDtoStr.Create; LStr.S := 'line'#10'break';           Add('string/lf',         LStr);
  LStr := TDtoStr.Create; LStr.S := 'quote"inside';             Add('string/dquote',     LStr);
  LStr := TDtoStr.Create; LStr.S := 'back\slash';               Add('string/bslash',     LStr);
  LStr := TDtoStr.Create; LStr.S := #9'tab';                    Add('string/tab',        LStr);
  LStr := TDtoStr.Create; LStr.S := #1'ctrl';                   Add('string/ctrl',       LStr);

  { --- Floats --- }
  LFlt := TDtoFloats.Create; LFlt.F := 19.99;  LFlt.G := 0.5;         Add('float/normal',  LFlt);
  LFlt := TDtoFloats.Create; LFlt.F := -0.0;   LFlt.G := 0;           Add('float/zero',    LFlt);
  LFlt := TDtoFloats.Create; LFlt.F := 1e100;  LFlt.G := 1e-10;
  Add('float/extrema', LFlt);

  { --- Boolean --- }
  LBool := TDtoBool.Create; LBool.B := True;  Add('bool/true',  LBool);
  LBool := TDtoBool.Create; LBool.B := False; Add('bool/false', LBool);

  { --- TDateTime --- }
  LDate := TDtoDate.Create; LDate.D := EncodeDate(2024,3,15) + EncodeTime(14,30,45,0); Add('date/normal', LDate);
  LDate := TDtoDate.Create; LDate.D := 0;                                              Add('date/zero',   LDate);

  { --- Mixed --- }
  LMix := TDtoMixed.Create;
  LMix.Id := 12345; LMix.Name := 'Bench'; LMix.Price := 19.99; LMix.Active := True;
  Add('mixed/basic', LMix);

  { --- TObjectList<TDtoMixed> --- }
  LList := TObjectList<TDtoMixed>.Create(True);
  for I := 1 to 3 do
  begin
    LMix := TDtoMixed.Create;
    LMix.Id := I; LMix.Name := 'item' + I.ToString;
    LMix.Price := 1.1 * I; LMix.Active := Odd(I);
    LList.Add(LMix);
  end;
  Add('list/3items', LList, True);

  { --- empty list --- }
  LList := TObjectList<TDtoMixed>.Create(True);
  Add('list/empty', LList, True);

  { --- Enum (non-Boolean) --- }
  var LEnum: TDtoEnum;
  LEnum := TDtoEnum.Create; LEnum.C := clRed;   Add('enum/red',   LEnum);
  LEnum := TDtoEnum.Create; LEnum.C := clGreen; Add('enum/green', LEnum);

  { --- TGUID --- }
  var LGuid: TDtoGuid;
  LGuid := TDtoGuid.Create;
  LGuid.G := StringToGUID('{550E8400-E29B-41D4-A716-446655440000}');
  Add('guid/basic', LGuid);

  { --- Currency --- }
  var LCur: TDtoCurrency;
  LCur := TDtoCurrency.Create; LCur.Amt := 19.99;       Add('currency/simple',  LCur);
  LCur := TDtoCurrency.Create; LCur.Amt := 0;           Add('currency/zero',    LCur);
  LCur := TDtoCurrency.Create; LCur.Amt := -1234.5678;  Add('currency/neg',     LCur);

  { --- String with multi-byte UTF-8 --- }
  var LW: TDtoWideChars;
  LW := TDtoWideChars.Create; LW.S := '日本語';    Add('string/cjk',   LW);
  LW := TDtoWideChars.Create; LW.S := '🎉';          Add('string/emoji', LW);

  { --- MVCNameAs --- }
  var LNAs: TDtoNameAs;
  LNAs := TDtoNameAs.Create; LNAs.One := 1; LNAs.Two := 'v'; Add('attr/name_as', LNAs);

  { --- MVCNameCase(snake) --- }
  var LSnake: TDtoNameCaseSnake;
  LSnake := TDtoNameCaseSnake.Create; LSnake.FirstName := 'Jane'; LSnake.SomeValue := 7;
  Add('attr/namecase_snake', LSnake);

  { --- MVCNameCase(upper) --- }
  var LUpper: TDtoNameCaseUpper;
  LUpper := TDtoNameCaseUpper.Create; LUpper.Value := 42;
  Add('attr/namecase_upper', LUpper);

  { --- MVCDoNotSerialize --- }
  var LHide: TDtoDoNotSer;
  LHide := TDtoDoNotSer.Create; LHide.Public_ := 10; LHide.Hidden := 'secret';
  Add('attr/do_not_ser', LHide);

  { --- Nullables all unset --- }
  var LN: TDtoNullables;
  LN := TDtoNullables.Create;  Add('nullable/all_unset', LN);

  { --- Nullables all set --- }
  LN := TDtoNullables.Create;
  LN.S   := 'hello';
  LN.I32 := 42;
  LN.I64 := Int64(1234567890123);
  LN.D   := 19.99;
  LN.B   := True;
  LN.DT  := EncodeDate(2024,3,15) + EncodeTime(14,30,45,0);
  LN.G   := StringToGUID('{550E8400-E29B-41D4-A716-446655440000}');
  Add('nullable/all_set', LN);

  { --- Nullables mixed (some set, some not) --- }
  LN := TDtoNullables.Create;
  LN.I32 := 7;
  LN.S   := 'x';
  Add('nullable/mixed', LN);

  { --- Nested class property --- }
  var LP: TDtoPerson;
  LP := TDtoPerson.Create; LP.Name := 'Alice';
  LP.Address := TDtoAddress.Create;
  LP.Address.Street := 'Main St 1'; LP.Address.City := 'Roma'; LP.Address.Zip := 184;
  Add('nested/person', LP);

  { --- Nested class = nil --- }
  LP := TDtoPerson.Create; LP.Name := 'Bob';
  LP.Address := nil;
  Add('nested/person_null_addr', LP);

  { --- Nested TObjectList<T> --- }
  var LO: TDtoOrder;
  LO := TDtoOrder.Create; LO.OrderId := 7;
  var LItem: TDtoLineItem;
  LItem := TDtoLineItem.Create; LItem.Sku := 'A'; LItem.Qty := 2; LItem.Price := 9.99; LO.Items.Add(LItem);
  LItem := TDtoLineItem.Create; LItem.Sku := 'B'; LItem.Qty := 1; LItem.Price := 19.5; LO.Items.Add(LItem);
  Add('nested/order_with_items', LO);

  { --- Nested TObjectList empty --- }
  LO := TDtoOrder.Create; LO.OrderId := 42;
  Add('nested/order_empty_items', LO);

  { --- TArray<Integer> --- }
  var LAI: TDtoArrayInts;
  LAI := TDtoArrayInts.Create; LAI.Values := TArray<Integer>.Create(1, 2, 3);
  Add('array/ints', LAI);
  LAI := TDtoArrayInts.Create; LAI.Values := nil;
  Add('array/ints_empty', LAI);

  { --- TArray<string> --- }
  var LAS: TDtoArrayStrings;
  LAS := TDtoArrayStrings.Create; LAS.Tags := TArray<string>.Create('red', 'green', 'blue');
  Add('array/strings', LAS);

  { --- TArray<Double> --- }
  var LAD: TDtoArrayDoubles;
  LAD := TDtoArrayDoubles.Create; LAD.Measures := TArray<Double>.Create(1.1, 2.2, 3.3);
  Add('array/doubles', LAD);

  { --- TArray<TObject> --- }
  var LAO: TDtoArrayObjects;
  LAO := TDtoArrayObjects.Create;
  var LAddr1, LAddr2: TDtoAddress;
  LAddr1 := TDtoAddress.Create; LAddr1.Street := 'S1'; LAddr1.City := 'Milano'; LAddr1.Zip := 20100;
  LAddr2 := TDtoAddress.Create; LAddr2.Street := 'S2'; LAddr2.City := 'Torino'; LAddr2.Zip := 10100;
  LAO.Children := TArray<TDtoAddress>.Create(LAddr1, LAddr2);
  Add('array/objects', LAO);

  { --- TStream --- }
  var LStr2: TDtoStream;
  LStr2 := TDtoStream.Create;
  LStr2.Data := TMemoryStream.Create;
  var LBuf: TBytes;
  LBuf := TEncoding.UTF8.GetBytes('Hello, stream!');
  LStr2.Data.WriteBuffer(LBuf, Length(LBuf));
  Add('stream/bytes', LStr2);

  { --- TStream nil --- }
  LStr2 := TDtoStream.Create;
  LStr2.Data := nil;
  Add('stream/nil', LStr2);

  { --- TDataSet (TClientDataSet with 2 rows) --- }
  var LDS: TDtoDataSet;
  LDS := TDtoDataSet.Create;
  LDS.Rows := TClientDataSet.Create(nil);
  LDS.Rows.FieldDefs.Add('id', ftInteger);
  LDS.Rows.FieldDefs.Add('name', ftString, 50);
  LDS.Rows.CreateDataSet;
  LDS.Rows.AppendRecord([1, 'Alice']);
  LDS.Rows.AppendRecord([2, 'Bob']);
  Add('dataset/rows', LDS);

  { --- TDataSet nil --- }
  LDS := TDtoDataSet.Create;
  LDS.Rows := nil;
  Add('dataset/nil', LDS);

  { --- stFields class: must fall back to legacy --- }
  var LSF: TDtoStFields;
  LSF := TDtoStFields.Create; LSF.Value := 1; LSF.Name := 'x';
  Add('stfields/fallback', LSF);
end;

function RunParityTests: Integer;
var
  Cases: TList<TTestCase>;
  TC: TTestCase;
  Pass, Fail: Integer;
begin
  Cases := TList<TTestCase>.Create;
  try
    FillCases(Cases);
    Pass := 0;
    Fail := 0;
    for TC in Cases do
    begin
      try
        if RunOne(TC) then
          Inc(Pass)
        else
          Inc(Fail);
      finally
        if TC.OwnsObj then
          TC.Obj.Free;
      end;
    end;
    Writeln('============================================================');
    Writeln(Format('PARITY: %d PASS, %d FAIL (of %d)', [Pass, Fail, Cases.Count]));
    Writeln('============================================================');
    Result := Fail;
  finally
    Cases.Free;
  end;
end;

end.
