unit TestDataSetCSVSerializer;

interface

uses
  DUnitX.TestFramework,
  System.Classes, System.SysUtils, Data.DB, Datasnap.DBClient,
  MVCFramework.DataSet.Utils, System.Variants;

type
  [TestFixture]
  TTestDataSetCSVSerializer = class
  private
    FDataSet: TClientDataSet;
    FSerializer: TMVCDataSetCSVSerializer;

    procedure SetupSampleData;
    procedure SetupComplexData;

  public
    [Setup]
    procedure SetUp;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestBasicSerialization;

    [Test]
    procedure TestWithHeaders;

    [Test]
    procedure TestWithoutHeaders;

    [Test]
    procedure TestCustomDelimiter;

    [Test]
    procedure TestQuotingAndEscaping;

    [Test]
    procedure TestNullValues;

    [Test]
    procedure TestNumericFormats;

    [Test]
    procedure TestDateTimeFormats;

    [Test]
    procedure TestBooleanValues;

    [Test]
    procedure TestEmptyDataSet;

    [Test]
    procedure TestSpecialCharacters;

    [Test]
    procedure TestDifferentSettings;

    [Test]
    procedure TestStreamSerialization;

    [Test]
    procedure TestFileSerialization;

    [Test]
    [TestCase('Null DataSet', 'nil')]
    procedure TestExceptionHandling(const AScenario: string);
  end;

implementation

uses
  System.IOUtils;

{ TTesTMVCDataSetCSVSerializer }

procedure TTestDataSetCSVSerializer.SetUp;
begin
  FDataSet := TClientDataSet.Create(nil);
  FSerializer := TMVCDataSetCSVSerializer.Create(TMVCCSVExportSettings.Default);
end;

procedure TTestDataSetCSVSerializer.TearDown;
begin
  FSerializer.Free;
  FDataSet.Free;
end;

procedure TTestDataSetCSVSerializer.SetupSampleData;
begin
  // Fields definition
  FDataSet.FieldDefs.Add('ID', ftInteger);
  FDataSet.FieldDefs.Add('Name', ftString, 50);
  FDataSet.FieldDefs.Add('Price', ftCurrency);
  FDataSet.FieldDefs.Add('Active', ftBoolean);
  FDataSet.FieldDefs.Add('CreatedDate', ftDate);
  FDataSet.CreateDataSet;

  // Sample Data
  FDataSet.AppendRecord([1, 'Product A', 10.50, True, Date]);
  FDataSet.AppendRecord([2, 'Product B', 25.00, False, Date - 1]);
  FDataSet.AppendRecord([3, 'Product C', 15.75, True, Date - 2]);
end;

procedure TTestDataSetCSVSerializer.SetupComplexData;
begin
  // edge cases
  FDataSet.FieldDefs.Add('ID', ftInteger);
  FDataSet.FieldDefs.Add('Description', ftString, 100);
  FDataSet.FieldDefs.Add('Notes', ftMemo);
  FDataSet.FieldDefs.Add('Amount', ftFloat);
  FDataSet.CreateDataSet;

  // special chars
  FDataSet.AppendRecord([1, 'Item with "quotes"', 'Notes with' + sLineBreak + 'line breaks', 123.45]);
  FDataSet.AppendRecord([2, 'Item, with, commas', 'Simple notes', 67.89]);
  FDataSet.AppendRecord([3, '', '', Null]); // Valori vuoti e null
end;

procedure TTestDataSetCSVSerializer.TestBasicSerialization;
var
  Result: string;
begin
  SetupSampleData;
  Result := FSerializer.SerializeToString(FDataSet);

  Assert.IsNotEmpty(Result);
  Assert.Contains(Result, 'Product A');
  Assert.Contains(Result, '10.5'); // Currency formatting
end;

procedure TTestDataSetCSVSerializer.TestWithHeaders;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.IncludeHeaders := True;
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, 'ID');
  Assert.Contains(Result, 'Name');
  Assert.Contains(Result, 'Price');
end;

procedure TTestDataSetCSVSerializer.TestWithoutHeaders;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.IncludeHeaders := False;
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.DoesNotContain(Result, 'ID,Name,Price');
  Assert.Contains(Result, 'Product A');
end;

procedure TTestDataSetCSVSerializer.TestCustomDelimiter;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.Delimiter := ';';
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, ';');
  Assert.DoesNotContain(Result, ',');
end;

procedure TTestDataSetCSVSerializer.TestQuotingAndEscaping;
var
  Result: string;
begin
  SetupComplexData;
  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, '""quotes""');

  Assert.Contains(Result, '"Item, with, commas"');
end;

procedure TTestDataSetCSVSerializer.TestNullValues;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupComplexData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.NullValueRepresentation := 'NULL';
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, 'NULL');
end;

procedure TTestDataSetCSVSerializer.TestNumericFormats;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupComplexData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.DecimalSeparator := ',';
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, '123,45');
end;

procedure TTestDataSetCSVSerializer.TestDateTimeFormats;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.DateFormat := 'dd/mm/yyyy';
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  // Verifica formato data personalizzato
  Assert.IsTrue(Pos('/', Result) > 0);
end;

procedure TTestDataSetCSVSerializer.TestBooleanValues;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  Settings := TMVCCSVExportSettings.Default;
  Settings.BooleanTrueValue := 'YES';
  Settings.BooleanFalseValue := 'NO';
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, 'YES');
  Assert.Contains(Result, 'NO');
end;

procedure TTestDataSetCSVSerializer.TestEmptyDataSet;
var
  Result: string;
begin
  FDataSet.FieldDefs.Add('ID', ftInteger);
  FDataSet.CreateDataSet;
  Result := FSerializer.SerializeToString(FDataSet);
  Assert.Contains(Result, 'ID');
  Assert.AreEqual<Integer>(1, Length(Result.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty)));
end;

procedure TTestDataSetCSVSerializer.TestSpecialCharacters;
var
  Result: string;
begin
  SetupComplexData;
  Result := FSerializer.SerializeToString(FDataSet);
  Assert.IsNotEmpty(Result);
end;

procedure TTestDataSetCSVSerializer.TestDifferentSettings;
var
  Settings: TMVCCSVExportSettings;
  Result: string;
begin
  SetupSampleData;

  // Test Excel settings
  Settings := TMVCCSVExportSettings.Excel;
  FSerializer.Settings := Settings;

  Result := FSerializer.SerializeToString(FDataSet);

  Assert.Contains(Result, ';'); // Excel usa semicolon
end;

procedure TTestDataSetCSVSerializer.TestStreamSerialization;
var
  Stream: TMemoryStream;
  Result: string;
begin
  SetupSampleData;

  Stream := TMemoryStream.Create;
  try
    FSerializer.SerializeToStream(FDataSet, Stream);

    Stream.Position := 0;
    Result := TEncoding.UTF8.GetString(TBytes(PByte(Stream.Memory)), 0, Stream.Size);

    Assert.IsNotEmpty(Result);
    Assert.Contains(Result, 'Product A');
  finally
    Stream.Free;
  end;
end;

procedure TTestDataSetCSVSerializer.TestFileSerialization;
var
  FileName: string;
  Content: string;
begin
  SetupSampleData;

  FileName := TPath.GetTempFileName + '.csv';
  try
    FSerializer.SerializeToFile(FDataSet, FileName);

    Assert.IsTrue(TFile.Exists(FileName));

    Content := TFile.ReadAllText(FileName, TEncoding.UTF8);
    Assert.IsNotEmpty(Content);
    Assert.Contains(Content, 'Product A');

  finally
    if TFile.Exists(FileName) then
      TFile.Delete(FileName);
  end;
end;

procedure TTestDataSetCSVSerializer.TestExceptionHandling(const AScenario: string);
begin
  if AScenario = 'nil' then
  begin
    Assert.WillRaise(
      procedure
      begin
        FSerializer.SerializeToString(nil);
      end,
      EMVCCSVSerializationError
    );
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDataSetCSVSerializer);

end.
