unit TestsBloomFilterU;

interface

uses
  DUnitX.TestFramework,
  MVCFramework.BloomFilter,
  System.SysUtils,
  System.Classes,
  System.Diagnostics;

type
  [TestFixture]
  TBloomFilterTests = class
  private
    fBloomFilter: TMVCBloomFilter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Basic functionality tests
    [Test]
    procedure TestCreateDestroy;
    [Test]
    procedure TestAddAndContains;
    [Test]
    procedure TestAddAndContainsAsInterface;
    [Test]
    procedure TestNoFalseNegatives;
    [Test]
    procedure TestClear;

    // Optimal constructor tests
    [Test]
    procedure TestCreateOptimal;
    [Test]
    procedure TestCreateOptimalWithDifferentRates;

    // Statistics and metrics tests
    [Test]
    procedure TestInsertedCount;
    [Test]
    procedure TestMemoryUsage;
    [Test]
    procedure TestEstimatedFalsePositiveRate;

    // Static methods tests
    [Test]
    procedure TestCalculateOptimalBitCount;
    [Test]
    procedure TestCalculateOptimalHashCount;

    // Edge cases tests
    [Test]
    procedure TestEmptyString;
    [Test]
    procedure TestDuplicateInserts;
    [Test]
    procedure TestLargeDataset;

    // Performance tests
    [Test]
    procedure TestPerformanceInsertAndSearch;

    // Error handling tests
    [Test]
    procedure TestInvalidConstructorParameters;
    [Test]
    procedure TestInvalidOptimalParameters;

    // Properties tests
    [Test]
    procedure TestProperties;

    // Mathematical verification tests
    [Test]
    procedure TestFalsePositiveRateWithKnownData;
  end;

implementation

uses
  System.DateUtils;

procedure TBloomFilterTests.Setup;
begin
  fBloomFilter := TMVCBloomFilter.Create(1000, 3);
end;

procedure TBloomFilterTests.TearDown;
begin
  fBloomFilter.Free;
end;

procedure TBloomFilterTests.TestCreateDestroy;
begin
  Assert.IsNotNull(fBloomFilter);
  Assert.AreEqual(UInt32(1000), fBloomFilter.BitCount);
  Assert.AreEqual(3, fBloomFilter.HashFunctions);
  Assert.AreEqual(UInt32(0), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestAddAndContains;
begin
  // Test element not present
  Assert.IsFalse(fBloomFilter.MightContain('test@example.com'));

  // Add element
  fBloomFilter.Add('test@example.com');
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);

  // Test element present
  Assert.IsTrue(fBloomFilter.MightContain('test@example.com'));

  // Test other element not present
  Assert.IsFalse(fBloomFilter.MightContain('other@example.com'));
end;

procedure TBloomFilterTests.TestAddAndContainsAsInterface;
var
  lBloomFilter: IMVCBloomFilter;
begin
  lBloomFilter := TMVCBloomFilter.Create(1000, 3);
  // Test element not present
  Assert.IsFalse(lBloomFilter.MightContain('test@example.com'));

  // Add element
  lBloomFilter.Add('test@example.com');
  Assert.AreEqual(UInt32(1), lBloomFilter.GetInsertedCount);

  // Test element present
  Assert.IsTrue(lBloomFilter.MightContain('test@example.com'));

  // Test other element not present
  Assert.IsFalse(lBloomFilter.MightContain('other@example.com'));
end;


procedure TBloomFilterTests.TestNoFalseNegatives;
var
  lTestData: TArray<string>;
  lI: Integer;
begin
  // Prepare test data
  SetLength(lTestData, 100);
  for lI := 0 to 99 do
    lTestData[lI] := Format('user%d@example.com', [lI]);

  // Insert all elements
  for lI := 0 to 99 do
    fBloomFilter.Add(lTestData[lI]);

  // Verify all inserted elements are detected (no false negatives)
  for lI := 0 to 99 do
    Assert.IsTrue(fBloomFilter.MightContain(lTestData[lI]),
      Format('False negative for: %s', [lTestData[lI]]));

  Assert.AreEqual(UInt32(100), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestClear;
begin
  fBloomFilter.Add('test@example.com');
  Assert.IsTrue(fBloomFilter.MightContain('test@example.com'));
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);

  fBloomFilter.Clear;
  Assert.IsFalse(fBloomFilter.MightContain('test@example.com'));
  Assert.AreEqual(UInt32(0), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestCreateOptimal;
var
  lOptimalFilter: TMVCBloomFilter;
begin
  lOptimalFilter := TMVCBloomFilter.CreateOptimal(1000, 0.01);
  try
    Assert.IsNotNull(lOptimalFilter);
    Assert.IsTrue(lOptimalFilter.BitCount > 0);
    Assert.IsTrue(lOptimalFilter.HashFunctions > 0);

    // Test basic functionality
    lOptimalFilter.Add('test');
    Assert.IsTrue(lOptimalFilter.MightContain('test'));
    Assert.IsFalse(lOptimalFilter.MightContain('not-inserted'));
    Assert.AreEqual(UInt32(1), lOptimalFilter.InsertedCount);
  finally
    lOptimalFilter.Free;
  end;
end;

procedure TBloomFilterTests.TestCreateOptimalWithDifferentRates;
var
  lFilter1, lFilter2: TMVCBloomFilter;
begin
  // Higher false positive rate should require fewer bits
  lFilter1 := TMVCBloomFilter.CreateOptimal(1000, 0.1);  // 10%
  lFilter2 := TMVCBloomFilter.CreateOptimal(1000, 0.01); // 1%

  try
    Assert.IsTrue(lFilter1.BitCount < lFilter2.BitCount,
      'Higher false positive rate should require fewer bits');
  finally
    lFilter1.Free;
    lFilter2.Free;
  end;
end;

procedure TBloomFilterTests.TestInsertedCount;
begin
  Assert.AreEqual(UInt32(0), fBloomFilter.InsertedCount);

  fBloomFilter.Add('test1');
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);

  fBloomFilter.Add('test2');
  Assert.AreEqual(UInt32(2), fBloomFilter.InsertedCount);

  // Adding the same element should still increment count
  fBloomFilter.Add('test1');
  Assert.AreEqual(UInt32(3), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestMemoryUsage;
var
  lUsage: UInt32;
begin
  lUsage := fBloomFilter.GetMemoryUsage;
  Assert.IsTrue(lUsage > 0);

  // Should be approximately: (1000 bits / 8) + overhead
  Assert.IsTrue(lUsage >= 125); // At least 125 bytes for 1000 bits
end;

procedure TBloomFilterTests.TestEstimatedFalsePositiveRate;
var
  lFilter: TMVCBloomFilter;
  lI: Integer;
  lRate: Double;
begin
  lFilter := TMVCBloomFilter.CreateOptimal(100, 0.05); // 5% target
  try
    // Empty filter should have 0% false positive rate
    Assert.AreEqual(0.0, lFilter.GetEstimatedFalsePositiveRate, 0.001);

    // Add some elements
    for lI := 0 to 49 do // Half capacity
      lFilter.Add(Format('element_%d', [lI]));

    lRate := lFilter.GetEstimatedFalsePositiveRate;
    Assert.IsTrue(lRate > 0, 'False positive rate should be > 0 with elements');
    Assert.IsTrue(lRate < 0.1, 'False positive rate should be reasonable');
  finally
    lFilter.Free;
  end;
end;

procedure TBloomFilterTests.TestCalculateOptimalBitCount;
var
  lBits1, lBits2: UInt32;
begin
  lBits1 := TMVCBloomFilter.CalculateOptimalBitCount(1000, 0.01);
  lBits2 := TMVCBloomFilter.CalculateOptimalBitCount(1000, 0.1);

  Assert.IsTrue(lBits1 > lBits2, 'Lower false positive rate should require more bits');
  Assert.IsTrue(lBits1 > 1000, 'Should require more bits than elements for low false positive rate');
end;

procedure TBloomFilterTests.TestCalculateOptimalHashCount;
var
  lHashes1, lHashes2: Integer;
begin
  lHashes1 := TMVCBloomFilter.CalculateOptimalHashCount(10000, 1000);
  lHashes2 := TMVCBloomFilter.CalculateOptimalHashCount(5000, 1000);

  Assert.IsTrue(lHashes1 > lHashes2, 'More bits per element should allow more hash functions');
  Assert.IsTrue(lHashes1 >= 1, 'Should always have at least 1 hash function');
end;

procedure TBloomFilterTests.TestEmptyString;
begin
  Assert.IsFalse(fBloomFilter.MightContain(''));
  fBloomFilter.Add('');
  Assert.IsTrue(fBloomFilter.MightContain(''));
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestDuplicateInserts;
begin
  fBloomFilter.Add('test');
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);

  fBloomFilter.Add('test'); // Insert again
  Assert.AreEqual(UInt32(2), fBloomFilter.InsertedCount); // Count increases

  Assert.IsTrue(fBloomFilter.MightContain('test'));
end;

procedure TBloomFilterTests.TestLargeDataset;
var
  lLargeFilter: TMVCBloomFilter;
  lI: Integer;
  lStartTime: TDateTime;
  lElapsedMs: Integer;
begin
  lLargeFilter := TMVCBloomFilter.CreateOptimal(10000, 0.01);
  try
    lStartTime := Now;

    // Insert 10000 elements
    for lI := 0 to 9999 do
      lLargeFilter.Add(Format('large_test_%d@example.com', [lI]));

    lElapsedMs := MilliSecondsBetween(Now, lStartTime);

    // Verify some elements
    Assert.IsTrue(lLargeFilter.MightContain('large_test_0@example.com'));
    Assert.IsTrue(lLargeFilter.MightContain('large_test_5000@example.com'));
    Assert.IsTrue(lLargeFilter.MightContain('large_test_9999@example.com'));
    Assert.IsFalse(lLargeFilter.MightContain('not_inserted@example.com'));

    Assert.AreEqual(UInt32(10000), lLargeFilter.InsertedCount);

    // Performance check - should be very fast
    Assert.IsTrue(lElapsedMs < 5000, 'Large dataset insertion should be fast');

  finally
    lLargeFilter.Free;
  end;
end;

procedure TBloomFilterTests.TestPerformanceInsertAndSearch;
var
  lStopwatch: TStopwatch;
  lI: Integer;
  lAddTime, lSearchTime: Int64;
begin
  lStopwatch := TStopwatch.StartNew;

  // Test insertion speed
  for lI := 0 to 9999 do
    fBloomFilter.Add(Format('perf_test_%d', [lI]));

  lAddTime := lStopwatch.ElapsedMilliseconds;
  lStopwatch := TStopwatch.StartNew;

  // Test search speed
  for lI := 0 to 9999 do
    fBloomFilter.MightContain(Format('perf_test_%d', [lI]));

  lSearchTime := lStopwatch.ElapsedMilliseconds;
  lStopwatch.Stop;

  // Sanity check - should be very fast
  Assert.IsTrue(lAddTime < 2000, 'Add operations should be fast');
  Assert.IsTrue(lSearchTime < 1000, 'Search operations should be fast');

  Assert.AreEqual(UInt32(10000), fBloomFilter.InsertedCount);
end;

procedure TBloomFilterTests.TestInvalidConstructorParameters;
begin
  // Test constructor with invalid parameters
  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.Create(0, 3);
    end,
    EMVCBloomFilterException
  );

  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.Create(1000, 0);
    end,
    EMVCBloomFilterException
  );

  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.Create(1000, -1);
    end,
    EMVCBloomFilterException
  );
end;

procedure TBloomFilterTests.TestInvalidOptimalParameters;
begin
  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.CreateOptimal(0, 0.01);
    end,
    EMVCBloomFilterException
  );

  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.CreateOptimal(1000, 0.0);
    end,
    EMVCBloomFilterException
  );

  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.CreateOptimal(1000, 1.0);
    end,
    EMVCBloomFilterException
  );

  Assert.WillRaise(
    procedure
    begin
      TMVCBloomFilter.CreateOptimal(1000, -0.1);
    end,
    EMVCBloomFilterException
  );
end;

procedure TBloomFilterTests.TestProperties;
begin
  Assert.AreEqual(UInt32(1000), fBloomFilter.BitCount);
  Assert.AreEqual(3, fBloomFilter.HashFunctions);
  Assert.AreEqual(UInt32(0), fBloomFilter.InsertedCount);

  fBloomFilter.Add('test');
  Assert.AreEqual(UInt32(1), fBloomFilter.InsertedCount);

  // BitCount and HashFunctions should remain constant
  Assert.AreEqual(UInt32(1000), fBloomFilter.BitCount);
  Assert.AreEqual(3, fBloomFilter.HashFunctions);
end;

procedure TBloomFilterTests.TestFalsePositiveRateWithKnownData;
var
  lFilter: TMVCBloomFilter;
  lI, lFalsePositives: Integer;
  lTestString: string;
  lRate: Double;
begin
  // Use parameters that give approximately 5% false positive rate
  lFilter := TMVCBloomFilter.CreateOptimal(500, 0.05);
  try
    // Insert 500 elements
    for lI := 0 to 499 do
      lFilter.Add(Format('inserted_%d', [lI]));

    // Test 1000 NON-inserted elements
    lFalsePositives := 0;
    for lI := 0 to 999 do
    begin
      lTestString := Format('not_inserted_%d', [lI]);
      if lFilter.MightContain(lTestString) then
        Inc(lFalsePositives);
    end;

    lRate := lFalsePositives / 1000.0;

    // Rate should be reasonably close to 5% (allowing some variance)
    Assert.IsTrue(lRate < 0.15, Format('False positive rate too high: %.2f%%', [lRate * 100]));
    Assert.IsTrue(lRate >= 0.0, 'False positive rate should be non-negative');

    // Estimated rate should be reasonable
    Assert.IsTrue(lFilter.GetEstimatedFalsePositiveRate > 0);
    Assert.IsTrue(lFilter.GetEstimatedFalsePositiveRate < 1.0);

  finally
    lFilter.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBloomFilterTests);

end.
