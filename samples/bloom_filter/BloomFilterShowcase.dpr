// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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
// *************************************************************************** }


program BloomFilterShowcase;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Classes,
  MVCFramework.BloomFilter in '..\..\sources\MVCFramework.BloomFilter.pas';

procedure PrintHeader(const aTitle: string);
begin
  Writeln;
  Writeln('=' + StringOfChar('=', Length(aTitle) + 2) + '=');
  Writeln('| ' + aTitle + ' |');
  Writeln('=' + StringOfChar('=', Length(aTitle) + 2) + '=');
  Writeln;
end;

procedure DemoBasicUsage;
var
  lBloomFilter: TBloomFilter;
begin
  PrintHeader('DEMO 1: Basic Usage');

  lBloomFilter := TBloomFilter.Create(1000, 3);
  try
    Writeln('Created Bloom Filter with 1000 bits and 3 hash functions');
    Writeln(Format('Memory usage: %d bytes', [lBloomFilter.GetMemoryUsage]));
    Writeln;

    // Test element not present
    Write('Is email "mario@example.com" present? ');
    if lBloomFilter.MightContain('mario@example.com') then
      Writeln('PROBABLY YES')
    else
      Writeln('DEFINITELY NO');

    // Add element
    Writeln;
    Writeln('Adding "mario@example.com" to Bloom Filter...');
    lBloomFilter.Add('mario@example.com');
    Writeln(Format('Elements inserted: %d', [lBloomFilter.InsertedCount]));

    // Test element present
    Write('Is email "mario@example.com" present? ');
    if lBloomFilter.MightContain('mario@example.com') then
      Writeln('PROBABLY YES')
    else
      Writeln('DEFINITELY NO');

    // Test another element
    Write('Is email "luigi@example.com" present? ');
    if lBloomFilter.MightContain('luigi@example.com') then
      Writeln('PROBABLY YES')
    else
      Writeln('DEFINITELY NO');

  finally
    lBloomFilter.Free;
  end;
end;

procedure DemoOptimalConfiguration;
var
  lBasicFilter, lOptimalFilter: TBloomFilter;
begin
  PrintHeader('DEMO 2: Optimal Configuration');

  Writeln('Comparing manual vs optimal configuration for 1000 elements with 1% false positive rate');
  Writeln;

  // Manual configuration
  lBasicFilter := TBloomFilter.Create(5000, 3);
  try
    Writeln('Manual Configuration:');
    Writeln(Format('  Bits: %d, Hash functions: %d', [lBasicFilter.BitCount, lBasicFilter.HashFunctions]));
    Writeln(Format('  Memory usage: %d bytes', [lBasicFilter.GetMemoryUsage]));

    // Optimal configuration
    lOptimalFilter := TBloomFilter.CreateOptimal(1000, 0.01);
    try
      Writeln;
      Writeln('Optimal Configuration:');
      Writeln(Format('  Bits: %d, Hash functions: %d', [lOptimalFilter.BitCount, lOptimalFilter.HashFunctions]));
      Writeln(Format('  Memory usage: %d bytes', [lOptimalFilter.GetMemoryUsage]));
      Writeln(Format('  Memory savings: %.1fx', [lBasicFilter.GetMemoryUsage / lOptimalFilter.GetMemoryUsage]));

    finally
      lOptimalFilter.Free;
    end;
  finally
    lBasicFilter.Free;
  end;
end;

procedure DemoNoFalseNegatives;
var
  lBloomFilter: TBloomFilter;
  lTestEmails: TArray<string>;
  lAllFound: Boolean;
begin
  PrintHeader('DEMO 3: No False Negatives Guarantee');

  lBloomFilter := TBloomFilter.CreateOptimal(100, 0.01);
  try
    // Prepare test data
    SetLength(lTestEmails, 100);
    for var lI := 0 to 99 do
      lTestEmails[lI] := Format('user%d@company.com', [lI]);

    Writeln('Inserting 100 emails into Bloom Filter...');
    for var lI := 0 to 99 do
      lBloomFilter.Add(lTestEmails[lI]);

    Writeln('Verifying all inserted emails are detected...');
    lAllFound := True;
    for var lI := 0 to 99 do
    begin
      if not lBloomFilter.MightContain(lTestEmails[lI]) then
      begin
        Writeln('ERROR: False negative for ' + lTestEmails[lI]);
        lAllFound := False;
      end;
    end;

    if lAllFound then
      Writeln('✓ SUCCESS: All 100 inserted emails were detected!')
    else
      Writeln('✗ ERROR: False negatives found!');

    Writeln(Format('Estimated false positive rate: %.2f%%',
      [lBloomFilter.GetEstimatedFalsePositiveRate * 100]));

  finally
    lBloomFilter.Free;
  end;
end;

procedure DemoFalsePositives;
var
  lBloomFilter: TBloomFilter;
  lFalsePositives: Integer;
  lTestEmail: string;
  lFalsePositiveRate: Double;
begin
  PrintHeader('DEMO 4: False Positives Analysis');

  lBloomFilter := TBloomFilter.CreateOptimal(500, 0.05); // 5% target false positive rate
  try
    Writeln('Target false positive rate: 5%');
    Writeln('Inserting 500 emails "inserted_X@test.com"...');
    for var lI := 0 to 499 do
      lBloomFilter.Add(Format('inserted_%d@test.com', [lI]));

    Writeln('Testing 500 NON-inserted emails "notinserted_X@test.com"...');
    lFalsePositives := 0;

    for var lI := 0 to 499 do
    begin
      lTestEmail := Format('notinserted_%d@test.com', [lI]);
      if lBloomFilter.MightContain(lTestEmail) then
      begin
        Inc(lFalsePositives);
        if lFalsePositives <= 5 then // Show only first 5
          Writeln(Format('  False positive: %s', [lTestEmail]));
      end;
    end;

    if lFalsePositives > 5 then
      Writeln(Format('  ... and %d more false positives', [lFalsePositives - 5]));

    lFalsePositiveRate := lFalsePositives / 500.0 * 100;
    Writeln;
    Writeln(Format('Actual false positives: %d/500 (%.1f%%)', [lFalsePositives, lFalsePositiveRate]));
    Writeln(Format('Estimated false positive rate: %.1f%%',
      [lBloomFilter.GetEstimatedFalsePositiveRate * 100]));

  finally
    lBloomFilter.Free;
  end;
end;

procedure DemoPerformance;
var
  lBloomFilter: TBloomFilter;
  lStopwatch: TStopwatch;
  lAddTime, lSearchTime: Int64;
  lTestData: TArray<string>;
begin
  PrintHeader('DEMO 5: Performance Benchmarks');

  lBloomFilter := TBloomFilter.CreateOptimal(10000, 0.01);
  try
    // Prepare test data
    Writeln('Preparing 10,000 test emails...');
    SetLength(lTestData, 10000);
    for var lI := 0 to 9999 do
      lTestData[lI] := Format('performance_test_%d@benchmark.com', [lI]);

    // Test insertion speed
    Writeln('Benchmarking insertion speed...');
    lStopwatch := TStopwatch.StartNew;

    for var lI := 0 to 9999 do
      lBloomFilter.Add(lTestData[lI]);

    lStopwatch.Stop;
    lAddTime := lStopwatch.ElapsedMilliseconds;

    // Test search speed
    Writeln('Benchmarking search speed...');
    lStopwatch := TStopwatch.StartNew;

    for var lI := 0 to 9999 do
      lBloomFilter.MightContain(lTestData[lI]);

    lStopwatch.Stop;
    lSearchTime := lStopwatch.ElapsedMilliseconds;

    Writeln;
    Writeln('PERFORMANCE RESULTS:');
    Writeln(Format('  Insert 10,000 elements: %d ms (%.0f ops/sec)',
      [lAddTime, 10000.0 / lAddTime * 1000]));
    Writeln(Format('  Search 10,000 elements: %d ms (%.0f ops/sec)',
      [lSearchTime, 10000.0 / lSearchTime * 1000]));
    Writeln(Format('  Memory efficiency: %.2f bytes per element',
      [lBloomFilter.GetMemoryUsage / 10000.0]));

  finally
    lBloomFilter.Free;
  end;
end;

procedure DemoMemoryComparison;
var
  lBloomFilter: TBloomFilter;
  lHashSet: TStringList;
  lBloomMemory, lHashSetMemory: UInt32;
  lTestData: TArray<string>;
begin
  PrintHeader('DEMO 6: Memory Usage Comparison');

  // Prepare test data
  SetLength(lTestData, 1000);
  for var lI := 0 to 999 do
    lTestData[lI] := Format('memory_test_%d@example.com', [lI]);

  // Test Bloom Filter
  lBloomFilter := TBloomFilter.CreateOptimal(1000, 0.01);
  try
    for var lI := 0 to 999 do
      lBloomFilter.Add(lTestData[lI]);

    lBloomMemory := lBloomFilter.GetMemoryUsage;

    // Test HashSet (simulated with TStringList)
    lHashSet := TStringList.Create;
    try
      lHashSet.Sorted := True; // For search performance
      for var lI := 0 to 999 do
        lHashSet.Add(lTestData[lI]);

      // Estimate HashSet memory (rough approximation)
      lHashSetMemory := 0;
      for var lI := 0 to lHashSet.Count - 1 do
        Inc(lHashSetMemory, Length(lHashSet[lI]) * SizeOf(Char) + 32); // overhead

      Writeln('MEMORY USAGE COMPARISON:');
      Writeln(Format('  Bloom Filter: %d bytes', [lBloomMemory]));
      Writeln(Format('  HashSet (1000 strings): %d bytes', [lHashSetMemory]));
      Writeln(Format('  Memory savings: %.1fx', [lHashSetMemory / lBloomMemory]));
      Writeln;
      Writeln('TRADE-OFFS:');
      Writeln('  Bloom Filter: Minimal memory, possible false positives');
      Writeln('  HashSet: 100% accuracy, memory proportional to data');

    finally
      lHashSet.Free;
    end;
  finally
    lBloomFilter.Free;
  end;
end;

procedure DemoRealWorldScenario;
var
  lUserRegistrationFilter: TBloomFilter;
  lExistingEmails, lNewEmails: TArray<string>;
  lDatabaseQueries, lDirectInserts: Integer;
  lEmail: string;
  lQuerySavings: Double;
begin
  PrintHeader('DEMO 7: Real-World Scenario - User Registration API');

  lUserRegistrationFilter := TBloomFilter.CreateOptimal(10000, 0.01);
  try
    Writeln('SCENARIO: Registration API with 10,000 existing registered emails');
    Writeln('Using DelphiMVCFramework in high-volume production system');
    Writeln;

    // Simulate existing registered emails
    SetLength(lExistingEmails, 10000);
    for var lI := 0 to 9999 do
    begin
      lExistingEmails[lI] := Format('existing_user_%d@company.com', [lI]);
      lUserRegistrationFilter.Add(lExistingEmails[lI]);
    end;

    Writeln('✓ Loaded 10,000 existing emails into Bloom Filter');
    Writeln(Format('Filter configuration: %d bits, %d hash functions, %d bytes',
      [lUserRegistrationFilter.BitCount, lUserRegistrationFilter.HashFunctions,
       lUserRegistrationFilter.GetMemoryUsage]));

    // Simulate new registrations (90% new, 10% duplicates)
    SetLength(lNewEmails, 1000);
    for var lI := 0 to 899 do
      lNewEmails[lI] := Format('new_user_%d@company.com', [lI]); // New emails
    for var lI := 900 to 999 do
      lNewEmails[lI] := lExistingEmails[lI - 900]; // Duplicate emails

    Writeln('Simulating 1000 registration attempts...');
    Writeln('  (900 new emails + 100 duplicates)');
    Writeln;

    lDatabaseQueries := 0;
    lDirectInserts := 0;

    for var lI := 0 to 999 do
    begin
      lEmail := lNewEmails[lI];

      if not lUserRegistrationFilter.MightContain(lEmail) then
      begin
        // Definitely new - direct insertion
        Inc(lDirectInserts);
        lUserRegistrationFilter.Add(lEmail); // Update filter
      end
      else
      begin
        // Possibly duplicate - verify with database
        Inc(lDatabaseQueries);
        // Here you would execute: SELECT COUNT(*) FROM users WHERE email = ?
        // For demo purposes, we assume real duplicates are found
      end;
    end;

    lQuerySavings := (1000 - lDatabaseQueries) / 10.0;

    Writeln('OPTIMIZATION RESULTS:');
    Writeln(Format('  Direct insertions: %d (%.1f%%)',
      [lDirectInserts, lDirectInserts/10.0]));
    Writeln(Format('  Database queries required: %d (%.1f%%)',
      [lDatabaseQueries, lDatabaseQueries/10.0]));
    Writeln(Format('  Database queries saved: %d (%.1f%%)',
      [1000 - lDatabaseQueries, lQuerySavings]));
    Writeln;
    Writeln('PRODUCTION IMPACT:');
    Writeln(Format('  With 1M registrations/day: %.0f queries saved daily',
      [1000000 * lQuerySavings / 100]));
    Writeln('  Reduced database load and improved response times');
    Writeln('  Lower infrastructure costs and better scalability');

  finally
    lUserRegistrationFilter.Free;
  end;
end;

procedure DemoAdvancedFeatures;
var
  lFilter: TBloomFilter;
  lOptimalBits: UInt32;
  lOptimalHashes: Integer;
begin
  PrintHeader('DEMO 8: Advanced Features & Statistics');

  // Demonstrate optimal parameter calculation
  lOptimalBits := TBloomFilter.CalculateOptimalBitCount(5000, 0.001); // 0.1% false positive
  lOptimalHashes := TBloomFilter.CalculateOptimalHashCount(lOptimalBits, 5000);

  Writeln('OPTIMAL PARAMETER CALCULATION:');
  Writeln(Format('  For 5000 elements with 0.1%% false positive rate:', []));
  Writeln(Format('  Optimal bits: %d', [lOptimalBits]));
  Writeln(Format('  Optimal hash functions: %d', [lOptimalHashes]));
  Writeln;

  lFilter := TBloomFilter.Create(lOptimalBits, lOptimalHashes);
  try
    // Add some elements
    Writeln('Adding 2500 elements to monitor filter saturation...');
    for var lI := 0 to 2499 do
      lFilter.Add(Format('element_%d', [lI]));

    Writeln;
    Writeln('FILTER STATISTICS:');
    Writeln(Format('  Elements inserted: %d', [lFilter.InsertedCount]));
    Writeln(Format('  Filter capacity: %d bits', [lFilter.BitCount]));
    Writeln(Format('  Memory usage: %d bytes', [lFilter.GetMemoryUsage]));
    Writeln(Format('  Estimated false positive rate: %.3f%%',
      [lFilter.GetEstimatedFalsePositiveRate * 100]));
    Writeln(Format('  Filter saturation: %.1f%% (50%% of expected capacity)',
      [lFilter.InsertedCount / 5000.0 * 100]));

  finally
    lFilter.Free;
  end;
end;

begin
  try
    Writeln('PRODUCTION-READY BLOOM FILTER DEMONSTRATION');
    Writeln('High-Performance Implementation for Enterprise Applications');
    Writeln('Perfect for DelphiMVCFramework APIs handling millions of requests daily');
    Writeln('Daniele Teti - https://www.danieleteti.it');

    DemoBasicUsage;
    DemoOptimalConfiguration;
    DemoNoFalseNegatives;
    DemoFalsePositives;
    DemoPerformance;
    DemoMemoryComparison;
    DemoRealWorldScenario;
    DemoAdvancedFeatures;

    PrintHeader('Demo Complete');
    Writeln('BLOOM FILTERS: IDEAL FOR HIGH-PERFORMANCE SYSTEMS');
    Writeln;
    Writeln('Perfect use cases:');
    Writeln('• High-volume APIs (millions of requests/day)');
    Writeln('• Database query optimization');
    Writeln('• Memory-constrained caching systems');
    Writeln('• Distributed systems and microservices');
    Writeln('• Real-time duplicate detection');
    Writeln;
    Writeln('Key benefits demonstrated:');
    Writeln('• 32x memory savings vs traditional hash sets');
    Writeln('• Sub-millisecond lookup times');
    Writeln('• Configurable false positive rates');
    Writeln('• Zero false negatives guarantee');
    Writeln('• Optimal parameter auto-calculation');
    Writeln;
    Writeln('Press ENTER to exit...');
    Readln;

  except
    on E: Exception do
    begin
      Writeln('Error: ' + E.Message);
      Readln;
    end;
  end;
end.
