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

unit MVCFramework.BloomFilter;

interface

uses
  System.SysUtils, System.Hash, System.Math;

type
  IMVCBloomFilter = interface
    ['{0CBB9FC6-379D-4E6A-94F7-76DEE9E108A1}']
    procedure Add(const aValue: string);
    function MightContain(const aValue: string): Boolean;
    procedure Clear;
    function GetInsertedCount: UInt32;
    function GetEstimatedFalsePositiveRate: Double;
    function GetMemoryUsage: UInt32;
  end;


  /// <summary>
  /// High-performance Bloom Filter implementation with optimal memory usage.
  /// Uses packed bit storage and configurable hash functions for production environments.
  /// </summary>
  TMVCBloomFilter = class(TInterfacedObject, IMVCBloomFilter)
  private
    fBitArray: TArray<UInt32>;    // Packed bit storage using UInt32 array
    fBitCount: UInt32;            // Total number of bits in the filter
    fHashFunctions: Integer;      // Number of hash functions to use
    fInsertedCount: UInt32;       // Count of inserted elements (for statistics)

    function GetBitArraySize: Integer;
    procedure SetBit(aIndex: UInt32);
    function GetBit(aIndex: UInt32): Boolean;
    function ComputeHash(const aValue: string; aSeed: UInt32): UInt32;

  public
    /// <summary>
    /// Creates a Bloom Filter with specified bit count and hash functions.
    /// </summary>
    /// <param name="aBitCount">Number of bits in the filter</param>
    /// <param name="aHashFunctions">Number of hash functions to use</param>
    constructor Create(aBitCount: UInt32; aHashFunctions: Integer); overload;

    /// <summary>
    /// Creates an optimally configured Bloom Filter based on expected elements and desired false positive rate.
    /// </summary>
    /// <param name="aExpectedElements">Expected number of elements to be inserted</param>
    /// <param name="aFalsePositiveRate">Desired false positive rate (0.0 to 1.0)</param>
    constructor CreateOptimal(aExpectedElements: UInt32; aFalsePositiveRate: Double = 0.01); overload;

    destructor Destroy; override;

    /// <summary>
    /// Adds an element to the Bloom Filter.
    /// </summary>
    /// <param name="aValue">String value to add</param>
    procedure Add(const aValue: string);

    /// <summary>
    /// Tests if an element might be in the set.
    /// </summary>
    /// <param name="aValue">String value to test</param>
    /// <returns>True if element might be present (with possible false positives),
    /// False if element is definitely not present (no false negatives)</returns>
    function MightContain(const aValue: string): Boolean;

    /// <summary>
    /// Clears all elements from the filter.
    /// </summary>
    procedure Clear;

    // Statistics and metrics
    /// <summary>
    /// Estimates the current false positive rate based on filter saturation.
    /// </summary>
    function GetEstimatedFalsePositiveRate: Double;

    /// <summary>
    /// Returns memory usage in bytes.
    /// </summary>
    function GetMemoryUsage: UInt32;

    /// <summary>
    /// Returns elements count.
    /// </summary>
    function GetInsertedCount: UInt32;

    // Static utility methods for optimal parameter calculation
    /// <summary>
    /// Calculates optimal bit count for given parameters.
    /// </summary>
    class function CalculateOptimalBitCount(aExpectedElements: UInt32; aFalsePositiveRate: Double): UInt32; static;

    /// <summary>
    /// Calculates optimal number of hash functions for given parameters.
    /// </summary>
    class function CalculateOptimalHashCount(aBitCount, aExpectedElements: UInt32): Integer; static;

    // Properties
    property BitCount: UInt32 read fBitCount;
    property HashFunctions: Integer read fHashFunctions;
    property InsertedCount: UInt32 read GetInsertedCount;
  end;

  EMVCBloomFilterException = class(Exception)
  end;

implementation

constructor TMVCBloomFilter.Create(aBitCount: UInt32; aHashFunctions: Integer);
begin
  inherited Create;

  if aBitCount = 0 then
    raise EMVCBloomFilterException.Create('Bit count must be greater than 0');
  if aHashFunctions <= 0 then
    raise EMVCBloomFilterException.Create('Hash function count must be greater than 0');

  fBitCount := aBitCount;
  fHashFunctions := aHashFunctions;
  fInsertedCount := 0;

  // Calculate required UInt32 array size for bit storage
  SetLength(fBitArray, GetBitArraySize);
  Clear;
end;

constructor TMVCBloomFilter.CreateOptimal(aExpectedElements: UInt32; aFalsePositiveRate: Double);
var
  lOptimalBits: UInt32;
  lOptimalHashes: Integer;
begin
  if aExpectedElements = 0 then
    raise EMVCBloomFilterException.Create('Expected elements must be greater than 0');
  if (aFalsePositiveRate <= 0) or (aFalsePositiveRate >= 1) then
    raise EMVCBloomFilterException.Create('False positive rate must be between 0 and 1');

  lOptimalBits := CalculateOptimalBitCount(aExpectedElements, aFalsePositiveRate);
  lOptimalHashes := CalculateOptimalHashCount(lOptimalBits, aExpectedElements);

  Create(lOptimalBits, lOptimalHashes);
end;

destructor TMVCBloomFilter.Destroy;
begin
  SetLength(fBitArray, 0);
  inherited;
end;

function TMVCBloomFilter.GetBitArraySize: Integer;
begin
  // Calculate ceiling division: (fBitCount + 31) div 32
  Result := (fBitCount + 31) div 32;
end;

procedure TMVCBloomFilter.SetBit(aIndex: UInt32);
var
  lArrayIndex: Integer;
  lBitIndex: Integer;
begin
  lArrayIndex := aIndex div 32;
  lBitIndex := aIndex mod 32;
  fBitArray[lArrayIndex] := fBitArray[lArrayIndex] or (1 shl lBitIndex);
end;

function TMVCBloomFilter.GetBit(aIndex: UInt32): Boolean;
var
  lArrayIndex: Integer;
  lBitIndex: Integer;
begin
  lArrayIndex := aIndex div 32;
  lBitIndex := aIndex mod 32;
  Result := (fBitArray[lArrayIndex] and (1 shl lBitIndex)) <> 0;
end;

function TMVCBloomFilter.ComputeHash(const aValue: string; aSeed: UInt32): UInt32;
var
  lHash1Val, lHash2Val: Cardinal;
  lCombined: UInt64;  // Use UInt64 to prevent overflow
begin
  // Use double hashing to simulate multiple independent hash functions
  lHash1Val := Cardinal(THashBobJenkins.GetHashValue(aValue));
  lHash2Val := Cardinal(THashFNV1a32.GetHashValue(aValue));

  // Combine with seed to create different hash functions
  lCombined := UInt64(lHash1Val) + UInt64(aSeed) * UInt64(lHash2Val);

  // Modulo ensures result is always within valid range
  Result := lCombined mod fBitCount;
end;

procedure TMVCBloomFilter.Add(const aValue: string);
var
  lHashValue: UInt32;
begin
  for var lI := 0 to fHashFunctions - 1 do
  begin
    lHashValue := ComputeHash(aValue, UInt32(lI));
    SetBit(lHashValue);
  end;
  Inc(fInsertedCount);
end;

function TMVCBloomFilter.MightContain(const aValue: string): Boolean;
var
  lHashValue: UInt32;
begin
  Result := True;
  for var lI := 0 to fHashFunctions - 1 do
  begin
    lHashValue := ComputeHash(aValue, UInt32(lI));
    if not GetBit(lHashValue) then
    begin
      Result := False;
      Exit; // Early exit optimization
    end;
  end;
end;

procedure TMVCBloomFilter.Clear;
begin
  for var lI := 0 to Length(fBitArray) - 1 do
    fBitArray[lI] := 0;
  fInsertedCount := 0;
end;

function TMVCBloomFilter.GetEstimatedFalsePositiveRate: Double;
var
  lBitsSet: UInt32;
  lMask: UInt32;
begin
  if fInsertedCount = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  // Count set bits in the filter
  lBitsSet := 0;
  for var lI := 0 to Length(fBitArray) - 1 do
  begin
    if fBitArray[lI] <> 0 then
    begin
      for var lJ := 0 to 31 do
      begin
        lMask := 1 shl lJ;
        if (fBitArray[lI] and lMask) <> 0 then
          Inc(lBitsSet);
      end;
    end;
  end;

  // Formula: (bits_set / total_bits) ^ hash_functions
  Result := Power(lBitsSet / fBitCount, fHashFunctions);
end;

function TMVCBloomFilter.GetInsertedCount: UInt32;
begin
  Result := fInsertedCount;
end;

function TMVCBloomFilter.GetMemoryUsage: UInt32;
begin
  Result := Length(fBitArray) * SizeOf(UInt32) + SizeOf(TMVCBloomFilter);
end;

class function TMVCBloomFilter.CalculateOptimalBitCount(aExpectedElements: UInt32; aFalsePositiveRate: Double): UInt32;
begin
  // Formula: m = -n * ln(p) / (ln(2)^2)
  // where m = bit count, n = expected elements, p = false positive rate
  Result := Round(-aExpectedElements * Ln(aFalsePositiveRate) / (Ln(2) * Ln(2)));
end;

class function TMVCBloomFilter.CalculateOptimalHashCount(aBitCount, aExpectedElements: UInt32): Integer;
begin
  // Formula: k = (m/n) * ln(2)
  // where k = hash count, m = bit count, n = expected elements
  Result := Round((aBitCount / aExpectedElements) * Ln(2));
  if Result < 1 then
    Result := 1;
end;

end.
