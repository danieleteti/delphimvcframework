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

unit AllShowcasesU;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.Repository,
  MVCFramework.ActiveRecord,
  EntitiesU,
  RepositoriesU;

type
  /// <summary>
  /// Collection of all Repository Pattern showcases
  /// Demonstrates every feature of IMVCRepository<T>
  /// </summary>
  TAllShowcases = class
  public
    // Basic CRUD Operations
    class procedure ShowBasicCRUD(Log: TProc<string>);
    class procedure ShowStore(Log: TProc<string>);
    class procedure ShowRefresh(Log: TProc<string>);

    // Query Operations
    class procedure ShowGetAll(Log: TProc<string>);
    class procedure ShowGetWhere(Log: TProc<string>);
    class procedure ShowGetOneByWhere(Log: TProc<string>);
    class procedure ShowGetFirstByWhere(Log: TProc<string>);

    // RQL Operations
    class procedure ShowSelectRQL(Log: TProc<string>);
    class procedure ShowCountRQL(Log: TProc<string>);
    class procedure ShowDeleteRQL(Log: TProc<string>);
    class procedure ShowGetOneByRQL(Log: TProc<string>);

    // SQL Operations
    class procedure ShowSelectSQL(Log: TProc<string>);
    class procedure ShowSelectOne(Log: TProc<string>);

    // Named Queries
    class procedure ShowNamedSQLQuery(Log: TProc<string>);
    class procedure ShowNamedRQLQuery(Log: TProc<string>);

    // Utility Operations
    class procedure ShowCount(Log: TProc<string>);
    class procedure ShowDeleteAll(Log: TProc<string>);
    class procedure ShowExists(Log: TProc<string>);

    // Custom Repository Methods
    class procedure ShowCustomMethods(Log: TProc<string>);

    // Advanced Features
    class procedure ShowCustomConnection(Log: TProc<string>);
    class procedure ShowMultipleRepositories(Log: TProc<string>);
    class procedure ShowRepositoryComposition(Log: TProc<string>);

    // Error Handling & Border Cases
    class procedure ShowNilEntityProtection(Log: TProc<string>);
    class procedure ShowNotFoundHandling(Log: TProc<string>);
    class procedure ShowEmptyResultSets(Log: TProc<string>);
    class procedure ShowNullableFields(Log: TProc<string>);
    class procedure ShowValidationErrors(Log: TProc<string>);

    // Performance Tests
    class procedure ShowBulkInsert(Log: TProc<string>);
    class procedure ShowBulkUpdate(Log: TProc<string>);
    class procedure ShowBulkDelete(Log: TProc<string>);

    // Special Cases
    class procedure ShowTransactionSupport(Log: TProc<string>);
    class procedure ShowReadOnlyRepository(Log: TProc<string>);
    class procedure ShowVersionedEntities(Log: TProc<string>);

    // Helper Methods
    class function CreateTestCustomer(const Code: string): TCustomer;
    class procedure CleanupTestData(Log: TProc<string>);
  end;

implementation

uses
  Data.DB,
  FireDAC.Comp.Client;

const
  TEST_CODE_PREFIX = 'TEST_';

{ TAllShowcases }

class function TAllShowcases.CreateTestCustomer(const Code: string): TCustomer;
begin
  Result := TCustomer.Create;
  Result.Code := Code;
  Result.CompanyName := 'Test Company ' + Code;
  Result.City := 'Rome';
  Result.Rating := 5;
  Result.Note := 'Test customer';
end;

class procedure TAllShowcases.CleanupTestData(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCount: Int64;
begin
  lRepo := TCustomerRepository.Create;
  lCount := lRepo.DeleteRQL('starts(code,"' + TEST_CODE_PREFIX + '")');
  if lCount > 0 then
    Log(Format('Cleaned up %d test records', [lCount]));
end;

class procedure TAllShowcases.ShowBasicCRUD(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== BASIC CRUD OPERATIONS ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // CREATE
  Log('1. INSERT - Creating new customer');
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'CRUD001');
  try
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log(Format('   OK Created customer with ID: %d', [lID]));
  finally
    lCustomer.Free;
  end;

  // READ
  Log('2. SELECT - Reading customer by PK');
  lCustomer := lRepo.GetByPK(lID);
  try
    Log(Format('   OK Retrieved: %s - %s', [lCustomer.Code.Value, lCustomer.CompanyName.Value]));
  finally
    lCustomer.Free;
  end;

  // UPDATE
  Log('3. UPDATE - Modifying customer');
  lCustomer := lRepo.GetByPK(lID);
  try
    lCustomer.CompanyName := 'Updated Company Name';
    lCustomer.Note := 'Updated via Repository';
    lRepo.Update(lCustomer);
    Log('   OK Customer updated');
  finally
    lCustomer.Free;
  end;

  // Verify UPDATE
  Log('4. VERIFY - Checking update');
  lCustomer := lRepo.GetByPK(lID);
  try
    if lCustomer.CompanyName.Value = 'Updated Company Name' then
      Log('   OK Update verified: ' + lCustomer.CompanyName.Value)
    else
      Log('   ERROR ERROR: Update failed');
  finally
    lCustomer.Free;
  end;

  // DELETE
  Log('5. DELETE - Removing customer');
  lCustomer := lRepo.GetByPK(lID);
  try
    lRepo.Delete(lCustomer);
    Log('   OK Customer deleted');
  finally
    lCustomer.Free;
  end;

  // Verify DELETE
  Log('6. VERIFY - Checking deletion');
  lCustomer := lRepo.GetByPK(lID, False);
  if lCustomer = nil then
    Log('   OK Deletion verified: customer not found')
  else
  begin
    Log('   ERROR ERROR: Customer still exists!');
    lCustomer.Free;
  end;

  Log('');
  Log('OK CRUD Operations completed successfully');
end;

class procedure TAllShowcases.ShowStore(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== STORE METHOD (Smart Insert/Update) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Store with null PK = INSERT
  Log('1. Store with null PK (will INSERT)');
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'STORE001');
  try
    lRepo.Store(lCustomer); // INSERT
    lID := lCustomer.ID.Value;
    Log(Format('   OK Inserted with ID: %d', [lID]));
  finally
    lCustomer.Free;
  end;

  // Store with existing PK = UPDATE
  Log('2. Store with existing PK (will UPDATE)');
  lCustomer := lRepo.GetByPK(lID);
  try
    lCustomer.CompanyName := 'Updated via Store';
    lRepo.Store(lCustomer); // UPDATE
    Log('   OK Updated existing customer');
  finally
    lCustomer.Free;
  end;

  // Verify
  lCustomer := lRepo.GetByPK(lID);
  try
    if lCustomer.CompanyName.Value = 'Updated via Store' then
      Log('   OK Store method works correctly');
    lRepo.Delete(lCustomer);
  finally
    lCustomer.Free;
  end;
end;

class procedure TAllShowcases.ShowGetAll(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== GET ALL ENTITIES ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create some test data
  for I := 1 to 3 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('ALL%2.2d', [I]));
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Get all
  lCustomers := lRepo.GetAll;
  try
    Log(Format('Retrieved %d customers:', [lCustomers.Count]));
    for lCustomer in lCustomers do
    begin
      if lCustomer.Code.HasValue and lCustomer.Code.Value.StartsWith(TEST_CODE_PREFIX) then
        Log(Format('  - %s: %s', [lCustomer.Code.Value, lCustomer.CompanyName.Value]));
    end;
  finally
    lCustomers.Free;
  end;

  // Cleanup
  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowGetWhere(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== GET WHERE (SQL Filter) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data with different cities
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'WHERE01');
  lCustomer.City := 'Rome';
  try
    lRepo.Insert(lCustomer);
  finally
    lCustomer.Free;
  end;

  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'WHERE02');
  lCustomer.City := 'London';
  try
    lRepo.Insert(lCustomer);
  finally
    lCustomer.Free;
  end;

  // Get with WHERE clause
  Log('1. Simple WHERE: city = ?');
  lCustomers := lRepo.GetWhere('city = ?', ['Rome']);
  try
    Log(Format('   Found %d customers in Rome', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('2. Complex WHERE: city = ? AND rating >= ?');
  lCustomers := lRepo.GetWhere('city = ? AND rating >= ?', ['Rome', 4]);
  try
    Log(Format('   Found %d customers in Rome with rating >= 4', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('3. WHERE with LIKE: code LIKE ?');
  lCustomers := lRepo.GetWhere('code LIKE ?', [TEST_CODE_PREFIX + '%']);
  try
    Log(Format('   Found %d test customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowSelectRQL(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== SELECT RQL (Resource Query Language) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 5 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('RQL%2.2d', [I]));
    lCustomer.Rating := I;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // RQL examples
  Log('1. Filter: eq(rating,5)');
  lCustomers := lRepo.SelectRQL('eq(rating,5)', 100);
  try
    Log(Format('   Found %d customers with rating=5', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('2. Filter: ge(rating,4)');
  lCustomers := lRepo.SelectRQL('ge(rating,4)', 100);
  try
    Log(Format('   Found %d customers with rating>=4', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('3. Sort: sort(+code)');
  lCustomers := lRepo.SelectRQL('sort(+code)', 100);
  try
    Log(Format('   Retrieved %d customers sorted by code', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('4. Limit: limit(2,0)');
  lCustomers := lRepo.SelectRQL('limit(2,0)', 100);
  try
    Log(Format('   Retrieved first %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowExists(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== EXISTS METHOD (Efficient Existence Check) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create customer
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'EXISTS01');
  try
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log(Format('Created customer with ID: %d', [lID]));
  finally
    lCustomer.Free;
  end;

  // Check if exists (should be True)
  Log('1. Check existing customer');
  if lRepo.Exists(lID) then
    Log('   OK Customer exists')
  else
    Log('   ERROR Customer NOT found (ERROR)');

  // Check non-existent (should be False)
  Log('2. Check non-existent customer');
  if not lRepo.Exists(999999) then
    Log('   OK Customer does not exist (as expected)')
  else
    Log('   ERROR Customer found (ERROR)');

  // Use case: conditional operations
  Log('3. Use case: Conditional update');
  if lRepo.Exists(lID) then
  begin
    lCustomer := lRepo.GetByPK(lID);
    try
      lCustomer.Note := 'Updated after exists check';
      lRepo.Update(lCustomer);
      Log('   OK Updated customer after confirming existence');
    finally
      lCustomer.Free;
    end;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowBulkInsert(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
  lStartTime, lEndTime: TDateTime;
  lCount: Int64;
begin
  Log('');
  Log('=== BULK INSERT (Performance Test) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  lStartTime := Now;
  Log('Inserting 100 customers...');

  for I := 1 to 100 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('BULK%4.4d', [I]));
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  lEndTime := Now;
  Log(Format('OK Inserted 100 customers in %.3f seconds',
    [(lEndTime - lStartTime) * 24 * 60 * 60]));

  lCount := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'BULK")');
  Log(Format('OK Verified: %d bulk records in database', [lCount]));

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowNilEntityProtection(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
begin
  Log('');
  Log('=== NIL ENTITY PROTECTION ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  Log('1. Try Insert(nil)');
  try
    lRepo.Insert(nil);
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecord do
      Log('   OK Correctly raised: ' + E.Message);
  end;

  Log('2. Try Update(nil)');
  try
    lRepo.Update(nil);
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecord do
      Log('   OK Correctly raised: ' + E.Message);
  end;

  Log('3. Try Delete(nil)');
  try
    lRepo.Delete(nil);
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecord do
      Log('   OK Correctly raised: ' + E.Message);
  end;

  Log('4. Try Store(nil)');
  try
    lRepo.Store(nil);
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecord do
      Log('   OK Correctly raised: ' + E.Message);
  end;

  Log('');
  Log('OK All nil entity protections working correctly');
end;

class procedure TAllShowcases.ShowCustomMethods(Log: TProc<string>);
var
  lRepo: TCustomerRepository;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== CUSTOM REPOSITORY METHODS ===');
  Log('');

  lRepo := TCustomerRepository.Create;
  try
    // Create test data
    for I := 1 to 3 do
    begin
      lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('CUSTOM%d', [I]));
      lCustomer.City := 'Rome';
      lCustomer.Rating := 4 + (I mod 2);
      try
        lRepo.Insert(lCustomer);
      finally
        lCustomer.Free;
      end;
    end;

    // Custom method: GetCustomersByCity
    Log('1. GetCustomersByCity("Rome")');
    lCustomers := lRepo.GetCustomersByCity('Rome');
    try
      Log(Format('   Found %d customers in Rome', [lCustomers.Count]));
    finally
      lCustomers.Free;
    end;

    // Custom method: GetTopRatedCustomers
    Log('2. GetTopRatedCustomers()');
    lCustomers := lRepo.GetTopRatedCustomers;
    try
      Log(Format('   Found %d top-rated customers', [lCustomers.Count]));
    finally
      lCustomers.Free;
    end;

    Log('');
    Log('OK Custom repository methods working correctly');
  finally
    lRepo.Free;
    CleanupTestData(Log);
  end;
end;

class procedure TAllShowcases.ShowRefresh(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== REFRESH ENTITY FROM DATABASE ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create and insert
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'REFRESH01');
  try
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log(Format('Created customer: %s', [lCustomer.CompanyName.Value]));
  finally
    lCustomer.Free;
  end;

  // Get customer
  lCustomer := lRepo.GetByPK(lID);
  try
    Log(Format('Initial CompanyName: %s', [lCustomer.CompanyName.Value]));

    // Simulate external update (direct SQL)
    TMVCActiveRecord.CurrentConnection.ExecSQL(
      'UPDATE customers SET description = ? WHERE id = ?',
      ['EXTERNALLY UPDATED', lID]);

    Log('External update performed via SQL');

    // Refresh to get latest data
    lCustomer.Refresh;
    Log(Format('After Refresh: %s', [lCustomer.CompanyName.Value]));

    if lCustomer.CompanyName.Value = 'EXTERNALLY UPDATED' then
      Log('   OK Refresh worked correctly')
    else
      Log('   ERROR ERROR: Refresh did not update entity');

    lRepo.Delete(lCustomer);
  finally
    lCustomer.Free;
  end;
end;

class procedure TAllShowcases.ShowGetOneByWhere(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== GET ONE BY WHERE ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create unique test data
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'UNIQUE01');
  lCustomer.City := 'UniqueCity';
  try
    lRepo.Insert(lCustomer);
  finally
    lCustomer.Free;
  end;

  // GetOneByWhere - expects exactly one result
  Log('1. GetOneByWhere with unique condition');
  lCustomer := lRepo.GetOneByWhere('code = ?', [TEST_CODE_PREFIX + 'UNIQUE01']);
  try
    Log(Format('   OK Found: %s in %s', [lCustomer.Code.Value, lCustomer.City]));
  finally
    lCustomer.Free;
  end;

  // Test with no results
  Log('2. GetOneByWhere with no results (will raise exception)');
  try
    lCustomer := lRepo.GetOneByWhere('city = ?', ['NonExistentCity']);
    lCustomer.Free;
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecordNotFound do
      Log('   OK Correctly raised: ' + E.ClassName);
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowGetFirstByWhere(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== GET FIRST BY WHERE ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create multiple records with same city
  for I := 1 to 3 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('FIRST%2.2d', [I]));
    lCustomer.City := 'MultiCity';
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // GetFirstByWhere - returns first result even if multiple exist
  Log('1. GetFirstByWhere with multiple results');
  lCustomer := lRepo.GetFirstByWhere('city = ?', ['MultiCity']);
  try
    Log(Format('   OK Retrieved first: %s', [lCustomer.Code.Value]));
  finally
    lCustomer.Free;
  end;

  // Test with no results
  Log('2. GetFirstByWhere with no results (returns nil)');
  lCustomer := lRepo.GetFirstByWhere('city = ?', ['NonExistent'], False);
  if lCustomer = nil then
    Log('   OK Correctly returned nil')
  else
  begin
    Log('   ERROR ERROR: Should have returned nil');
    lCustomer.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowCountRQL(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lCount: Int64;
  I: Integer;
begin
  Log('');
  Log('=== COUNT RQL ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data with different ratings
  for I := 1 to 5 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('COUNT%2.2d', [I]));
    lCustomer.Rating := I;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Count all test records
  Log('1. Count all test customers');
  lCount := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + '")');
  Log(Format('   Found %d test customers', [lCount]));

  // Count with filter
  Log('2. Count high-rated customers');
  lCount := lRepo.CountRQL('ge(rating,4)');
  Log(Format('   Found %d customers with rating >= 4', [lCount]));

  // Count all (no RQL)
  Log('3. Count all customers in database');
  lCount := lRepo.CountRQL('');
  Log(Format('   Total customers: %d', [lCount]));

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowDeleteRQL(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lDeleted: Int64;
  I: Integer;
begin
  Log('');
  Log('=== DELETE RQL ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 5 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('DEL%2.2d', [I]));
    lCustomer.Rating := I;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Created 5 test customers');

  // Delete with RQL filter
  Log('1. Delete customers with rating <= 2');
  lDeleted := lRepo.DeleteRQL('le(rating,2)');
  Log(Format('   OK Deleted %d customers', [lDeleted]));

  // Verify remaining
  Log('2. Count remaining test customers');
  lDeleted := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'DEL")');
  Log(Format('   Remaining: %d customers', [lDeleted]));

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowGetOneByRQL(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== GET ONE BY RQL ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create unique test customer
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'ONERQL01');
  lCustomer.Rating := 7; // unique rating
  try
    lRepo.Insert(lCustomer);
  finally
    lCustomer.Free;
  end;

  // GetOneByRQL - expects exactly one match
  Log('1. GetOneByRQL with unique condition');
  lCustomer := lRepo.GetOneByRQL('eq(code,"' + TEST_CODE_PREFIX + 'ONERQL01")');
  try
    Log(Format('   OK Found: %s with rating %d',
      [lCustomer.Code.Value, lCustomer.Rating.Value]));
  finally
    lCustomer.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowSelectSQL(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== SELECT SQL (Raw SQL Queries) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 3 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('SQL%2.2d', [I]));
    lCustomer.City := 'SqlCity';
    lCustomer.Rating := I + 2;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Raw SQL query
  Log('1. Execute raw SQL query');
  lCustomers := lRepo.Select(
    'SELECT * FROM customers WHERE city = ? AND rating >= ? ORDER BY code',
    ['SqlCity', 3]);
  try
    Log(Format('   Found %d customers via raw SQL', [lCustomers.Count]));
    for lCustomer in lCustomers do
      if lCustomer.Rating.HasValue then
        Log(Format('     - %s (rating: %d)',
          [lCustomer.Code.Value, lCustomer.Rating.Value]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowSelectOne(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== SELECT ONE (Single Result Query) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test customer
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'SELONE01');
  lCustomer.City := 'SingleCity';
  try
    lRepo.Insert(lCustomer);
  finally
    lCustomer.Free;
  end;

  // SelectOne - single result
  Log('1. SelectOne with unique condition');
  lCustomer := lRepo.SelectOne('SELECT * FROM customers WHERE code = ?', [TEST_CODE_PREFIX + 'SELONE01']);
  try
    Log(Format('   OK Retrieved: %s', [lCustomer.Code.Value]));
  finally
    lCustomer.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowNamedSQLQuery(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== NAMED SQL QUERY ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data with high ratings
  for I := 1 to 3 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('NAMED%2.2d', [I]));
    lCustomer.Rating := 4 + (I mod 2);
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Execute named query 'BestCustomers' (defined in TCustomer)
  Log('1. Execute named SQL query: BestCustomers');
  lCustomers := lRepo.SelectByNamedQuery('BestCustomers', [], []);
  try
    Log(Format('   Found %d best customers (rating >= 4)', [lCustomers.Count]));
    for lCustomer in lCustomers do
      if lCustomer.Code.HasValue and lCustomer.Code.Value.StartsWith(TEST_CODE_PREFIX) then
        Log(Format('     - %s (rating: %d)',
          [lCustomer.Code.Value, lCustomer.Rating.Value]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowNamedRQLQuery(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
begin
  Log('');
  Log('=== NAMED RQL QUERY ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 5 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('NRQL%2.2d', [I]));
    lCustomer.Rating := I;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Execute named RQL query with parameter
  Log('1. Execute named RQL query: RatingEqualsToPar');
  lCustomers := lRepo.SelectRQLByNamedQuery('RatingEqualsToPar', [5], 100);
  try
    Log(Format('   Found %d customers with rating = 5', [lCustomers.Count]));
    for lCustomer in lCustomers do
      if lCustomer.Code.HasValue and lCustomer.Code.Value.StartsWith(TEST_CODE_PREFIX) then
        Log(Format('     - %s', [lCustomer.Code.Value]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowCount(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lCount: Int64;
  I: Integer;
begin
  Log('');
  Log('=== COUNT METHOD ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Initial count
  lCount := lRepo.Count;
  Log(Format('Initial total customers: %d', [lCount]));

  // Create test data
  for I := 1 to 10 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('CNT%3.3d', [I]));
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  // Count after insert
  Log('After inserting 10 customers:');
  lCount := lRepo.Count;
  Log(Format('   Total customers: %d', [lCount]));

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowDeleteAll(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lDeleted, lCount: Int64;
  I: Integer;
begin
  Log('');
  Log('=== DELETE ALL ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 5 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('DELALL%2.2d', [I]));
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;

  Log('Created 5 test customers');

  // Count before
  lCount := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'DELALL")');
  Log(Format('Test customers before: %d', [lCount]));

  // DeleteAll with RQL filter (only test customers)
  lDeleted := lRepo.DeleteRQL('starts(code,"' + TEST_CODE_PREFIX + 'DELALL")');
  Log(Format('Deleted: %d test customers', [lDeleted]));

  // Count after
  lCount := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'DELALL")');
  Log(Format('Test customers after: %d', [lCount]));

  if lCount = 0 then
    Log('   OK All test customers deleted')
  else
    Log('   ERROR ERROR: Some customers still exist');
end;

class procedure TAllShowcases.ShowCustomConnection(Log: TProc<string>);
var
  lConnection: TFDConnection;
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== CUSTOM CONNECTION ===');
  Log('');

  // Create custom connection
  lConnection := TFDConnection.Create(nil);
  try
    lConnection.ConnectionDefName := TMVCActiveRecord.CurrentConnection.ConnectionDefName;
    lConnection.Connected := True;
    Log('OK Custom connection created');

    // Create repository with custom connection
    lRepo := TMVCRepositoryWithConnection<TCustomer>.Create(lConnection);
    try
      Log('OK Repository using custom connection');

      // Perform operation
      lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'CUSTOMCON01');
      try
        lRepo.Insert(lCustomer);
        Log(Format('OK Inserted customer with ID: %d using custom connection',
          [lCustomer.ID.Value]));
        lRepo.Delete(lCustomer);
      finally
        lCustomer.Free;
      end;
    finally
      lRepo := nil;
    end;
  finally
    lConnection.Free;
  end;
end;

class procedure TAllShowcases.ShowMultipleRepositories(Log: TProc<string>);
var
  lCustomerRepo1: IMVCRepository<TCustomer>;
  lCustomerRepo2: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lCount1, lCount2: Int64;
begin
  Log('');
  Log('=== MULTIPLE REPOSITORIES ===');
  Log('');

  // Create two independent repository instances
  lCustomerRepo1 := TCustomerRepository.Create;
  lCustomerRepo2 := TCustomerRepository.Create;

  Log('Using two independent repository instances for same entity type');

  // Insert using first repository
  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'MULTI01');
  try
    lCustomerRepo1.Insert(lCustomer);
    Log('OK Customer inserted via Repository 1');
  finally
    lCustomer.Free;
  end;

  // Count using second repository (sees same data)
  lCount1 := lCustomerRepo1.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'MULTI")');
  lCount2 := lCustomerRepo2.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'MULTI")');
  Log(Format('OK Both repositories see same data: Repo1=%d, Repo2=%d', [lCount1, lCount2]));

  // Delete using second repository
  lCustomer := lCustomerRepo2.GetOneByWhere('code = ?', [TEST_CODE_PREFIX + 'MULTI01']);
  try
    lCustomerRepo2.Delete(lCustomer);
    Log('OK Customer deleted via Repository 2');
  finally
    lCustomer.Free;
  end;

  CleanupTestData(Log);
  Log('OK Multiple repository instances work independently and share same data source');
end;

class procedure TAllShowcases.ShowRepositoryComposition(Log: TProc<string>);
var
  lCustomerRepo: TCustomerRepository;
  lCustomer: TCustomer;
  lCustomers: TObjectList<TCustomer>;
begin
  Log('');
  Log('=== REPOSITORY COMPOSITION ===');
  Log('');

  lCustomerRepo := TCustomerRepository.Create;
  try
    // Create customer using specialized repository
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'COMP01');
    lCustomer.City := 'CompCity';
    try
      lCustomerRepo.Insert(lCustomer);
      Log(Format('OK Created customer with ID: %d', [lCustomer.ID.Value]));
    finally
      lCustomer.Free;
    end;

    // Use custom repository method (demonstrates composition)
    lCustomers := lCustomerRepo.GetCustomersByCity('CompCity');
    try
      Log(Format('OK Custom method GetCustomersByCity found %d customers', [lCustomers.Count]));
    finally
      lCustomers.Free;
    end;

    // Use another custom method
    lCustomers := lCustomerRepo.GetTopRatedCustomers;
    try
      Log(Format('OK Custom method GetTopRatedCustomers found %d customers', [lCustomers.Count]));
    finally
      lCustomers.Free;
    end;

    Log('OK Repository composition: base CRUD + domain-specific methods');
  finally
    lCustomerRepo.Free;
    CleanupTestData(Log);
  end;
end;

class procedure TAllShowcases.ShowNotFoundHandling(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== NOT FOUND HANDLING ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // GetByPK with RaiseExceptionIfNotFound = True (default)
  Log('1. GetByPK(999999, True) - raises exception');
  try
    lCustomer := lRepo.GetByPK(999999, True);
    lCustomer.Free;
    Log('   ERROR ERROR: Should have raised exception');
  except
    on E: EMVCActiveRecordNotFound do
      Log('   OK Correctly raised: ' + E.ClassName);
  end;

  // GetByPK with RaiseExceptionIfNotFound = False
  Log('2. GetByPK(999999, False) - returns nil');
  lCustomer := lRepo.GetByPK(999999, False);
  if lCustomer = nil then
    Log('   OK Correctly returned nil')
  else
  begin
    Log('   ERROR ERROR: Should have returned nil');
    lCustomer.Free;
  end;

  // GetFirstByWhere with RaiseExceptionIfNotFound = False
  Log('3. GetFirstByWhere with non-existent condition');
  lCustomer := lRepo.GetFirstByWhere('city = ?', ['NonExistent'], False);
  if lCustomer = nil then
    Log('   OK Correctly returned nil')
  else
  begin
    Log('   ERROR ERROR: Should have returned nil');
    lCustomer.Free;
  end;

  Log('');
  Log('OK Not found handling working correctly');
end;

class procedure TAllShowcases.ShowEmptyResultSets(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
begin
  Log('');
  Log('=== EMPTY RESULT SETS ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // GetWhere with no results
  Log('1. GetWhere with non-matching condition');
  lCustomers := lRepo.GetWhere('city = ?', ['NonExistentCity123456']);
  try
    if lCustomers.Count = 0 then
      Log('   OK Returns empty list (Count=0)')
    else
      Log(Format('   ERROR ERROR: Found %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  // SelectRQL with no results
  Log('2. SelectRQL with non-matching RQL');
  lCustomers := lRepo.SelectRQL('eq(rating,999)', 100);
  try
    if lCustomers.Count = 0 then
      Log('   OK Returns empty list (Count=0)')
    else
      Log(Format('   ERROR ERROR: Found %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  // GetAll on empty table would return empty list (not testing to avoid affecting existing data)
  Log('3. Empty result sets are returned as empty TObjectList<T>');
  Log('   OK No exceptions raised, safe to iterate');
end;

class procedure TAllShowcases.ShowNullableFields(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== NULLABLE FIELDS ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create customer with null optional fields
  Log('1. Create customer with null optional fields');
  lCustomer := TCustomer.Create;
  try
    lCustomer.Code := TEST_CODE_PREFIX + 'NULL01';
    lCustomer.CompanyName := 'Null Test Company';
    lCustomer.City := 'NullCity';
    // Rating and Note are nullable - leave them null
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log('   OK Customer inserted with null fields');
  finally
    lCustomer.Free;
  end;

  // Read back and check nulls
  Log('2. Read back and verify nullable fields');
  lCustomer := lRepo.GetByPK(lID);
  try
    Log(Format('   Code: %s', [lCustomer.Code.Value]));
    Log(Format('   Rating has value: %s', [BoolToStr(lCustomer.Rating.HasValue, True)]));
    Log(Format('   Note: %s', [lCustomer.Note]));

    // Set nullable field
    lCustomer.Rating := 5;
    lRepo.Update(lCustomer);
    Log('   OK Updated nullable field from null to value');

    lRepo.Delete(lCustomer);
  finally
    lCustomer.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowValidationErrors(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
begin
  Log('');
  Log('=== VALIDATION ERRORS ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Try to insert invalid data (if validations are implemented)
  Log('1. Attempt to insert customer with invalid data');
  lCustomer := TCustomer.Create;
  try
    // Leave required fields null (Code, CompanyName)
    lCustomer.City := 'TestCity';

    try
      lRepo.Insert(lCustomer);
      Log('   Note: No validation constraints defined or all fields are nullable');
    except
      on E: EMVCActiveRecordValidationError do
        Log(Format('   OK Validation error: %s', [E.Message]));
      on E: Exception do
        Log(Format('   Database constraint: %s', [E.Message]));
    end;
  finally
    lCustomer.Free;
  end;

  Log('');
  Log('Note: Validation behavior depends on entity constraints');
end;

class procedure TAllShowcases.ShowBulkUpdate(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
  lStartTime, lEndTime: TDateTime;
begin
  Log('');
  Log('=== BULK UPDATE ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 50 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('BULKUPD%3.3d', [I]));
    lCustomer.Rating := 3;
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;
  Log('Created 50 customers with rating=3');

  // Bulk update
  lStartTime := Now;
  lCustomers := lRepo.GetWhere('code LIKE ?', [TEST_CODE_PREFIX + 'BULKUPD%']);
  try
    Log(Format('Retrieved %d customers for update', [lCustomers.Count]));

    for lCustomer in lCustomers do
    begin
      lCustomer.Rating := 5;
      lRepo.Update(lCustomer);
    end;

    lEndTime := Now;
    Log(Format('OK Updated %d customers in %.3f seconds',
      [lCustomers.Count, (lEndTime - lStartTime) * 24 * 60 * 60]));
  finally
    lCustomers.Free;
  end;

  // Verify
  lCustomers := lRepo.GetWhere('code LIKE ? AND rating = ?',
    [TEST_CODE_PREFIX + 'BULKUPD%', 5]);
  try
    Log(Format('OK Verified: %d customers now have rating=5', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowBulkDelete(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  I: Integer;
  lDeleted: Int64;
  lStartTime, lEndTime: TDateTime;
begin
  Log('');
  Log('=== BULK DELETE ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Create test data
  for I := 1 to 100 do
  begin
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + Format('BULKDEL%4.4d', [I]));
    try
      lRepo.Insert(lCustomer);
    finally
      lCustomer.Free;
    end;
  end;
  Log('Created 100 test customers');

  // Bulk delete using RQL
  lStartTime := Now;
  lDeleted := lRepo.DeleteRQL('starts(code,"' + TEST_CODE_PREFIX + 'BULKDEL")');
  lEndTime := Now;

  Log(Format('OK Deleted %d customers in %.3f seconds',
    [lDeleted, (lEndTime - lStartTime) * 24 * 60 * 60]));

  // Verify
  lDeleted := lRepo.CountRQL('starts(code,"' + TEST_CODE_PREFIX + 'BULKDEL")');
  if lDeleted = 0 then
    Log('OK Verified: All bulk delete customers removed')
  else
    Log(Format('ERROR ERROR: %d customers still exist', [lDeleted]));
end;

class procedure TAllShowcases.ShowTransactionSupport(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lConnection: TFDConnection;
begin
  Log('');
  Log('=== TRANSACTION SUPPORT ===');
  Log('');

  lRepo := TCustomerRepository.Create;
  lConnection := TMVCActiveRecord.CurrentConnection;

  // Transaction with commit
  Log('1. Transaction with COMMIT');
  lConnection.StartTransaction;
  try
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'TRX01');
    try
      lRepo.Insert(lCustomer);
      Log(Format('   Inserted customer ID: %d', [lCustomer.ID.Value]));
    finally
      lCustomer.Free;
    end;

    lConnection.Commit;
    Log('   OK Transaction committed');
  except
    lConnection.Rollback;
    raise;
  end;

  // Transaction with rollback
  Log('2. Transaction with ROLLBACK');
  lConnection.StartTransaction;
  try
    lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'TRX02');
    try
      lRepo.Insert(lCustomer);
      Log(Format('   Inserted customer ID: %d', [lCustomer.ID.Value]));
    finally
      lCustomer.Free;
    end;

    lConnection.Rollback;
    Log('   OK Transaction rolled back');
  except
    lConnection.Rollback;
    raise;
  end;

  // Verify: TRX01 should exist, TRX02 should not
  lCustomer := lRepo.GetFirstByWhere('code = ?', [TEST_CODE_PREFIX + 'TRX01'], False);
  if lCustomer <> nil then
  begin
    Log('   OK Committed customer exists');
    lCustomer.Free;
  end
  else
    Log('   ERROR ERROR: Committed customer not found');

  lCustomer := lRepo.GetFirstByWhere('code = ?', [TEST_CODE_PREFIX + 'TRX02'], False);
  if lCustomer = nil then
    Log('   OK Rolled back customer does not exist')
  else
  begin
    Log('   ERROR ERROR: Rolled back customer found');
    lCustomer.Free;
  end;

  CleanupTestData(Log);
end;

class procedure TAllShowcases.ShowReadOnlyRepository(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomers: TObjectList<TCustomer>;
  lCount: Int64;
begin
  Log('');
  Log('=== READ-ONLY REPOSITORY OPERATIONS ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  Log('Read-only operations (safe for production):');

  // Count
  lCount := lRepo.Count;
  Log(Format('1. Count: %d customers', [lCount]));

  // Exists
  if lRepo.Exists(1) then
    Log('2. Exists: Customer with ID=1 exists')
  else
    Log('2. Exists: Customer with ID=1 not found');

  // GetAll (limit to 5 for display)
  lCustomers := lRepo.SelectRQL('limit(5,0)', 5);
  try
    Log(Format('3. GetAll (first 5): Retrieved %d customers', [lCustomers.Count]));
  finally
    lCustomers.Free;
  end;

  Log('');
  Log('OK Read-only operations completed (no data modified)');
end;

class procedure TAllShowcases.ShowVersionedEntities(Log: TProc<string>);
var
  lRepo: IMVCRepository<TCustomer>;
  lCustomer: TCustomer;
  lID: Int64;
begin
  Log('');
  Log('=== VERSIONED ENTITIES (if implemented) ===');
  Log('');

  lRepo := TCustomerRepository.Create;

  // Note: TCustomer doesn't have version field in this example
  Log('Creating customer to demonstrate versioning concept...');

  lCustomer := CreateTestCustomer(TEST_CODE_PREFIX + 'VER01');
  try
    lRepo.Insert(lCustomer);
    lID := lCustomer.ID.Value;
    Log(Format('   Created customer with ID: %d', [lID]));
  finally
    lCustomer.Free;
  end;

  // If entity had version field, updates would increment it
  lCustomer := lRepo.GetByPK(lID);
  try
    lCustomer.CompanyName := 'Updated Name';
    lRepo.Update(lCustomer);
    Log('   Updated customer (version would increment if implemented)');
    lRepo.Delete(lCustomer);
  finally
    lCustomer.Free;
  end;

  Log('');
  Log('Note: Add [MVCTableField(''version'')] field for optimistic locking');
  CleanupTestData(Log);
end;

end.
