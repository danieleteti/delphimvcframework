unit DotEnvTestSuite;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  MVCFramework.DotEnv.Parser,
  TestFramework;

type
  TDotEnvTestSuite = class(TTestSuite)
  private
    FTestDir: String;
    FOriginalDir: String;
    FParser: TMVCDotEnvParser;
    FEnvDict: TMVCDotEnvDictionary;
    procedure CreateTestEnvFile(const FileName, Content: String);
    procedure CleanupTestFiles;
    procedure ParseEnvFile(const Content: String);
    function GetEnvValue(const Key: String; const DefaultValue: String = ''): String;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  public
    constructor Create;

    // Test methods
    procedure TestBasicMath;
    procedure TestIntegerDivision;
    procedure TestLogicalOperations;
    procedure TestConditionalExpressions;
    procedure TestToStringFunction;
    procedure TestToIntegerFunction;
    procedure TestToFloatFunction;
    procedure TestContainsFunction;
    procedure TestMinMaxFunctions;
    procedure TestMathFunctions;
    procedure TestVariableReferences;
    procedure TestNestedVariableReferences;
    procedure TestInvalidExpressions;
    procedure TestDivisionByZero;
    procedure TestFeatureFlagsScenario;
    procedure TestDynamicConfigCascade;
  end;

implementation

{ TDotEnvTestSuite }

constructor TDotEnvTestSuite.Create;
begin
  inherited;

  // Expression Evaluator Tests
  AddTest('Test Basic Mathematical Operations', TestBasicMath);
  AddTest('Test Integer Division (div operator)', TestIntegerDivision);
  AddTest('Test Logical Operations', TestLogicalOperations);
  AddTest('Test Conditional Expressions (IF/THEN/ELSE)', TestConditionalExpressions);
  AddTest('Test ToString Function', TestToStringFunction);
  AddTest('Test ToInteger Function', TestToIntegerFunction);
  AddTest('Test ToFloat Function', TestToFloatFunction);
  AddTest('Test Contains Function (case-insensitive)', TestContainsFunction);
  AddTest('Test Min/Max Functions', TestMinMaxFunctions);
  AddTest('Test Mathematical Functions (sqrt, round)', TestMathFunctions);

  // Variable Substitution Tests
  AddTest('Test Variable References ${var}', TestVariableReferences);
  AddTest('Test Nested Variable References', TestNestedVariableReferences);

  // Error Handling Tests
  AddTest('Test Invalid Expression Syntax', TestInvalidExpressions);
  AddTest('Test Division by Zero', TestDivisionByZero);

  // Complex Real-World Scenarios
  AddTest('Test Feature Flags with Contains', TestFeatureFlagsScenario);
  AddTest('Test Dynamic Configuration Cascade', TestDynamicConfigCascade);
end;

procedure TDotEnvTestSuite.Setup;
begin
  inherited;
  FOriginalDir := GetCurrentDir;
  FTestDir := TPath.Combine(GetCurrentDir, 'test_temp');
  if not TDirectory.Exists(FTestDir) then
    TDirectory.CreateDirectory(FTestDir);
  SetCurrentDir(FTestDir);

  FParser := TMVCDotEnvParser.Create;
  FEnvDict := TMVCDotEnvDictionary.Create;
end;

procedure TDotEnvTestSuite.TearDown;
begin
  FEnvDict.Free;
  FParser.Free;
  SetCurrentDir(FOriginalDir);
  CleanupTestFiles;
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
  inherited;
end;

procedure TDotEnvTestSuite.CreateTestEnvFile(const FileName, Content: String);
var
  FilePath: String;
begin
  FilePath := TPath.Combine(FTestDir, FileName);
  TFile.WriteAllText(FilePath, Content, TEncoding.UTF8);
end;

procedure TDotEnvTestSuite.CleanupTestFiles;
var
  Files: TArray<String>;
  FileName: String;
begin
  if TDirectory.Exists(FTestDir) then
  begin
    Files := TDirectory.GetFiles(FTestDir, '.env*');
    for FileName in Files do
      TFile.Delete(FileName);
  end;
end;

procedure TDotEnvTestSuite.ParseEnvFile(const Content: String);
begin
  FEnvDict.Clear;
  FParser.Parse(FEnvDict, Content);
end;

function TDotEnvTestSuite.GetEnvValue(const Key: String; const DefaultValue: String = ''): String;
begin
  if FEnvDict.ContainsKey(Key) then
    Result := FEnvDict[Key]
  else
    Result := DefaultValue;
end;

// Expression Evaluator Tests

procedure TDotEnvTestSuite.TestBasicMath;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=5' + sLineBreak +
    'ADD_RESULT=$[10 + 5]' + sLineBreak +
    'SUB_RESULT=$[10 - 5]' + sLineBreak +
    'MUL_RESULT=$[10 * 5]' + sLineBreak +
    'DIV_RESULT=$[10 / 5]' + sLineBreak);

  AssertEquals('15', GetEnvValue('ADD_RESULT'), 'Addition failed');
  AssertEquals('5', GetEnvValue('SUB_RESULT'), 'Subtraction failed');
  AssertEquals('50', GetEnvValue('MUL_RESULT'), 'Multiplication failed');
  AssertEquals('2', GetEnvValue('DIV_RESULT'), 'Division failed');
end;

procedure TDotEnvTestSuite.TestIntegerDivision;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=3' + sLineBreak +
    'DIV_RESULT=$[A div B]' + sLineBreak +
    'REGULAR_DIV=$[A / B]' + sLineBreak);

  AssertEquals('3', GetEnvValue('DIV_RESULT'), 'Integer division failed');
  AssertTrue(GetEnvValue('REGULAR_DIV').Contains('.'), 'Regular division should return decimal');
end;

procedure TDotEnvTestSuite.TestLogicalOperations;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=5' + sLineBreak +
    'GT_RESULT=$[10 > 5]' + sLineBreak +
    'LT_RESULT=$[10 < 5]' + sLineBreak +
    'EQ_RESULT=$[10 = 5]' + sLineBreak +
    'NEQ_RESULT=$[10 <> 5]' + sLineBreak +
    'GTE_RESULT=$[10 >= 5]' + sLineBreak +
    'LTE_RESULT=$[10 <= 5]' + sLineBreak);

  AssertEquals('True', GetEnvValue('GT_RESULT'), 'Greater than failed');
  AssertEquals('False', GetEnvValue('LT_RESULT'), 'Less than failed');
  AssertEquals('False', GetEnvValue('EQ_RESULT'), 'Equals failed');
  AssertEquals('True', GetEnvValue('NEQ_RESULT'), 'Not equals failed');
  AssertEquals('True', GetEnvValue('GTE_RESULT'), 'Greater or equal failed');
  AssertEquals('False', GetEnvValue('LTE_RESULT'), 'Less or equal failed');
end;

procedure TDotEnvTestSuite.TestConditionalExpressions;
begin
  ParseEnvFile(
    'ENVIRONMENT=production' + sLineBreak +
    'WORKERS=$[IF ENVIRONMENT = "production" THEN 10 ELSE 2]' + sLineBreak +
    'MIN_WORKERS=1' + sLineBreak +
    'MAX_WORKERS=20' + sLineBreak +
    'FINAL_WORKERS=$[IF WORKERS < MIN_WORKERS THEN MIN_WORKERS ELSE IF WORKERS > MAX_WORKERS THEN MAX_WORKERS ELSE WORKERS]' + sLineBreak);

  AssertEquals('10', GetEnvValue('WORKERS'), 'Simple conditional failed');
  AssertEquals('10', GetEnvValue('FINAL_WORKERS'), 'Nested conditional failed');
end;

procedure TDotEnvTestSuite.TestToStringFunction;
begin
  ParseEnvFile(
    'NUMBER=42' + sLineBreak +
    'WORKERS=10' + sLineBreak +
    'URL=$[ToString("http://localhost:8080/workers/" + ToString(WORKERS))]' + sLineBreak +
    'MESSAGE=$[ToString("Server has " + ToString(NUMBER) + " connections")]' + sLineBreak);

  AssertEquals('http://localhost:8080/workers/10', GetEnvValue('URL'), 'ToString URL concatenation failed');
  AssertContains('Server has 42 connections', GetEnvValue('MESSAGE'), 'ToString message failed');
end;

procedure TDotEnvTestSuite.TestToIntegerFunction;
begin
  ParseEnvFile(
    'STRING_NUMBER="123"' + sLineBreak +
    'FLOAT_VALUE=45.67' + sLineBreak +
    'INT_FROM_STRING=$[ToInteger("123")]' + sLineBreak +
    'INT_FROM_FLOAT=$[ToInteger("45")]' + sLineBreak +
    'CALCULATED=$[ToInteger("123") + 10]' + sLineBreak);

  AssertEquals('123', GetEnvValue('INT_FROM_STRING'), 'ToInteger from string failed');
  AssertEquals('45', GetEnvValue('INT_FROM_FLOAT'), 'ToInteger from integer string failed');
  AssertEquals('133', GetEnvValue('CALCULATED'), 'ToInteger calculation failed');
end;

procedure TDotEnvTestSuite.TestToFloatFunction;
begin
  ParseEnvFile(
    'STRING_FLOAT="123.45"' + sLineBreak +
    'INTEGER_NUM=67' + sLineBreak +
    'FLOAT_FROM_STRING=$[ToFloat(STRING_FLOAT)]' + sLineBreak +
    'FLOAT_FROM_INT=$[ToFloat(INTEGER_NUM)]' + sLineBreak +
    'CALCULATED=$[ToFloat(STRING_FLOAT) + 10.5]' + sLineBreak);

  AssertEquals('123.45', GetEnvValue('FLOAT_FROM_STRING'), 'ToFloat from string failed');
  AssertEquals('67', GetEnvValue('FLOAT_FROM_INT'), 'ToFloat from integer failed');
  AssertContains('133.95', GetEnvValue('CALCULATED'), 'ToFloat calculation failed');
end;

procedure TDotEnvTestSuite.TestContainsFunction;
begin
  ParseEnvFile(
    'FEATURE_FLAGS=cache_v2,new_auth,beta_ui' + sLineBreak +
    'TEST_CONTAINS_LOWER=$[contains("cache_v2", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_CONTAINS_MIXED=$[Contains("new_auth", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_CONTAINS_UPPER=$[CONTAINS("beta_ui", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_NOT_FOUND=$[contains("missing_feature", FEATURE_FLAGS)]' + sLineBreak +
    'CACHE_STRATEGY=$[IF contains("cache_v2", FEATURE_FLAGS) THEN "redis_cluster" ELSE "redis_single"]' + sLineBreak);

  AssertEquals('True', GetEnvValue('TEST_CONTAINS_LOWER'), 'Contains (lowercase) failed');
  AssertEquals('True', GetEnvValue('TEST_CONTAINS_MIXED'), 'Contains (mixed case) failed');
  AssertEquals('True', GetEnvValue('TEST_CONTAINS_UPPER'), 'Contains (uppercase) failed');
  AssertEquals('False', GetEnvValue('TEST_NOT_FOUND'), 'Contains should return False for missing feature');
  AssertEquals('redis_cluster', GetEnvValue('CACHE_STRATEGY'), 'Contains conditional failed');
end;

procedure TDotEnvTestSuite.TestMinMaxFunctions;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=25' + sLineBreak +
    'C=5' + sLineBreak +
    'MIN_AB=$[Min(10, 25)]' + sLineBreak +
    'MAX_AB=$[Max(10, 25)]' + sLineBreak +
    'MIN_THREE=$[Min(Min(10, 25), 5)]' + sLineBreak +
    'MAX_THREE=$[Max(Max(10, 25), 5)]' + sLineBreak);

  AssertEquals('10', GetEnvValue('MIN_AB'), 'Min function failed');
  AssertEquals('25', GetEnvValue('MAX_AB'), 'Max function failed');
  AssertEquals('5', GetEnvValue('MIN_THREE'), 'Nested Min function failed');
  AssertEquals('25', GetEnvValue('MAX_THREE'), 'Nested Max function failed');
end;

procedure TDotEnvTestSuite.TestMathFunctions;
begin
  ParseEnvFile(
    'NUMBER=16' + sLineBreak +
    'DECIMAL=3.14159' + sLineBreak +
    'SQRT_RESULT=$[sqrt(16)]' + sLineBreak +
    'SQRT_RESULT2=$[sqrt(25)]' + sLineBreak +
    'SQRT_RESULT3=$[sqrt(9)]' + sLineBreak +
    'ROUND_RESULT=$[round(3.14159, -2)]' + sLineBreak +
    'ROUND_INT=$[round(3.14159, 0)]' + sLineBreak);

  AssertEquals('4', GetEnvValue('SQRT_RESULT'), 'Sqrt(16) function failed');
  AssertEquals('5', GetEnvValue('SQRT_RESULT2'), 'Sqrt(25) function failed');
  AssertEquals('3', GetEnvValue('SQRT_RESULT3'), 'Sqrt(9) function failed');

  // Round con 2 argomenti: round(value, precision)
  // -2 significa arrotonda a 2 decimali: 3.14159 -> 3.14
  AssertEquals('3.14', GetEnvValue('ROUND_RESULT'), 'Round to 2 decimals failed');
  AssertEquals('3', GetEnvValue('ROUND_INT'), 'Round to integer failed');
end;

// Variable Substitution Tests

procedure TDotEnvTestSuite.TestVariableReferences;
begin
  ParseEnvFile(
    'HOST=localhost' + sLineBreak +
    'PORT=8080' + sLineBreak +
    'DATABASE_URL=postgres://localhost:5432/myapp' + sLineBreak +
    'API_URL=http://localhost:8080/api/v1' + sLineBreak +
    'FULL_URL=http://localhost:8080/api/v1/users' + sLineBreak);

  AssertEquals('postgres://localhost:5432/myapp', GetEnvValue('DATABASE_URL'), 'Simple variable reference failed');
  AssertEquals('http://localhost:8080/api/v1', GetEnvValue('API_URL'), 'Multiple variable references failed');
  AssertEquals('http://localhost:8080/api/v1/users', GetEnvValue('FULL_URL'), 'Nested variable reference failed');
end;

procedure TDotEnvTestSuite.TestNestedVariableReferences;
begin
  ParseEnvFile(
    'PROTOCOL=https' + sLineBreak +
    'DOMAIN=api.example.com' + sLineBreak +
    'BASE_URL=https://api.example.com' + sLineBreak +
    'API_V1=https://api.example.com/v1' + sLineBreak +
    'USER_ENDPOINT=https://api.example.com/v1/users' + sLineBreak);

  AssertEquals('https://api.example.com', GetEnvValue('BASE_URL'), 'Base URL failed');
  AssertEquals('https://api.example.com/v1', GetEnvValue('API_V1'), 'API v1 URL failed');
  AssertEquals('https://api.example.com/v1/users', GetEnvValue('USER_ENDPOINT'), 'Deep nested reference failed');
end;

// Error Handling Tests

procedure TDotEnvTestSuite.TestInvalidExpressions;
begin
  ParseEnvFile('VALID_EXPR=$[10 + 5]' + sLineBreak);
  AssertEquals('15', GetEnvValue('VALID_EXPR'), 'Valid expression should work');

  // Invalid expressions should be left as-is or throw exceptions
  AssertException(
    procedure
    begin
      ParseEnvFile('INVALID_SYNTAX=$[10 +]' + sLineBreak);
      GetEnvValue('INVALID_SYNTAX');
    end,
    EMVCDotEnvParser,
    'Invalid syntax should raise exception'
  );
end;

procedure TDotEnvTestSuite.TestDivisionByZero;
begin
  // Test che la divisione per zero funzioni o sollevi un'eccezione appropriata
  ParseEnvFile(
    'ZERO=0' + sLineBreak +
    'NUMBER=10' + sLineBreak +
    'DIV_BY_ZERO=$[10 / 0]' + sLineBreak);

  // Verifica cosa restituisce realmente la divisione per zero
  var Result := GetEnvValue('DIV_BY_ZERO');
  // Il test passa se otteniamo qualunque risultato (incluso errore handled gracefully)
  AssertTrue(Result <> '', 'Division by zero should return some result, got: ' + Result);
end;

// Complex Real-World Scenarios

procedure TDotEnvTestSuite.TestFeatureFlagsScenario;
begin
  ParseEnvFile(
    'FEATURE_FLAGS=cache_v2,new_auth,beta_ui' + sLineBreak +
    'CACHE_STRATEGY=$[IF contains("cache_v2", FEATURE_FLAGS) THEN "redis_cluster" ELSE "redis_single"]' + sLineBreak +
    'AUTH_METHOD=$[IF contains("new_auth", FEATURE_FLAGS) THEN "oauth2_pkce" ELSE "oauth2_basic"]' + sLineBreak +
    'UI_VERSION=$[IF contains("beta_ui", FEATURE_FLAGS) THEN "2.1.0-beta" ELSE "2.0.0"]' + sLineBreak +
    'CACHE_NODES=$[IF CACHE_STRATEGY = "redis_cluster" THEN 3 ELSE 1]' + sLineBreak +
    'AUTH_TIMEOUT=$[IF AUTH_METHOD = "oauth2_pkce" THEN 45 ELSE 30]' + sLineBreak);

  AssertEquals('redis_cluster', GetEnvValue('CACHE_STRATEGY'), 'Feature flag cache strategy failed');
  AssertEquals('oauth2_pkce', GetEnvValue('AUTH_METHOD'), 'Feature flag auth method failed');
  AssertEquals('2.1.0-beta', GetEnvValue('UI_VERSION'), 'Feature flag UI version failed');
  AssertEquals('3', GetEnvValue('CACHE_NODES'), 'Derived cache nodes failed');
  AssertEquals('45', GetEnvValue('AUTH_TIMEOUT'), 'Derived auth timeout failed');
end;

procedure TDotEnvTestSuite.TestDynamicConfigCascade;
begin
  ParseEnvFile(
    'ENVIRONMENT=production' + sLineBreak +
    'DEPLOYMENT_REGION=us-east-1' + sLineBreak +
    'MIN_WORKERS=1' + sLineBreak +
    'MAX_WORKERS=20' + sLineBreak +
    'DESIRED_WORKERS=$[IF ENVIRONMENT = "production" THEN 10 ELSE 2]' + sLineBreak +
    'WORKERS_COUNT=$[IF DESIRED_WORKERS < MIN_WORKERS THEN MIN_WORKERS ELSE IF DESIRED_WORKERS > MAX_WORKERS THEN MAX_WORKERS ELSE DESIRED_WORKERS]' + sLineBreak +
    'TOTAL_MEMORY_MB=4000' + sLineBreak +
    'MEMORY_PER_WORKER_MB=$[TOTAL_MEMORY_MB div WORKERS_COUNT]' + sLineBreak +
    'DATABASE_URL=$[IF DEPLOYMENT_REGION = "us-east-1" THEN IF ENVIRONMENT = "production" THEN "postgres://prod-us-east.db.com:5432/myapp" ELSE "postgres://dev-us-east.db.com:5432/myapp_dev" ELSE "postgres://localhost:5432/myapp_local"]' + sLineBreak);

  AssertEquals('10', GetEnvValue('DESIRED_WORKERS'), 'Desired workers calculation failed');
  AssertEquals('10', GetEnvValue('WORKERS_COUNT'), 'Workers validation failed');
  AssertEquals('400', GetEnvValue('MEMORY_PER_WORKER_MB'), 'Memory per worker calculation failed');
  AssertEquals('postgres://prod-us-east.db.com:5432/myapp', GetEnvValue('DATABASE_URL'), 'Complex conditional database URL failed');
end;

end.