unit DotEnvTestU;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  MVCFramework.DotEnv.Parser,
  DUnitX.TestFramework;

type
  [TestFixture]
  TDotEnvTest = class
  private
    FTestDir: String;
    FOriginalDir: String;
    FParser: TMVCDotEnvParser;
    FEnvDict: TMVCDotEnvDictionary;
    procedure CleanupTestFiles;
    procedure ParseEnvFile(const Content: String);
    function GetEnvValue(const Key: String; const DefaultValue: String = ''): String;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Test methods
    [Test]
    procedure TestBasicMath;
    [Test]
    procedure TestIntegerDivision;
    [Test]
    procedure TestLogicalOperations;
    [Test]
    procedure TestConditionalExpressions;
    [Test]
    procedure TestToStringFunction;
    [Test]
    procedure TestToIntegerFunction;
    [Test]
    procedure TestToFloatFunction;
    [Test]
    procedure TestContainsFunction;
    [Test]
    procedure TestMinMaxFunctions;
    [Test]
    procedure TestMathFunctions;
    [Test]
    procedure TestVariableReferences;
    [Test]
    procedure TestNestedVariableReferences;
    [Test]
    procedure TestInvalidExpressions;
    [Test]
    procedure TestDivisionByZero;
    [Test]
    procedure TestFeatureFlagsScenario;
    [Test]
    procedure TestDynamicConfigCascade;
  end;

implementation

{ TDotEnvTest }

procedure TDotEnvTest.Setup;
begin
  FOriginalDir := GetCurrentDir;
  FTestDir := TPath.Combine(GetCurrentDir, 'test_temp');
  if not TDirectory.Exists(FTestDir) then
    TDirectory.CreateDirectory(FTestDir);
  SetCurrentDir(FTestDir);

  FParser := TMVCDotEnvParser.Create;
  FEnvDict := TMVCDotEnvDictionary.Create;
end;

procedure TDotEnvTest.TearDown;
begin
  FEnvDict.Free;
  FParser.Free;
  SetCurrentDir(FOriginalDir);
  CleanupTestFiles;
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
end;

procedure TDotEnvTest.CleanupTestFiles;
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

procedure TDotEnvTest.ParseEnvFile(const Content: String);
begin
  FEnvDict.Clear;
  FParser.Parse(FEnvDict, Content);
end;

function TDotEnvTest.GetEnvValue(const Key: String; const DefaultValue: String = ''): String;
begin
  if FEnvDict.ContainsKey(Key) then
    Result := FEnvDict[Key]
  else
    Result := DefaultValue;
end;

// Expression Evaluator Tests

procedure TDotEnvTest.TestBasicMath;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=5' + sLineBreak +
    'ADD_RESULT=$[10 + 5]' + sLineBreak +
    'SUB_RESULT=$[10 - 5]' + sLineBreak +
    'MUL_RESULT=$[10 * 5]' + sLineBreak +
    'DIV_RESULT=$[10 / 5]' + sLineBreak);

  Assert.AreEqual('15', GetEnvValue('ADD_RESULT'), 'Addition failed');
  Assert.AreEqual('5', GetEnvValue('SUB_RESULT'), 'Subtraction failed');
  Assert.AreEqual('50', GetEnvValue('MUL_RESULT'), 'Multiplication failed');
  Assert.AreEqual('2', GetEnvValue('DIV_RESULT'), 'Division failed');
end;

procedure TDotEnvTest.TestIntegerDivision;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=3' + sLineBreak +
    'DIV_RESULT=$[A div B]' + sLineBreak +
    'REGULAR_DIV=$[A / B]' + sLineBreak);

  Assert.AreEqual('3', GetEnvValue('DIV_RESULT'), 'Integer division failed');
  Assert.IsTrue(GetEnvValue('REGULAR_DIV').Contains('.'), 'Regular division should return decimal');
end;

procedure TDotEnvTest.TestLogicalOperations;
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

  Assert.AreEqual('True', GetEnvValue('GT_RESULT'), 'Greater than failed');
  Assert.AreEqual('False', GetEnvValue('LT_RESULT'), 'Less than failed');
  Assert.AreEqual('False', GetEnvValue('EQ_RESULT'), 'Equals failed');
  Assert.AreEqual('True', GetEnvValue('NEQ_RESULT'), 'Not equals failed');
  Assert.AreEqual('True', GetEnvValue('GTE_RESULT'), 'Greater or equal failed');
  Assert.AreEqual('False', GetEnvValue('LTE_RESULT'), 'Less or equal failed');
end;

procedure TDotEnvTest.TestConditionalExpressions;
begin
  ParseEnvFile(
    'ENVIRONMENT=production' + sLineBreak +
    'WORKERS=$[IF ENVIRONMENT = "production" THEN 10 ELSE 2]' + sLineBreak +
    'MIN_WORKERS=1' + sLineBreak +
    'MAX_WORKERS=20' + sLineBreak +
    'FINAL_WORKERS=$[IF WORKERS < MIN_WORKERS THEN MIN_WORKERS ELSE IF WORKERS > MAX_WORKERS THEN MAX_WORKERS ELSE WORKERS]' + sLineBreak);

  Assert.AreEqual('10', GetEnvValue('WORKERS'), 'Simple conditional failed');
  Assert.AreEqual('10', GetEnvValue('FINAL_WORKERS'), 'Nested conditional failed');
end;

procedure TDotEnvTest.TestToStringFunction;
begin
  ParseEnvFile(
    'NUMBER=42' + sLineBreak +
    'WORKERS=10' + sLineBreak +
    'URL=$[ToString("http://localhost:8080/workers/" + ToString(WORKERS))]' + sLineBreak +
    'MESSAGE=$[ToString("Server has " + ToString(NUMBER) + " connections")]' + sLineBreak);

  Assert.AreEqual('http://localhost:8080/workers/10', GetEnvValue('URL'), 'ToString URL concatenation failed');
  Assert.Contains(GetEnvValue('MESSAGE'), 'Server has 42 connections', 'ToString message failed');
end;

procedure TDotEnvTest.TestToIntegerFunction;
begin
  ParseEnvFile(
    'STRING_NUMBER="123"' + sLineBreak +
    'FLOAT_VALUE=45.67' + sLineBreak +
    'INT_FROM_STRING=$[ToInteger("123")]' + sLineBreak +
    'INT_FROM_FLOAT=$[ToInteger("45")]' + sLineBreak +
    'CALCULATED=$[ToInteger("123") + 10]' + sLineBreak);

  Assert.AreEqual('123', GetEnvValue('INT_FROM_STRING'), 'ToInteger from string failed');
  Assert.AreEqual('45', GetEnvValue('INT_FROM_FLOAT'), 'ToInteger from integer string failed');
  Assert.AreEqual('133', GetEnvValue('CALCULATED'), 'ToInteger calculation failed');
end;

procedure TDotEnvTest.TestToFloatFunction;
begin
  ParseEnvFile(
    'STRING_FLOAT="123.45"' + sLineBreak +
    'INTEGER_NUM=67' + sLineBreak +
    'FLOAT_FROM_STRING=$[ToFloat(STRING_FLOAT)]' + sLineBreak +
    'FLOAT_FROM_INT=$[ToFloat(INTEGER_NUM)]' + sLineBreak +
    'CALCULATED=$[ToFloat(STRING_FLOAT) + 10.5]' + sLineBreak);

  Assert.AreEqual('123.45', GetEnvValue('FLOAT_FROM_STRING'), 'ToFloat from string failed');
  Assert.AreEqual('67', GetEnvValue('FLOAT_FROM_INT'), 'ToFloat from integer failed');
  Assert.Contains(GetEnvValue('CALCULATED'), '133.95', 'ToFloat calculation failed');
end;

procedure TDotEnvTest.TestContainsFunction;
begin
  ParseEnvFile(
    'FEATURE_FLAGS=cache_v2,new_auth,beta_ui' + sLineBreak +
    'TEST_CONTAINS_LOWER=$[contains("cache_v2", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_CONTAINS_MIXED=$[Contains("new_auth", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_CONTAINS_UPPER=$[CONTAINS("beta_ui", FEATURE_FLAGS)]' + sLineBreak +
    'TEST_NOT_FOUND=$[contains("missing_feature", FEATURE_FLAGS)]' + sLineBreak +
    'CACHE_STRATEGY=$[IF contains("cache_v2", FEATURE_FLAGS) THEN "redis_cluster" ELSE "redis_single"]' + sLineBreak);

  Assert.AreEqual('True', GetEnvValue('TEST_CONTAINS_LOWER'), 'Contains (lowercase) failed');
  Assert.AreEqual('True', GetEnvValue('TEST_CONTAINS_MIXED'), 'Contains (mixed case) failed');
  Assert.AreEqual('True', GetEnvValue('TEST_CONTAINS_UPPER'), 'Contains (uppercase) failed');
  Assert.AreEqual('False', GetEnvValue('TEST_NOT_FOUND'), 'Contains should return False for missing feature');
  Assert.AreEqual('redis_cluster', GetEnvValue('CACHE_STRATEGY'), 'Contains conditional failed');
end;

procedure TDotEnvTest.TestMinMaxFunctions;
begin
  ParseEnvFile(
    'A=10' + sLineBreak +
    'B=25' + sLineBreak +
    'C=5' + sLineBreak +
    'MIN_AB=$[Min(10, 25)]' + sLineBreak +
    'MAX_AB=$[Max(10, 25)]' + sLineBreak +
    'MIN_THREE=$[Min(Min(10, 25), 5)]' + sLineBreak +
    'MAX_THREE=$[Max(Max(10, 25), 5)]' + sLineBreak);

  Assert.AreEqual('10', GetEnvValue('MIN_AB'), 'Min function failed');
  Assert.AreEqual('25', GetEnvValue('MAX_AB'), 'Max function failed');
  Assert.AreEqual('5', GetEnvValue('MIN_THREE'), 'Nested Min function failed');
  Assert.AreEqual('25', GetEnvValue('MAX_THREE'), 'Nested Max function failed');
end;

procedure TDotEnvTest.TestMathFunctions;
begin
  ParseEnvFile(
    'NUMBER=16' + sLineBreak +
    'DECIMAL=3.14159' + sLineBreak +
    'SQRT_RESULT=$[sqrt(16)]' + sLineBreak +
    'SQRT_RESULT2=$[sqrt(25)]' + sLineBreak +
    'SQRT_RESULT3=$[sqrt(9)]' + sLineBreak +
    'ROUND_RESULT=$[round(3.14159, -2)]' + sLineBreak +
    'ROUND_INT=$[round(3.14159, 0)]' + sLineBreak);

  Assert.AreEqual('4', GetEnvValue('SQRT_RESULT'), 'Sqrt(16) function failed');
  Assert.AreEqual('5', GetEnvValue('SQRT_RESULT2'), 'Sqrt(25) function failed');
  Assert.AreEqual('3', GetEnvValue('SQRT_RESULT3'), 'Sqrt(9) function failed');

  // Round con 2 argomenti: round(value, precision)
  // -2 significa arrotonda a 2 decimali: 3.14159 -> 3.14
  Assert.AreEqual('3.14', GetEnvValue('ROUND_RESULT'), 'Round to 2 decimals failed');
  Assert.AreEqual('3', GetEnvValue('ROUND_INT'), 'Round to integer failed');
end;

// Variable Substitution Tests

procedure TDotEnvTest.TestVariableReferences;
begin
  ParseEnvFile(
    'HOST=localhost' + sLineBreak +
    'PORT=8080' + sLineBreak +
    'DATABASE_URL=postgres://localhost:5432/myapp' + sLineBreak +
    'API_URL=http://localhost:8080/api/v1' + sLineBreak +
    'FULL_URL=http://localhost:8080/api/v1/users' + sLineBreak);

  Assert.AreEqual('postgres://localhost:5432/myapp', GetEnvValue('DATABASE_URL'), 'Simple variable reference failed');
  Assert.AreEqual('http://localhost:8080/api/v1', GetEnvValue('API_URL'), 'Multiple variable references failed');
  Assert.AreEqual('http://localhost:8080/api/v1/users', GetEnvValue('FULL_URL'), 'Nested variable reference failed');
end;

procedure TDotEnvTest.TestNestedVariableReferences;
begin
  ParseEnvFile(
    'PROTOCOL=https' + sLineBreak +
    'DOMAIN=api.example.com' + sLineBreak +
    'BASE_URL=https://api.example.com' + sLineBreak +
    'API_V1=https://api.example.com/v1' + sLineBreak +
    'USER_ENDPOINT=https://api.example.com/v1/users' + sLineBreak);

  Assert.AreEqual('https://api.example.com', GetEnvValue('BASE_URL'), 'Base URL failed');
  Assert.AreEqual('https://api.example.com/v1', GetEnvValue('API_V1'), 'API v1 URL failed');
  Assert.AreEqual('https://api.example.com/v1/users', GetEnvValue('USER_ENDPOINT'), 'Deep nested reference failed');
end;

// Error Handling Tests

procedure TDotEnvTest.TestInvalidExpressions;
begin
  ParseEnvFile('VALID_EXPR=$[10 + 5]' + sLineBreak);
  Assert.AreEqual('15', GetEnvValue('VALID_EXPR'), 'Valid expression should work');

  // Invalid expressions should be left as-is or throw exceptions
  Assert.WillRaise(
    procedure
    begin
      ParseEnvFile('INVALID_SYNTAX=$[10 +]' + sLineBreak);
      GetEnvValue('INVALID_SYNTAX');
    end,
    EMVCDotEnvParser,
    'Invalid syntax should raise exception'
  );
end;

procedure TDotEnvTest.TestDivisionByZero;
begin
  // Test che la divisione per zero sollevi un'eccezione appropriata
  Assert.WillRaise(
    procedure
    begin
      ParseEnvFile(
        'ZERO=0' + sLineBreak +
        'NUMBER=10' + sLineBreak +
        'DIV_BY_ZERO=$[10 / 0]' + sLineBreak);
    end,
    EMVCDotEnvParser,
    'Division by zero should raise exception'
  );
end;

// Complex Real-World Scenarios

procedure TDotEnvTest.TestFeatureFlagsScenario;
begin
  ParseEnvFile(
    'FEATURE_FLAGS=cache_v2,new_auth,beta_ui' + sLineBreak +
    'CACHE_STRATEGY=$[IF contains("cache_v2", FEATURE_FLAGS) THEN "redis_cluster" ELSE "redis_single"]' + sLineBreak +
    'AUTH_METHOD=$[IF contains("new_auth", FEATURE_FLAGS) THEN "oauth2_pkce" ELSE "oauth2_basic"]' + sLineBreak +
    'UI_VERSION=$[IF contains("beta_ui", FEATURE_FLAGS) THEN "2.1.0-beta" ELSE "2.0.0"]' + sLineBreak +
    'CACHE_NODES=$[IF CACHE_STRATEGY = "redis_cluster" THEN 3 ELSE 1]' + sLineBreak +
    'AUTH_TIMEOUT=$[IF AUTH_METHOD = "oauth2_pkce" THEN 45 ELSE 30]' + sLineBreak);

  Assert.AreEqual('redis_cluster', GetEnvValue('CACHE_STRATEGY'), 'Feature flag cache strategy failed');
  Assert.AreEqual('oauth2_pkce', GetEnvValue('AUTH_METHOD'), 'Feature flag auth method failed');
  Assert.AreEqual('2.1.0-beta', GetEnvValue('UI_VERSION'), 'Feature flag UI version failed');
  Assert.AreEqual('3', GetEnvValue('CACHE_NODES'), 'Derived cache nodes failed');
  Assert.AreEqual('45', GetEnvValue('AUTH_TIMEOUT'), 'Derived auth timeout failed');
end;

procedure TDotEnvTest.TestDynamicConfigCascade;
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

  Assert.AreEqual('10', GetEnvValue('DESIRED_WORKERS'), 'Desired workers calculation failed');
  Assert.AreEqual('10', GetEnvValue('WORKERS_COUNT'), 'Workers validation failed');
  Assert.AreEqual('400', GetEnvValue('MEMORY_PER_WORKER_MB'), 'Memory per worker calculation failed');
  Assert.AreEqual('postgres://prod-us-east.db.com:5432/myapp', GetEnvValue('DATABASE_URL'), 'Complex conditional database URL failed');
end;

initialization
  TDUnitX.RegisterTestFixture(TDotEnvTest);

end.