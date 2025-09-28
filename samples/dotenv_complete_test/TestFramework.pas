unit TestFramework;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils;

type
  TTestResults = record
    TotalTests: Integer;
    PassedTests: Integer;
    FailedTests: Integer;
    FailedTestNames: TArray<String>;
    function SuccessRate: Double;
  end;

  TTestMethod = reference to procedure;

  TTestCase = class
  private
    FName: String;
    FTestMethod: TTestMethod;
    FSetupMethod: TTestMethod;
    FTearDownMethod: TTestMethod;
  public
    constructor Create(const Name: String; TestMethod: TTestMethod);
    property Name: String read FName;
    property TestMethod: TTestMethod read FTestMethod;
    property SetupMethod: TTestMethod read FSetupMethod write FSetupMethod;
    property TearDownMethod: TTestMethod read FTearDownMethod write FTearDownMethod;
  end;

  TTestSuite = class
  private
    FTestCases: TObjectList<TTestCase>;
    FResults: TTestResults;
  protected
    procedure AddTest(const Name: String; TestMethod: TTestMethod);
    procedure Assert(Condition: Boolean; const Message: String = '');
    procedure AssertEquals(const Expected, Actual: String; const Message: String = ''); overload;
    procedure AssertEquals(Expected, Actual: Integer; const Message: String = ''); overload;
    procedure AssertEquals(Expected, Actual: Double; const Message: String = ''; Delta: Double = 0.001); overload;
    procedure AssertEquals(Expected, Actual: Boolean; const Message: String = ''); overload;
    procedure AssertNotEmpty(const Value: String; const Message: String = '');
    procedure AssertContains(const Substring, Text: String; const Message: String = '');
    procedure AssertTrue(Condition: Boolean; const Message: String = '');
    procedure AssertFalse(Condition: Boolean; const Message: String = '');
    procedure AssertException(TestMethod: TTestMethod; ExceptionClass: ExceptClass; const Message: String = '');
    procedure Setup; virtual;
    procedure TearDown; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function RunAllTests: TTestResults;
    procedure WriteTestResult(const TestName: String; Passed: Boolean; const ErrorMessage: String = '');
  end;

implementation

{ TTestResults }

function TTestResults.SuccessRate: Double;
begin
  if TotalTests = 0 then
    Result := 0
  else
    Result := (PassedTests / TotalTests) * 100;
end;

{ TTestCase }

constructor TTestCase.Create(const Name: String; TestMethod: TTestMethod);
begin
  inherited Create;
  FName := Name;
  FTestMethod := TestMethod;
end;

{ TTestSuite }

constructor TTestSuite.Create;
begin
  inherited;
  FTestCases := TObjectList<TTestCase>.Create(True);
  FResults.TotalTests := 0;
  FResults.PassedTests := 0;
  FResults.FailedTests := 0;
  SetLength(FResults.FailedTestNames, 0);
end;

destructor TTestSuite.Destroy;
begin
  FTestCases.Free;
  inherited;
end;

procedure TTestSuite.AddTest(const Name: String; TestMethod: TTestMethod);
begin
  FTestCases.Add(TTestCase.Create(Name, TestMethod));
end;

procedure TTestSuite.Assert(Condition: Boolean; const Message: String);
begin
  if not Condition then
    raise Exception.Create('Assertion failed: ' + Message);
end;

procedure TTestSuite.AssertEquals(const Expected, Actual: String; const Message: String);
begin
  Assert(Expected = Actual,
    Format('Expected: "%s", Actual: "%s"%s', [Expected, Actual,
      IfThen(Message <> '', ' - ' + Message, '')]));
end;

procedure TTestSuite.AssertEquals(Expected, Actual: Integer; const Message: String);
begin
  Assert(Expected = Actual,
    Format('Expected: %d, Actual: %d%s', [Expected, Actual,
      IfThen(Message <> '', ' - ' + Message, '')]));
end;

procedure TTestSuite.AssertEquals(Expected, Actual: Double; const Message: String; Delta: Double);
begin
  Assert(Abs(Expected - Actual) <= Delta,
    Format('Expected: %.3f, Actual: %.3f%s', [Expected, Actual,
      IfThen(Message <> '', ' - ' + Message, '')]));
end;

procedure TTestSuite.AssertEquals(Expected, Actual: Boolean; const Message: String);
begin
  Assert(Expected = Actual,
    Format('Expected: %s, Actual: %s%s', [BoolToStr(Expected, True), BoolToStr(Actual, True),
      IfThen(Message <> '', ' - ' + Message, '')]));
end;

procedure TTestSuite.AssertNotEmpty(const Value: String; const Message: String);
begin
  Assert(Value <> '', 'Value should not be empty' + IfThen(Message <> '', ' - ' + Message, ''));
end;

procedure TTestSuite.AssertContains(const Substring, Text: String; const Message: String);
begin
  Assert(Text.Contains(Substring),
    Format('Text "%s" should contain "%s"%s', [Text, Substring,
      IfThen(Message <> '', ' - ' + Message, '')]));
end;

procedure TTestSuite.AssertTrue(Condition: Boolean; const Message: String);
begin
  Assert(Condition, 'Expected True' + IfThen(Message <> '', ' - ' + Message, ''));
end;

procedure TTestSuite.AssertFalse(Condition: Boolean; const Message: String);
begin
  Assert(not Condition, 'Expected False' + IfThen(Message <> '', ' - ' + Message, ''));
end;

procedure TTestSuite.AssertException(TestMethod: TTestMethod; ExceptionClass: ExceptClass; const Message: String);
var
  ExceptionRaised: Boolean;
begin
  ExceptionRaised := False;
  try
    TestMethod();
  except
    on E: Exception do
    begin
      ExceptionRaised := True;
      Assert(E.ClassType = ExceptionClass,
        Format('Expected exception %s but got %s%s', [ExceptionClass.ClassName, E.ClassName,
          IfThen(Message <> '', ' - ' + Message, '')]));
    end;
  end;
  Assert(ExceptionRaised, 'Expected exception ' + ExceptionClass.ClassName + ' was not raised' +
    IfThen(Message <> '', ' - ' + Message, ''));
end;

procedure TTestSuite.Setup;
begin
  // Override in descendants
end;

procedure TTestSuite.TearDown;
begin
  // Override in descendants
end;

function TTestSuite.RunAllTests: TTestResults;
var
  TestCase: TTestCase;
  ErrorMessage: String;
  Passed: Boolean;
  FailedTests: TList<String>;
begin
  FailedTests := TList<String>.Create;
  try
    FResults.TotalTests := FTestCases.Count;
    FResults.PassedTests := 0;
    FResults.FailedTests := 0;

    for TestCase in FTestCases do
    begin
      ErrorMessage := '';
      Passed := True;

      try
        Setup;
        if Assigned(TestCase.SetupMethod) then
          TestCase.SetupMethod();

        TestCase.TestMethod();

      except
        on E: Exception do
        begin
          Passed := False;
          ErrorMessage := E.Message;
          FailedTests.Add(TestCase.Name);
        end;
      end;

      try
        if Assigned(TestCase.TearDownMethod) then
          TestCase.TearDownMethod();
        TearDown;
      except
        on E: Exception do
        begin
          if Passed then
          begin
            Passed := False;
            ErrorMessage := 'TearDown failed: ' + E.Message;
            FailedTests.Add(TestCase.Name);
          end;
        end;
      end;

      WriteTestResult(TestCase.Name, Passed, ErrorMessage);

      if Passed then
        Inc(FResults.PassedTests)
      else
        Inc(FResults.FailedTests);
    end;

    FResults.FailedTestNames := FailedTests.ToArray;
    Result := FResults;

  finally
    FailedTests.Free;
  end;
end;

procedure TTestSuite.WriteTestResult(const TestName: String; Passed: Boolean; const ErrorMessage: String);
begin
  if Passed then
    WriteLn(Format('✓ %s - PASSED', [TestName]))
  else
    WriteLn(Format('✗ %s - FAILED: %s', [TestName, ErrorMessage]));
end;

end.