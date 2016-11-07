unit TestCommonsU;

interface

uses
  TestFramework;

type
  TTestCommons = class(TTestCase)
  private
    FirstRec: Integer;
    LastRec: Integer;
    ExpectedFirstRec: Integer;
    ExpectedLastRec: Integer;
    Page: Integer;

  published
    procedure TestGetLimitByPagePage0;
    procedure TestGetLimitByPagePage1;
    procedure TestGetLimitByPagePage2;
    procedure TestGetLimitByPagePage3;
  end;

implementation

{ TTestCommons }

uses CommonsU;

procedure TTestCommons.TestGetLimitByPagePage0;
begin
  Page := 0;
  ExpectedException := EWrongPage;
  GetLimitByPage(Page, FirstRec, LastRec);
end;

procedure TTestCommons.TestGetLimitByPagePage1;
begin
  Page := 1;
  ExpectedFirstRec := 1;
  ExpectedLastRec := 10;
  GetLimitByPage(Page, FirstRec, LastRec);
  CheckEquals(ExpectedFirstRec, FirstRec, 'FirstRec');
  CheckEquals(ExpectedLastRec, LastRec, 'LastRec');
end;

procedure TTestCommons.TestGetLimitByPagePage2;
begin
  Page := 2;
  ExpectedFirstRec := 11;
  ExpectedLastRec := 20;
  GetLimitByPage(Page, FirstRec, LastRec);
  CheckEquals(ExpectedFirstRec, FirstRec, 'FirstRec');
  CheckEquals(ExpectedLastRec, LastRec, 'LastRec');
end;

procedure TTestCommons.TestGetLimitByPagePage3;
begin
  Page := 3;
  ExpectedFirstRec := 21;
  ExpectedLastRec := 30;
  GetLimitByPage(Page, FirstRec, LastRec);
  CheckEquals(ExpectedFirstRec, FirstRec, 'FirstRec');
  CheckEquals(ExpectedLastRec, LastRec, 'LastRec');
end;

initialization

// Register any test cases with the test runner
RegisterTest(TTestCommons.Suite);

end.
