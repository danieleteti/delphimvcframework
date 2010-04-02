unit TestStompClientU;

interface

uses
  TestFramework;

type
  TTestStompClient = class(TTestCase)
  published
    procedure TestAssertTrue;
    procedure TestAssertFalse;
  end;

implementation

{ TTestStompClient }

procedure TTestStompClient.TestAssertFalse;
begin
  CheckTrue(false);
end;

procedure TTestStompClient.TestAssertTrue;
begin
  CheckTrue(False);
end;

initialization

RegisterTest(TTestStompClient.Suite);

end.
