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
  CheckTrue(true);
end;

procedure TTestStompClient.TestAssertTrue;
begin
  CheckTrue(true);
end;

initialization

RegisterTest(TTestStompClient.Suite);

end.
