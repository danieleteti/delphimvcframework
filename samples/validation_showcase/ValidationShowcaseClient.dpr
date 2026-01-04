program ValidationShowcaseClient;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Commons,
  MVCFramework.Console,
  JsonDataObjects;

const
  BASE_URL = 'http://localhost:8080';

var
  GClient: IMVCRESTClient;
  GTestNumber: Integer = 0;
  GPassedCount: Integer = 0;
  GFailedCount: Integer = 0;

procedure WaitForKey(const AMessage: string = 'Press any key to continue...');
begin
  WriteLn;
  TextColor(DarkGray);
  Write(AMessage);
  ResetConsole;
  GetCh;
end;

procedure ShowTestScreen(const ATestName: string; const ADescription: string;
  const AExpectedOutcome: string; AExpectSuccess: Boolean);
begin
  Inc(GTestNumber);
  ClrScr;

  // Header
  WriteHeader(Format('TEST #%d', [GTestNumber]), 78);
  WriteLn;

  // Test info box
  TextColor(White);
  WriteLn('  TEST: ' + ATestName);
  WriteLn;

  TextColor(Cyan);
  WriteLn('  ' + ADescription);
  WriteLn;

  TextColor(Yellow);
  Write('  EXPECTED: ');
  if AExpectSuccess then
    TextColor(Green)
  else
    TextColor(Red);
  WriteLn(AExpectedOutcome);

  ResetConsole;
  WriteSeparator(78);
  WriteLn;
end;

procedure ShowRequestBody(const AJson: TJsonObject);
begin
  TextColor(DarkGray);
  WriteLn('REQUEST BODY:');
  TextColor(Gray);
  WriteLn(AJson.ToJSON(False));
  ResetConsole;
  WriteLn;
end;

procedure ShowResponse(const AResponse: IMVCRESTResponse; AExpectSuccess: Boolean);
var
  LJson: TJsonObject;
  LActualSuccess: Boolean;
begin
  WriteSeparator(78, '-');
  WriteLn;

  LActualSuccess := AResponse.StatusCode in [200, 201];

  // HTTP Status
  TextColor(White);
  Write('HTTP STATUS: ');
  if LActualSuccess then
    TextColor(Green)
  else
    TextColor(Red);
  WriteLn(Format('%d %s', [AResponse.StatusCode, AResponse.StatusText]));
  WriteLn;

  // Result banner
  if LActualSuccess then
  begin
    TextBackground(DarkGreen);
    TextColor(White);
    WriteLn('                                                                              ');
    WriteLn('    VALIDATION PASSED - Action was EXECUTED                                   ');
    WriteLn('                                                                              ');
    ResetConsole;
  end
  else
  begin
    TextBackground(DarkRed);
    TextColor(White);
    WriteLn('                                                                              ');
    WriteLn('    VALIDATION FAILED - Action was NOT executed (HTTP 422)                    ');
    WriteLn('                                                                              ');
    ResetConsole;
  end;

  WriteLn;

  // Test result
  if LActualSuccess = AExpectSuccess then
  begin
    Inc(GPassedCount);
    TextColor(Green);
    WriteLn('  [OK] Test result matches expected outcome');
  end
  else
  begin
    Inc(GFailedCount);
    TextColor(Red);
    WriteLn('  [FAIL] Test result does NOT match expected outcome!');
  end;
  ResetConsole;

  WriteLn;

  // Response body
  TextColor(DarkGray);
  WriteLn('RESPONSE BODY:');
  TextColor(Gray);

  if AResponse.Content.StartsWith('{') or AResponse.Content.StartsWith('[') then
  begin
    try
      LJson := TJsonObject.Parse(AResponse.Content) as TJsonObject;
      try
        WriteLn(LJson.ToJSON(False));
      finally
        LJson.Free;
      end;
    except
      WriteLn(AResponse.Content);
    end;
  end
  else
    WriteLn(AResponse.Content);

  ResetConsole;
end;

// =============================================================================
// USER REGISTRATION TESTS
// =============================================================================

procedure TestUserRegistration_Valid;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - VALID DATA',
    'All fields valid including password confirmation (cross-field)',
    'HTTP 201 - Action executed', True);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := 'johndoe123';
    LBody.S['email'] := 'john.doe@example.com';
    LBody.S['password'] := 'SecurePass123!';
    LBody.S['confirmPassword'] := 'SecurePass123!';
    LBody.I['age'] := 25;
    LBody.S['website'] := 'https://johndoe.com';

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, True);
end;

procedure TestUserRegistration_PasswordMismatch;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - PASSWORD MISMATCH',
    'confirmPassword differs from password (MVCCompareField cross-field validator)',
    'HTTP 422 - Validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := 'validuser';
    LBody.S['email'] := 'test@example.com';
    LBody.S['password'] := 'SecurePass123!';
    LBody.S['confirmPassword'] := 'DifferentPassword!';  // MISMATCH!
    LBody.I['age'] := 25;

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestUserRegistration_EmptyUsername;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - EMPTY USERNAME',
    'Username is empty (MVCRequired validator)',
    'HTTP 422 - Validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := '';  // EMPTY!
    LBody.S['email'] := 'test@example.com';
    LBody.S['password'] := 'SecurePass123!';
    LBody.S['confirmPassword'] := 'SecurePass123!';
    LBody.I['age'] := 25;

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestUserRegistration_InvalidEmail;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - INVALID EMAIL',
    'Email format is invalid (MVCEmail validator)',
    'HTTP 422 - Validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := 'validuser';
    LBody.S['email'] := 'not-an-email';  // INVALID!
    LBody.S['password'] := 'SecurePass123!';
    LBody.S['confirmPassword'] := 'SecurePass123!';
    LBody.I['age'] := 25;

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestUserRegistration_AgeTooYoung;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - AGE TOO YOUNG',
    'Age is 15, minimum is 18 (MVCRange validator)',
    'HTTP 422 - Validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := 'younguser';
    LBody.S['email'] := 'young@example.com';
    LBody.S['password'] := 'SecurePass123!';
    LBody.S['confirmPassword'] := 'SecurePass123!';
    LBody.I['age'] := 15;  // TOO YOUNG!

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestUserRegistration_MultipleErrors;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'User Registration - MULTIPLE ERRORS',
    'Many fields invalid: username too short, bad email, password mismatch, etc.',
    'HTTP 422 - Multiple validation errors', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['username'] := 'ab';
    LBody.S['email'] := 'bad-email';
    LBody.S['password'] := 'short';
    LBody.S['confirmPassword'] := 'wrong';
    LBody.I['age'] := 10;
    LBody.S['website'] := 'not-a-url';

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/users/register');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

// =============================================================================
// CUSTOMER TESTS (NESTED VALIDATION)
// =============================================================================

procedure TestCustomer_Valid;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Customer - VALID DATA (Nested Object)',
    'Customer with valid BillingAddress nested object',
    'HTTP 201 - Action executed', True);

  LBody := TJsonObject.Create;
  try
    LBody.S['name'] := 'Mario Rossi';
    LBody.S['email'] := 'mario.rossi@example.it';
    LBody.S['taxId'] := 'RSSMRA85M01H501Z';
    LBody.O['billingAddress'].S['street'] := 'Via Roma 123';
    LBody.O['billingAddress'].S['city'] := 'Roma';
    LBody.O['billingAddress'].S['postalCode'] := '00100';
    LBody.O['billingAddress'].S['country'] := 'IT';

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/customers');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, True);
end;

procedure TestCustomer_InvalidNestedAddress;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Customer - INVALID NESTED ADDRESS',
    'BillingAddress has invalid fields (street too short, city empty, bad postal code)',
    'HTTP 422 - Errors with "BillingAddress.xxx" paths', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['name'] := 'Luigi Verdi';
    LBody.S['email'] := 'luigi@example.it';
    LBody.O['billingAddress'].S['street'] := 'Via';  // TOO SHORT!
    LBody.O['billingAddress'].S['city'] := '';       // EMPTY!
    LBody.O['billingAddress'].S['postalCode'] := '123';  // BAD FORMAT!
    LBody.O['billingAddress'].S['country'] := 'ITALY';   // TOO LONG!

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/customers');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

// =============================================================================
// ORDER TESTS (COLLECTION VALIDATION)
// =============================================================================

procedure TestOrder_Valid;
var
  LBody: TJsonObject;
  LItem: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Order - VALID DATA (Collection)',
    'Order with valid Items collection (2 items)',
    'HTTP 201 - Action executed', True);

  LBody := TJsonObject.Create;
  try
    LBody.S['orderNumber'] := 'ORD-12345678';
    LBody.S['customerEmail'] := 'customer@example.com';

    LItem := LBody.A['items'].AddObject;
    LItem.S['productCode'] := 'ABC-1234';
    LItem.S['productName'] := 'Widget Pro';
    LItem.I['quantity'] := 5;
    LItem.F['unitPrice'] := 29.99;

    LItem := LBody.A['items'].AddObject;
    LItem.S['productCode'] := 'XYZ-5678';
    LItem.S['productName'] := 'Gadget Plus';
    LItem.I['quantity'] := 2;
    LItem.F['unitPrice'] := 49.99;

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/orders');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, True);
end;

procedure TestOrder_EmptyItems;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Order - EMPTY ITEMS COLLECTION',
    'Items array is empty (MVCMinCount validator)',
    'HTTP 422 - "Items" min count error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['orderNumber'] := 'ORD-12345678';
    LBody.S['customerEmail'] := 'customer@example.com';
    LBody.A['items'];  // Empty array

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/orders');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestOrder_InvalidItemInCollection;
var
  LBody: TJsonObject;
  LItem: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Order - INVALID ITEM IN COLLECTION',
    'Second item (index 1) has invalid fields: empty name, negative quantity',
    'HTTP 422 - Errors with "Items[1].xxx" paths', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['orderNumber'] := 'ORD-12345678';
    LBody.S['customerEmail'] := 'customer@example.com';

    // First item - VALID
    LItem := LBody.A['items'].AddObject;
    LItem.S['productCode'] := 'ABC-1234';
    LItem.S['productName'] := 'Valid Product';
    LItem.I['quantity'] := 5;
    LItem.F['unitPrice'] := 29.99;

    // Second item - INVALID
    LItem := LBody.A['items'].AddObject;
    LItem.S['productCode'] := 'INVALID';
    LItem.S['productName'] := '';       // EMPTY!
    LItem.I['quantity'] := -3;          // NEGATIVE!
    LItem.F['unitPrice'] := -10.00;     // NEGATIVE!

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/orders');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

// =============================================================================
// PAYMENT TESTS (FORMAT VALIDATORS)
// =============================================================================

procedure TestPayment_Valid;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Payment - VALID CREDIT CARD',
    'Card number passes Luhn algorithm (4111111111111111 is test Visa)',
    'HTTP 200 - Payment processed', True);

  LBody := TJsonObject.Create;
  try
    LBody.S['cardNumber'] := '4111111111111111';
    LBody.S['cardHolder'] := 'MARIO ROSSI';
    LBody.I['expiryMonth'] := 12;
    LBody.I['expiryYear'] := 2026;
    LBody.S['cvv'] := '123';
    LBody.S['iban'] := 'IT60X0542811101000000123456';

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/payments');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, True);
end;

procedure TestPayment_InvalidCreditCard;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Payment - INVALID CREDIT CARD',
    'Card number fails Luhn checksum algorithm (last digit wrong)',
    'HTTP 422 - CardNumber validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['cardNumber'] := '4111111111111112';  // INVALID LUHN!
    LBody.S['cardHolder'] := 'MARIO ROSSI';
    LBody.I['expiryMonth'] := 12;
    LBody.I['expiryYear'] := 2026;
    LBody.S['cvv'] := '123';

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/payments');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

procedure TestPayment_InvalidIBAN;
var
  LBody: TJsonObject;
  LResponse: IMVCRESTResponse;
begin
  ShowTestScreen(
    'Payment - INVALID IBAN',
    'IBAN checksum is wrong (IT00X... is invalid)',
    'HTTP 422 - IBAN validation error', False);

  LBody := TJsonObject.Create;
  try
    LBody.S['cardNumber'] := '4111111111111111';
    LBody.S['cardHolder'] := 'MARIO ROSSI';
    LBody.I['expiryMonth'] := 12;
    LBody.I['expiryYear'] := 2026;
    LBody.S['cvv'] := '123';
    LBody.S['iban'] := 'IT00X0000000000000000000000';  // INVALID CHECKSUM!

    ShowRequestBody(LBody);

    LResponse := GClient
      .AddBody(LBody.ToJSON, TMVCMediaType.APPLICATION_JSON)
      .Post('/api/payments');
  finally
    LBody.Free;
  end;

  ShowResponse(LResponse, False);
end;

// =============================================================================
// SUMMARY
// =============================================================================

procedure ShowSummary;
var
  LContent: TStringArray;
begin
  ClrScr;
  WriteHeader('TEST SUMMARY', 78);
  WriteLn;

  SetLength(LContent, 5);
  LContent[0] := Format('Total tests executed: %d', [GTestNumber]);
  LContent[1] := Format('Passed: %d', [GPassedCount]);
  LContent[2] := Format('Failed: %d', [GFailedCount]);
  LContent[3] := '';
  if GFailedCount = 0 then
    LContent[4] := 'ALL TESTS PASSED!'
  else
    LContent[4] := 'SOME TESTS FAILED!';

  DrawSimpleBox('Results', LContent, 50);

  WriteLn;
  WriteLn;

  WriteLineColored('Key observations:', White);
  WriteLn;

  TextColor(Green);
  WriteLn('  * HTTP 201/200 = Validation PASSED, action was executed');

  TextColor(Red);
  WriteLn('  * HTTP 422     = Validation FAILED, action was NOT executed');

  TextColor(Cyan);
  WriteLn('  * Nested errors use dot notation: "BillingAddress.City"');

  TextColor(Yellow);
  WriteLn('  * Collection errors use index: "Items[1].Quantity"');

  TextColor(Magenta);
  WriteLn('  * Cross-field validators compare fields: "ConfirmPassword"');

  ResetConsole;
end;

// =============================================================================
// MAIN
// =============================================================================

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    ClrScr;

    WriteHeader('DMVCFramework - VALIDATION SHOWCASE CLIENT', 78);
    WriteLn;

    WriteInfo('This client demonstrates the validation system by making');
    WriteInfo('requests with VALID and INVALID data.');
    WriteLn;

    TextColor(Yellow);
    WriteLn('Make sure the server is running on ' + BASE_URL);
    ResetConsole;

    WaitForKey('Press any key to start the tests...');

    GClient := TMVCRESTClient.New.BaseURL(BASE_URL);

    // =========================================================================
    // USER REGISTRATION TESTS
    // =========================================================================
    TestUserRegistration_Valid;
    WaitForKey;

    TestUserRegistration_PasswordMismatch;
    WaitForKey;

    TestUserRegistration_EmptyUsername;
    WaitForKey;

    TestUserRegistration_InvalidEmail;
    WaitForKey;

    TestUserRegistration_AgeTooYoung;
    WaitForKey;

    TestUserRegistration_MultipleErrors;
    WaitForKey;

    // =========================================================================
    // CUSTOMER TESTS (NESTED VALIDATION)
    // =========================================================================
    TestCustomer_Valid;
    WaitForKey;

    TestCustomer_InvalidNestedAddress;
    WaitForKey;

    // =========================================================================
    // ORDER TESTS (COLLECTION VALIDATION)
    // =========================================================================
    TestOrder_Valid;
    WaitForKey;

    TestOrder_EmptyItems;
    WaitForKey;

    TestOrder_InvalidItemInCollection;
    WaitForKey;

    // =========================================================================
    // PAYMENT TESTS (FORMAT VALIDATORS)
    // =========================================================================
    TestPayment_Valid;
    WaitForKey;

    TestPayment_InvalidCreditCard;
    WaitForKey;

    TestPayment_InvalidIBAN;
    WaitForKey;

    // =========================================================================
    // SUMMARY
    // =========================================================================
    ShowSummary;

    WaitForKey('Press any key to exit...');
    ClrScr;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteError('ERROR: ' + E.Message);
      WriteLn;
      WriteWarning('Make sure the server is running on ' + BASE_URL);
      WaitForKey('Press any key to exit...');
      ExitCode := 1;
    end;
  end;
end.
