// ***************************************************************************
//
// Delphi MVC Framework - Validation Showcase Sample
//
// This sample demonstrates FUNCTIONAL ACTIONS with automatic validation.
// Notice how each action is a FUNCTION that returns IMVCResponse.
//
// ***************************************************************************

unit ValidationControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.ValidationEngine,
  ValidationModelsU;

type
  [MVCPath('/api')]
  TValidationController = class(TMVCController)
  public
    // =====================================================================
    // FUNCTIONAL ACTIONS WITH AUTOMATIC VALIDATION
    // Each action is a function returning IMVCResponse.
    // Validation happens automatically when using [MVCFromBody].
    // If validation fails, HTTP 422 is returned BEFORE the action executes.
    // =====================================================================

    [MVCPath('/users/register')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Register a new user. Validation is automatic!')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function RegisterUser([MVCFromBody] const AUser: TUserRegistration): IMVCResponse;

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Create a customer with nested address validation')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function CreateCustomer([MVCFromBody] const ACustomer: TCustomer): IMVCResponse;

    [MVCPath('/orders')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Create an order with collection validation')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function CreateOrder([MVCFromBody] const AOrder: TOrder): IMVCResponse;

    [MVCPath('/payments')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Process payment with credit card validation')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function ProcessPayment([MVCFromBody] const APayment: TPaymentInfo): IMVCResponse;

    [MVCPath('/events')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Create event booking - demonstrates OnValidate for object-level validation')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function CreateEventBooking([MVCFromBody] const AEvent: TEventBooking): IMVCResponse;

    // =====================================================================
    // MANUAL VALIDATION EXAMPLE
    // You can also validate objects manually using TMVCValidationEngine
    // =====================================================================

    [MVCPath('/validate/user')]
    [MVCHTTPMethod([httpPOST])]
    [MVCDoc('Validate user without saving (manual validation example)')]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function ValidateUserManually: IMVCResponse;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  JsonDataObjects,
  MVCFramework.Validation;

{ TValidationController }

function TValidationController.RegisterUser(const AUser: TUserRegistration): IMVCResponse;
var
  LResponse: TJsonObject;
begin
  // ==========================================================================
  // VALIDATION OK! If we reach here, ALL validators passed!
  // The framework automatically validated AUser BEFORE calling this method.
  // If validation had failed, this code would NEVER execute.
  // ==========================================================================

  WriteLn('>>> ACTION EXECUTED: RegisterUser - Validation PASSED for user: ' + AUser.Username);

  LResponse := TJsonObject.Create;
  LResponse.S['status'] := 'success';
  LResponse.S['message'] := 'User registered successfully';
  LResponse.S['actionExecuted'] := 'YES - Validation passed, action was called';
  LResponse.O['user'].S['username'] := AUser.Username;
  LResponse.O['user'].S['email'] := AUser.Email;
  LResponse.O['user'].I['age'] := AUser.Age;

  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.Created)
    .Body(LResponse)
    .Build;
end;

function TValidationController.CreateCustomer(const ACustomer: TCustomer): IMVCResponse;
var
  LResponse: TJsonObject;
begin
  // ==========================================================================
  // VALIDATION OK! Nested BillingAddress was also validated automatically!
  // ==========================================================================

  WriteLn('>>> ACTION EXECUTED: CreateCustomer - Validation PASSED for: ' + ACustomer.Name);

  LResponse := TJsonObject.Create;
  LResponse.S['status'] := 'success';
  LResponse.S['message'] := 'Customer created successfully';
  LResponse.S['actionExecuted'] := 'YES - Validation passed (including nested Address)';
  LResponse.O['customer'].S['name'] := ACustomer.Name;
  LResponse.O['customer'].S['email'] := ACustomer.Email;
  LResponse.O['customer'].O['billingAddress'].S['street'] := ACustomer.BillingAddress.Street;
  LResponse.O['customer'].O['billingAddress'].S['city'] := ACustomer.BillingAddress.City;
  LResponse.O['customer'].O['billingAddress'].S['postalCode'] := ACustomer.BillingAddress.PostalCode;

  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.Created)
    .Body(LResponse)
    .Build;
end;

function TValidationController.CreateOrder(const AOrder: TOrder): IMVCResponse;
var
  LResponse: TJsonObject;
  LItem: TOrderItem;
  LItemJson: TJsonObject;
begin
  // ==========================================================================
  // VALIDATION OK! Each item in the Items collection was validated too!
  // ==========================================================================

  WriteLn(Format('>>> ACTION EXECUTED: CreateOrder - Validation PASSED for order %s with %d items',
    [AOrder.OrderNumber, AOrder.Items.Count]));

  LResponse := TJsonObject.Create;
  LResponse.S['status'] := 'success';
  LResponse.S['message'] := 'Order created successfully';
  LResponse.S['actionExecuted'] := 'YES - Validation passed (including all collection items)';
  LResponse.O['order'].S['orderNumber'] := AOrder.OrderNumber;
  LResponse.O['order'].S['customerEmail'] := AOrder.CustomerEmail;
  LResponse.O['order'].I['itemCount'] := AOrder.Items.Count;

  for LItem in AOrder.Items do
  begin
    LItemJson := LResponse.O['order'].A['items'].AddObject;
    LItemJson.S['productCode'] := LItem.ProductCode;
    LItemJson.S['productName'] := LItem.ProductName;
    LItemJson.I['quantity'] := LItem.Quantity;
    LItemJson.F['unitPrice'] := LItem.UnitPrice;
  end;

  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.Created)
    .Body(LResponse)
    .Build;
end;

function TValidationController.ProcessPayment(const APayment: TPaymentInfo): IMVCResponse;
var
  LResponse: TJsonObject;
  LMaskedCard: string;
begin
  // ==========================================================================
  // VALIDATION OK! Credit card Luhn check passed, IBAN checksum valid!
  // ==========================================================================

  LMaskedCard := '****-****-****-' + Copy(APayment.CardNumber, Length(APayment.CardNumber) - 3, 4);
  WriteLn('>>> ACTION EXECUTED: ProcessPayment - Card validated: ' + LMaskedCard);

  LResponse := TJsonObject.Create;
  LResponse.S['status'] := 'success';
  LResponse.S['message'] := 'Payment processed successfully';
  LResponse.S['actionExecuted'] := 'YES - Credit card Luhn algorithm passed';
  LResponse.O['payment'].S['cardMasked'] := LMaskedCard;
  LResponse.O['payment'].S['cardHolder'] := APayment.CardHolder;
  LResponse.O['payment'].S['expiry'] := Format('%d/%d', [APayment.ExpiryMonth, APayment.ExpiryYear]);

  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(LResponse)
    .Build;
end;

function TValidationController.CreateEventBooking(const AEvent: TEventBooking): IMVCResponse;
var
  LResponse: TJsonObject;
begin
  // ==========================================================================
  // VALIDATION OK! Both property validators AND OnValidate passed!
  // The OnValidate method checked cross-field rules:
  // - EndDate >= StartDate
  // - MaxParticipants >= MinParticipants
  // - BudgetMax >= BudgetMin
  // - Events > 7 days require MinParticipants >= 10
  // ==========================================================================

  WriteLn(Format('>>> ACTION EXECUTED: CreateEventBooking - Event "%s" from %s to %s',
    [AEvent.EventName, DateToStr(AEvent.StartDate), DateToStr(AEvent.EndDate)]));

  LResponse := TJsonObject.Create;
  LResponse.S['status'] := 'success';
  LResponse.S['message'] := 'Event booking created successfully';
  LResponse.S['actionExecuted'] := 'YES - Property validators AND OnValidate passed';
  LResponse.O['event'].S['eventName'] := AEvent.EventName;
  LResponse.O['event'].S['startDate'] := DateToStr(AEvent.StartDate);
  LResponse.O['event'].S['endDate'] := DateToStr(AEvent.EndDate);
  LResponse.O['event'].I['minParticipants'] := AEvent.MinParticipants;
  LResponse.O['event'].I['maxParticipants'] := AEvent.MaxParticipants;
  LResponse.O['event'].F['budgetMin'] := AEvent.BudgetMin;
  LResponse.O['event'].F['budgetMax'] := AEvent.BudgetMax;

  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.Created)
    .Body(LResponse)
    .Build;
end;

function TValidationController.ValidateUserManually: IMVCResponse;
var
  LUser: TUserRegistration;
  LErrors: TDictionary<string, string>;
  LResponse: TJsonObject;
  LKey: string;
begin
  // Create and populate user from request body manually
  LUser := Context.Request.BodyAs<TUserRegistration>;
  try
    LResponse := TJsonObject.Create;

    // Manual validation - get errors dictionary
    if not TMVCValidationEngine.Validate(LUser, LErrors) then
    begin
      try
        WriteLn('>>> MANUAL VALIDATION: FAILED with ' + IntToStr(LErrors.Count) + ' errors');

        LResponse.B['valid'] := False;
        LResponse.I['errorCount'] := LErrors.Count;
        for LKey in LErrors.Keys do
          LResponse.O['errors'].S[LKey] := LErrors[LKey];

        Result := MVCResponseBuilder
          .StatusCode(HTTP_STATUS.UnprocessableEntity)
          .Body(LResponse)
          .Build;
      finally
        LErrors.Free;
      end;
    end
    else
    begin
      // LErrors is nil when validation passes (lazy dictionary creation)
      WriteLn('>>> MANUAL VALIDATION: PASSED for user ' + LUser.Username);

      LResponse.B['valid'] := True;
      LResponse.S['message'] := 'User data is valid';

      Result := MVCResponseBuilder
        .StatusCode(HTTP_STATUS.OK)
        .Body(LResponse)
        .Build;
    end;
  finally
    LUser.Free;
  end;
end;

end.
