unit TestControllersU;

interface

uses MVCFramework.Commons,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type

  [MVCPath('/')]
  TSimpleController = class(TMVCController)
  private
    FCalledActions: TStringList;
    procedure AddCall(ActionName: string);

  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;

  public
    [MVCPath('/')]
    procedure Index(Context: TWebContext);

    [MVCPath('/orders')]
    [MVCProduces('application/json')]
    procedure OrdersProduceJSON(Context: TWebContext);

    [MVCPath('/orders')]
    procedure Orders(Context: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/orders/($ordernumber)')]
    procedure OrderNumber(Context: TWebContext);

    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCPath('/orders/($ordernumber)')]
    procedure UpdateOrderNumber(Context: TWebContext);

    [MVCHTTPMethod([httpPATCH])]
    [MVCPath('/orders/($ordernumber)')]
    procedure PatchOrder(Context: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/patient/\$match')]
    procedure GetOrderIssue513;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/patient/\$match/($par1)/($par2)')]
    procedure GetOrderIssue513WithPars(par1: string; par2: string);

    { Two GET actions on the SAME parametric path, separated only by MVCProduces -
      the ordinary content-negotiation shape, and the one that makes the router
      evaluate a second candidate after the first has already filled part of the
      shared parameters table. See TTestRouting.TestARefusedCandidateLeavesNoParametersBehind. }
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/negotiated/($folder)/($name)')]
    [MVCProduces('application/json')]
    procedure NegotiatedJSON(folder: string; name: string);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/negotiated/($folder)/($name)')]
    [MVCProduces('text/plain')]
    procedure NegotiatedText(folder: string; name: string);

    property CalledActions: TStringList read FCalledActions; // only for tests
  end;

  TNotSoSimpleController = class(TMVCController)
  public
    procedure Method1(CTX: TWebContext);
  end;

implementation

{ TSimpleController }

procedure TSimpleController.AddCall(ActionName: string);
begin
  FCalledActions.Add(ActionName);
end;

procedure TSimpleController.GetOrderIssue513;
begin
  AddCall('GetOrderIssue513');
end;

procedure TSimpleController.GetOrderIssue513WithPars(par1, par2: string);
begin
  AddCall('GetOrderIssue513WithPars');
end;

procedure TSimpleController.NegotiatedJSON(folder, name: string);
begin
  AddCall('NegotiatedJSON');
end;

procedure TSimpleController.NegotiatedText(folder, name: string);
begin
  AddCall('NegotiatedText');
end;

procedure TSimpleController.Index(Context: TWebContext);
begin
  AddCall('Index');
end;

procedure TSimpleController.MVCControllerAfterCreate;
begin
  inherited;
  FCalledActions := TStringList.Create;
end;

procedure TSimpleController.MVCControllerBeforeDestroy;
begin
  FCalledActions.Free;
  inherited;

end;

procedure TSimpleController.Orders(Context: TWebContext);
begin

end;

procedure TSimpleController.OrdersProduceJSON(Context: TWebContext);
begin

end;

procedure TSimpleController.PatchOrder(Context: TWebContext);
begin

end;

procedure TSimpleController.UpdateOrderNumber(Context: TWebContext);
begin

end;

procedure TSimpleController.OrderNumber(Context: TWebContext);
begin

end;

{ TNotSoSimpleController }

procedure TNotSoSimpleController.Method1(CTX: TWebContext);
begin

end;

end.
