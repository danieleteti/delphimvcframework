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
    procedure AddCall(ActionName: String);

  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;

  public
    [MVCPath('/')]
    procedure Index(Context: TWebContext);
    [MVCPath('/orders')]
    procedure Orders(Context: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/orders/($ordernumber)')]
    procedure OrderNumber(Context: TWebContext);

    [MVCHTTPMethod([httpPOST, httpPUT])]
    [MVCPath('/orders/($ordernumber)')]
    procedure UpdateOrderNumber(Context: TWebContext);

    property CalledActions: TStringList read FCalledActions; // only for tests
  end;

implementation

{ TSimpleController }

procedure TSimpleController.AddCall(ActionName: String);
begin
  FCalledActions.Add(ActionName);
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

procedure TSimpleController.UpdateOrderNumber(Context: TWebContext);
begin

end;

procedure TSimpleController.OrderNumber(Context: TWebContext);
begin

end;

end.
