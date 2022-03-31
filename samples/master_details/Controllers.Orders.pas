unit Controllers.Orders;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.Serializer.Commons,
  System.Generics.Collections,
  Controllers.Base, BusinessObjects;

type

  [MVCDoc('Resource that manages Orders CRUD')]
  [MVCPath('/orders')]
  TOrdersController = class(TBaseController)
  public
    [MVCDoc('Returns the list of Orders')]
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetOrders;

    [MVCDoc('Returns the list of Orders')]
    [MVCPath('/searches')]
    [MVCHTTPMethod([httpGET])]
    procedure GetOrdersByDescription(const [MVCFromQueryString('q', '')] Search: String);

    [MVCDoc('Returns the Order with the specified id')]
    [MVCPath('/meta')]
    [MVCHTTPMethod([httpGET])]
    procedure GetOrderMeta;

    [MVCDoc('Returns the Order with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetOrderByID(id: Integer);

    [MVCDoc('Deletes the Order with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDelete])]
    procedure DeleteOrderByID(id: Integer);

    [MVCDoc('Updates the Order with the specified id and return "200: OK"')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateOrderByID(const [MVCFromBody] Order: TOrder; const id: Integer);

    [MVCDoc('Creates a new Order and returns "201: Created"')]
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateOrder(const [MVCFromBody] Order: TOrder);

    [MVCDoc('Creates new Orders from a list and returns "201: Created"')]
    [MVCPath('/bulk')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateOrders(const [MVCFromBody] OrderList: TObjectList<TOrder>);
  end;

implementation

{ TOrdersController }

uses
  Commons,
  mvcframework.Serializer.Intf,
  System.SysUtils, mvcframework.ActiveRecord;

procedure TOrdersController.CreateOrder(const [MVCFromBody] Order: TOrder);
begin
  Order.Insert;
  Render201Created('/orders/' + Order.id.ToString, 'Order Created');
end;

procedure TOrdersController.CreateOrders(const OrderList: TObjectList<TOrder>);
begin
  // GetOrdersService.StartTransaction;
  // try
  // for lOrder in OrderList do
  // begin
  // GetOrdersService.Add(lOrder);
  // end;
  // GetOrdersService.Commit;
  // except
  // GetOrdersService.Rollback;
  // raise;
  // end;
  Render(201, 'Orders Created');
end;

procedure TOrdersController.DeleteOrderByID(id: Integer);
var
  Order: TOrder;
begin
  TMVCActiveRecord.CurrentConnection.StartTransaction;
  try
    Order := TMVCActiveRecord.GetByPk<TOrder>(id);
    try
      Order.Delete;
    finally
      Order.Free;
    end;
    TMVCActiveRecord.CurrentConnection.Commit;
  except
    TMVCActiveRecord.CurrentConnection.Rollback;
    raise;
  end;
end;

procedure TOrdersController.GetOrders;
begin
  try
    Render(ObjectDict().Add('data', TMVCActiveRecord.All<TOrder>));
    // Render(ObjectDict().Add('data', GetOrdersService.GetByID(id)));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
  end;

end;

procedure TOrdersController.GetOrdersByDescription(const Search: String);
  lDict: IMVCObjectDictionary;
begin
//  Render(
//    ObjectDict()
//      .Add('data', TMVCActiveRecord.SelectRQL<TOrder>(Format('contains(description,"%s")',[Search]),100))
//      );
  // try
  // if Search = '' then
  // begin
  // lDict := ObjectDict().Add('data', GetOrdersService.GetAll);
  // end
  // else
  // begin
  // lDict := ObjectDict().Add('data', GetOrdersService.GetOrders(Search));
  // end;
  // Render(lDict);
  // except
  // on E: EServiceException do
  // begin
  // raise EMVCException.Create(E.Message, '', 0, 404);
  // end
  // else
  // raise;
  // end;
end;

procedure TOrdersController.UpdateOrderByID(const Order: TOrder; const id: Integer);
var
  lCurrentOrder: TOrder;
begin
  Order.id := id;
  lCurrentOrder := TMVCActiveRecord.GetByPK<TOrder>(id);
  try
    Order.Update();
  finally
    lCurrentOrder.Free;
  end;
  Render(200, 'Order Updated');
end;

procedure TOrdersController.GetOrderByID(id: Integer);
begin
  try
    Render(TMVCActiveRecord.GetByPK<TOrder>(id));
    // Render(ObjectDict().Add('data', GetOrdersService.GetByID(id)));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
  end;
end;

procedure TOrdersController.GetOrderMeta;
begin
  try
    // Render(ObjectDict().Add('data', GetOrdersService.GetMeta));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
    else
      raise;
  end;
end;

end.
