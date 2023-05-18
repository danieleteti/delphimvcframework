unit Controllers.Orders;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.Serializer.Commons,
  mvcframework.DataSet.Utils,
  jsondataobjects,
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
    procedure GetOrdersByDescription(const [MVCFromQueryString('totalGrOrEq', '0')] Search: Single);

    [MVCDoc('Returns the dataset metadata (Delphi Specific)')]
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

    [MVCDoc('Updates the Order and return "200: OK" - overwrites childs by default')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateOrderByID(
      const id: Integer;
      [MVCFromBody] OrderIn: TOrderIn
      );

    [MVCDoc('Updates the Order Items and return "200: OK" - merge childs')]
    [MVCPath('/($id)/details')]
    [MVCHTTPMethod([httpPATCH])]
    procedure MergeOrderDetailsByOrderID(const id: Integer; const [MVCFromBody] OrderDetails: TObjectList<TOrderDetail>);

    [MVCDoc('Creates a new Order and returns "201: Created - creates childs"')]
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
  System.SysUtils, mvcframework.ActiveRecord, Data.DB, System.TypInfo;

procedure TOrdersController.CreateOrder(const [MVCFromBody] Order: TOrder);
begin
  Order.Insert;
  Render201Created('/orders/' + Order.ID.Value.ToString, 'Order Created');
end;

procedure TOrdersController.CreateOrders(const OrderList: TObjectList<TOrder>);
begin
  TMVCActiveRecord.CurrentConnection.StartTransaction;
  try
    for var lOrder in OrderList do
    begin
      lOrder.Store;
    end;
    TMVCActiveRecord.CurrentConnection.Commit;
    Render(ObjectDict(false).Add('data', OrderList));
  except
    TMVCActiveRecord.CurrentConnection.Rollback;
    raise;
  end;
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
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
  end;
end;

procedure TOrdersController.GetOrdersByDescription(const Search: Single);
var
  lFS: TFormatSettings;
  lRQLCriteria: string;
begin
  lFS.DecimalSeparator := '.';
  lRQLCriteria := Format('ge(total,"%0.2f")', [Search], lFS);
  Render(
    ObjectDict()
      .Add('data',
        TMVCActiveRecord.SelectRQL<TOrder>(lRQLCriteria, 100))
      .Add('criteria', StrDict(['rql'],[lRQLCriteria]))
      );
end;

procedure TOrdersController.MergeOrderDetailsByOrderID(const id: Integer;
  const OrderDetails: TObjectList<TOrderDetail>);
var
  lCurrentOrder: TOrder;
  lResp: TJSONArray;
begin
  lResp := TJSONArray.Create;
  try
    TMVCActiveRecord.CurrentConnection.StartTransaction;
    try
      lCurrentOrder := TMVCActiveRecord.GetByPk<TOrder>(id);
      try
        TMVCActiveRecord
          .Merge<TOrderDetail>(lCurrentOrder.OrderItems, OrderDetails).Apply(
            procedure(
              const OrderDetail: TOrderDetail;
              const EntityAction: TMVCEntityAction;
              var Handled: Boolean)
              begin
                case EntityAction of
                  eaDelete:
                    begin
                      lResp.Add('deleted order item with id = ' + IntToStr(OrderDetail.ID));
                      lCurrentOrder.OrderItems.Extract(OrderDetail); //remove the detail from the master
                      Handled := False; //will be automatically deleted
                    end;
                  eaCreate:
                    begin
                      lCurrentOrder.AddOrderItem(OrderDetail.Clone); //add detail to master
                      lResp.Add('added new order item');
                      Handled := True; //will not be inserted yet
                    end;
                  eaUpdate:
                    begin
                      lCurrentOrder.UpdateOrderItemByID(OrderDetail.ID, OrderDetail); //update the detail
                      lResp.Add('updated order item with id = ' + IntToStr(OrderDetail.ID));
                      Handled := True; //will not be updated yet
                    end;
                end;
              end);
        lCurrentOrder.Update(); // fires logic, update the master, insert and update details
      finally
        lCurrentOrder.Free;
      end;
      TMVCActiveRecord.CurrentConnection.Commit;
    except
      TMVCActiveRecord.CurrentConnection.Rollback;
      raise;
    end;
    Render(lResp, False);
  finally
    lResp.Free;
  end;
end;

procedure TOrdersController.UpdateOrderByID(
      const id: Integer;
      [MVCFromBody] OrderIn: TOrderIn);
var
  lCurrentOrder: TOrder;
  lResp: TJSONArray;
begin
  lResp := TJSONArray.Create;
  try
    lCurrentOrder := TMVCActiveRecord.GetByPk<TOrder>(id);
    try
      if OrderIn.IDCustomer.HasValue then
        lCurrentOrder.IDCustomer := OrderIn.IDCustomer.Value;
      if OrderIn.OrderDate.HasValue then
        lCurrentOrder.OrderDate := OrderIn.OrderDate.Value;
      TMVCActiveRecord
        .Merge<TOrderDetail>(lCurrentOrder.OrderItems, OrderIn.OrderItems).Apply(
          procedure(
            const OrderDetail: TOrderDetail;
            const EntityAction: TMVCEntityAction;
            var Handled: Boolean)
            var
              lObj: TOrderDetail;
            begin
              case EntityAction of
                eaDelete:
                  begin
                    lResp.Add('deleted order item with id = ' + IntToStr(OrderDetail.ID));
                    lCurrentOrder.OrderItems.Extract(OrderDetail);
                    Handled := False;
                  end;
                eaCreate:
                  begin
                    Handled := True;
                    lObj := TOrderDetail.Create;
                    lObj.Assign(OrderDetail);
                    lCurrentOrder.AddOrderItem(lObj);
                    lResp.Add('added new order item');
                  end;
                eaUpdate:
                  begin
                    Handled := True;
                    lCurrentOrder.UpdateOrderItemByID(OrderDetail.ID, OrderDetail);
                    lResp.Add('updated order item with id = ' + IntToStr(OrderDetail.ID));
                  end;
              end;
            end);
      lCurrentOrder.Update();
    finally
      lCurrentOrder.Free;
    end;
    Render(lResp, False);
  finally
    lResp.Free;
  end;
end;

procedure TOrdersController.GetOrderByID(id: Integer);
begin
  try
    Render(ObjectDict().Add('data', TMVCActiveRecord.GetByPk<TOrder>(id)));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
  end;
end;

procedure TOrdersController.GetOrderMeta;
var
  lDS: TDataSet;
begin
  lDS := TMVCActiveRecord.SelectDataSet('select * from orders where 1=0', [], True);
  try
    Render(ObjectDict().Add('metadata', lDS.MetadataAsJSONObject()));
  finally
    lDS.Free;
  end;
end;

end.
