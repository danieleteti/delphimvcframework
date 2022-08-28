unit Controllers.Orders;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.Serializer.Commons,
  mvcframework.DataSet.Utils,
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
    procedure UpdateOrderByID(const id: Integer);

    [MVCDoc('Updates the Order and return "200: OK" - overwrites childs by default')]
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
begin
  ResponseStream.AppendLine('---');
  lCurrentOrder := TMVCActiveRecord.GetByPk<TOrder>(id);
  try
    TMVCActiveRecord
      .Merge<TOrderDetail>(lCurrentOrder.OrderItems, OrderDetails).Apply(
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
                  lObj := lCurrentOrder.OrderItems.Extract(OrderDetail);
                  try
                    lObj.Delete;
                  finally
                    lObj.Free;
                  end;
                end;
              eaCreate:
                begin
                  lCurrentOrder.OrderItems.Add(OrderDetail);
                end;
              eaUpdate:
                begin
                  lObj := lCurrentOrder.GetOrderDetailByID(OrderDetail.ID.Value);
                  lObj.Assign(OrderDetail);
                end;
            end;
            Handled := True;
            ResponseStream.AppendLine(GetEnumName(TypeInfo(TMVCEntityAction), Ord(EntityAction)));
          end);
    lCurrentOrder.Update();
  finally
    lCurrentOrder.Free;
  end;
  RenderResponseStream;
end;

procedure TOrdersController.UpdateOrderByID(const id: Integer);
var
  lCurrentOrder: TOrder;
begin
  lCurrentOrder := TMVCActiveRecord.GetByPk<TOrder>(id);
  try
    Context.Request.BodyFor<TOrder>(lCurrentOrder);
    lCurrentOrder.Update();
  finally
    lCurrentOrder.Free;
  end;
  Render(200, 'Order Updated');
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
