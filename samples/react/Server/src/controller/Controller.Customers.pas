unit Controller.Customers;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.ActiveRecord,
  FireDAC.Comp.Client,
  FireDAC.Phys.SQLite,
  MVCFramework.SQLGenerators.Sqlite,
  System.Generics.Collections,
  Model.Customer,
  System.JSON;

type

  [MVCPath('/api')]
  TCustomersController = class(TMVCController)
  private
    FDConn : TFDConnection;
  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(id: Integer);

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer;

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateCustomer(id: Integer);

    [MVCPath('/customers/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteCustomer(id: Integer);

    constructor Create; override;
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;


//Sample CRUD Actions for a "Customer" entity
procedure TCustomersController.GetCustomers;
begin
  Render<TCustomer>(TMVCActiveRecord.SelectRQL<TCustomer>('sort(+id)', 200));
end;

procedure TCustomersController.GetCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(id);

  Render(lCustomer);
end;

constructor TCustomersController.Create;
begin
  inherited;
  FDConn := TFDConnection.Create(nil);
  FDConn.Params.Clear;
  FDConn.Params.Database :=  '../../../data/activerecorddb.db';
  FDConn.DriverName := 'SQLite';
  FDConn.Connected := True;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(FDConn);
end;

procedure TCustomersController.CreateCustomer;
var
  lCustomer : TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  try
    lCustomer.Insert;
  finally
    lCustomer.Free;
  end;
  Render201Created();
end;

procedure TCustomersController.UpdateCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  lCustomer.id := id;

  lCustomer.Update;
  Render(lCustomer);
end;

procedure TCustomersController.DeleteCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(id);
  lCustomer.Delete;

  Render(TJSONObject.Create(TJSONPair.Create('result', 'register successefully deleted')));
end;

destructor TCustomersController.Destroy;
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
  inherited;
end;

end.
