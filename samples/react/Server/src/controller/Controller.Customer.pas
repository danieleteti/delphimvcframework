unit Controller.Customer;

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
  TCustomerController = class(TMVCController)
  private
    FDConn : TFDConnection;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

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


procedure TCustomerController.OnAfterAction(Context: TWebContext; const AActionName: string); 
begin
  { Executed after each action }
  inherited;
end;

procedure TCustomerController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

//Sample CRUD Actions for a "Customer" entity
procedure TCustomerController.GetCustomers;
var
  lCustomers : TObjectList<TCustomer>;
begin
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('', 20);

  Render<TCustomer>(lCustomers);
end;

procedure TCustomerController.GetCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(id);

  Render(lCustomer);
end;

constructor TCustomerController.Create;
begin
  inherited;
  FDConn := TFDConnection.Create(nil);
  FDConn.Params.Clear;
  FDConn.Params.Database :=  '../../../data/activerecorddb.db';
  FDConn.DriverName := 'SQLite';
  FDConn.Connected := True;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(FDConn);
end;

procedure TCustomerController.CreateCustomer;
var
  lCustomer : TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;

  lCustomer.Insert;
  Render(lCustomer);
end;

procedure TCustomerController.UpdateCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  lCustomer.id := id;

  lCustomer.Update;
  Render(lCustomer);
end;

procedure TCustomerController.DeleteCustomer(id: Integer);
var
  lCustomer : TCustomer;
begin
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(id);
  lCustomer.Delete;

  Render(TJSONObject.Create(TJSONPair.Create('result', 'register successefully deleted')));
end;

destructor TCustomerController.Destroy;
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
  inherited;
end;

end.
