unit CustomersControllerU;

interface

uses
  MVCFramework,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons;

type

  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    procedure GetCustomers;

    [MVCPath('/($ID)')]
    [MVCHTTPMethods([httpGET])]
    procedure GetCustomerByID(const ID: Integer);

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    procedure CreateCustomers;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons,
  JsonDataObjects,
  MainDM;

{ TCustomersController }

procedure TCustomersController.CreateCustomers;
var
  lJSON: TJsonObject;
  lCustDM: TCustomersDM;
  lSQL: string;
  lNewCustomerId: Integer;
begin
  lJSON := StrToJSONObject(Context.Request.Body);
  if lJSON = nil then
    raise EMVCDeserializationException.Create('Invalid JSON Body');
  try
    lSQL := 'INSERT INTO customers (code, description, city, rating, note) ' +
      'VALUES (:code, :description, :city, :rating, :note) returning id';
    lCustDM := TCustomersDM.Create(nil);
    try
      lCustDM.MyQuery.SQL.Text := lSQL;
      lCustDM.MyQuery.ParamByName('code').AsString := lJSON.S['code'];
      lCustDM.MyQuery.ParamByName('description').AsString :=
        lJSON.S['description'];
      lCustDM.MyQuery.ParamByName('city').AsString := lJSON.S['city'];
      lCustDM.MyQuery.ParamByName('rating').AsInteger := lJSON.I['rating'];
      lCustDM.MyQuery.ParamByName('note').AsString := lJSON.S['note'];
      lCustDM.MyQuery.OpenOrExecute;
      lNewCustomerId := lCustDM.MyQuery.FieldByName('id').AsInteger;
      Render201Created('/api/customers/' + lNewCustomerId.ToString);
    finally
      lCustDM.Free;
    end;
  finally
    lJSON.Free;
  end;
end;

procedure TCustomersController.GetCustomerByID(const ID: Integer);
var
  lCustDM: TCustomersDM;
begin
  lCustDM := TCustomersDM.Create(nil);
  try
    lCustDM.MyQuery.Open('select * from customers where id = ?', [ID]);
    if lCustDM.MyQuery.Eof then
      raise EMVCException.Create(HTTP_STATUS.NotFound, 'Customer not found');
    Render(
      ObjectDict(False)
        .Add('data', lCustDM.MyQuery, nil, dstSingleRecord));
  finally
    lCustDM.Free;
  end;
end;

procedure TCustomersController.GetCustomers;
var
  lCustDM: TCustomersDM;
begin
  lCustDM := TCustomersDM.Create(nil);
  try
    lCustDM.MyQuery.Open('select * from customers order by code');
    Render(ObjectDict(False).Add('data', lCustDM.MyQuery));
  finally
    lCustDM.Free;
  end;
end;

end.
