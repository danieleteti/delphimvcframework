unit RenderSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers/($id)')]
    [MVCProduces('application/json')]
    procedure GetCustomerByID(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetPersonJSON(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers/($id)/html')]
    [MVCConsumes('text/html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetPersonAsText(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, WebModuleU;

{ TRoutingSampleController }

procedure TRenderSampleController.GetCustomerByID(CTX: TWebContext);
var
  Cust: TCustomer;
begin
  if CTX.Request.ParamsAsInteger['id'] = 7 then
    Render(404, 'Customer Not Found')
  else
  begin
    Cust := TCustomer.Create;
    Cust.Name := 'Daniele Teti Inc.';
    Cust.ContactFirst := 'Daniele';
    Cust.ContactLast := 'Teti';
    Cust.AddressLine1 := 'Rome Street 12';
    Cust.AddressLine2 := '00100';
    Cust.City := 'ROME';
    Render(Cust);
  end;
end;

procedure TRenderSampleController.GetCustomers(CTX: TWebContext);
var
  wm: TWebModule1;
begin
  wm := GetCurrentWebModule as TWebModule1;
  wm.qryCustomers.Open;
  Render(wm.qryCustomers);
end;

procedure TRenderSampleController.GetPersonAsText(CTX: TWebContext);
begin
  ResponseStream.
    Append('<html><body><ul>').
    Append('<li>FirstName: Daniele</li>').
    Append('<li>LastName: Teti').
    AppendFormat('<li>DOB: %s</li>', [ISODateToString(EncodeDate(1975, 5, 2))]).
    Append('<li>Married: yes</li>').
    Append('</ul></body></html>');
  Render;
end;

procedure TRenderSampleController.GetPersonJSON(CTX: TWebContext);
var
  P: TJSONObject;
  IDPerson: Integer;
begin
  IDPerson := CTX.Request.ParamsAsInteger['id'];
  {
    Use IDPerson to load the person from a database...
    In this example, we're creating a fake person
  }
  P := TJSONObject.Create;
  P.AddPair('FirstName', 'Daniele');
  P.AddPair('LastName', 'Teti');
  P.AddPair('DOB', ISODateToString(EncodeDate(1975, 5, 2)));
  P.AddPair('Married', TJSONTrue.Create);
  Render(P);
end;

end.
