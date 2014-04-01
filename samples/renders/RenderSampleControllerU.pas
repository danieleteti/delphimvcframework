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
    [MVCProduces('text/html')]
    procedure GetCustomers_AsDataSet(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers/($id).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetPerson_AsHTML(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers/($id)')]
    [MVCProduces('application/json')]
    procedure GetCustomerByID_AsTObject(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/files/customers.json')]
    [MVCProduces('application/json')]
    procedure GetPersonJSON(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/files/customers.txt')]
    [MVCProduces('text/plain')]
    procedure GetPerson_AsText(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/files/customers.png')]
    [MVCProduces('image/png')]
    procedure GetPersonPhoto(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, WebModuleU, Generics.Collections;

{ TRoutingSampleController }

procedure TRenderSampleController.GetCustomerByID_AsTObject(CTX: TWebContext);
var
  Cust: TCustomer;
begin
  if CTX.Request.ParamsAsInteger['id'] = 7 then // just a sample
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

procedure TRenderSampleController.GetCustomers_AsDataSet(CTX: TWebContext);
var
  wm: TWebModule1;
begin
  wm := GetCurrentWebModule as TWebModule1;
  wm.qryCustomers.Open;
  Render(wm.qryCustomers);
end;

procedure TRenderSampleController.GetPerson_AsHTML(CTX: TWebContext);
begin
  ResponseStream.
    Append('<html><body><ul>').
    Append('<li>FirstName: Daniele</li>').
    Append('<li>LastName: Teti').
    AppendFormat('<li>DOB: %s</li>',
    [ISODateToString(EncodeDate(1975, 5, 2))]).
    Append('<li>Married: yes</li>').
    Append('</ul></body></html>');
  Render;
end;

procedure TRenderSampleController.GetPerson_AsText(CTX: TWebContext);
begin
  ResponseStream.
    AppendLine('FirstName: Daniele').
    AppendLine('LastName : Teti').
    AppendLine('DOB      : ' + ISODateToString(EncodeDate(1975, 5, 2))).
    AppendLine('Married  : yes');
  Render;
end;

procedure TRenderSampleController.GetPeople_AsObjectList(CTX: TWebContext);
var
  P: TPerson;
  People: TObjectList<TPerson>;
begin
  People := TObjectList<TPerson>.Create(True);

{$REGION 'Fake data'}
  P := TPerson.Create;
  P.FirstName := 'Daniele';
  P.LastName := 'Teti';
  P.DOB := EncodeDate(1979, 11, 4);
  P.Married := True;
  People.Add(P);

  P := TPerson.Create;
  P.FirstName := 'John';
  P.LastName := 'Doe';
  P.DOB := EncodeDate(1879, 10, 2);
  P.Married := False;
  People.Add(P);

  P := TPerson.Create;
  P.FirstName := 'Jane';
  P.LastName := 'Doe';
  P.DOB := EncodeDate(1883, 1, 5);
  P.Married := True;
  People.Add(P);
{$ENDREGION}
  Render<TPerson>(People);
  // or if you want to be more opne to future extension
  // RenderListAsProperty<TPerson>('people', People);
end;

procedure TRenderSampleController.GetPersonJSON(CTX: TWebContext);
var
  P: TJSONObject;
begin
  P := TJSONObject.Create;
  P.AddPair('FirstName', 'Daniele');
  P.AddPair('LastName', 'Teti');
  P.AddPair('DOB', ISODateToString(EncodeDate(1975, 5, 2)));
  P.AddPair('Married', TJSONTrue.Create);
  Render(P);
end;

procedure TRenderSampleController.GetPersonPhoto(CTX: TWebContext);
begin
  // ContentType := 'image/jpeg';
  SendFile('..\..\..\_\customer.png');
end;

end.
