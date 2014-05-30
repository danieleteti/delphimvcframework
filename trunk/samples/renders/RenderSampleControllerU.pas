unit RenderSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers, System.JSON;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetCustomers_AsDataSet(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/skilledpeople')]
    // [MVCProduces('application/json')]
    procedure GetProgrammersAndPhilosophersAsObjectList(CTX: TWebContext);

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

procedure TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList(
  CTX: TWebContext);
var
  List: TObjectList<TPerson>;
  p: TProgrammer;
  ph: TPhilosopher;
begin
  List := TObjectList<TPerson>.Create(True);
  p := TProgrammer.Create;
  p.Married := True;
  p.FirstName := 'Peter';
  p.LastName := 'Parker';
  p.Skills := 'Delphi, JavaScript, Python, C++';
  List.Add(p);
  ph := TPhilosopher.Create;
  p.Married := False;
  ph.FirstName := 'Bruce';
  ph.LastName := 'Banner';
  ph.Mentors := 'Abbagnano, Algarotti, Cavalieri, Pareyson';
  List.Add(ph);
  p := TProgrammer.Create;
  p.Married := False;
  p.FirstName := 'Sue';
  p.LastName := 'Storm';
  p.Skills := 'Delphi, JavaScript';
  List.Add(p);
  Render<TPerson>(List);
end;

procedure TRenderSampleController.GetPeople_AsObjectList(CTX: TWebContext);
var
  p: TPerson;
  People: TObjectList<TPerson>;
begin
  People := TObjectList<TPerson>.Create(True);

{$REGION 'Fake data'}
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := 'Teti';
  p.DOB := EncodeDate(1979, 11, 4);
  p.Married := True;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'John';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1879, 10, 2);
  p.Married := False;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'Jane';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1883, 1, 5);
  p.Married := True;
  People.Add(p);
{$ENDREGION}
  Render<TPerson>(People);
  // or if you want to be more opne to future extension
  // RenderListAsProperty<TPerson>('people', People);
end;

procedure TRenderSampleController.GetPersonJSON(CTX: TWebContext);
var
  p: TJSONObject;
begin
  p := TJSONObject.Create;
  p.AddPair('FirstName', 'Daniele');
  p.AddPair('LastName', 'Teti');
  p.AddPair('DOB', ISODateToString(EncodeDate(1975, 5, 2)));
  p.AddPair('Married', TJSONTrue.Create);
  Render(p);
end;

procedure TRenderSampleController.GetPersonPhoto(CTX: TWebContext);
begin
  // ContentType := 'image/jpeg';
  SendFile('..\..\..\_\customer.png');
end;

end.
