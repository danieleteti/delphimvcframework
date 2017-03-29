unit RenderSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers, System.JSON;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($id)')]
    [MVCProduces('text/plain')]
    procedure GetPerson_AsText(const id: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetCustomers_AsDataSet(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/multi')]
    [MVCProduces('application/json')]
    procedure GetCustomersAndCountry_AsDataSet;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/skilledpeople')]
    [MVCProduces('application/json')]
    procedure GetProgrammersAndPhilosophersAsObjectList;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/view/($id).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetPerson_AsHTMLView;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($id).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetPerson_AsHTML(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/unicode/($id).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetUnicodeText_AsHTML(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($id)')]
    [MVCProduces('application/json')]
    procedure GetCustomerByID_AsTObject(const id: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/files/customers.json')]
    [MVCProduces('application/json')]
    procedure GetPersonJSON;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/files/customers.png')]
    [MVCProduces('image/png')]
    procedure GetPersonPhoto;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/images/customers/($id)')]
    procedure GetPersonPhotoAsStream(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/exception')]
    procedure RaiseException;

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, WebModuleU,
  Generics.Collections,
  System.Classes, MyDataModuleU;

{ TRoutingSampleController }

procedure TRenderSampleController.GetUnicodeText_AsHTML(CTX: TWebContext);
var
  s: string;
begin
  s := '<html><body>';
  s := s + '什么是Unicode(统一码)? in Simplified Chinese <br>';
  s := s + 'Što je Unicode? in Croatian <br>';
  s := s + 'Co je Unicode? in Czech';
  s := s + '</body></html>';
  Render(s);
end;

procedure TRenderSampleController.RaiseException;
var
  a: Integer;
begin
  a := 0;
  Render(IntToStr(10 div a));
end;

procedure TRenderSampleController.GetCustomerByID_AsTObject(const id: Integer);
var
  Cust: TCustomer;
begin
  if id = 7 then // just a sample
    Render(HTTP_STATUS.NotFound, 'Customer Not Found')
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

procedure TRenderSampleController.GetCustomersAndCountry_AsDataSet;
var
  lDM: TMyDataModule;
  lJObj: TJSONObject;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    lDM.qryCountry.Open;
    lJObj := TJSONObject.Create;
    try
      lJObj.AddPair('customers', lDM.qryCustomers.AsJSONArray);
      lJObj.AddPair('countries', lDM.qryCountry.AsJSONArray);
      Render(lJObj);
    except // avoid memory leaks
      lJObj.Free;
      raise;
    end;
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomers_AsDataSet(CTX: TWebContext);
var
  lDM: TMyDataModule;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    Render(lDM.qryCustomers);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetPerson_AsHTML(CTX: TWebContext);
begin
  ResponseStream
    .Append('<html><body><ul>')
    .Append('<li>FirstName: Daniele</li>')
    .Append('<li>LastName: Teti')
    .AppendFormat('<li>DOB: %s</li>', [ISODateToString(EncodeDate(1975, 5, 2))])
    .Append('<li>Married: yes</li>')
    .Append('</ul></body></html>');
  RenderResponseStream;
end;

procedure TRenderSampleController.GetPerson_AsHTMLView;
var
  Cust: TCustomer;
begin
  Cust := TCustomer.Create;
  Cust.Name := 'Daniele Teti Inc.';
  Cust.ContactFirst := 'Daniele';
  Cust.ContactLast := 'Teti';
  Cust.AddressLine1 := 'Rome Street 12';
  Cust.AddressLine2 := '00100';
  Cust.City := 'ROME';
  PushObjectToView('customer', Cust);
  LoadView(['header', 'customer', 'footer']);
  RenderResponseStream;
  { If you need more flexibility, you can use GetRenderedView to compose your
    output using small views.
    Here's an example:

    ContentType := TMVCMediaType.TEXT_HTML;
    Render(GetRenderedView(['header', 'customer','footer']));
  }
end;

procedure TRenderSampleController.GetPerson_AsText(const id: Integer);
begin
  ResponseStream
    .AppendLine('ID        :  ' + id.ToString)
    .AppendLine('FirstName : Daniele')
    .AppendLine('LastName  : Teti')
    .AppendLine('DOB       : ' + DateToStr(EncodeDate(1979, 5, 2)))
    .AppendLine('Married   : yes');
  RenderResponseStream;
end;

procedure TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList
  ;
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

procedure TRenderSampleController.GetPeople_AsObjectList;
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
end;

procedure TRenderSampleController.GetPersonJSON;
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

procedure TRenderSampleController.GetPersonPhoto;
begin
  // ContentType := 'image/jpeg';
  SendFile('..\..\_\customer.png');
end;

procedure TRenderSampleController.GetPersonPhotoAsStream(CTX: TWebContext);
var
  LPhoto: TFileStream;
begin
  LPhoto := TFileStream.Create('..\..\_\customer.png',
    fmOpenRead or fmShareDenyWrite);
  ContentType := 'image/png'; // you can also use MVCProduces attribute

  // LPhoto is a plain TStream descendant, so it can be rendered as usual
  Render(LPhoto, True);
end;

end.
