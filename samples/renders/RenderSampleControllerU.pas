// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit RenderSampleControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  System.Rtti;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;
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
    [MVCPath('/people/withtiming')]
    [MVCProduces('application/json')]
    procedure GetPeopleWithTiming;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/lotofobjects')]
    procedure GetLotOfPeople;

    //this action is polymorphic
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
    [MVCProduces('text/html')]
    procedure GetPerson_AsHTML(CTX: TWebContext);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers.csv')]
    procedure GetPeopleAsCSV;


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

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customserializationtype')]
    procedure GetCustomSerializationType;

  end;

implementation

uses
  BusinessObjectsU,
  Generics.Collections,
  MVCFramework.DataSet.Utils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Defaults,
  MyDataModuleU,
  System.Classes,
  System.SysUtils,
  WebModuleU,
  CustomTypesU,
  InMemoryDataU,
  JsonDataObjects,
  MVCFramework.Serializer.JsonDataObjects;

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

procedure TRenderSampleController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;

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
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    lDM.qryCountry.Open;

    lJObj := TJSONObject.Create;
    try
      // We need a non standard representation, let's create a specific serializer.
      lSer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSer.DataSetToJsonArray(lDM.qryCustomers, lJObj.A['customers'], TMVCNameCase.ncLowerCase, []);
        lSer.DataSetToJsonArray(lDM.qryCountry, lJObj.A['countries'], TMVCNameCase.ncLowerCase, []);
      finally
        lSer.Free;
      end;
      Render(lJObj);
    except // avoid memory leaks in case of exceptions
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

procedure TRenderSampleController.GetCustomSerializationType;
begin
  // TSysUser contains a type with a custom serializer
  Render(TSysUser.Create('daniele', ['poweruser', 'role1', 'role2']), True);
end;

procedure TRenderSampleController.GetLotOfPeople;
begin
  Render<TPerson>(GetPeopleList, False);
end;

procedure TRenderSampleController.GetPerson_AsHTML(CTX: TWebContext);
begin
  ResponseStream
    .Append('<html><body><ul>')
    .Append('<li>FirstName: Daniele</li>')
    .Append('<li>LastName: Teti')
    .AppendFormat('<li>DOB: %s</li>', [DateToISODate(EncodeDate(1975, 5, 2))])
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

procedure TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList;
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

procedure TRenderSampleController.GetPeopleAsCSV;
begin
  ResponseStream.AppendLine('first_name;last_name;age');
  ResponseStream.AppendLine('Daniele;Teti;38');
  ResponseStream.AppendLine('Peter;Parker;22');
  ResponseStream.AppendLine('Bruce;Banner;60');
  ContentType := TMVCMediaType.TEXT_CSV;
  RenderResponseStream;
end;

procedure TRenderSampleController.GetPeopleWithTiming;
var
  p: TPerson;
  People: TPeopleWithMetadata;
begin
  People := TPeopleWithMetadata.Create;
  try
    People.Metadata.StartProcessing := Now;

    {$REGION 'Fake data'}
    Sleep(1000); //processing...

    p := TPerson.Create;
    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.DOB := EncodeDate(1979, 11, 4);
    p.Married := True;
    People.Items.Add(p);

    p := TPerson.Create;
    p.FirstName := 'John';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1879, 10, 2);
    p.Married := False;
    People.Items.Add(p);

    p := TPerson.Create;
    p.FirstName := 'Jane';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1883, 1, 5);
    p.Married := True;
    People.Items.Add(p);

    {$ENDREGION}

    People.Metadata.CustomData := Format('There are %d people in the list', [People.Items.Count]);
    People.Metadata.StopProcessing := Now;
    Render(People, False);
  finally
    People.Free;
  end;
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
  p.S['FirstName'] := 'Daniele';
  p.S['LastName'] := 'Teti';
  p.S['DOB'] := DateToISODate(EncodeDate(1975, 5, 2));
  p.B['Married'] := True;
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
