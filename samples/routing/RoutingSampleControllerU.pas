unit RoutingSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRoutingSampleController = class(TMVCController)
  public
    [MVCPath('/')]
    procedure Index(CTX: TWebContext);

    { This action requires that the ACCEPT header is text/plain to be invocated }
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/searches/($searchtext)')]
    [MVCProduces('text/plain', 'UTF-8')]
    procedure SearchCustomers(CTX: TWebContext);

    { This action requires that the ACCEPT header is application/json to be invocated }
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    { double MVCPath }
    [MVCPath('/($id)')]
    [MVCProduces('application/json')]
    procedure GetPerson(const id: Integer);

    [MVCHTTPMethod([httpDelete])]
    [MVCPath('/people/($id)')]
    procedure DeletePerson(const id: Integer);

    { To be invocated this action requires that:
      - the CONTENT-TYPE header is application/json and
      - that the ACCEPT header is application/json
    }
    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people')]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    procedure CreatePerson;

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, System.JSON;

{ TRoutingSampleController }

procedure TRoutingSampleController.CreatePerson;
var
  lJObj: TJSONObject;
begin
  lJObj := Context.Request.BodyAsJSONObject.Clone as TJSONObject;
  lJObj.AddPair('server_datetime', DateTimeToStr(now));
  Render(lJObj);
end;

procedure TRoutingSampleController.DeletePerson(const id: Integer);
begin
  { Here you should do something with id }
  // RemovePerson(ID)
  Render(204 { 'No content' } , 'Person deleted');
end;

procedure TRoutingSampleController.GetPerson(const id: Integer);
var
  P: TPerson;
begin
  {
    Use ID to load the person from a database...
    In this example, we're creating a fake person
  }
  P := TPerson.Create;
  P.FirstName := 'Daniele';
  P.LastName := 'Teti';
  P.DOB := EncodeDate(1975, 5, 2);
  P.Married := True;
  Render(P);
end;

procedure TRoutingSampleController.Index(CTX: TWebContext);
begin
  Render('This is the root path');
end;

procedure TRoutingSampleController.SearchCustomers(CTX: TWebContext);
var
  search: string;
  Page: Integer;
  orderby: string;
  S: string;
begin
  search := CTX.Request.Params['searchtext'];
  Page := 1;
  if CTX.Request.QueryStringParamExists('page') then
    Page := StrToInt(CTX.Request.QueryStringParam('page'));
  orderby := '';
  if CTX.Request.QueryStringParamExists('order') then
    orderby := CTX.Request.QueryStringParam('order');
  S := Format('SEARCHTEXT: "%s" - PAGE: %d - ORDER BY FIELD: "%s"',
    [search, Page, orderby]);
  ResponseStream.AppendLine(S).AppendLine(StringOfChar('*', 30))
    .AppendLine('1. Daniele Teti').AppendLine('2. John Doe')
    .AppendLine('3. Mark Rossi').AppendLine('4. Jack Verdi')
    .AppendLine(StringOfChar('*', 30));
  Render;
end;

end.
