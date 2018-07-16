unit PeopleControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  PeopleModuleU,
  Redis.Commons,
  MVCFramework.Controllers.CacheController;

type

  [MVCPath('/people')]
  TPeopleController = class(TMVCCacheController)
  private
    FPeopleM: TPeopleModule;
  protected
    function PeopleModule: TPeopleModule;
    procedure OnAfterAction(Context: TWebContext;
      const AActionNAme: string); override;
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
  public
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPersonByID(id: Integer);

    [MVCPath('/($id)/photo')]
    [MVCHTTPMethod([httpGET])]
    procedure GetPhotoByID(id: Integer);

    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes('application/json')]
    procedure CreatePerson(CTX: TWebContext);

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    [MVCConsumes('application/json')]
    procedure UpdatePerson(CTX: TWebContext);

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeletePerson(CTX: TWebContext);

    [MVCPath('/searches')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes('application/json')]
    procedure SearchPeople;

    [MVCPath('/tests/people')]
    [MVCHTTPMethod([httpGET])]
    procedure GetLotOfPeople;

    [MVCPath('/tests/2')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateBulkData(CTX: TWebContext);
  end;

implementation

uses
  PersonBO, SysUtils, Data.DBXJSON, System.Math, CommonsU,
  System.Classes, System.JSON, Redis.Client, Redis.NetLib.INDY, System.Generics.Collections,
  Redis.Values, MVCFramework.SystemJSONUtils, MVCFramework.Serializer.Defaults;

{ TPeopleController }

procedure TPeopleController.GetLotOfPeople;
var
  lPerson: TPerson;
  I: Integer;
  lPeople: TObjectList<TPerson>;
const
  CACHE_KEY: string = 'cache::lotofpeople';
  FIRST_NAMES: array of string = ['Daniele', 'Peter', 'Bruce', 'Scott', 'Sue'];
  LAST_NAMES: array of string = ['Teti', 'Parker', 'Banner', 'Summers',
    'Storm'];

begin
  SetCacheKey('cache::lotofpeople');
  if CacheAvailable then
    Exit;

  lPeople := TObjectList<TPerson>.Create(True);
  try
    for I := 1 to 3000 do
    begin
      lPerson := TPerson.Create;
      lPeople.Add(lPerson);
      lPerson.FIRST_NAME := FIRST_NAMES[RandomRange(0, 5)];
      lPerson.LAST_NAME := LAST_NAMES[RandomRange(0, 5)];
      lPerson.WORK_PHONE_NUMBER :=
        IntToStr(100000 + RandomRange(200000, 999999));
      lPerson.MOBILE_PHONE_NUMBER :=
        IntToStr(100000 + RandomRange(200000, 999999));
      lPerson.EMAIL := FIRST_NAMES[RandomRange(0, 5)] + '@' + LAST_NAMES
        [RandomRange(0, 5)] + '.com';
    end;
  except
    lPeople.Free;
    raise;
  end;
  Render<TPerson>(lPeople);
  SetCache(60);
end;

procedure TPeopleController.CreateBulkData(CTX: TWebContext);
var
  Person: TPerson;
  I: Integer;
const
  FIRST_NAMES: array of string = ['Daniele', 'Peter', 'Bruce', 'Scott', 'Sue'];
  LAST_NAMES: array of string = ['Teti', 'Parker', 'Banner', 'Summers',
    'Storm'];

begin
  Person := TPerson.Create;
  try
    for I := 1 to 10000 do
    begin
      Person.FIRST_NAME := FIRST_NAMES[RandomRange(0, 5)];
      Person.LAST_NAME := LAST_NAMES[RandomRange(0, 5)];
      Person.WORK_PHONE_NUMBER :=
        IntToStr(100000 + RandomRange(200000, 999999));
      Person.MOBILE_PHONE_NUMBER :=
        IntToStr(100000 + RandomRange(200000, 999999));
      Person.EMAIL := FIRST_NAMES[RandomRange(0, 5)] + '@' + LAST_NAMES
        [RandomRange(0, 5)] + '.com';
      PeopleModule.CreatePerson(Person);
    end;
  finally
    Person.Free;
  end;
end;

procedure TPeopleController.CreatePerson(CTX: TWebContext);
var
  Person: TPerson;
begin
  Person := CTX.Request.BodyAs<TPerson>;
  try
    PeopleModule.CreatePerson(Person);
    CTX.Response.Location := '/people/' + Person.id.ToString;
    Render(201, 'Person created');
  finally
    Person.Free;
  end;
end;

procedure TPeopleController.UpdatePerson(CTX: TWebContext);
var
  Person: TPerson;
begin
  Person := CTX.Request.BodyAs<TPerson>;
  try
    Person.id := CTX.Request.ParamsAsInteger['id'];
    PeopleModule.UpdatePerson(Person);
    Render(200, 'Person updated');
  finally
    Person.Free;
  end;
end;

procedure TPeopleController.DeletePerson(CTX: TWebContext);
begin
  PeopleModule.DeletePerson(CTX.Request.ParamsAsInteger['id']);
  Render(204, 'Person deleted');
end;

procedure TPeopleController.GetPersonByID(id: Integer);
var
  Person: TPerson;
begin
  // This action put in cache the response for 10 seconds...

  SetCacheKey('cache::people::' + id.ToString);
  if CacheAvailable then
    Exit;

  Person := PeopleModule.GetPersonByID(id);
  if Assigned(Person) then
    Render(Person)
  else
    Render(404, 'Person not found');

  SetCache(120);
end;

procedure TPeopleController.GetPhotoByID(id: Integer);
begin
  // This action put in cache the response (which is binary) for 30 seconds

  SetCacheKey('cache::photo::' + id.ToString);
  if CacheAvailable then
    Exit;

  SendStream(PeopleModule.GetPhotoByID(id));
  ContentType := 'image/png';

  SetCache(30); // the photo will be in cache for 30 seconds
end;

procedure TPeopleController.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  inherited;
  FPeopleM.Free;
end;

procedure TPeopleController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  // Setting CacheEnabled to false will be disable the cache for all the controller actions
  CacheEnabled := True;
end;

function TPeopleController.PeopleModule: TPeopleModule;
begin
  if FPeopleM = nil then
    FPeopleM := TPeopleModule.Create(nil);
  Result := FPeopleM;
end;

procedure TPeopleController.SearchPeople;
var
  Filters: TJSONObject;
  SearchText: string;
  CurrPage: Integer;
  lResponseBody: string;
begin
  Filters := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
  try
    if not Assigned(Filters) then
      raise Exception.Create('Invalid search parameters');
    SearchText := TSystemJSON.GetStringDef(Filters, 'text');
    CurrPage := System.Math.Max(1, StrToIntDef(Context.Request.Params['page'], 1));

    // define the redis key depending by the search term and the requested page
    SetCacheKey('searches::' + SearchText + '::' + CurrPage.ToString);
    // get content from cache
    if CacheAvailable then
    begin
      // if CacheAvailable returns true, then the response object is already
      // initialized with the cache contents, so just exit
      Exit;
    end;

    // we know that the cached version of the response is not available,
    // let's create the response from scratch

    lResponseBody := GetDefaultSerializer.SerializeCollection(PeopleModule.FindPeople(SearchText, CurrPage));
    ResponseStream.Append(lResponseBody);
    RenderResponseStream;
    // JSON := Mapper.ObjectListToJSONArray<TPerson>(PeopleModule.FindPeople(SearchText, CurrPage), True,
    // procedure(const Item: TJSONObject)
    // var
    // id: string;
    // begin
    // id := (Item.GetValue('ID') as TJSONNumber).AsInt.ToString;
    // Item.AddPair('_personuri', '/people/' + id);
    // Item.AddPair('_personphotouri', '/people/photo/' + id);
    // end);
    // Render(JSON.ToJSON);
    // JSON.Free;

    MergePaginationMetainfos('/people/searches?page=%d', Context.Response.CustomHeaders, CurrPage);

    // Here the response object has been correctly populated.
    // Set the cache using the current response object values
    // and let expires in 20 seconds
    SetCache(20);
  finally
    Filters.Free;
  end;
end;

end.
