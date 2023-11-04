unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, JsonDataObjects, MVCFramework.Commons, MVCFramework.HTMX;

type

  [MVCPath('/people')]
  TWebSiteController = class(TMVCController)
  protected
    function GeneratePeopleListAsCSV: String;
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
  public
    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    function PeopleList: String;

    [MVCPath('/search')]
    [MVCHTTPMethods([httpGET])]
    function PeopleSearch(const [MVCFromQueryString('q', '')] SearchText: String): String;

    [MVCPath('/exports/csv')]
    [MVCHTTPMethods([httpGET])]
    function ExportPeopleListAsCSV_API: String;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure SavePerson(
      const [MVCFromContentField('first_name')] FirstName: String;
      const [MVCFromContentField('last_name')] LastName: String;
      const [MVCFromContentField('age', 0)] Age: Integer;
      const [MVCFromContentField('items')] Devices: TArray<String>
    );

    [MVCPath('/delete/($guid)')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeletePerson(const guid: string);

    [MVCPath('/new')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function NewPerson: String;

    [MVCPath('/modal/fordelete/($guid)')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function ShowModalForDelete(guid: string): String;


    [MVCPath('/edit/($guid)')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function EditPerson(guid: string): String;

    [MVCPath]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure Index;

    [MVCPath('/modal')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function ShowModal: String;

    [MVCPath('/mustacheshowcase')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function MustacheTemplateShowCase: String;
  end;

implementation

{ TWebSiteController }

uses DAL, System.SysUtils, Web.HTTPApp;

procedure TWebSiteController.DeletePerson(const guid: string);
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(GUID);
  Context.Response.HXSetPageRefresh(true);
  RenderStatusMessage(HTTP_STATUS.OK);
end;

function TWebSiteController.EditPerson(guid: string): String;
var
  LDAL: IPeopleDAL;
  lPerson: TPerson;
  lDevices: TDeviceList;
  lItem: TDevice;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPerson := LDAL.GetPersonByGUID(guid);
  try
    lDevices := LDAL.GetDevicesList;
    try
      ViewData['person'] := lPerson;
      for lItem in lDevices do
      begin
        lItem.Selected := lPerson.Items.Contains(lItem.DeviceName);
      end;
      ViewData['deviceslist'] := lDevices;
      Result := Page(['editperson']);
    finally
      lDevices.Free;
    end;
  finally
    lPerson.Free;
  end;
end;

function TWebSiteController.ExportPeopleListAsCSV_API: String;
begin
  ContentType := TMVCMediaType.TEXT_CSV;
  Context.Response.CustomHeaders.Values['Content-Disposition'] := 'attachment; filename=people.csv';
  Result := GeneratePeopleListAsCSV;
end;

function TWebSiteController.GeneratePeopleListAsCSV: String;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    Result := GetRenderedView(['people_header.csv', 'people_list.csv']);
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

function TWebSiteController.MustacheTemplateShowCase: String;
var
  LDAL: IPeopleDAL;
  lPeople, lPeople2: TPeople;
  lMyObj: TMyObj;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    lPeople2 := TPeople.Create;
    try
      lMyObj := TMyObj.Create;
      try
        lMyObj.RawHTML := '<h1>This is</h1>Raw<br><span>HTML</span>';
        ViewData['people'] := lPeople;
        ViewData['people2'] := lPeople2;
        ViewData['myobj'] := lMyObj;
        Result := PageFragment(['showcase']); //it is not a "fragment" but we don't need standard headers/footer
      finally
        lMyObj.Free;
      end;
    finally
      lPeople2.Free;
    end;
  finally
    lPeople.Free;
  end;
end;

function TWebSiteController.NewPerson: String;
var
  LDAL: IPeopleDAL;
  lDevices: TDeviceList;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lDevices := LDAL.GetDevicesList;
  try
    ViewData['deviceslist'] := lDevices;
    Result := Page(['editperson']);
  finally
    lDevices.Free;
  end;
end;

procedure TWebSiteController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  SetPagesCommonHeaders(['header']);
  SetPagesCommonFooters(['footer']);
end;

function TWebSiteController.PeopleList: String;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    Result := Page(['people_list_search', 'people_list', 'people_list_bottom']);
  finally
    lPeople.Free;
  end;

end;

function TWebSiteController.PeopleSearch(const SearchText: String): String;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;

  lPeople := LDAL.GetPeople(SearchText);
  try
    ViewData['people'] := lPeople;
    if Context.Request.IsHTMX then
    begin
      Result := PageFragment(['people_list']);
      if SearchText.IsEmpty then
        Context.Response.HXSetPushUrl('/people/search')
      else
        Context.Response.HXSetPushUrl('/people/search?q=' + SearchText);
    end
    else
    begin
      var lJSON := TJSONObject.Create;
      try
        lJSON.S['q'] := SearchText;
        Result := Page(['people_list_search', 'people_list', 'people_list_bottom'], lJSON);
      finally
        lJSON.Free;
      end;
    end;
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.SavePerson(
      const FirstName: String;
      const LastName: String;
      const Age: Integer;
      const Devices: TArray<String>);
var
  LPeopleDAL: IPeopleDAL;
begin
  if FirstName.IsEmpty or LastName.IsEmpty or (Age <= 0) then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data', 'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(FirstName, LastName, Age, Devices);
  Context.Response.HXSetRedirect('/people');
end;

function TWebSiteController.ShowModal: String;
begin
  var lJSON := StrToJSONObject('{"message":"Do you really want to delete row?", "title":"Bootstrap Modal Dialog"}');
  try
    Result := PageFragment(['modal'], lJSON);
  finally
    lJSON.Free;
  end;
end;

function TWebSiteController.ShowModalForDelete(guid: string): String;
begin
  var lJSON := TJsonObject.Create;
  try
    lJSON.S['title'] := 'Bootstrap Modal Dialog';
    lJSON.S['message'] := 'Do you really want to delete row?';
    lJSON.S['guid'] := guid;
    Result := PageFragment(['modal'], lJSON);
  finally
    lJSON.Free;
  end;
end;

end.
