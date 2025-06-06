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
    function PeopleSearch(const [MVCFromQueryString('q', '')] SearchText: String): String;

    [MVCPath('/exports/csv')]
    [MVCHTTPMethods([httpGET])]
    function ExportPeopleListAsCSV_API: String;

    [MVCPath]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure SavePerson(
      const [MVCFromContentField('guid','')] GUID: String;
      const [MVCFromContentField('first_name', '')] FirstName: String;
      const [MVCFromContentField('last_name', '')] LastName: String;
      const [MVCFromContentField('age', 0)] Age: Integer;
      const [MVCFromContentField('items')] Devices: TArray<String>;
      const [MVCFromContentField('csrf_token', '')] CSRF: String
    );

    [MVCPath('/($guid)')]
    [MVCHTTPMethods([httpDELETE])]
    function DeletePerson(const guid: string): String;

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
  end;

implementation

{ TWebSiteController }

uses DAL, System.SysUtils, Web.HTTPApp, MVCFramework.Utils;

function TWebSiteController.DeletePerson(const guid: string): String;
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(GUID);
  Context.Response.HXSetReplaceUrl('/people');
  Result := PeopleSearch('');
end;

function TWebSiteController.EditPerson(guid: string): String;
var
  LDAL: IPeopleDAL;
  lPerson: TPerson;
  lDevices: TDeviceList;
  lItem: TDevice;
  lICSRFTokenManager: ICSRFTokenManager;
  lToken: string;
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
      lICSRFTokenManager := TCSRFTokenManager.Create;
      lToken := lICSRFTokenManager.GenerateToken(CSRF_SECRET, CSRF_SECONDS_TIMEOUT);
      Session['csrf_token'] := lToken;
      ViewData['csrf_token'] := lToken;
      ViewData['deviceslist'] := lDevices;
      Result := RenderView('editperson');
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
    Result := RenderView('people_list.csv');
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

function TWebSiteController.NewPerson: String;
var
  LDAL: IPeopleDAL;
  lICSRFTokenManager: ICSRFTokenManager;
  lDevices: TDeviceList;
  lToken: string;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lDevices := LDAL.GetDevicesList;
  try
    lICSRFTokenManager := TCSRFTokenManager.Create();
    lToken := lICSRFTokenManager.GenerateToken(CSRF_SECRET, CSRF_SECONDS_TIMEOUT);
    Session['csrf_token'] := lToken;
    ViewData['csrf_token'] := lToken;
    ViewData['deviceslist'] := lDevices;
    Result := RenderView('editperson');
  finally
    lDevices.Free;
  end;
end;

procedure TWebSiteController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  ViewData['ishtmx'] := AContext.Request.IsHTMX;
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
      if SearchText.IsEmpty then
        Context.Response.HXSetPushUrl('/people')
      else
        Context.Response.HXSetPushUrl('/people?q=' + SearchText);
    end;
    ViewData['q'] := SearchText;
    Result := RenderView('people_list');
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.SavePerson(
      const GUID: String;
      const FirstName: String;
      const LastName: String;
      const Age: Integer;
      const Devices: TArray<String>;
      const CSRF: String);
var
  LPeopleDAL: IPeopleDAL;
begin
  var lCSRF: ICSRFTokenManager := TCSRFTokenManager.Create;
  if (Session['csrf_token'] <> CSRF) or (lCSRF.IsTokenExpired(CSRF_SECRET, CSRF)) then
  begin
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Data entry is expired or has been tampered. Please, restart editing.');
  end;

  if FirstName.IsEmpty or LastName.IsEmpty or (Age <= 0) then
  begin
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'First name, last name and age are not optional');
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  if not GUID.IsEmpty then
    LPeopleDAL.DeleteByGUID(GUID);
  LPeopleDAL.AddPerson(FirstName, LastName, Age, Devices);
  Context.Response.HXSetRedirect('/people');
end;

function TWebSiteController.ShowModal: String;
begin
  ViewData['message'] := 'Do you really want to delete row?';
  ViewData['title'] := 'Bootstrap Modal Dialog';
  Result := RenderView('modal');
end;

function TWebSiteController.ShowModalForDelete(guid: string): String;
begin
  ViewData['title'] := 'Bootstrap Modal Dialog';
  ViewData['message'] := 'Do you really want to delete row?';
  ViewData['guid'] := guid;
  Result := RenderView('modal');
end;

end.
