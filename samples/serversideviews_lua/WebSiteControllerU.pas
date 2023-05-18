unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, System.JSON, MVCFramework.Commons;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    procedure GeneratePeopleListAsCSV;
  public
    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure PeopleList;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_CSV)]
    // RESTful API, requires ACCEPT=text/csv
    procedure ExportPeopleListAsCSV_API;

    [MVCPath('/people/formats/csv')]
    [MVCHTTPMethods([httpGET])]
    // Route usable by the browser, doesn't requires ACCEPT=text/csv
    procedure ExportPeopleListAsCSV;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure SavePerson;

    [MVCPath('/deleteperson')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure DeletePerson;

    [MVCPath('/new')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure NewPerson;

    [MVCPath('/edit/($guid)')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure EditPerson(guid: string);

    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure Index;

  end;

implementation

{ TWebSiteController }

uses DAL, System.SysUtils, Web.HTTPApp, JsonDataObjects;

procedure TWebSiteController.DeletePerson;
var
  lGUID: string;
  LDAL: IPeopleDAL;
begin
  lGUID := Context.Request.Params['guid'];
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(lGUID);
  Redirect('/people');
end;

procedure TWebSiteController.EditPerson(guid: string);
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
      LoadView(['header', 'editperson', 'footer']);
      RenderResponseStream;
    finally
      lDevices.Free;
    end;
  finally
    lPerson.Free;
  end;
end;

procedure TWebSiteController.ExportPeopleListAsCSV;
begin
  GeneratePeopleListAsCSV;
  // define the correct behaviour to download the csv inside the browser
  ContentType := TMVCMediaType.TEXT_CSV;
  Context.Response.CustomHeaders.Values['Content-Disposition'] :=
    'attachment; filename=people.csv';
end;

procedure TWebSiteController.ExportPeopleListAsCSV_API;
begin
  GeneratePeopleListAsCSV;
end;

procedure TWebSiteController.GeneratePeopleListAsCSV;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    LoadView(['people_header.csv', 'people_list.csv']);
    RenderResponseStream; // rember to call RenderResponseStream!!!
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

procedure TWebSiteController.NewPerson;
var
  lDAL: IPeopleDAL;
  lDevices: TDeviceList;
  lJSONPerson: TJDOJSONObject;
begin
  lDAL := TServicesFactory.GetPeopleDAL;
  lDevices := LDAL.GetDevicesList;
  try
    ViewData['deviceslist'] := lDevices;
    lJSONPerson := TJDOJsonObject.Create;
    try
      lJSONPerson.S['guid'] := '';
      lJSONPerson.S['first_name'] := '';
      lJSONPerson.S['last_name'] := '';
      lJSONPerson.S['age'] := '';
      ViewData['person'] := lJSONPerson;
      LoadView(['header', 'editperson', 'footer']);
      RenderResponseStream;
    finally
      lJSONPerson.Free;
    end;
  finally
    lDevices.Free;
  end;
end;

procedure TWebSiteController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  ContentType := 'text/html';
  Handled := False;
end;

procedure TWebSiteController.PeopleList;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    LoadView(['header', 'people_list', 'footer']);
    RenderResponseStream; // rember to call RenderResponseStream!!!
  finally
    lPeople.Free;
  end;

end;

procedure TWebSiteController.SavePerson;
var
  LFirstName: string;
  LLastName: string;
  LAge: string;
  LPeopleDAL: IPeopleDAL;
  lDevices: TArray<string>;
begin
  LFirstName := Context.Request.Params['first_name'].Trim;
  LLastName := Context.Request.Params['last_name'].Trim;
  LAge := Context.Request.Params['age'];
  lDevices := Context.Request.ParamsMulti['items'];

  if LFirstName.IsEmpty or LLastName.IsEmpty or LAge.IsEmpty then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data',
      'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(LFirstName, LLastName, LAge.ToInteger(), lDevices);

  Redirect('/people');
end;

end.
