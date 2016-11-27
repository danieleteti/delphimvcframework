unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, System.JSON, MVCFramework.Commons;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  private
    FStopWatch: TStopwatch;
    function GetSpeed: TJSONString;
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

uses DAL, System.SysUtils, Web.HTTPApp;

procedure TWebSiteController.DeletePerson;
var
  lID: string;
  LDAL: IPeopleDAL;
begin
  lID := Context.Request.Params['id'];
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteById(lID);
  Redirect('/people');
end;

procedure TWebSiteController.EditPerson(guid: string);
var
  LDAL: IPeopleDAL;
  lPerson: TJSONObject;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPerson := LDAL.GetPersonByID(guid);
  PushJSONToView('person', lPerson);
  PushJSONToView('speed', GetSpeed);
  LoadView(['header', 'editperson', 'footer']);
  RenderResponseStream;
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
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  PushJSONToView('people', LDAL.GetPeople);
  LoadView(['people_header.csv', 'people_list.csv']);
  RenderResponseStream; // rember to call RenderResponseStream!!!
end;

function TWebSiteController.GetSpeed: TJSONString;
begin
  Result := TJSONString.Create(FStopWatch.Elapsed.TotalMilliseconds.ToString);
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

procedure TWebSiteController.NewPerson;
begin
  PushJSONToView('speed', GetSpeed);
  LoadView(['header', 'editperson', 'footer']);
  RenderResponseStream;
end;

procedure TWebSiteController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  ContentType := 'text/html';
  Handled := False;
  FStopWatch := TStopwatch.StartNew;
end;

procedure TWebSiteController.PeopleList;
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  PushJSONToView('people', LDAL.GetPeople);
  PushJSONToView('speed', GetSpeed);
  LoadView(['header', 'people_list', 'footer']);
  RenderResponseStream; // rember to call RenderResponseStream!!!
end;

procedure TWebSiteController.SavePerson;
var
  LFirstName: string;
  LLastName: string;
  LAge: string;
  LPeopleDAL: IPeopleDAL;
begin
  LFirstName := Context.Request.Params['first_name'].Trim;
  LLastName := Context.Request.Params['last_name'].Trim;
  LAge := Context.Request.Params['age'];

  if LFirstName.IsEmpty or LLastName.IsEmpty or LAge.IsEmpty then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data',
      'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(LFirstName, LLastName, LAge.ToInteger());

  Redirect('/people');
end;

end.
