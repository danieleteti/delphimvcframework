unit WebSiteControllerU;

interface

uses
  MVCFramework,  MVCFramework.Commons, System.Diagnostics, System.JSON;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  private
    FStopWatch: TStopwatch;
    function GetSpeed: TJSONString;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
  public
    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    procedure PeopleList(CTX: TWebContext);

    [MVCPath('/people/($guid)')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeletePerson(const guid: string);

    [MVCPath('/people')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes('application/x-www-form-urlencoded')]
    procedure SavePerson(CTX: TWebContext);
    [MVCPath('/newperson')]
    [MVCHTTPMethods([httpGET])]
    procedure NewPerson(CTX: TWebContext);
    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    procedure Index(CTX: TWebContext);

  end;

implementation

{ TWebSiteController }

uses DAL, System.SysUtils, Web.HTTPApp;

procedure TWebSiteController.DeletePerson(const guid: string);
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(guid);
end;

function TWebSiteController.GetSpeed: TJSONString;
begin
  Result := TJSONString.Create(FStopWatch.Elapsed.TotalMilliseconds.ToString);
end;

procedure TWebSiteController.Index(CTX: TWebContext);
begin
  Redirect('/people');
end;

procedure TWebSiteController.NewPerson(CTX: TWebContext);
begin
  PushToView('speed', GetSpeed.ToJSON);
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

procedure TWebSiteController.PeopleList(CTX: TWebContext);
var
  LDAL: IPeopleDAL;
  lCookie: TCookie;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  PushToView('people', LDAL.GetPeople.ToJSON);
  PushToView('speed', GetSpeed.ToJSON);
  LoadView(['header', 'people_list', 'footer']);

  // send a cookie with the server datetime at the page rendering
  lCookie := CTX.Response.Cookies.Add;
  lCookie.Name := 'lastresponse';
  lCookie.Value := DateTimeToStr(now);
  lCookie.Expires := 0; // session cookie
  // END cookie sending

  RenderResponseStream; // rember to call render!!!
end;

procedure TWebSiteController.SavePerson(CTX: TWebContext);
var
  LFirstName: string;
  LLastName: string;
  LAge: String;
  LPeopleDAL: IPeopleDAL;
begin
  LFirstName := CTX.Request.Params['first_name'].Trim;
  LLastName := CTX.Request.Params['last_name'].Trim;
  LAge := CTX.Request.Params['age'];

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
