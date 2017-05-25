unit WebSiteControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, System.Diagnostics, System.JSON,
  MVCFramework.Serializer.Commons;

type

  [MVCNameCase(ncLowerCase)]
  TSpeedValue = class
  private
    FValue: string;
    procedure SetValue(const Value: string);
  public
    property Value: string read FValue write SetValue;
    constructor Create(aValue: string);
  end;

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  private
    FStopWatch: TStopwatch;
    function GetSpeed: TSpeedValue;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
  public
    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    procedure PeopleList;

    [MVCPath('/people/($guid)')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeletePerson(const guid: string);

    [MVCPath('/people')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes('application/x-www-form-urlencoded')]
    procedure SavePerson;
    [MVCPath('/newperson')]
    [MVCHTTPMethods([httpGET])]
    procedure NewPerson;
    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    procedure Index;

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

function TWebSiteController.GetSpeed: TSpeedValue;
begin
  Result := TSpeedValue.Create(FStopWatch.Elapsed.TotalMilliseconds.ToString);
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

procedure TWebSiteController.NewPerson;
begin
  PushObjectToView('speed', GetSpeed);
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
  lCookie: TCookie;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  PushObjectToView('people', LDAL.GetPeople);
  PushObjectToView('speed', GetSpeed);
  LoadView(['header', 'people_list', 'footer']);

  // send a cookie with the server datetime at the page rendering
  lCookie := Context.Response.Cookies.Add;
  lCookie.Name := 'lastresponse';
  lCookie.Value := DateTimeToStr(now);
  lCookie.Expires := 0; // session cookie
  // END cookie sending

  RenderResponseStream; // rember to call render!!!
end;

procedure TWebSiteController.SavePerson;
var
  lFirstName: string;
  lLastName: string;
  lAge: string;
  lPeopleDAL: IPeopleDAL;
  lItems: TArray<string>;
begin
  lItems := Context.Request.ParamsMulti['items'];
  lFirstName := Context.Request.Params['first_name'].Trim;
  lLastName := Context.Request.Params['last_name'].Trim;
  lAge := Context.Request.Params['age'];

  if LFirstName.IsEmpty or LLastName.IsEmpty or LAge.IsEmpty then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data',
      'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(LFirstName, LLastName, LAge.ToInteger(), lItems);

  Redirect('/people');
end;

{ TSpeedValue }

constructor TSpeedValue.Create(aValue: string);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TSpeedValue.SetValue(const Value: string);
begin
  FValue := Value;
end;

end.
