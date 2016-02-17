unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, System.JSON;

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

uses DAL, System.SysUtils, MVCFramework.Commons;

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
  PushJSONToView('speed', GetSpeed);
  LoadView(['header', 'editperson', 'footer']);
  Render;
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
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  PushJSONToView('people', LDAL.GetPeople);
  PushJSONToView('speed', GetSpeed);
  LoadView(['header', 'people_list', 'footer']);
  Render; // rember to call render!!!
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
    { TODO -oDaniele -cGeneral : Show how to propertly render an exception }
    raise EMVCException.Create('Invalid data',
      'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(LFirstName, LLastName, LAge.ToInteger());
  Redirect('/people');
end;

end.
