unit ActionFiltersControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TActionFiltersController = class(TMVCController)
  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;

    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionNAme: string);
      override;

  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/people/($id)')]
    [MVCProduces('application/json')]
    { This action cannot be called by a browser address bar because requires the
      ACCEPT header to be application/json. Use Postman or RAD Studio's RESTDebugger. }
    procedure GetPerson(id: Integer);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, MVCFramework.Logger;

{ TActionFiltersController }

procedure TActionFiltersController.GetPerson(id: Integer);
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

procedure TActionFiltersController.MVCControllerAfterCreate;
begin
  inherited;
  // raise Exception.Create('Error Message');
  Log.Info('MVCControllerAfterCreate', 'ACTIONFILTERS');
end;

procedure TActionFiltersController.MVCControllerBeforeDestroy;
begin
  inherited;
end;

procedure TActionFiltersController.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  inherited;
  Log.Info('ACTION CALLED: ' + AActionNAme +
    ' mapped to ' + Context.Request.PathInfo +
    ' from ' + Context.Request.ClientIP, 'ACTIONFILTERS');
end;

procedure TActionFiltersController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  if DayOfWeek(date) in [1, 7] then
    raise Exception.Create('You cannot use this service in the WeekEnd');
  // if handled = true (or exception raised) then actual action will not be called
end;

end.
