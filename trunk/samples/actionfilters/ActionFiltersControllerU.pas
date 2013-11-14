unit ActionFiltersControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

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
    procedure GetPerson(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, MVCFramework.Logger;

{ TActionFiltersController }

procedure TActionFiltersController.GetPerson(CTX: TWebContext);
var
  P: TPerson;
  IDPerson: Integer;
begin
  IDPerson := CTX.Request.ParamsAsInteger['id'];
  {
    Use IDPerson to load the person from a database...
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
  // some safe initialization... do not raise exception here
end;

procedure TActionFiltersController.MVCControllerBeforeDestroy;
begin
  inherited;
  // finalizations...
end;

procedure TActionFiltersController.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  inherited;
  Log('ACTION CALLED: ' + AActionNAme +
    ' mapped to ' + Context.Request.PathInfo +
    ' from ' + Context.Request.ClientIP);
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
