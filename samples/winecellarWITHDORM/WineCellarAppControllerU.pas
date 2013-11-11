unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MainDataModuleUnit,
  dorm, dorm.loggers;

type

  [MVCPath('/')]
  TWineCellarApp = class(TMVCController)
  private
    dm: TWineCellarDataModule;
    dormSession: TSession;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean);
      override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionNAme: string); override;

  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);

    [MVCPath('/wines')]
    [MVCHTTPMethod([httpGET])]
    procedure WinesList(ctx: TWebContext);

    [MVCPath('/wines')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveWine(ctx: TWebContext);

    [MVCPath('/wines/search/($value)')]
    procedure FindWines(ctx: TWebContext);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpGET, httpDELETE])]
    procedure WineById(ctx: TWebContext);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateWineById(ctx: TWebContext);
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Classes, dorm.Commons, WinesBO,
  ObjectsMappers, dorm.ObjectStatus, dorm.Query, System.Generics.Collections;

procedure TWineCellarApp.FindWines(ctx: TWebContext);
var
  Wines: TObjectList<TWine>;
begin
  Wines := dormSession.LoadListSQL<TWine>(Select.From(TWine).Where('#TWine.name# containing ? order by #TWine.ID#',
    [ctx.Request.Params['value']]));
  Render<TWine>(Wines);
end;

procedure TWineCellarApp.Index(ctx: TWebContext);
begin
  Redirect('/index.html');
end;

procedure TWineCellarApp.OnAfterAction(Context: TWebContext;
  const AActionNAme: string);
begin
  inherited;
  dm.Free;
end;

procedure TWineCellarApp.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string;
  var Handled: Boolean);
begin
  inherited;
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create('dorm.conf', TEncoding.ASCII), deDevelopment);
  dm := TWineCellarDataModule.Create(nil);
end;

procedure TWineCellarApp.SaveWine(ctx: TWebContext);
var
  Wine: TWine;
begin
  Wine := Mapper.JSONObjectToObject<TWine>(ctx.Request.BodyAsJSONObject);
  dormSession.Persist(Wine);
  dormSession.Commit();
  // dm.AddWine(ctx.Request.BodyAsJSONObject);
end;

procedure TWineCellarApp.UpdateWineById(ctx: TWebContext);
var
  Wine: TWine;
begin
  Wine := Mapper.JSONObjectToObject<TWine>(ctx.Request.BodyAsJSONObject);
  Wine.ObjStatus := osDirty;
  dormSession.Persist(Wine);
  dormSession.Commit();
  Render(200, 'Wine updated');
end;

procedure TWineCellarApp.WineById(ctx: TWebContext);
var
  Wine: TWine;
begin
  Wine := dormSession.Load<TWine>(ctx.Request.ParamsAsInteger['id']);

  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      begin
        Wine.ObjStatus := osDeleted;
        dormSession.Persist(Wine);
        dormSession.Commit();
        Render(200, 'Wine deleted');
      end;
    httpGET:
      Render(Wine);
  else
    raise Exception.Create('Invalid http method for action');
  end;
end;

procedure TWineCellarApp.WinesList(ctx: TWebContext);
begin
  Render<TWine>(dormSession.ListAll<TWine>);
end;

end.
