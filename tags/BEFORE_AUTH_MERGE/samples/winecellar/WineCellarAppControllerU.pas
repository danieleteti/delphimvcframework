unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MainDataModuleUnit;

type

  [MVCPath('/')]
  TWineCellarApp = class(TMVCController)
  private
    dm: TWineCellarDataModule;

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
  System.SysUtils, WinesBO, MVCFramework.Logger;

procedure TWineCellarApp.FindWines(ctx: TWebContext);
begin
  Render(dm.FindWines(ctx.Request.Params['value']));
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
  dm := TWineCellarDataModule.Create(nil);
end;

procedure TWineCellarApp.SaveWine(ctx: TWebContext);
var
  Wine: TWine;
begin
  Wine := ctx.Request.BodyAs<TWine>;
  try
    dm.AddWine(Wine);
    Log(TLogLevel.levNormal, 'Wine correctly saved');
  finally
    Wine.Free;
  end;
end;

procedure TWineCellarApp.UpdateWineById(ctx: TWebContext);
var
  Wine: TWine;
begin
  Wine := ctx.Request.BodyAs<TWine>;
  try
    dm.UpdateWine(Wine);
    Log(TLogLevel.levNormal, 'Wine correctly updated');
  finally
    Wine.Free;
  end;
  Render(200, 'Wine updated');
end;

procedure TWineCellarApp.WineById(ctx: TWebContext);
begin
  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      begin
        dm.DeleteWine(StrToInt(ctx.Request.Params['id']));
        Log(TLogLevel.levNormal, 'Wine deleted');
        Render(200, 'Wine deleted');
      end;
    httpGET:
      begin
        Render(dm.GetWineById(StrToInt(ctx.Request.Params['id'])));
      end
  else
    raise Exception.Create('Invalid http method for action');
  end;

end;

procedure TWineCellarApp.WinesList(ctx: TWebContext);
begin
  Render(dm.FindWines(''));
  Log(TLogLevel.levNormal, 'Getting Wines list');
end;

end.
