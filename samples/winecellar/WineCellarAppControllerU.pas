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
  System.SysUtils;

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
begin
  dm.AddWine(ctx.Request.BodyAsJSONObject);
end;

procedure TWineCellarApp.UpdateWineById(ctx: TWebContext);
begin
  Render(dm.UpdateWine(ctx.Request.BodyAsJSONObject));
end;

procedure TWineCellarApp.WineById(ctx: TWebContext);
begin
  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      Render(dm.DeleteWine(StrToInt(ctx.Request.Params['id'])));
    httpGET:
      Render(dm.GetWineById(StrToInt(ctx.Request.Params['id'])));
  else
    raise Exception.Create('Invalid http method for action');
  end;
end;

procedure TWineCellarApp.WinesList(ctx: TWebContext);
begin
  Render(dm.FindWines(''));
end;

end.
