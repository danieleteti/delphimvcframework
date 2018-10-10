unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MainDataModuleUnit;

type

  [MVCPath('/api')]
  TWineCellarApp = class(TMVCController)
  private
    FDataModule: TWineCellarDataModule;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionNAme: string); override;

  public
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
  System.SysUtils,
  WinesBO,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons;

procedure TWineCellarApp.FindWines(ctx: TWebContext);
begin
  Render(FDataModule.FindWines(ctx.Request.Params['value']));
end;

procedure TWineCellarApp.OnAfterAction(Context: TWebContext; const AActionNAme: string);
begin
  inherited;
  FDataModule.Free;
end;

procedure TWineCellarApp.OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  FDataModule := TWineCellarDataModule.Create(nil);
end;

procedure TWineCellarApp.SaveWine(ctx: TWebContext);
var
  lWine: TWine;
begin
  lWine := ctx.Request.BodyAs<TWine>;
  try
    FDataModule.AddWine(lWine);
    Log.Info('Wine correctly saved', 'WINESERVER');
  finally
    lWine.Free;
  end;
end;

procedure TWineCellarApp.UpdateWineById(ctx: TWebContext);
var
  lWine: TWine;
begin
  lWine := ctx.Request.BodyAs<TWine>;
  try
    FDataModule.UpdateWine(lWine);
    Log.Info('Wine correctly updated', 'WINESERVER');
  finally
    lWine.Free;
  end;
  Render(200, 'Wine updated');
end;

procedure TWineCellarApp.WineById(ctx: TWebContext);
begin
  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      begin
        FDataModule.DeleteWine(StrToInt(ctx.Request.Params['id']));
        Log.Info('Wine deleted', 'WINESERVER');
        Render(200, 'Wine deleted');
      end;
    httpGET:
      begin
        Render(FDataModule.GetWineById(StrToInt(ctx.Request.Params['id'])), False, dstSingleRecord);
      end
  else
    raise Exception.Create('Invalid http method for action');
  end;

end;

procedure TWineCellarApp.WinesList(ctx: TWebContext);
begin
  Render(FDataModule.GetAllWines, False);
  Log.Info('Getting Wines list', 'WINESERVER');
end;

end.
