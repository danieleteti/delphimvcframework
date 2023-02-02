unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MainDataModuleUnit, WinesBO;

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
    procedure WinesList;

    [MVCPath('/wines')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveWine([MVCFromBody] Wine: TWine);

    [MVCPath('/wines/search/($value)')]
    procedure FindWines(value: String);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetWineById(id: Integer);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteWineById(id: Integer);

    [MVCPath('/wines/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateWineById([MVCFromBody] Wine: TWine);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons;

procedure TWineCellarApp.DeleteWineById(id: Integer);
begin
  FDataModule.DeleteWine(id);
  Log.Info('Wine %d deleted', [id], 'WINESERVER');
  Render(200, 'Wine deleted');
end;

procedure TWineCellarApp.FindWines(value: String);
begin
  Render(FDataModule.FindWines(value));
end;

procedure TWineCellarApp.GetWineById(id: Integer);
begin
  Render(FDataModule.GetWineById(id), False, dstSingleRecord);
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

procedure TWineCellarApp.SaveWine([MVCFromBody] Wine: TWine);
begin
  FDataModule.AddWine(Wine);
  Log.Info('Wine correctly saved', 'WINESERVER');
end;

procedure TWineCellarApp.UpdateWineById([MVCFromBody] Wine: TWine);
begin
  FDataModule.UpdateWine(Wine);
  Log.Info('Wine correctly updated', 'WINESERVER');
  Render(200, 'Wine updated');
end;

procedure TWineCellarApp.WinesList;
begin
  Render(FDataModule.GetAllWines, False);
  Log.Info('Getting Wines list', 'WINESERVER');
end;

end.
