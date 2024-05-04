unit WineCellarAppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MainDataModuleUnit,
  WinesBO;

type

  [MVCPath('/api')]
  TWineCellarApp = class(TMVCController)
  private
    fDataModule: TWineCellarDataModule;
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
    procedure UpdateWineById(const id: Integer; const [MVCFromBody] Wine: TWine);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Serializer.Commons;

procedure TWineCellarApp.DeleteWineById(id: Integer);
begin
  fDataModule.DeleteWine(id);
  Log.Info('Wine %d deleted', [id], 'WINESERVER');
  Render(HTTP_STATUS.NoContent, 'Wine deleted');
end;

procedure TWineCellarApp.FindWines(value: String);
begin
  Render(fDataModule.FindWines(value));
end;

procedure TWineCellarApp.GetWineById(id: Integer);
begin
  Render(fDataModule.GetWineById(id), False, dstSingleRecord);
end;

procedure TWineCellarApp.OnAfterAction(Context: TWebContext; const AActionNAme: string);
begin
  inherited;
  fDataModule.Free;
end;

procedure TWineCellarApp.OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  fDataModule := TWineCellarDataModule.Create(nil);
end;

procedure TWineCellarApp.SaveWine([MVCFromBody] Wine: TWine);
begin
  fDataModule.AddWine(Wine);
  Log.Info('Wine correctly saved', 'WINESERVER');
  StatusCode := HTTP_STATUS.NoContent;
end;

procedure TWineCellarApp.UpdateWineById(const id: Integer; const [MVCFromBody] Wine: TWine);
begin
  Wine.id := id;
  fDataModule.UpdateWine(Wine);
  Log.Info('Wine correctly updated', 'WINESERVER');
  Render(HTTP_STATUS.NoContent, 'Wine updated');
end;

procedure TWineCellarApp.WinesList;
begin
  Render(fDataModule.GetAllWines, False);
  Log.Info('Getting Wines list', 'WINESERVER');
end;

end.
