unit Controllers.Base;

interface

uses
  MVCFramework, Services, MainDM;

type
  TBaseController = class abstract(TMVCController)
  strict private
    FDM: TdmMain;
    FArticlesService: TArticlesService;
    function GetDataModule: TdmMain;
  strict protected
    function GetArticlesService: TArticlesService;
  public
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils;

{ TBaseController }

destructor TBaseController.Destroy;
begin
  FArticlesService.Free;
  FDM.Free;
  inherited;
end;

function TBaseController.GetArticlesService: TArticlesService;
begin
  if not Assigned(FArticlesService) then
    FArticlesService := TArticlesService.Create(GetDataModule);
  Result := FArticlesService;
end;

function TBaseController.GetDataModule: TdmMain;
begin
  if not Assigned(FDM) then
    FDM := TdmMain.Create(nil);
  Result := FDM;
end;

end.
