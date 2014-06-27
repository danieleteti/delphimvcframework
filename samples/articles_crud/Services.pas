unit Services;

interface

uses
  System.Generics.Collections, BusinessObjects, MainDM, System.SysUtils, Commons;

type

  TServiceBase = class abstract
  strict protected
    FDM: TdmMain;
  public
    constructor Create(AdmMain: TdmMain); virtual;
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
  end;

  TArticlesService = class(TServiceBase)
  public
    function GetAll: TObjectList<TArticle>;
    function GetByID(const AID: Integer): TArticle;
    procedure Delete(AArticolo: TArticle);
    procedure Add(AArticolo: TArticle);
  end;

implementation

uses
  ObjectsMappers, FireDAC.Stan.Option, FireDAC.Comp.Client;

{ TArticoliService }

procedure TArticlesService.Add(AArticolo: TArticle);
var
  Cmd: TFDCustomCommand;
begin
  AArticolo.CheckInsert;
  Cmd := FDM.updArticles.Commands[arInsert];
  Mapper.ObjectToFDParameters(Cmd.Params, AArticolo, 'NEW_');
  Cmd.OpenOrExecute;
end;

procedure TArticlesService.Delete(AArticolo: TArticle);
var
  Cmd: TFDCustomCommand;
begin
  AArticolo.CheckDelete;
  Cmd := FDM.updArticles.Commands[arDelete];
  Mapper.ObjectToFDParameters(Cmd.Params, AArticolo, 'OLD_');
  Cmd.Execute;
end;

function TArticlesService.GetAll: TObjectList<TArticle>;
begin
  FDM.dsArticles.Open('SELECT * FROM ARTICOLI ORDER BY ID');
  Result := FDM.dsArticles.AsObjectList<TArticle>;
  FDM.dsArticles.Close;
end;

function TArticlesService.GetByID(const AID: Integer): TArticle;
begin
  FDM.dsArticles.Open('SELECT * FROM ARTICOLI WHERE ID = :ID', [AID]);
  try
    if not FDM.dsArticles.Eof then
      Result := FDM.dsArticles.AsObject<TArticle>
    else
      raise EServiceException.Create('Article not found');
  finally
    FDM.dsArticles.Close;
  end;
end;

{ TServiceBase }

procedure TServiceBase.Commit;
begin
  FDM.Connection.Commit;
end;

constructor TServiceBase.Create(AdmMain: TdmMain);
begin
  inherited Create;
  FDM := AdmMain;
end;

procedure TServiceBase.Rollback;
begin
  FDM.Connection.Rollback;
end;

procedure TServiceBase.StartTransaction;
begin
  FDM.Connection.StartTransaction;
end;

end.
