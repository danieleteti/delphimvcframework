unit Services;

interface

uses
  System.Generics.Collections,
  BusinessObjects,
  System.SysUtils,
  Commons, JsonDataObjects;

type
  IArticlesService = interface
  ['{D6843E17-1B98-435C-9EBC-8E76DCEF9A3B}']
    function GetAll: TObjectList<TArticle>;
    function GetArticles(const aTextSearch: string): TObjectList<TArticle>;
    function GetByID(const AID: Integer): TArticle;
    procedure Delete(AArticolo: TArticle);
    procedure DeleteAllArticles;
    procedure Add(AArticolo: TArticle);
    procedure CreateArticles(const ArticleList: TObjectList<TArticle>);
    procedure Update(AArticolo: TArticle);
    function GetMeta: TJSONObject;
  end;

  TArticlesService = class(TInterfacedObject, IArticlesService)
  public
    function GetAll: TObjectList<TArticle>;
    function GetArticles(const aTextSearch: string): TObjectList<TArticle>;
    function GetByID(const AID: Integer): TArticle;
    procedure Delete(AArticolo: TArticle);
    procedure DeleteAllArticles;
    procedure Add(AArticolo: TArticle);
    procedure CreateArticles(const ArticleList: TObjectList<TArticle>);
    procedure Update(AArticolo: TArticle);
    function GetMeta: TJSONObject;
  end;

implementation

uses
  FireDAC.Stan.Param,
  MVCFramework.ActiveRecord,
  MVCFramework.FireDAC.Utils,
  MVCFramework.DataSet.Utils,
  MVCFramework.Serializer.Commons, Data.DB;


{ TArticoliService }

procedure TArticlesService.Add(AArticolo: TArticle);
begin
  AArticolo.Insert;
end;

procedure TArticlesService.CreateArticles(const ArticleList: TObjectList<TArticle>);
begin
  var Ctx := TMVCActiveRecord.UseTransactionContext;
  for var lArticle in ArticleList do
  begin
    Add(lArticle);
  end;
end;

procedure TArticlesService.Delete(AArticolo: TArticle);
begin
  AArticolo.Delete();
end;

procedure TArticlesService.DeleteAllArticles;
begin
  TMVCActiveRecord.DeleteAll(TArticle);
end;

function TArticlesService.GetAll: TObjectList<TArticle>;
begin
  Result := TMVCActiveRecord.SelectRQL<TArticle>('sort(+id)', 1000);
end;

function TArticlesService.GetArticles(
  const aTextSearch: string): TObjectList<TArticle>;
begin
  if aTextSearch.Trim.IsEmpty then
    Result := GetAll
  else
    Result := TMVCActiveRecord.SelectByNamedQuery<TArticle>('search_by_text',[aTextSearch],[ftString]);
end;

function TArticlesService.GetByID(const AID: Integer): TArticle;
begin
  Result := TMVCActiveRecord.GetByPK<TArticle>(AID);
end;

function TArticlesService.GetMeta: TJSONObject;
begin
  var lDS := TMVCActiveRecord.SelectDataSet('SELECT ID, CODICE as CODE, DESCRIZIONE as DESCRIPTION, PREZZO as PRICE, CREATED_AT as CREATEDAT, UPDATED_AT as UPDATEDAT FROM ARTICOLI WHERE TRUE = FALSE',[]);
  try
    Result := lDS.MetadataAsJSONObject()
  finally
    lDS.Free;
  end;
end;

procedure TArticlesService.Update(AArticolo: TArticle);
begin
  AArticolo.Update();
end;

end.
