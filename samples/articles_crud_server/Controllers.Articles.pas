unit Controllers.Articles;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.Serializer.Commons,
  System.Generics.Collections,
  Controllers.Base, BusinessObjects, Services;

type

  [MVCDoc('Resource that manages articles CRUD')]
  [MVCPath('/articles')]
  TArticlesController = class(TBaseController)
  private
    fArticlesService: IArticlesService;
  public
    [MVCInject]
    constructor Create(ArticlesService: IArticlesService); reintroduce;

    [MVCDoc('Returns the list of articles')]
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticles;

    [MVCDoc('Returns the list of articles')]
    [MVCPath('/searches')]
    [MVCHTTPMethod([httpGET])]
    function GetArticlesByDescription(const [MVCFromQueryString('q', '')] Search: String): IMVCObjectDictionary;

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/meta')]
    [MVCHTTPMethod([httpGET])]
    function GetArticleMeta: IMVCObjectDictionary;

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    function GetArticleByID(id: Integer): IMVCObjectDictionary;

    [MVCDoc('Deletes the article with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDelete])]
    procedure DeleteArticleByID(id: Integer);

    [MVCDoc('Updates the article with the specified id and return "200: OK"')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateArticleByID(const [MVCFromBody] Article: TArticle; const id: Integer);

    [MVCDoc('Creates a new article and returns "201: Created"')]
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateArticle(const [MVCFromBody] Article: TArticle);

    [MVCDoc('Creates new articles from a list and returns "201: Created"')]
    [MVCPath('/bulk')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateArticles(const [MVCFromBody] ArticleList: TObjectList<TArticle>);
  end;

implementation

{ TArticlesController }

uses
  Commons,
  mvcframework.Serializer.Intf,
  System.SysUtils;

constructor TArticlesController.Create(ArticlesService: IArticlesService);
begin
  inherited Create;
  fArticlesService := ArticlesService;
end;

procedure TArticlesController.CreateArticle(const Article: TArticle);
begin
  fArticlesService.Add(Article);
  Render201Created('/articles/' + Article.id.ToString, 'Article Created');
end;

procedure TArticlesController.CreateArticles(const ArticleList: TObjectList<TArticle>);
var
  lArticle: TArticle;
begin
//  fArticlesService.StartTransaction;
//  try
//    for lArticle in ArticleList do
//    begin
//      fArticlesService.Add(lArticle);
//    end;
//    fArticlesService.Commit;
//  except
//    fArticlesService.Rollback;
//    raise;
//  end;
//  Render(201, 'Articles Created');
end;

procedure TArticlesController.DeleteArticleByID(id: Integer);
var
  lArticle: TArticle;
begin
  lArticle := fArticlesService.GetByID(id);
  fArticlesService.Delete(lArticle);
end;

procedure TArticlesController.GetArticles;
begin
  Render(ObjectDict().Add('data', fArticlesService.GetAll));
end;

function TArticlesController.GetArticlesByDescription(const Search: String): IMVCObjectDictionary;
begin
  if Search = '' then
  begin
    Result := ObjectDict().Add('data', fArticlesService.GetAll);
  end
  else
  begin
    Result := ObjectDict().Add('data', fArticlesService.GetArticles(Search));
  end;
end;

procedure TArticlesController.UpdateArticleByID(const Article: TArticle; const id: Integer);
begin
  Article.id := id;
  fArticlesService.Update(Article);
  Render(200, 'Article Updated');
end;

function TArticlesController.GetArticleByID(id: Integer): IMVCObjectDictionary;
begin
  Result := ObjectDict().Add('data', fArticlesService.GetByID(id));
end;

function TArticlesController.GetArticleMeta: IMVCObjectDictionary;
begin
  Result := ObjectDict().Add('data', fArticlesService.GetMeta);
end;

end.
