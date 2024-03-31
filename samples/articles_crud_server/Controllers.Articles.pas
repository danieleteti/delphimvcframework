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
    function GetArticles: IMVCResponse;

    [MVCDoc('Returns the list of articles')]
    [MVCPath('/searches')]
    [MVCHTTPMethod([httpGET])]
    function GetArticlesByDescription(const [MVCFromQueryString('q', '')] Search: String): IMVCResponse;

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/meta')]
    [MVCHTTPMethod([httpGET])]
    function GetArticleMeta: IMVCResponse;

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    function GetArticleByID(id: Integer): IMVCResponse;

    [MVCDoc('Deletes the article with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDelete])]
    procedure DeleteArticleByID(id: Integer);

    [MVCDoc('Updates the article with the specified id and return "200: OK"')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    function UpdateArticleByID(const [MVCFromBody] Article: TArticle; const id: Integer): IMVCResponse;

    [MVCDoc('Creates a new article and returns "201: Created"')]
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    function CreateArticle(const [MVCFromBody] Article: TArticle): IMVCResponse;

    [MVCDoc('Creates new articles from a list and returns "201: Created"')]
    [MVCPath('/bulk')]
    [MVCHTTPMethod([httpPOST])]
    function CreateArticles(const [MVCFromBody] ArticleList: TObjectList<TArticle>): IMVCResponse;
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

function TArticlesController.CreateArticle(const Article: TArticle): IMVCResponse;
begin
  fArticlesService.Add(Article);
  Render201Created('/articles/' + Article.id.ToString, 'Article Created');
end;

function TArticlesController.CreateArticles(const ArticleList: TObjectList<TArticle>): IMVCResponse;
begin
  fArticlesService.CreateArticles(ArticleList);
  Result := MVCResponseBuilder.StatusCode(HTTP_STATUS.Created).Build;
end;

procedure TArticlesController.DeleteArticleByID(id: Integer);
var
  lArticle: TArticle;
begin
  lArticle := fArticlesService.GetByID(id);
  fArticlesService.Delete(lArticle);
end;

function TArticlesController.GetArticles: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(fArticlesService.GetAll)
    .Build;
end;

function TArticlesController.GetArticlesByDescription(const Search: String): IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(fArticlesService.GetArticles(Search))
    .Build;
end;

function TArticlesController.UpdateArticleByID(const Article: TArticle; const id: Integer): IMVCResponse;
begin
  Article.id := id;
  fArticlesService.Update(Article);
  Result := MVCResponseBuilder.StatusCode(HTTP_STATUS.OK).Build;
end;

function TArticlesController.GetArticleByID(id: Integer): IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(fArticlesService.GetByID(id))
    .Build;
end;

function TArticlesController.GetArticleMeta: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(fArticlesService.GetMeta)
    .Build;
end;

end.
