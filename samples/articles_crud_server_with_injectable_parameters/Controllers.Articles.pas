unit Controllers.Articles;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.Serializer.Commons,
  System.Generics.Collections,
  Controllers.Base, BusinessObjects;

type

  [MVCDoc('Resource that manages articles CRUD')]
  [MVCPath('/articles')]
  TArticlesController = class(TBaseController)
  public
    [MVCDoc('Returns the list of articles')]
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticles;

    [MVCDoc('Returns the list of articles')]
    [MVCPath('/searches')]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticlesByDescription(const [MVCFromQueryString('q', '')] Search: String);

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/meta')]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticleMeta;

    [MVCDoc('Returns the article with the specified id')]
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticleByID(id: Integer);

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
  Services,
  Commons,
  mvcframework.Serializer.Intf,
  System.SysUtils;

procedure TArticlesController.CreateArticle(const Article: TArticle);
begin
  GetArticlesService.Add(Article);
  Render201Created('/articles/' + Article.id.ToString, 'Article Created');
end;

procedure TArticlesController.CreateArticles(const ArticleList: TObjectList<TArticle>);
var
  lArticle: TArticle;
begin
  GetArticlesService.StartTransaction;
  try
    for lArticle in ArticleList do
    begin
      GetArticlesService.Add(lArticle);
    end;
    GetArticlesService.Commit;
  except
    GetArticlesService.Rollback;
    raise;
  end;
  Render(201, 'Articles Created');
end;

procedure TArticlesController.DeleteArticleByID(id: Integer);
var
  Article: TArticle;
begin
  GetArticlesService.StartTransaction;
  try
    Article := GetArticlesService.GetByID(id);
    try
      GetArticlesService.Delete(Article);
    finally
      Article.Free;
    end;
    GetArticlesService.Commit;
  except
    GetArticlesService.Rollback;
    raise;
  end;
end;

procedure TArticlesController.GetArticles;
begin
  Render(ObjectDict().Add('data', GetArticlesService.GetAll));
end;

procedure TArticlesController.GetArticlesByDescription(const Search: String);
var
  lDict: IMVCObjectDictionary;
begin
  try
    if Search = '' then
    begin
      lDict := ObjectDict().Add('data', GetArticlesService.GetAll);
    end
    else
    begin
      lDict := ObjectDict().Add('data', GetArticlesService.GetArticles(Search));
    end;
    Render(lDict);
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
    else
      raise;
  end;
end;

procedure TArticlesController.UpdateArticleByID(const Article: TArticle; const id: Integer);
begin
  Article.id := id;
  GetArticlesService.Update(Article);
  Render(200, 'Article Updated');
end;

procedure TArticlesController.GetArticleByID(id: Integer);
begin
  try
    Render(ObjectDict().Add('data', GetArticlesService.GetByID(id)));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
    else
      raise;
  end;
end;

procedure TArticlesController.GetArticleMeta;
begin
  try
    Render(ObjectDict().Add('data', GetArticlesService.GetMeta));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
    else
      raise;
  end;
end;

end.
