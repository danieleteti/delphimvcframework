unit Controllers.Articles;

interface

uses
  mvcframework,
  mvcframework.Commons,
  Controllers.Base;

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
    procedure GetArticlesByDescription;

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
    procedure UpdateArticleByID(id: Integer);

    [MVCDoc('Creates a new article and returns "201: Created"')]
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateArticle;

    [MVCDoc('Creates new articles from a list and returns "201: Created"')]
    [MVCPath('/bulk')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateArticles;
  end;

implementation

{ TArticlesController }

uses
  Services,
  BusinessObjects,
  Commons,
  mvcframework.Serializer.Intf,
  System.Generics.Collections, System.SysUtils;

procedure TArticlesController.CreateArticle;
var
  Article: TArticle;
begin
  Article := Context.Request.BodyAs<TArticle>;
  try
    GetArticlesService.Add(Article);
    ResponseCreated('/articles/' + Article.ID.ToString, 'Article Created');
  finally
    Article.Free;
  end;
end;

procedure TArticlesController.CreateArticles;
var
  lArticles: TObjectList<TArticle>;
  lArticle: TArticle;
begin
  lArticles := Context.Request.BodyAsListOf<TArticle>;
  try
    for lArticle in lArticles do
    begin
      GetArticlesService.Add(lArticle);
    end;
    Render(201, 'Articles Created');
  finally
    lArticles.Free;
  end;
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
  Render<TArticle>(GetArticlesService.GetAll);
end;

procedure TArticlesController.GetArticlesByDescription;
var
  lSearch: String;
begin
  try
    if random(10) < 2 then
      raise EMVCException.Create('ERRORONE!!!');

    lSearch := Context.Request.Params['q'];
    if lSearch = '' then
      Render<TArticle>(GetArticlesService.GetAll)
    else
      Render<TArticle>(GetArticlesService.GetArticles(lSearch));
  except
    on E: EServiceException do
    begin
      raise EMVCException.Create(E.Message, '', 0, 404);
    end
    else
      raise;
  end;
end;

procedure TArticlesController.UpdateArticleByID(id: Integer);
var
  Article: TArticle;
begin
  Article := Context.Request.BodyAs<TArticle>;
  try
    Article.id := id;
    GetArticlesService.Update(Article);
    Render(200, 'Article Updated');
  finally
    Article.Free;
  end;
end;

procedure TArticlesController.GetArticleByID(id: Integer);
var
  Article: TArticle;
begin
  try
    Article := GetArticlesService.GetByID(id);
    Render(Article);
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
