unit Controllers.Articles;

interface

uses mvcframework, Controllers.Base;

type

  [MVCDoc('Resource that manages articles CRUD')]
  [MVCPath('/articles')]
  TArticlesController = class(TBaseController)
  public
    [MVCDoc('Returns the list of articles')]
    [MVCPath]
    [MVCHTTPMethod([httpGET])]

    procedure GetArticles;
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
    procedure CreateArticle(Context: TWebContext);
  end;

implementation

{ TArticlesController }

uses Services, BusinessObjects, Commons, mvcframework.Commons;

procedure TArticlesController.CreateArticle(Context: TWebContext);
var
  Article: TArticle;
begin
  Article := Context.Request.BodyAs<TArticle>;
  try
    GetArticlesService.Add(Article);
    Render(201, 'Article Created');
  finally
    Article.Free;
  end;
end;

procedure TArticlesController.DeleteArticleByID(id: Integer);
var
  Article: TArticle;
begin
  GetArticlesService.StartTransaction;
  try
    Article := GetArticlesService.GetByID
      (Context.Request.ParamsAsInteger['id']);
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

procedure TArticlesController.UpdateArticleByID(id: Integer);
var
  Article: TArticle;
begin
  Article := Context.Request.BodyAs<TArticle>;
  try
    Article.id := Context.Request.ParamsAsInteger['id'];
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
    Article := GetArticlesService.GetByID
      (Context.Request.ParamsAsInteger['id']);
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
