unit Controllers.Articles;

interface

uses mvcframework, Controllers.Base;

type

  [MVCPath('/articles')]
  TArticlesController = class(TBaseController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticles(Context: TWebContext);
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticleByID(Context: TWebContext);
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDelete])]
    procedure DeleteArticleByID(Context: TWebContext);
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateArticleByID(Context: TWebContext);
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

procedure TArticlesController.DeleteArticleByID(Context: TWebContext);
var
  Article: TArticle;
begin
  GetArticlesService.StartTransaction;
  try
    Article := GetArticlesService.GetByID(Context.Request.ParamsAsInteger['id']);
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

procedure TArticlesController.GetArticles(Context: TWebContext);
begin
  Render<TArticle>(GetArticlesService.GetAll);
end;

procedure TArticlesController.UpdateArticleByID(Context: TWebContext);
var
  Article: TArticle;
begin
  Article := Context.Request.BodyAs<TArticle>;
  try
    Article.ID := Context.Request.ParamsAsInteger['id'];
    GetArticlesService.Update(Article);
    Render(201, 'Article Updated');
  finally
    Article.Free;
  end;
end;

procedure TArticlesController.GetArticleByID(Context: TWebContext);
var
  Article: TArticle;
begin
  try
    Article := GetArticlesService.GetByID(Context.Request.ParamsAsInteger['id']);
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
