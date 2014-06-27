unit Controllers.Articles;

interface

uses mvcframework, Controllers.Base;

type

  [MVCPath('/articles')]
  TArticlesController = class(TBaseController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticles(context: TWebContext);
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetArticleByID(context: TWebContext);
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpDelete])]
    procedure DeleteArticleByID(context: TWebContext);
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateArticle(context: TWebContext);
  end;

implementation

{ TArticlesController }

uses Services, BusinessObjects, Commons, mvcframework.Commons;

procedure TArticlesController.CreateArticle(context: TWebContext);
var
  Article: TArticle;
begin
  Article := context.Request.BodyAs<TArticle>;
  try
    GetArticlesService.Add(Article);
    Render(201, 'Article creato');
  finally
    Article.Free;
  end;
end;

procedure TArticlesController.DeleteArticleByID(context: TWebContext);
var
  Article: TArticle;
begin
  GetArticlesService.StartTransaction;
  try
    Article := GetArticlesService.GetByID(context.Request.ParamsAsInteger['id']);
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

procedure TArticlesController.GetArticles(context: TWebContext);
begin
  Render<TArticle>(GetArticlesService.GetAll);
end;

procedure TArticlesController.GetArticleByID(context: TWebContext);
var
  Article: TArticle;
begin
  try
    Article := GetArticlesService.GetByID(context.Request.ParamsAsInteger['id']);
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
