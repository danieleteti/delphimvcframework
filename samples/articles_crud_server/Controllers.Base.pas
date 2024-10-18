unit Controllers.Base;

interface

uses
  MVCFramework, MVCFramework.Commons, Services;

type
  TBaseController = class abstract(TMVCController)
  end;

  [MVCPath('/private')]
  TPrivateController = class(TBaseController)
  public
    [MVCPath('/articles')]
    [MVCHTTPMethods([httpDELETE])]
    procedure DeleteAllArticles(ArticlesService: IArticlesService);
  end;

implementation

uses
  System.SysUtils;

{ TPrivateController }

procedure TPrivateController.DeleteAllArticles(ArticlesService: IArticlesService);
begin
  ArticlesService.DeleteAllArticles();
end;

end.
