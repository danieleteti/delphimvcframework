unit WebSiteControllerU;

interface

uses MVCFramework,
 MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);

  end;

implementation

uses
  Data.DBXJSON,
  System.SysUtils;

{ TApp1MainController }

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  Redirect('/index.html');
end;

end.
