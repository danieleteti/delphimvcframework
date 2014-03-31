unit AppControllerU;

interface

uses MVCFramework,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogin(ctx: TWebContext);

    [MVCPath('/logout')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogout(ctx: TWebContext);

  end;

implementation

uses
  Data.DBXJSON,
  System.SysUtils, MVCFramework.Commons;

{ TApp1MainController }

procedure TApp1MainController.DoLogin(ctx: TWebContext);
begin
  Session['username'] := ctx.Request.Params['username'];
  Render(204, 'No Content');
end;

procedure TApp1MainController.DoLogout(ctx: TWebContext);
begin
  SessionStop(false);
  Render(204, 'No Content');
end;

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(Session['username']);
end;

end.
