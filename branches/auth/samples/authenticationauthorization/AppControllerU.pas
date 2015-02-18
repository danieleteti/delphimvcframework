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
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons;

{ TApp1MainController }

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(StringOfChar('*', 1024));
end;

{ TAdminController }

procedure TAdminController.Index(ctx: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + ctx.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' + sLineBreak +
    string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  Render;
end;

end.
