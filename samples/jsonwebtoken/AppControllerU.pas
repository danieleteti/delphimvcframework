unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicSection(ctx: TWebContext);
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/role1')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1(ctx: TWebContext);
    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1EmittingJSON(ctx: TWebContext);
    [MVCPath('/role2')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole2(ctx: TWebContext);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons;

{ TApp1MainController }

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  Redirect('/index.html');
end;

procedure TApp1MainController.PublicSection(ctx: TWebContext);
begin
  Render('This is a public section');
end;

{ TAdminController }

procedure TAdminController.OnlyRole1(ctx: TWebContext);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + ctx.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' +
    sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  Render;
end;

procedure TAdminController.OnlyRole1EmittingJSON(ctx: TWebContext);
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;
  Render('This is protected content accessible only by user1: paremeter = ' +
    ctx.Request.Params['par1']);
end;

procedure TAdminController.OnlyRole2(ctx: TWebContext);
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + ctx.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' +
    sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  Render;
end;

end.
