unit AppControllerU;

interface

uses
  MVCFramework,
	MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicSection;
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/role1')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1;
    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1EmittingJSON;
    [MVCPath('/role2')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole2;
  end;

implementation

uses
  System.SysUtils;

{ TApp1MainController }

procedure TApp1MainController.Index;
begin
  Redirect('/index.html');
end;

procedure TApp1MainController.PublicSection;
begin
  Render('This is a public section');
end;

{ TAdminController }

procedure TAdminController.OnlyRole1;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + Context.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' +
    sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  RenderResponseStream;
end;

procedure TAdminController.OnlyRole1EmittingJSON;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;
  Render('This is protected content accessible only by user1: parameter = ' +
    Context.Request.Params['par1']);
end;

procedure TAdminController.OnlyRole2;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + Context.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' +
    sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  RenderResponseStream;
end;

end.
