unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicSection(ctx: TWebContext);
  end;

  [MVCPath('/admin')]
  [MVCRequiresAuthentication] { Defines that all control methods require authentication }
  TAdminController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
  public
    [MVCPath('/role1')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role1')] { Define the role required to access this method }
    procedure OnlyRole1;

    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role1')]
    procedure OnlyRole1EmittingJSON;

    [MVCPath('/role2')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role2')]
    procedure OnlyRole2;
  end;

implementation

uses
  System.SysUtils,
  System.JSON,
  System.Classes,
  System.Generics.Collections;

{ TApp1MainController }

procedure TApp1MainController.PublicSection(ctx: TWebContext);
begin
  Render('This is a public section');
end;

{ TAdminController }

procedure TAdminController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  Assert(AContext.LoggedUser.CustomData['customkey1'] = 'customvalue1', 'customkey1 not valid');
  Assert(AContext.LoggedUser.CustomData['customkey2'] = 'customvalue2', 'customkey2 not valid');
  AHandled := False;
end;

procedure TAdminController.OnlyRole1;
var
  lPair: TPair<String, String>;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream.AppendLine('Hey! Hello ' + Context.LoggedUser.UserName +
    ', now you are a logged user and this is a protected content!');
  ResponseStream.AppendLine('As logged user you have the following roles: ' +
    sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray));
  ResponseStream.AppendLine('You CustomClaims are: ' +
    sLineBreak);
  for lPair in Context.LoggedUser.CustomData do
  begin
    ResponseStream.AppendFormat('%s = %s' + sLineBreak, [lPair.Key, lPair.Value]);
  end;
  RenderResponseStream;
end;

procedure TAdminController.OnlyRole1EmittingJSON;
var
  lJObj: TJSONObject;
  lJArr: TJSONArray;
  lQueryParams: TStrings;
  I: Integer;
  lPair: TPair<String, String>;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;
  lJObj := TJSONObject.Create;
  lJObj.AddPair('message', 'This is protected content accessible only by user1');
  lJArr := TJSONArray.Create;
  lJObj.AddPair('querystringparameters', lJArr);

  lQueryParams := Context.Request.QueryStringParams;
  for I := 0 to lQueryParams.Count - 1 do
  begin
    lJArr.AddElement(TJSONObject.Create(TJSONPair.Create(
      lQueryParams.Names[I],
      lQueryParams.ValueFromIndex[I])));
  end;

  lJArr := TJSONArray.Create;
  lJObj.AddPair('customclaims', lJArr);
  for lPair in Context.LoggedUser.CustomData do
  begin
    lJArr.AddElement(TJSONObject.Create(TJSONPair.Create(lPair.Key, lPair.Value)));
  end;

  Render(lJObj);
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
