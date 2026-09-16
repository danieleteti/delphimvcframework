unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

type
  [MVCPath('/')]
  TPublicController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicSection;

    [MVCPath('/status')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/json')]
    procedure Status;
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1;

    [MVCPath('/role2')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole2;

    [MVCPath('/profile')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure UserProfile;
  end;

implementation

uses
  System.SysUtils,
  System.JSON,
  System.Classes,
  System.Generics.Collections,
  JsonDataObjects;

procedure AddRolesToArray(AArray: TJDOJsonArray; ARoles: TList<string>);
var
  I: Integer;
begin
  for I := 0 to ARoles.Count - 1 do
    AArray.Add(ARoles[I]);
end;

{ TPublicController }

procedure TPublicController.Index;
begin
  Redirect('/static/index.html');
end;

procedure TPublicController.PublicSection;
begin
  Render('This is a public section - no authentication required');
end;

procedure TPublicController.Status;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := TJDOJsonObject.Create;
  try
    lJSON.S['status'] := 'ok';
    lJSON.B['authenticated'] := Context.LoggedUser.IsValid;
    if Context.LoggedUser.IsValid then
    begin
      lJSON.S['username'] := Context.LoggedUser.UserName;
      AddRolesToArray(lJSON.A['roles'], Context.LoggedUser.Roles);
    end;
    Render(lJSON, False);
  finally
    lJSON.Free;
  end;
end;

{ TAdminController }

procedure TAdminController.OnlyRole1;
var
  lJSON: TJDOJsonObject;
  lPair: TPair<String, String>;
begin
  lJSON := TJDOJsonObject.Create;
  try
    lJSON.S['message'] := 'Welcome! You have access to role1 protected content.';
    lJSON.S['username'] := Context.LoggedUser.UserName;
    AddRolesToArray(lJSON.A['roles'], Context.LoggedUser.Roles);
    lJSON.D['loggedSince'] := Context.LoggedUser.LoggedSince;

    // Include custom data from session
    for lPair in Context.LoggedUser.CustomData do
    begin
      lJSON.O['customData'].S[lPair.Key] := lPair.Value;
    end;

    Render(lJSON, False);
  finally
    lJSON.Free;
  end;
end;

procedure TAdminController.OnlyRole2;
var
  lJSON: TJDOJsonObject;
begin
  lJSON := TJDOJsonObject.Create;
  try
    lJSON.S['message'] := 'Welcome! You have access to role2 protected content.';
    lJSON.S['username'] := Context.LoggedUser.UserName;
    AddRolesToArray(lJSON.A['roles'], Context.LoggedUser.Roles);
    Render(lJSON, False);
  finally
    lJSON.Free;
  end;
end;

procedure TAdminController.UserProfile;
var
  lJSON: TJDOJsonObject;
  lPair: TPair<String, String>;
begin
  lJSON := TJDOJsonObject.Create;
  try
    lJSON.S['username'] := Context.LoggedUser.UserName;
    AddRolesToArray(lJSON.A['roles'], Context.LoggedUser.Roles);
    lJSON.D['loggedSince'] := Context.LoggedUser.LoggedSince;
    lJSON.S['realm'] := Context.LoggedUser.Realm;

    for lPair in Context.LoggedUser.CustomData do
    begin
      lJSON.O['customData'].S[lPair.Key] := lPair.Value;
    end;

    Render(lJSON, False);
  finally
    lJSON.Free;
  end;
end;

end.
