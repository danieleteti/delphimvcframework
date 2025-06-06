unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp,
  JsonDataObjects;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    function PublicSection: String;
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    function Index: String;
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/role1')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    function OnlyRole1: String;
    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    function OnlyRole1EmittingJSON: TJSONObject;
    [MVCPath('/role2')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    function OnlyRole2: String;
  end;

implementation

uses
  System.SysUtils, System.Classes;

{ TApp1MainController }

function TApp1MainController.Index: String;
begin
  Redirect('/index.html');
end;

function TApp1MainController.PublicSection: String;
begin
  Result := 'This is a public section';
end;

{ TAdminController }

function TAdminController.OnlyRole1: String;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Result := Context.LoggedUser.CustomData['mycustomvalue'] + sLineBreak +
  'Hey! Hello ' + Context.LoggedUser.UserName + ', now you are a logged user and this is a protected content!' + sLineBreak +
  'As logged user you have the following roles: ' + sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray);
end;

function TAdminController.OnlyRole1EmittingJSON: TJsonObject;
var
  lJObj: TJSONObject;
  lJArr: TJSONArray;
  lQueryParams: TStrings;
  I: Integer;
  lItem: TJsonObject;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;
  lJObj := TJSONObject.Create;
  try
    lJObj.S['message'] := 'This is protected content accessible only by user1';
    lJArr := lJObj.A['querystringparameters'];
    lQueryParams := Context.Request.QueryStringParams;
    for I := 0 to lQueryParams.Count - 1 do
    begin
      lItem := lJArr.AddObject;
      lItem.S[lQueryParams.Names[I]] := lQueryParams.ValueFromIndex[I];
    end;
    Result := lJObj;
  except
    lJObj.Free;
    raise;
  end;
end;

function TAdminController.OnlyRole2: String;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Result := Context.LoggedUser.CustomData['mycustomvalue'] + sLineBreak +
    'Hey! Hello ' + Context.LoggedUser.UserName + ', now you are a logged user and this is a protected content!' + sLineBreak +
    'As logged user you have the following roles: ' + sLineBreak + string.Join(sLineBreak, Context.LoggedUser.Roles.ToArray);
end;

end.
