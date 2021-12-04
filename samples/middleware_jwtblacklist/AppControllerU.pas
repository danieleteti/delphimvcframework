unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/public')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicSection;
  end;

  [MVCPath('/admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/role1')]
    [MVCProduces('application/json')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  MVCFramework.Cache, JsonDataObjects;

{ TApp1MainController }

procedure TApp1MainController.PublicSection;
begin
  Render(StrDict(['message'], ['This is a public section']));
end;

{ TAdminController }

procedure TAdminController.OnlyRole1;
var
  lJObj: TJSONObject;
  lJArr: TJSONArray;
  lQueryParams: TStrings;
  I: Integer;
  lPair: TPair<String, String>;
  lObj: TJSONObject;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;
  lJObj := TJSONObject.Create;
  lJObj.S['message'] := 'This is protected content accessible only by user1';
  lJArr := lJObj.A['querystringparameters'];

  lQueryParams := Context.Request.QueryStringParams;
  for I := 0 to lQueryParams.Count - 1 do
  begin
    lObj := lJArr.AddObject;
    lObj.S[lQueryParams.Names[I]] := lQueryParams.ValueFromIndex[I];
  end;

  lJArr := lJObj.A['customclaims'];
  for lPair in Context.LoggedUser.CustomData do
  begin
    lObj := lJArr.AddObject;
    lObj.S[lPair.Key] := lPair.Value;
  end;

  Render(lJObj);
end;

end.
