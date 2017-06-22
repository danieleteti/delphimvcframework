unit MVCFramework.Tests.AppController;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Server;

type

  TAppUser = class
  strict private
    FCod: Integer;
    FName: string;
    FPass: string;
  public
    property Cod: Integer read FCod write FCod;
    property Name: string read FName write FName;
    property Pass: string read FPass write FPass;
  end;

  [MVCPath('/')]
  TAppController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld(ctx: TWebContext);

    [MVCPath('/user')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUser(ctx: TWebContext);

    [MVCPath('/user/save')]
    [MVCHTTPMethod([httpPOST])]
    procedure PostUser(ctx: TWebContext);

    [MVCPath('/users')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUsers(ctx: TWebContext);

    [MVCPath('/users/save')]
    [MVCHTTPMethod([httpPOST])]
    procedure PostUsers(ctx: TWebContext);
  end;

implementation

{ TAppController }

procedure TAppController.GetUser(ctx: TWebContext);
var
  LUser: TAppUser;
begin
  LUser := TAppUser.Create;
  try
    LUser.Cod := 1;
    LUser.Name := 'Ezequiel';
    LUser.Pass := '123';
  finally
    Render(LUser, True);
  end;
end;

procedure TAppController.GetUsers(ctx: TWebContext);
var
  LUsers: TObjectList<TAppUser>;
  LUser: TAppUser;
  I: Integer;
begin
  LUsers := TObjectList<TAppUser>.Create(True);

  for I := 0 to 10 do
  begin
    LUser := TAppUser.Create;
    LUser.Cod := I;
    LUser.Name := 'Ezequiel ' + IntToStr(I);
    LUser.Pass := IntToStr(I);

    LUsers.Add(LUser);
  end;

  Self.Render<TAppUser>(LUsers, True);
end;

procedure TAppController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
end;

procedure TAppController.PostUser(ctx: TWebContext);
var
  LUser: TAppUser;
begin
  LUser := ctx.Request.BodyAs<TAppUser>();
  try
    if (LUser.Cod > 0) then
      Render('Success!')
    else
      Render('Error!');
  finally
    LUser.Free;
  end;
end;

procedure TAppController.PostUsers(ctx: TWebContext);
var
  LUsers: TObjectList<TAppUser>;
begin
  LUsers := ctx.Request.BodyAsListOf<TAppUser>();
  try
    LUsers.OwnsObjects := True;

    if (LUsers.Count > 0) then
      Render('Success!')
    else
      Render('Error!');

  finally
    LUsers.Free;
  end;
end;

end.
