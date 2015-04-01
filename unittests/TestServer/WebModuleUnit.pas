unit WebModuleUnit;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    MVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

{$R *.dfm}

uses
  TestServerControllerU, TestServerControllerExceptionU, SpeedMiddlewareU,
  MVCFramework.Middleware.Authentication, System.Generics.Collections,
  MVCFramework.Commons;

type
  TSampleAuth = class(TInterfacedObject, IMVCAuthenticationHandler)

  public
    procedure OnRequest(const ControllerQualifiedClassName: string; const ActionName: string;
      var AuthenticationRequired: Boolean);
    procedure OnAuthentication(const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>; var IsValid: Boolean);
    procedure OnAuthorization(UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);
  end;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.Messaging] := 'true';
    end);
  MVCEngine.AddController(TTestServerController).AddController(TTestPrivateServerController)
    .AddController(TTestServerControllerExceptionAfterCreate)
    .AddController(TTestServerControllerExceptionBeforeDestroy)
    .AddMiddleware(TMVCSpeedMiddleware.Create)
    .AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(TSampleAuth.Create));

  // MVCEngine.Config[TMVCConfigKey.Messaging] := 'false';
end;

{ TSampleAuth }

procedure TSampleAuth.OnAuthentication(const UserName, Password: string;
UserRoles: System.Generics.Collections.TList<System.string>; var IsValid: Boolean);
begin
  UserRoles.Clear;
  IsValid := UserName = Password;
  if not IsValid then
    Exit;

  if UserName = 'user1' then
  begin
    IsValid := True;
    UserRoles.Add('role1');
  end;
  if UserName = 'user2' then
  begin
    IsValid := True;
    UserRoles.Add('role2');
  end;
end;

procedure TSampleAuth.OnAuthorization(UserRoles: System.Generics.Collections.TList<System.string>;
const ControllerQualifiedClassName, ActionName: string; var IsAuthorized: Boolean);
begin
  IsAuthorized := False;
  if ActionName = 'OnlyRole1' then
    IsAuthorized := UserRoles.Contains('role1');

  if ActionName = 'OnlyRole2' then
    IsAuthorized := UserRoles.Contains('role2');
end;

procedure TSampleAuth.OnRequest(const ControllerQualifiedClassName, ActionName: string;
var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired := ControllerQualifiedClassName.EndsWith('TTestPrivateServerController');
end;

end.
