unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);

  private
    MVC: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses AppControllerU, System.Generics.Collections,
  MVCFramework.Middleware.Authentication;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LOnAuthentication: TOnAuthenticationEvent;
  LOnAuthorization: TOnAuthorizationEvent;
begin
  LOnAuthentication :=
      procedure(const AUserName, APassword: string; AControllerQualifiedClassName,
      AActionNAme: string; AUserRoles: TList<string>; var AIsValid: Boolean)
    begin
      if AControllerQualifiedClassName = TAdminController.QualifiedClassName then
      begin
        AIsValid := AUserName.Equals(APassword);
        if AIsValid then
        begin
          AUserRoles.Add('role1');
          AUserRoles.Add('role2');
        end
        else
          AUserRoles.Clear;
      end
      else // all the other controllers don't require authentication
      begin
        AUserRoles.Clear;
        AIsValid := True;
      end;
    end;

  LOnAuthorization :=
      procedure(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionNAme: string; var AIsAuthorized: Boolean)
    begin
      AIsAuthorized := AContext.LoggedUser.Roles.Contains('role1');
    end;

  MVC := TMVCEngine.Create(Self);
  MVC.Config[TMVCConfigKey.SessionTimeout] := '30';
  MVC.Config[TMVCConfigKey.DefaultContentType] := 'text/plain';
  MVC.AddController(TApp1MainController).AddController(TAdminController)
    .AddMiddleware(TMVCAuthenticationMiddleware.Create(LOnAuthentication, LOnAuthorization));
end;

end.
