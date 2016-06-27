unit MVCFramework.Tests.WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp,
  MVCFramework;

type

  TTestWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  TestWebModuleClass: TComponentClass = TTestWebModule;
  TestWebModuleClass2: TComponentClass = TTestWebModule;

implementation

uses
  MVCFramework.Tests.StandaloneServer,
  MVCFramework.Middleware.Authentication,
  MVCFramework.Server,
  MVCFramework.Server.Impl;

{$R *.dfm}

procedure TTestWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self);

  // Add With Delegate Constructor Controller
  FMVCEngine.AddController(TTestController,
    function: TMVCController
    begin
      Result := TTestController.Create;
    end
    );

  FMVCEngine.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(
    TMVCDefaultAuthenticationHandler.New
    .SetOnAuthentication(
    procedure(const AUserName, APassword: string;
      AUserRoles: TList<string>; var IsValid: Boolean; const ASessionData: TDictionary<String, String>)
    begin
      IsValid := AUserName.Equals('dmvc') and APassword.Equals('123');
    end
    )
    ));
end;

procedure TTestWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
