unit StandAloneServerWebModuleTest;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp,
  MVCFramework;

type

  TTestWebModule2 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  TestWebModuleClass: TComponentClass = TTestWebModule2;
  TestWebModuleClass2: TComponentClass = TTestWebModule2;

implementation

uses
  MVCFramework.Middleware.Authentication,
  MVCFramework.Server,
  MVCFramework.Server.Impl,
  StandaloneServerTestU;

{$R *.dfm}

procedure TTestWebModule2.WebModuleCreate(Sender: TObject);
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

procedure TTestWebModule2.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
