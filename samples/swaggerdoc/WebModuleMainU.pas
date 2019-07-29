unit WebModuleMainU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  MVCFramework.Commons,
  MVCFramework.Controllers.Register,
  MVCFramework.Middleware.Swagger,
  MVCFramework.Swagger.Commons;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}


procedure TWebModule1.WebModuleCreate(Sender: TObject);
var
  LSwagInfo: TMVCSwaggerInfo;
begin
  FEngine := TMVCEngine.Create(Self);

  // Path prefix will be swagger basepath
  FEngine.Config[TMVCConfigKey.PathPrefix] := '/api';
  FEngine.Config[TMVCConfigKey.DocumentRoot] := '.\www';

  LSwagInfo.Title := 'Sample Swagger API';
  LSwagInfo.Version := 'v1';
  LSwagInfo.TermsOfService := 'http://www.apache.org/licenses/LICENSE-2.0.txt';
  LSwagInfo.Description := 'Swagger Documentation Example';
  LSwagInfo.ContactName := 'João Antônio Duarte';
  LSwagInfo.ContactEmail := 'joao.antonioduarte@hotmail.com';
  LSwagInfo.ContactUrl := 'https://github.com/joaoduarte19';
  LSwagInfo.LicenseName := 'Apache License - Version 2.0, January 2004';
  LSwagInfo.LicenseUrl := 'http://www.apache.org/licenses/LICENSE-2.0';

  FEngine.AddMiddleware(TMVCSwaggerMiddleware.Create(FEngine, LSwagInfo, '/api/swagger.json'));

  /// Add your registered controllers to engine.
  /// Only registered controls such as "MyServerName" will be added
  TControllersRegister.Instance.AddControllersInEngine(FEngine, 'MyServerName');
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
