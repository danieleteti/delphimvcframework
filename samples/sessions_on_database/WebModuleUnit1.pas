unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework, FireDAC.Phys.MySQLDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.MySQL;

type
  TWebModule1 = class(TWebModule)
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
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


uses AppControllerU, MVCFramework.Commons, MVCFramework.Middleware.ActiveRecord;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.SessionTimeout] := '10'; // 10minutes
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.TEXT_HTML;
      Config[TMVCConfigKey.SessionType] := 'dbactiverecord';
    end);
  MVC.AddController(TApp1MainController);


  { You need to have a running database engine and a correct configuration in the FDConnectionDefs.ini file.
  Also you need to create a dmvc_sessions table using one of SQL DDL script available at }

  { Use the following line for MySQL database connection configured in FDConnectionDefs.ini}
  //MVC.AddMiddleware(TMVCActiveRecordMiddleware.Create('mysqldb'));

  { Use the following line for FirebirdSQL database connection configured in FDConnectionDefs.ini}
  MVC.AddMiddleware(TMVCActiveRecordMiddleware.Create('firebirddb'));
end;

end.
