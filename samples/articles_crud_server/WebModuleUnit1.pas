unit WebModuleUnit1;


interface

uses System.SysUtils, System.Classes, Web.HTTPApp, mvcframework, FireDAC.Phys.FBDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

uses
  Controllers.Articles,
  MVCFramework.Middleware.CORS,
  MVCFramework.Middleware.ActiveRecord,
  MVCFramework.Middleware.Compression,
  MVCFramework.Middleware.Trace,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.Commons,
  Controllers.Base,
  Commons

  ;

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      //Enabling the following line, the API will start to respond from "/api/v1"
      //So "/articles/1" becomes "/api/v1/articles/1"
      //Config[TMVCConfigKey.PathPrefix] := '/api/v1';
    end);
  FEngine.AddController(TArticlesController);
{$IFDEF TESTINSTANCE}
  FEngine.AddController(TPrivateController);
{$ENDIF}
  FEngine.AddMiddleware(TCORSMiddleware.Create);
  FEngine.AddMiddleware(TMVCCompressionMiddleware.Create(256));
  FEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create(CON_DEF_NAME));

//  FEngine.AddMiddleware(TMVCTraceMiddleware.Create);
end;

end.
