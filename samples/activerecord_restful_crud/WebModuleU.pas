unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet;

type
  TMyWebModule = class(TWebModule)
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;
  ConnectionDefinitionName: string = '';

implementation

{$R *.dfm}


uses
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.ActiveRecordController,
  MVCFramework.ActiveRecord,
  MVCFramework.Middleware.StaticFiles,
  FDConnectionConfigU,
  OtherControllerU;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end);

  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www'), { DocumentRoot }
    'index.html' { IndexDocument }
    ));
  FMVC.AddController(TOtherController, '/api/foo');
  FMVC.AddController(TMVCActiveRecordController,
    function: TMVCController
    begin
      Result := TMVCActiveRecordController.Create(
        function: TFDConnection
        begin
          Result := TFDConnection.Create(nil);
          Result.ConnectionDefName := ConnectionDefinitionName;
        end,
        function(aContext: TWebContext; aClass: TMVCActiveRecordClass; aAction: TMVCActiveRecordAction): Boolean
        begin
          if aContext.LoggedUser.IsValid then
          begin
            Result := True;
          end
          else
          begin
            Result := True; // not(aAction in [TMVCActiveRecordAction.Delete]);
          end;
        end);
    end, '/api/entities');
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
