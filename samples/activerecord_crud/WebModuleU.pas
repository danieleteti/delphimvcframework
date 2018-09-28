unit WebModuleU;

interface

uses
  MVCFramework.RQL.AST2FirebirdSQL, {RQL Compiler for firebirdsql}
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}


uses
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.ActiveRecordController,
  MVCFramework.ActiveRecord,
  FireDAC.Comp.Client,
  FDConnectionConfigU;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // enable static files
      Config[TMVCConfigKey.DocumentRoot] := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www');
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
      // Define a default URL for requests that don't map to a route or a file (useful for client side web app)
      Config[TMVCConfigKey.FallbackResource] := 'index.html';
    end);
  FMVC.AddController(TMVCActiveRecordController,
    function: TMVCController
    begin
      Result := TMVCActiveRecordController.Create(
        function: TFDConnection
        begin
          Result := TFDConnection.Create(nil);
          Result.ConnectionDefName := CON_DEF_NAME;
          Result.Open;
        end,
        function(aContext: TWebContext; aClass: TMVCActiveRecordClass; aAction: TMVCActiveRecordAction): Boolean
        begin
          if aContext.LoggedUser.IsValid then
          begin
            Result := True;
          end
          else
          begin
            Result := not(aAction in [TMVCActiveRecordAction.Delete]);
          end;
        end);
    end, '/api/entities');
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
