unit uConfig.Module;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TConfigModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
  end;

var
  WebModuleClass: TComponentClass = TConfigModule;

implementation

{$R *.dfm}

uses
  uBase.Controller,
  uMovie.Controller,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.HTMX,
  JsonDataObjects,
  MVCFramework.View.Renderers.Mustache;

procedure TConfigModule.WebModuleCreate(Sender: TObject);
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
      // enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := 'true';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'htmx';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'htmx_templates';
      // Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Enable X-Powered-By Header in response
      Config[TMVCConfigKey.ExposeXPoweredBy] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);

  (* Use Mustache View Engine *)
  FMVC.SetViewEngine(TMVCMustacheViewEngine);

  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create('/img', TPath.Combine(ExtractFilePath(GetModuleName(HInstance)),
    'www\img')));

  FMVC.AddController(TBaseController);
  FMVC.AddController(TMovieController);

  FMVC.SetExceptionHandler(
    procedure(E: Exception; SelectedController: TMVCController; WebContext: TWebContext; var ExceptionHandled: Boolean)

    function ProcessJSONErrors: TJsonObject;
    var
      Nested: Exception;
      Errors: TJsonArray;
      Error: TJsonObject;
    begin
      Result := TJsonObject.Create;
      Errors := TJsonArray.Create;
      Result.A['errors'] := Errors;
      Nested := E;
      while Nested <> nil do
      begin
        Error := TJsonObject.Create;
        Error.S['className'] := Nested.ClassName;
        Error.S['message'] := Nested.Message;
        Errors.Add(Error);
        Nested := Nested.InnerException;
      end;
    end;

    begin
      if WebContext.Request.IsHTMX then
      begin
        if SelectedController <> nil then
        begin
          SelectedController.Render(E.Message);
          // else etc
          ExceptionHandled := true;
          WebContext.Response.StatusCode := 400;
        end;

      end;

    end);
end;

procedure TConfigModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
