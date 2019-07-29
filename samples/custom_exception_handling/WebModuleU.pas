unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  MVCFramework,
  Web.HTTPApp,
  System.NetEncoding;

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
  MyControllerU,
  MVCFramework.Commons,
  System.Rtti;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  lExceptionHandler: TMVCExceptionHandlerProc;
begin
  lExceptionHandler := procedure(
      E: Exception;
      SelectedController: TMVCController;
      WebContext: TWebContext;
      var ExceptionHandled: Boolean)

    var
      lColor: string;
    begin
      if E is EMyException then
      begin
        case EMyException(E).Severity of
          Fatal, Error:
            lColor := 'red';
          Warning:
            lColor := 'yellow';
          Information:
            lColor := 'blue';
        else
          lColor := 'black';
        end;
        WebContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
        WebContext.Response.Content :=
          '<html><body><h1>Error occurred</h1>' +
          Format('<h2 style="color: %s">', [lColor]) + TNetEncoding.HTML.Encode
          (EMyException(E).ToString) + '</h2>' +
          '<p>your truly custom exception handler...</p>' +
          '</body></html>';
        ExceptionHandled := True;
      end;
    end;

  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // enable static files
      Config[TMVCConfigKey.DocumentRoot] := ExtractFilePath(GetModuleName(HInstance)) + '\www';
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
  FMVC.AddController(TMyController);
  FMVC.SetExceptionHandler(lExceptionHandler);
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
