unit WebModuleU;

interface

uses System.SysUtils,
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
  MVCFramework.Commons,
  MyControllerU,
  MVCFramework.Logger,
  MVCFramework.Middleware.StaticFiles, CustomLoggerConfigU;

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
    end, GetLogger);
  FMVC.AddController(TMyController);

  { This is a custom router log. It is not mandatory; you can use it to log
    more (or less or different) information than the default ones logs }

  {** Uncomment this section and see how the OnRouterLog event works **}

  {
  FMVC.OnRouterLog :=
      procedure(const Sender: TMVCCustomRouter; const RouterLogState: TMVCRouterLogState; const Context: TWebContext)
    begin
      Log('** CUSTOM ROUTER LOG **');
      case RouterLogState of
        rlsRouteFound:
          begin
            Log(TLogLevel.levNormal, Context.Request.HTTPMethodAsString + ':' + Context.Request.PathInfo + ' -> ' +
              Context.Request.ClientIp + ' ' +
              Sender.GetQualifiedActionName + ' - ' + IntToStr(Context.Response.StatusCode) + ' ' +
              Context.Response.ReasonString);
          end;
        rlsRouteNotFound:
          begin
            Log(TLogLevel.levNormal, Context.Request.HTTPMethodAsString + ':' + Context.Request.PathInfo + ' -> ' +
              ' <NOT FOUND> - ' + IntToStr(Context.Response.StatusCode) + ' ' +
              Context.Response.ReasonString);
          end;
      else
        raise EMVCException.Create('Invalid RouterLogState');
      end;
    end;
  }
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
