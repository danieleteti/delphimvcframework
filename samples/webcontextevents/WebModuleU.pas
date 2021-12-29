unit WebModuleU;

interface

uses 
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework, ServicesU;

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
  MainControllerU,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.StaticFiles,
  MVCFramework.Middleware.Compression;


type
  TMyService = class(TInterfacedObject, ICalculator)
  public
    function DoCalc(a,b: Integer): Integer;
  end;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      //default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      //default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      //unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      //enables or not system controllers loading (available only from localhost requests)
      Config[TMVCConfigKey.LoadSystemControllers] := 'true';
      //default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      //view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      //Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      //Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      //Enable X-Powered-By Header in response
      Config[TMVCConfigKey.ExposeXPoweredBy] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
  FMVC.AddController(TMyController);

  // Enable the following middleware declaration if you want to
  // serve static files from this dmvcframework service.
  // The folder mapped as documentroot must exists!
  // FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create( 
  //    '/static', 
  //    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www')) 
  //  );	
  FMVC.OnWebContextCreate(
    procedure(const Ctx: TWebContext)
    begin
      Ctx.CustomIntfObject := TMyService.Create;
    end);
  FMVC.OnWebContextDestroy(
    procedure(const Ctx: TWebContext)
    begin

    end);
  // To enable compression (deflate, gzip) just add this middleware as the last one
  FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

{ TMyService }

function TMyService.DoCalc(a, b: Integer): Integer;
begin
  Result := a + b;
end;

end.
