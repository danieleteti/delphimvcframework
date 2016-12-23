unit WebModuleU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

uses WebSiteControllerU, MVCFramework.Commons;

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // enable static files
      Config[TMVCConfigKey.DocumentRoot] :=
        ExtractFilePath(GetModuleName(HInstance)) + '\www';
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] :=
        TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] :=
        TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'mustache';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable STOMP messaging controller
      Config[TMVCConfigKey.Messaging] := 'false';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end).AddController(TWebSiteController);

end;

end.
