// ***************************************************************************
//
// Delphi MVC Framework - Validation Showcase Sample
//
// ***************************************************************************

unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons;

type
  TValidationWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TValidationWebModule;

implementation

{$R *.dfm}

uses
  MVCFramework.Middleware.Compression,
  ValidationControllerU;

procedure TValidationWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.PathPrefix] := '';
      Config[TMVCConfigKey.DefaultContentType] := TMVCMediaType.APPLICATION_JSON;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      Config[TMVCConfigKey.LoadSystemControllers] := 'false';
    end);

  FMVCEngine.AddController(TValidationController);
  FMVCEngine.AddMiddleware(TMVCCompressionMiddleware.Create);
end;

procedure TValidationWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.Free;
end;

end.
