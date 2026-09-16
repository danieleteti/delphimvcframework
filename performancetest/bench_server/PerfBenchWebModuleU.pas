unit PerfBenchWebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TPerfBenchWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TPerfBenchWebModule;

implementation

{$R *.dfm}

uses
  PerfBenchEngineConfigU,
  MVCFramework.Commons;

procedure TPerfBenchWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  ConfigureBenchEngine(FMVC);
end;

procedure TPerfBenchWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
