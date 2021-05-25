unit CustomWebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type

  TCustomWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  protected
    procedure DoConfigureEngine(const aMVCEngine: TMVCEngine); virtual; abstract;
  public

  end;

implementation

{$R *.dfm}

uses
  App1MainControllerU,
  MVCFramework.Commons;

procedure TCustomWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // nothing to configure here
    end);
  DoConfigureEngine(FMVCEngine);
end;

procedure TCustomWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVCEngine.free;
end;

end.
