unit TestWebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type

  TTestWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  TestWebModuleClass: TComponentClass = TTestWebModule;

implementation

uses
  MVCFrameworkServerTestsU;

{$R *.dfm}

procedure TTestWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(Self);
  FMVCEngine.AddController(TTestAppController);
end;

procedure TTestWebModule.WebModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FMVCEngine);
end;

end.
