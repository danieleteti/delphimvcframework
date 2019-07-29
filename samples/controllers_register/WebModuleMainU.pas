unit WebModuleMainU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FEngine: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  MVCFramework.Controllers.Register;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self);

  ///  Add your registered controllers to engine.
  ///  Only registered controls such as "MyServerName" will be added
  TControllersRegister.Instance.AddControllersInEngine(FEngine, 'MyServerName');
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

end.
