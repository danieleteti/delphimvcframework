unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp, MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    DMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation


{$R *.dfm}


uses RoutingSampleControllerU;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DMVC := TMVCEngine.Create(self);
  DMVC.AddController(TRoutingSampleController);
  if IsConsole then
    DMVC.Config['ISAPI_PATH'] := '/sampleisapi/isapiapp.dll';
end;

end.
