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


uses RoutingSampleControllerU, MVCFramework.Commons;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DMVC := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      if IsConsole then
        DMVC.Config['ISAPI_PATH'] := '/sampleisapi/isapiapp.dll';
    end);
  DMVC.AddController(TRoutingSampleController);
end;

end.
