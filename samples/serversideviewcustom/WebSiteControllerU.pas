unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, System.JSON, MVCFramework.Commons;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    procedure Index;
  end;

implementation

{ TWebSiteController }

uses System.SysUtils, Web.HTTPApp, MyDataModuleU;

procedure TWebSiteController.Index;
var
  lDM: TMyDataModule;
begin
  ContentType := BuildContentType(TMVCMediaType.TEXT_HTML, tmvcCharSet.UTF_8);
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    ViewDataset['people'] := lDM.qryCustomers;
    LoadView(['header', 'people_list', 'footer']);
  finally
    lDM.Free;
  end;
  RenderResponseStream; // rember to call RenderResponseStream!!!
end;

end.
