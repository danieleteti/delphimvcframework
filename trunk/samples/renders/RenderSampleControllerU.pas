unit RenderSampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, ObjectsMappers;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  public
    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('application/json')]
    procedure GetCustomers(CTX: TWebContext);

    [MVCHTTPMethod([httpGet])]
    [MVCPath('/customers')]
    [MVCProduces('text/xml')]
    procedure GetCustomersXML(CTX: TWebContext);

  end;

implementation

uses
  System.SysUtils, BusinessObjectsU, Data.DBXJSON, WebModuleU;

{ TRoutingSampleController }

procedure TRenderSampleController.GetCustomers(CTX: TWebContext);
var
  wm: TWebModule1;
begin
  wm := GetCurrentWebModule as TWebModule1;
  wm.qryCustomers.Open;
  Render(wm.qryCustomers);
end;

procedure TRenderSampleController.GetCustomersXML(CTX: TWebContext);
var
  wm: TWebModule1;
begin
  wm := GetCurrentWebModule as TWebModule1;
  wm.qryCustomers.Open;
  Render(wm.qryCustomers);
end;

end.
