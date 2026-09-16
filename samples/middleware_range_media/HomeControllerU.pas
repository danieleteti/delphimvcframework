unit HomeControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/')]
  THomeController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    function Index: string;
  end;

implementation

function THomeController.Index: string;
begin
  Result := RenderView('index');
end;

end.
