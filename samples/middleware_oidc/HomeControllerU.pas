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

    [MVCPath('/dashboard')]
    [MVCHTTPMethod([httpGET])]
    function Dashboard: string;
  end;

implementation

uses
  System.SysUtils;

function THomeController.Index: string;
begin
  ViewData['logged_in'] := Context.LoggedUser.IsValid;
  Result := RenderView('index');
end;

function THomeController.Dashboard: string;
begin
  ViewData['sub'] := Context.LoggedUser.CustomData['sub'];
  ViewData['email'] := Context.LoggedUser.CustomData['email'];
  ViewData['display_name'] := Context.LoggedUser.CustomData['display_name'];
  Result := RenderView('dashboard');
end;

end.
