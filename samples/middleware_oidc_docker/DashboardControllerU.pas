/// <summary>
/// Protected dashboard controller showing the authenticated user's
/// OIDC claims and application role.
/// </summary>
unit DashboardControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Serves the dashboard page at /dashboard. Requires authentication
  /// via the OIDC middleware.
  /// </summary>
  [MVCPath('/dashboard')]
  TDashboardController = class(TMVCController)
  public
    /// <summary>
    /// Render the dashboard with OIDC claims and role information
    /// from the current session.
    /// </summary>
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function Dashboard: string;
  end;

implementation

uses
  System.SysUtils;

function TDashboardController.Dashboard: string;
begin
  ViewData['sub'] := Context.LoggedUser.CustomData['sub'];
  ViewData['email'] := Context.LoggedUser.CustomData['email'];
  ViewData['display_name'] := Context.LoggedUser.CustomData['display_name'];
  ViewData['user_id'] := Context.LoggedUser.CustomData['user_id'];
  ViewData['role'] := Context.LoggedUser.CustomData['role'];
  ViewData['is_admin'] := Context.LoggedUser.Roles.Contains('admin');
  Result := RenderView('dashboard');
end;

end.
