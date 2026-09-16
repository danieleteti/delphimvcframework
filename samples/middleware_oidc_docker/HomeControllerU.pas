/// <summary>
/// Public landing page controller. Displays a login button or a
/// link to the dashboard depending on authentication state.
/// </summary>
unit HomeControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Serves the public landing page at the root URL.
  /// </summary>
  [MVCPath('/')]
  THomeController = class(TMVCController)
  public
    /// <summary>
    /// Render the landing page. Sets the logged_in flag based on
    /// whether the user has a valid session.
    /// </summary>
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function Index: string;
  end;

implementation

function THomeController.Index: string;
begin
  ViewData['logged_in'] := Context.LoggedUser.IsValid;
  Result := RenderView('landing');
end;

end.
