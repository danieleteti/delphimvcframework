/// <summary>
/// Admin-only user list controller. Displays all registered users
/// with their roles and login timestamps.
/// </summary>
unit UserControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Serves the user list at /users. Restricted to users with the
  /// admin role.
  /// </summary>
  [MVCPath('/users')]
  TUserController = class(TMVCController)
  public
    /// <summary>
    /// List all users. Redirects to /dashboard if the current user
    /// does not have the admin role.
    /// </summary>
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function List: string;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.ActiveRecord,
  UserEntityU;

function TUserController.List: string;
var
  LUsers: TObjectList<TUser>;
begin
  if not Context.LoggedUser.Roles.Contains('admin') then
  begin
    Redirect('/dashboard');
    Exit('');
  end;

  LUsers := TMVCActiveRecord.All<TUser>;
  ViewData['users'] := LUsers;
  ViewData['display_name'] := Context.LoggedUser.CustomData['display_name'];
  ViewData['is_admin'] := True;
  Result := RenderView('users');
end;

end.
