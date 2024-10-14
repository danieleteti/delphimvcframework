unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections, Web.Stencils;

type
  [MVCPath]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function Home: String;

    [MVCPath('/users')]
    [MVCHTTPMethod([httpGET])]
    function Users: String;

    [MVCPath('/posts')]
    [MVCHTTPMethod([httpGET])]
    function Posts: String;

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGET])]
    function Customers: String;
  end;

implementation

uses
  System.StrUtils, System.SysUtils, MVCFramework.Logger, MVCFramework.HTMX, RandomUtilsU;

function TMyController.Customers: String;
begin
  var lCustomers := GetPeople();
  try
    ViewData['customers'] := lCustomers;
    Result := Page('customers' + IfThen(Context.Request.IsHTMX, '_body'))
  finally
    lCustomers.Free;
  end;
end;

function TMyController.Home: String;
begin
  Result := Page('home' + IfThen(Context.Request.IsHTMX, '_body'))
end;

function TMyController.Posts: String;
begin
  var lPosts := GetPosts(20);
  try
    ViewData['posts'] := lPosts;
    Result := Page('posts' + IfThen(Context.Request.IsHTMX, '_body'));
  finally
    lPosts.Free;
  end;
end;

function TMyController.Users: String;
begin
  var lUsers := GetUsers();
  try
    ViewData['users'] := lUsers;
    Result := Page('users' + IfThen(Context.Request.IsHTMX, '_body'));
  finally
    lUsers.Free;
  end;
end;

end.
