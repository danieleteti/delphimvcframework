unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons,
  System.Generics.Collections;

type
  [MVCPath]
  TMyController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;
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
  System.StrUtils, System.SysUtils, MVCFramework.Logger, MVCFramework.HTMX, RandomUtilsU,
  TemplatePro;

function TMyController.Customers: String;
begin
  var lCustomers := GetPeople();
  try
    ViewData['customers'] := lCustomers;
    Result := Page(['pages/customers']);
  finally
    lCustomers.Free;
  end;
end;

function TMyController.Home: String;
begin
  Result := Page(['pages/home']);
end;

procedure TMyController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  ViewData['ispage'] := not AContext.Request.IsHTMX;
end;

function TMyController.Posts: String;
begin
  var lPosts := GetPosts(20);
  try
    ViewData['posts'] := lPosts;
    Result := Page('pages/posts');
  finally
    lPosts.Free;
  end;
end;

function TMyController.Users: String;
begin
  var lUsers := GetUsers();
  try
    ViewData['users'] := lUsers;
    Result := Page('pages/users', False,
    procedure (const Tmpl: TObject)
    begin
      (Tmpl as TTProCompiledTemplate).SetData('var1', 1234);
    end);
  finally
    lUsers.Free;
  end;
end;

end.
