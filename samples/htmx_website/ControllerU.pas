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
  System.StrUtils, System.SysUtils, MVCFramework.Logger, MVCFramework.HTMX;

function TMyController.Customers: String;
begin
  Result := Page(['pages/customers'], not Context.Request.IsHTMX);
end;

function TMyController.Home: String;
begin
  Result := Page(['pages/home'], not Context.Request.IsHTMX);
end;

procedure TMyController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  SetPagesCommonHeaders(['_header']);
  SetPagesCommonFooters(['_footer']);
end;

function TMyController.Posts: String;
begin
  Result := Page(['pages/posts'], not Context.Request.IsHTMX);
end;

function TMyController.Users: String;
begin
  Result := Page(['pages/users'], not Context.Request.IsHTMX);
end;

end.
