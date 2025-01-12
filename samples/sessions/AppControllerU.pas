unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/name')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/started')]
    [MVCHTTPMethod([httpGET])]
    function SessionStarted: Boolean;

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogin(username: string);

    [MVCPath('/logout')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogout;

  end;

implementation

{ TApp1MainController }

procedure TApp1MainController.DoLogin(username: string);
begin
  if not Context.SessionStarted then
  begin
    Session['username'] := username;
    Render(200, 'Logged in');
  end
  else
  begin
    Render('Session non started');
  end;
end;

procedure TApp1MainController.DoLogout;
begin
  Context.SessionStop(false);
  Render(200, 'Logged out');
end;

procedure TApp1MainController.Index;
begin
  // do not create session if not already created
  if Context.SessionStarted then
  begin
    // automaticaly create the session
    Render(200, 'Hello ' + Session['username']);
  end
  else
  begin
    Render(http_status.BadRequest, 'Session not created. Do login first');
  end;
end;


function TApp1MainController.SessionStarted: Boolean;
begin
  Result := Context.SessionStarted;
end;

end.
