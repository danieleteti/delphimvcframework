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

    [MVCPath('/inc')]
    [MVCHTTPMethod([httpGET])]
    function DoInc: String;

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

uses
  System.SysUtils;

{ TApp1MainController }

function TApp1MainController.DoInc: String;
begin
  Session['value'] := (StrToIntDef(Session['value'], 0) + 1).ToString;
  Result := Session['value'];
end;

procedure TApp1MainController.DoLogin(username: string);
begin
  Session['username'] := username;
  Render(200, 'Logged in');
end;

procedure TApp1MainController.DoLogout;
begin
  Context.SessionStop(False);
  Render(200, 'Logged out');
  Assert(not Context.SessionStarted);
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
