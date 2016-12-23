unit AppControllerU;

interface

uses MVCFramework,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/name')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogin(username: String);

    [MVCPath('/logout')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogout;

  end;

implementation

uses
  Data.DBXJSON,
  System.SysUtils, MVCFramework.Commons;

{ TApp1MainController }

procedure TApp1MainController.DoLogin(username: String);
begin
  Session['username'] := username;
  Render(200, 'Logged in');
end;

procedure TApp1MainController.DoLogout;
begin
  Context.SessionStop(false);
  Render(200, 'Logged out');
end;

procedure TApp1MainController.Index;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;

  // do not create session if not already created
  if Context.SessionStarted then
  begin
    // automaticaly create the session
    Render('Hello ' + Session['username']);
  end
  else
  begin
    Render(400, 'Session not created. Do login first');
  end;
end;

end.
