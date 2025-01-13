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
    function Index: String;

    [MVCPath('/started')]
    [MVCHTTPMethod([httpGET])]
    function GetStarted: String;

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    function DoLogin(username: string): String;

    [MVCPath('/logout')]
    [MVCHTTPMethod([httpGET])]
    function DoLogout: String;

  end;

implementation

uses
  TemplatePro;

{ TApp1MainController }

function TApp1MainController.DoLogin(username: string): String;
begin
  Session['username'] := username;
  Result := TTProCompiler.CompileAndRender('<h2>Logged In</h2>', [], []);
end;

function TApp1MainController.DoLogout: String;
begin
  Context.SessionStop(false);
  Result := TTProCompiler.CompileAndRender('<h2>Logged Out</h2>', [], []);
end;

function TApp1MainController.GetStarted: String;
begin
  Result := TTProCompiler.CompileAndRender('<h2>{{:sessionstarted}}</h2>', ['sessionstarted'], [Context.SessionStarted]);
end;

function TApp1MainController.Index: String;
begin
  // do not create session if not already created
  if Context.SessionStarted then
  begin
    // automaticaly create the session
    Result := TTProCompiler.CompileAndRender('<h2>Hello {{:username}}</h2>', ['username'], [Session['username']]);
  end
  else
  begin
    StatusCode := HTTP_STATUS.BadRequest;
    Result := TTProCompiler.CompileAndRender('<h2 style="color: red">Session not created. Do login first!</h2>', [], []);
  end;
end;

end.
