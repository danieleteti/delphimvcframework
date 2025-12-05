unit AppControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger;

type
  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/name')]
    [MVCHTTPMethod([httpGET])]
    function Index: String;

    [MVCPath('/list')]
    [MVCHTTPMethod([httpGET])]
    function GetCustomSessionData: String;

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    function DoLogin(username: String): String;

    [MVCPath('/fruit/($nameOfFruit)')]
    [MVCHTTPMethod([httpGET])]
    procedure RegisterFruit(nameOfFruit: String);

    [MVCPath('/logout')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogout;

  end;

implementation

uses
  System.SysUtils,
  TemplatePro,
  System.Classes;

{ TApp1MainController }

function TApp1MainController.DoLogin(username: String): String;
begin
  Session['username'] := username;
  Result := TTProCompiler.CompileAndRender(
    '''
    <h2>Logged as {{:username}} </h2>

    Click on the following links to explore session features:<br>
    <a href="http://localhost:8080/list">http://localhost:8080/list</a>                   to check the current values in session <br>
    <a href="http://localhost:8080/fruit/apple">http://localhost:8080/fruit/apple</a>     to register apple <br>
    <a href="http://localhost:8080/fruit/banana">http://localhost:8080/fruit/banana</a>   to register banana <br>
    <a href="http://localhost:8080/fruit/banana?dtsessionid={{:sessionid}}">http://localhost:8080/fruit/banana(with session on url)</a>   to register banana <br>
    <a href="http://localhost:8080/logout">http://localhost:8080/logout</a>               to end session  <br>
    <a href="http://localhost:8080/login/johndoe">http://localhost:8080/login/johndoe</a> to login as johndoe <br>
    ''',
    ['username', 'sessionid'],
    [username, Session.SessionId]
    )
end;

procedure TApp1MainController.RegisterFruit(nameOfFruit: String);
begin
  Session[nameOfFruit] := nameOfFruit;
  Redirect('/list');
end;

procedure TApp1MainController.DoLogout;
begin
  Context.SessionStop(false);
  Render('Logout');
end;

function TApp1MainController.GetCustomSessionData: String;
var
  I: Integer;
  lList: TArray<String>;
begin
  lList := Session.Keys;
  Result := 'List of fruits: <ul>';
  for I := 0 to Length(lList) - 1 do
  begin
    Result := Result + '<li>' + IntToStr(I + 1) + ' - ' + Session[lList[I]] + '</li>';
  end;
  Result := Result + '</ul>';
end;

function TApp1MainController.Index: String;
begin
  // do not create session if not already created
  if Context.SessionStarted then
  begin
    // automaticaly create the session
    Result := 'Session[''username''] = ' + Session['username']
  end
  else
  begin
    StatusCode := HTTP_STATUS.BadRequest;
    Result := 'Session not created. Do login first';
  end;
end;

end.

