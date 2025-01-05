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
  System.Classes;

{ TApp1MainController }

function TApp1MainController.DoLogin(username: String): String;
begin
  Session['username'] := username;
  Result :=
    'Logged as ' + username + sLineBreak +
     sLineBreak +
    'In the browser address bar, you can write: ' + sLineBreak +
    'http://localhost:8080/list             to check the current values in session ' + sLineBreak +
    'http://localhost:8080/fruit/apple      to register apple ' + sLineBreak +
    'http://localhost:8080/fruit/banana     to register banana ' + sLineBreak +
    'http://localhost:8080/logout           to end session ' + sLineBreak +
    'http://localhost:8080/login/johndoe    to login as johndoe';
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
  Result := 'List of fruits:' + sLineBreak;
  for I := 0 to Length(lList) - 1 do
  begin
    Result := Result + sLineBreak + IntToStr(I + 1) + '-' + Session[lList[I]] + sLineBreak;
  end;
end;

function TApp1MainController.Index: String;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;

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

