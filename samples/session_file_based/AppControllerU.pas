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
    procedure Index;

    [MVCPath('/list')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomSessionData;

    [MVCPath('/login/($username)')]
    [MVCHTTPMethod([httpGET])]
    procedure DoLogin(username: String);

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

procedure TApp1MainController.DoLogin(username: String);
begin
  Session['username'] := username;
  ResponseStream
    .AppendLine('Logged as ' + username)
    .AppendLine
    .AppendLine('in address of browser type: ')
    .AppendLine('http://localhost:8080/list             to check the current values in session ')
    .AppendLine('http://localhost:8080/fruit/apple      to register apple ')
    .AppendLine('http://localhost:8080/fruit/banana     to register banana ')
    .AppendLine('http://localhost:8080/logout           to end session ')
    .AppendLine('http://localhost:8080/login/johndoe    to login as johndoe');
  RenderResponseStream;
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

procedure TApp1MainController.GetCustomSessionData;
var
  I: Integer;
  lList: TArray<String>;
begin
  lList := Session.Keys;
  ResponseStream.AppendLine('List of fruits:');
  for I := 0 to Length(lList) - 1 do
  begin
    ResponseStream.AppendLine(IntToStr(I + 1) + '-' + Session[lList[I]]);
  end;
  RenderResponseStream;
end;

procedure TApp1MainController.Index;
begin
  ContentType := TMVCMediaType.TEXT_PLAIN;

  // do not create session if not already created
  if Context.SessionStarted then
  begin
    // automaticaly create the session
    Render('Session[''username''] = ' + Session['username']);
  end
  else
  begin
    Render(400, 'Session not created. Do login first');
  end;
end;

end.

