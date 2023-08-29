unit AppControllerU;

interface

uses MVCFramework,
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
  Data.DBXJSON,
  System.SysUtils,
  Classes,
  MemoryWebSessionController;

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
var
  lList: TStringList;
begin
  lList := SessionAs<TWebSessionMemoryController>.List;
  lList.Add(nameOfFruit);
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
  lList: TStringList;
begin
  if Session is TWebSessionMemoryController then // just if you want to do a check
  begin
    lList := SessionAs<TWebSessionMemoryController>.List;
    ResponseStream.AppendLine('List of fruits:');
    for I := 0 to (lList.Count - 1) do
    begin
      ResponseStream.AppendLine(IntToStr(I + 1) + '-' + lList[I]);
    end;
  end
  else
  begin
    ResponseStream.AppendLine('The current session is not a custom session (TWebSessionMemoryController)');
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
