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
  System.SysUtils, MVCFramework.Commons,
  Classes,MemoryWebSessionController;

{ TApp1MainController }

procedure TApp1MainController.DoLogin(username: String);
var strRetorno:string;
  I: Integer;
  lista:TStringList;
begin
  Session['username'] := username;
  strRetorno :=  'Logged as '+userName;
  strRetorno := strRetorno + #13 +
     'in address of browser type: ' + #13 +
     'http://localhost:8080/fruit/apple      to register apple ' + #13 +
     'http://localhost:8080/fruit/banana     to register banana ' + #13 +
     'http://localhost:8080/logout           to end session ' + #13 +
     'http://localhost:8080/login/joao       to login as joao';
  Render(strRetorno);
end;

procedure TApp1MainController.RegisterFruit(nameOfFruit: String);
var strRetorno:string;
  I: Integer;
  lista:TStringList;
begin
  strRetorno:='';
  if Session is TWebSessionMemoryController then
  begin
     lista:=(Session as TWebSessionMemoryController).list;
     lista.Add(nameOfFruit);
     strRetorno := strRetorno + #13+ 'List of fruit´s:';
     for I := 0 to (lista.Count-1) do
     begin
        strRetorno := strRetorno + #13+ IntToStr(i+1) +'-'+ lista.Strings[i];
     end;
  end;
  Render(strRetorno);
end;

procedure TApp1MainController.DoLogout;
begin
  Context.SessionStop(false);
  Render( 'Logout');
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
