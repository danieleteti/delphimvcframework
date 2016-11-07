unit App1MainControllerU;

interface

uses MVCFramework,
  MVCFramework.Logger,
  Web.HTTPApp;

type

  [MVCPath('/')]
  TApp1MainController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld;

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpPOST])]
    procedure HelloWorldPost;

    [MVCPath('/div/($par1)/($par2)')]
    [MVCHTTPMethod([httpGET])]
    procedure RaiseException(par1, par2: String);
  end;

implementation

uses
{$IF CompilerVersion >= 27} System.JSON,
{$ELSE} Data.DBXJSON,
{$ENDIF}
  System.SysUtils;

{ TApp1MainController }

procedure TApp1MainController.HelloWorld;
begin
  Render('Hello World called with GET');
  if Context.Request.ThereIsRequestBody then
    Log('Body:' + Context.Request.Body);
end;

procedure TApp1MainController.HelloWorldPost;
var
  JSON: TJSONObject;
begin
  JSON := Context.Request.BodyAsJSONObject;
  JSON.AddPair('modified', 'from server');
  Render(JSON, false);
  Log('Hello world called with POST');
end;

procedure TApp1MainController.Index;
begin
  Redirect('index.html');
end;

procedure TApp1MainController.RaiseException(par1, par2: String);
var
  R: Extended;
begin
  Log('Parameter1=' + QuotedStr(par1));
  Log('Parameter2=' + QuotedStr(par2));
  R := StrToInt(par1) / StrToInt(par2);
  Render(TJSONObject.Create(TJSONPair.Create('result', TJSONNumber.Create(R))));
end;

end.
