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
    procedure Index(ctx: TWebContext);

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld(ctx: TWebContext);

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpPOST])]
    procedure HelloWorldPost(ctx: TWebContext);

    [MVCPath('/div/($par1)/($par2)')]
    [MVCHTTPMethod([httpGET])]
    procedure RaiseException(ctx: TWebContext);
  end;

implementation

uses
{$IF CompilerVersion >= 27} System.JSON,
{$ELSE} Data.DBXJSON,
{$ENDIF}
  System.SysUtils;

{ TApp1MainController }

procedure TApp1MainController.HelloWorld(ctx: TWebContext);
begin
  Render('Hello World called with GET');
  if ctx.Request.ThereIsRequestBody then
    Log('Body:' + ctx.Request.Body);
end;

procedure TApp1MainController.HelloWorldPost(ctx: TWebContext);
var
  JSON: TJSONObject;
begin
  JSON := ctx.Request.BodyAsJSONObject;
  JSON.AddPair('modified', 'from server');
  Render(JSON, false);
  Log('Hello world called with POST');
end;

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  Redirect('index.html');
end;

procedure TApp1MainController.RaiseException(ctx: TWebContext);
var
  R: Extended;
begin
  Log('Parameter1=' + ctx.Request.Params['par1'].QuotedString);
  Log('Parameter2=' + ctx.Request.Params['par2'].QuotedString);
  R := StrToInt(ctx.Request.Params['par1']) /
    StrToInt(ctx.Request.Params['par2']);
  Render(TJSONObject.Create(TJSONPair.Create('result', TJSONNumber.Create(R))));
end;

end.
