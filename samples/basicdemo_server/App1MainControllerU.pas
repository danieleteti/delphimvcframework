unit App1MainControllerU;

interface

{$I dmvcframework.inc}

uses MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons,
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
    procedure RaiseException(par1, par2: string);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.SystemJSONUtils,
  System.JSON;

{ TApp1MainController }

procedure TApp1MainController.HelloWorld;
begin
  Render('Hello World called with GET');
  if Context.Request.ThereIsRequestBody then
    Log.Info('Body:' + Context.Request.Body, 'basicdemo');
end;

procedure TApp1MainController.HelloWorldPost;
var
  JSON: TJSONObject;
begin
  JSON := TSystemJSON.StringAsJSONObject(Context.Request.Body);
  try
    JSON.AddPair('modified', 'from server');
    Render(JSON, false);
  finally
    JSON.Free;
  end;
  Log.Info('Hello world called with POST', 'basicdemo');
end;

procedure TApp1MainController.Index;
begin
  Redirect('index.html');
end;

procedure TApp1MainController.RaiseException(par1, par2: string);
var
  R: Extended;
begin
  Log.Info('Parameter1=' + QuotedStr(par1), 'basicdemo');
  Log.Info('Parameter2=' + QuotedStr(par2), 'basicdemo');
  R := StrToInt(par1) / StrToInt(par2);
  Render(TJSONObject.Create(TJSONPair.Create('result', TJSONNumber.Create(R))));
end;

end.
