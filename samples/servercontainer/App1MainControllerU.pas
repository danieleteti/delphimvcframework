unit App1MainControllerU;

{$I dmvcframework.inc}

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
    procedure RaiseException;
  end;

implementation

uses
  JsonDataObjects,
  MVCFramework.Serializer.JsonDataObjects,
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
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    JSON := lSer.ParseObject(Context.Request.Body);
    try
      JSON.S['modified'] := 'from server';
      Render(JSON, False);
      Log('Hello world called with POST');
    finally
      JSON.Free;
    end;
  finally
    lSer.Free;
  end;
end;

procedure TApp1MainController.Index;
begin
  Redirect('index.html');
end;

procedure TApp1MainController.RaiseException;
var
  R: Extended;
  lJSON: TJSONObject;
begin
  Log('Parameter1=' + Context.Request.Params['par1'].QuotedString);
  Log('Parameter2=' + Context.Request.Params['par2'].QuotedString);
  R := StrToInt(Context.Request.Params['par1']) / StrToInt(Context.Request.Params['par2']);
  lJSON := TJSONObject.Create;
  lJSON.F['result'] := R;
  Render(lJSON);
end;

end.
