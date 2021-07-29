unit App1MainControllerU;

interface

{$I dmvcframework.inc}

uses
  MVCFramework,
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
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure HelloWorld;

    [MVCPath('/hello')]
    [MVCHTTPMethod([httpPOST])]
    procedure HelloWorldPost;

    [MVCPath('/div/($par1)/($par2)')]
    [MVCHTTPMethod([httpGET])]
    procedure RaiseException(const par1, par2: integer);
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Serializer.JSONDataObjects,
  MVCFramework.Serializer.Commons,
  JSONDataObjects;

{ TApp1MainController }

procedure TApp1MainController.HelloWorld;
begin
  Render('Hello World! It''s ' + TimeToStr(Time) + ' in the DMVCFramework Land!');
end;

procedure TApp1MainController.HelloWorldPost;
var
  lJSON: TJSONObject;
begin
  lJSON := StrToJSONObject(Context.Request.Body);
  try
    Log.Info('Hello world called with POST (request is: ' + lJSON.ToJSON(True) + ')', 'basicdemo');
    lJSON.S['modified'] := 'from server';
    Render(lJSON, false);
  finally
    lJSON.Free;
  end;
end;

procedure TApp1MainController.Index;
begin
  Redirect('/app/index.html');
end;

procedure TApp1MainController.RaiseException(const par1, par2: integer);
var
  lFS: TFormatSettings;
begin
  lFS.DecimalSeparator := '.';
  Log.Info('Parameter1=%d', [par1], 'basicdemo');
  Log.Info('Parameter2=%d', [par2], 'basicdemo');
  Render(StrDict().Add('result', FloatToStr(par1 / par2, lFS)));
end;

end.
