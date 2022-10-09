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
    [MVCPath('/welcome/($Name)')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure Welcome(const Name: string);

    [MVCPath('/image')]
    [MVCHTTPMethod([httpGET])]
    procedure GetImage;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  MVCFramework.Serializer.JSONDataObjects,
  MVCFramework.Serializer.Commons,
  JSONDataObjects;

{ TApp1MainController }

procedure TApp1MainController.GetImage;
begin
  Context.Response.ContentType := TMVCMediaType.IMAGE_PNG;
  SendFile(TPath.Combine(AppPath, 'logo.png'));
end;

procedure TApp1MainController.Welcome(const Name: string);
begin
  Render(Name + ', welcome to DMVCFramework!');
end;

end.
