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
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Serializer.JSONDataObjects,
  MVCFramework.Serializer.Commons,
  JSONDataObjects;

{ TApp1MainController }

procedure TApp1MainController.Welcome(const Name: string);
begin
  Render(Name + ', welcome to DMVCFramework!');
end;

end.
