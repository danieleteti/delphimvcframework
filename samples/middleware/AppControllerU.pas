unit AppControllerU;

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
  end;

implementation

uses
  System.SysUtils, MVCFramework.Commons;

{ TApp1MainController }

procedure TApp1MainController.Index(ctx: TWebContext);
begin
  ContentType := TMVCMimeType.TEXT_PLAIN;
  Render(StringOfChar('*', 1024));
end;

end.
