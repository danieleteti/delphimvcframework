unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController) 
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController.Index;
begin
  //use Context property to access to the HTTP request and response
  Render('X');
end;

end.
