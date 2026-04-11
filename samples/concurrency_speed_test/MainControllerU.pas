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
    function Index: IMVCResponse;
  end;

implementation

uses
  System.SysUtils;

function TMyController.Index: IMVCResponse;
begin
  Result := OKResponse('X');
end;

end.
