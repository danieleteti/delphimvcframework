unit ControllersU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/api')]
  TTestController = class(TMVCController)
  public
    // the action raises: the engine catches it and logs it
    [MVCPath('/boom')]
    [MVCHTTPMethod([httpGET])]
    function Boom: IMVCResponse;

    // the action logs an error of its own, no exception
    [MVCPath('/plain')]
    [MVCHTTPMethod([httpGET])]
    function Plain: IMVCResponse;

    [MVCPath('/info')]
    [MVCHTTPMethod([httpGET])]
    function Info: IMVCResponse;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger;

function TTestController.Boom: IMVCResponse;
begin
  raise Exception.Create('Something went wrong in /api/boom');
end;

function TTestController.Plain: IMVCResponse;
begin
  LogE('a plain error from the action');
  Result := OKResponse('ok');
end;

function TTestController.Info: IMVCResponse;
begin
  LogI('an info from the action');
  Result := OKResponse('ok');
end;

end.
