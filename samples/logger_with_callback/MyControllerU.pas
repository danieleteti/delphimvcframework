unit MyControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function Index: String;

    [MVCPath('/log/debug')]
    [MVCHTTPMethod([httpPOST])]
    function LogDebug: String;

    [MVCPath('/log/info')]
    [MVCHTTPMethod([httpPOST])]
    function LogInfo: String;

    [MVCPath('/log/warning')]
    [MVCHTTPMethod([httpPOST])]
    function LogWarning: String;

    [MVCPath('/log/error')]
    [MVCHTTPMethod([httpPOST])]
    function LogError: String;

    [MVCPath('/log/fatal')]
    [MVCHTTPMethod([httpPOST])]
    function LogFatal: String;
  end;

implementation

uses
  MVCFramework.Logger;

function TMyController.Index: String;
begin
  Result := RenderView('index');
end;

function TMyController.LogDebug: String;
begin
  LogD('Debug message from web UI');
  Result := 'DEBUG log sent!';
end;

function TMyController.LogInfo: String;
begin
  LogI('Info message from web UI');
  Result := 'INFO log sent!';
end;

function TMyController.LogWarning: String;
begin
  LogW('Warning message from web UI');
  Result := 'WARNING log sent!';
end;

function TMyController.LogError: String;
begin
  LogE('Error message from web UI');
  Result := 'ERROR log sent!';
end;

function TMyController.LogFatal: String;
begin
  LogF('Fatal message from web UI');
  Result := 'FATAL log sent!';
end;

end.
