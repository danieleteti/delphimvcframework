unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function Index: String;

    [MVCPath('/notlogged')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]
    function GetButNotLogged: String;

  end;

implementation

uses
  System.StrUtils, System.SysUtils, MVCFramework.Logger;


function TMyController.Index: String;
begin
  //use Context property to access to the HTTP request and response
  Result := '<p>Hello <strong>DelphiMVCFramework</strong> World</p>' + 
            '<p><small>dmvcframework-' + DMVCFRAMEWORK_VERSION + '</small></p>';
end;

function TMyController.GetButNotLogged: String;
begin
  Result := DateTimeToStr(Now);
end;

end.
