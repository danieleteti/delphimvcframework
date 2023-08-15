unit SPARedirectController;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type

  [MVCPath]
  TSPARedirectController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TSPARedirectController.Index;
begin
  Redirect('/static');
end;

end.
