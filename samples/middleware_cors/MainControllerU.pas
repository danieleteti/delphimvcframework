unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer(const [MVCFromBody] Dict: TMVCStringDictionary);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController.CreateCustomer(const [MVCFromBody] Dict: TMVCStringDictionary);
begin
  Render(
    ObjectDict().Add('data', StrDict.Add('message', Dict['hello']))
  )
end;

end.
