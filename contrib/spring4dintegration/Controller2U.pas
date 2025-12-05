unit Controller2U;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/controller2')]
  TMyController2 = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController2.Index;
begin
  // use Context property to access to the HTTP request and response
  Render('Hello DelphiMVCFramework World');
end;

end.
