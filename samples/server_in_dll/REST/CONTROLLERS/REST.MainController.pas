unit REST.MainController;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.logger,
  generics.collections;

type

  [MVCDoc('')]
  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils, System.IOUtils;

procedure TMainController.Index;
begin
  LogD('[TMainController] Index');
  Render('Hello DelphiMVCFramework World (Server is hosted by ' + TPath.GetFileName(GetModuleName(HInstance)) + ')');
end;

end.
