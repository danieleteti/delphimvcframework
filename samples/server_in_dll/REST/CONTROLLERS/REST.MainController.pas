unit REST.MainController;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.logger,
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
  ContentType := TMVCMediaType.TEXT_PLAIN;
  Render('Hello DelphiMVCFramework World (Server is hosted by ' + TPath.GetFileName(GetModuleName(HInstance)) + ')');
end;

end.
