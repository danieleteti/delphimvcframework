unit MyController1U;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type

  [MVCPath('/api')]
  TMyController1 = class(TMVCController)
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  JsonDataObjects,
  System.SysUtils,
  System.DateUtils,
  MVCFramework.Controllers.Register;

{ TMyController1 }

procedure TMyController1.Index;
var
  LObj: TJDOJsonObject;
begin
  LObj := TJDOJsonObject.Create;
  LObj.S['application'] := Context.Config[TMVCConfigKey.ServerName];
  LObj.B['online'] := True;
  LObj.S['serverdatetime'] := DateToISO8601(Now);
  Render(LObj);
end;

initialization

TControllersRegister.Instance.RegisterController(TMyController1, 'MyServerName');

end.
