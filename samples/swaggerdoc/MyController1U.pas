unit MyController1U;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

const
  INDEX_JSON_SCHEMA =
    '{' + sLineBreak +
    '    "type": "object",' + sLineBreak +
    '    "properties": {' + sLineBreak +
    '        "application": {' + sLineBreak +
    '            "type": "string",' + sLineBreak +
    '            "description": "Application Name"' + sLineBreak +
    '        },' + sLineBreak +
    '        "online": {' + sLineBreak +
    '            "type": "boolean",' + sLineBreak +
    '            "description": "Defines if the server is online"' + sLineBreak +
    '        },' + sLineBreak +
    '        "serverdatetime": {' + sLineBreak +
    '            "type": "string",' + sLineBreak +
    '            "description": "Current server time"' + sLineBreak +
    '        }' + sLineBreak +
    '    }' + sLineBreak +
    '}';

type

  [MVCPath('/status')]
  TMyController1 = class(TMVCController)
  public
    [MVCPath('')]
    [MVCSwagSummary('Status', 'API Status')]
    [MVCSwagResponses(200, 'Success', INDEX_JSON_SCHEMA)]
    [MVCSwagResponses(500, 'Internal Server Error')]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

  [MVCSwagIgnorePath]
  [MVCPath]
  TRedirectController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure DoRedirect;

    [MVCPath('/index.html')]
    [MVCHTTPMethod([httpGET])]
    procedure DoRedirectIndex;
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

{ TRedirectController }

procedure TRedirectController.DoRedirect;
begin
  Redirect('/swagger');
end;

procedure TRedirectController.DoRedirectIndex;
begin
  DoRedirect;
end;

initialization

//TControllersRegister.Instance.RegisterController(TMyController1, 'MyServerName');
//TControllersRegister.Instance.RegisterController(TRedirectController, 'MyServerName');

end.
