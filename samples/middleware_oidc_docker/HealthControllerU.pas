/// <summary>
/// Health check endpoint for Docker container monitoring.
/// </summary>
unit HealthControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  /// <summary>
  /// Returns HTTP 200 with a JSON status object. Used by the Docker
  /// health check to verify the application is running.
  /// </summary>
  [MVCPath('/health')]
  THealthController = class(TMVCController)
  public
    /// <summary>
    /// Returns health status as JSON.
    /// </summary>
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure Health;
  end;

implementation

uses
  JsonDataObjects;

procedure THealthController.Health;
var
  LObj: TJsonObject;
begin
  LObj := TJsonObject.Create;
  LObj.S['status'] := 'ok';
  LObj.S['service'] := 'oidc-docker-sample';
  Render(LObj);
end;

end.
