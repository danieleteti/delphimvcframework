unit MyControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.SysUtils;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
  protected
    procedure OnException(const aContext: TWebContext; const aException: Exception; var aHandled: Boolean); override;
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(const ID: Integer);

    [MVCPath('/error')]
    [MVCHTTPMethod([httpGET])]
    procedure Error;
  end;

  TMyExceptionSeverity = (Fatal, Error, Warning, Information);

  EMyException = class(Exception)
  private
    FSeverity: TMyExceptionSeverity;
    FCode: Integer;
    FDetails: string;
    FDiagnostics: string;
    FExpression: string;
  public
    constructor Create(Msg: string; ASeverity: TMyExceptionSeverity; ACode: Integer; ADetails, ADiagnostics, AExpression: string);
    property Severity: TMyExceptionSeverity read FSeverity write FSeverity;
    property Code: Integer read FCode write FCode;
    property Details: string read FDetails write FDetails;
    property Diagnostics: string read FDiagnostics write FDiagnostics;
    property Expression: string read FExpression write FExpression;
  end;

implementation

uses
  MVCFramework.Logger, System.NetEncoding;

procedure TMyController.GetCustomer(const ID: Integer);
begin
  Render204NoContent();
end;

procedure TMyController.Index;
begin
  raise EMyException.Create('My Custom Error', Fatal, 25, 'some real problem', 'Ensure Patient resource is valid',
    'Patient/Identifier/value');
end;

procedure TMyController.OnException(const aContext: TWebContext; const aException: Exception; var aHandled: Boolean);
var
  lColor: string;
begin
  inherited;
  StatusCode := HTTP_STATUS.InternalServerError;
  ContentType := TMVCMediaType.TEXT_HTML;
  aContext.Response.Content := 'This is an error: ' + aException.Message;
  aContext.Response.StatusCode := HTTP_STATUS.InternalServerError;
  if aException is EMyException then
  begin
    case EMyException(aException).Severity of
      Fatal, TMyExceptionSeverity.Error:
        lColor := 'red';
      Warning:
        lColor := 'yellow';
      Information:
        lColor := 'blue';
    else
      lColor := 'black';
    end;
    aContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
    aContext.Response.Content := '<html><body><h1>Error occurred</h1>' + Format('<h2 style="color: %s">', [lColor]) +
      TNetEncoding.HTML.Encode(EMyException(aException).ToString) + '</h2>' +
      '<p>your truly custom controller exception handler...</p>' + '</body></html>';
    aHandled := True;
  end
  else if aException is EMVCException then
  begin
    aContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
    aContext.Response.Content := '<html><body><h1>Error occurred</h1>' + Format('<h2 style="color: red">', [lColor]) +
      TNetEncoding.HTML.Encode(aException.Message) + '</h2>' + '<p>your truly custom controller exception handler...</p>' + '</body></html>';
    aHandled := True;
  end;

end;

procedure TMyController.Error;
begin
  raise Exception.Create('Standard Error');
end;

constructor EMyException.Create(Msg: string; ASeverity: TMyExceptionSeverity; ACode: Integer; ADetails, ADiagnostics, AExpression: string);
begin
  inherited Create(Msg);
  FSeverity := ASeverity;
  FCode := ACode;
  FDetails := ADetails;
  FDiagnostics := ADiagnostics;
  FExpression := AExpression;
end;

end.
