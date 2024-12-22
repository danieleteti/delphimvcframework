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

implementation

uses
  MVCFramework.Logger, System.NetEncoding, TemplatePro;

procedure TMyController.GetCustomer(const ID: Integer);
begin
  Render204NoContent();
end;

procedure TMyController.Index;
begin
  raise EMVCException.Create(500, 'My Custom Error');
end;

procedure TMyController.OnException(const aContext: TWebContext; const aException: Exception; var aHandled: Boolean);
var
  lColor: string;
  lStatusCode: Word;
begin
  inherited;
  if Context.Request.ClientPrefer(TMVCMediaType.APPLICATION_JSON) then
  begin
    aHandled := False;
    Exit;
  end;

  ContentType := TMVCMediaType.TEXT_HTML;
  aHandled := True;
  if aException is EMVCException then
  begin
    lStatusCode := EMVCException(aException).HTTPStatusCode;
    if lStatusCode >= 500 then
      lColor := 'red'
    else if lStatusCode >= 400 then
      lColor := 'yellow'
    else if lStatusCode > 200 then
      lColor := 'blue';
    aContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
    aContext.Response.Content := TTProCompiler.CompileAndRender(
      '<html><body><h1>Error occurred</h1>' +
      '<h2 style="color: {{:color}}">{{:text}}</h2>' +
      '<p>your truly custom controller exception handler...</p></body></html>', ['color','text'], [lColor, aException.ToString]);
    aContext.Response.StatusCode := StatusCode;
  end
  else
  begin
    aContext.Response.StatusCode := HTTP_STATUS.InternalServerError;
    aContext.Response.Content := TTProCompiler.CompileAndRender(
      '<html><body><h1>Generic Error Occurred</h1>' +
      '<h2 style="color: red">{{:text}}</h2>' +
      '<p>your truly custom controller exception handler...</p></body></html>', ['text'], [aException.ToString]);
  end;
end;

procedure TMyController.Error;
begin
  raise Exception.Create('Standard Error');
end;

end.
