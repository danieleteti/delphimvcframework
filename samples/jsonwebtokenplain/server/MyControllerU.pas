unit MyControllerU;

interface

uses
  MVCFramework;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    procedure DoLogin(ctx: TWebContext);
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  end;

implementation

uses
  MVCFramework.JWT, MVCFramework.Commons, System.SysUtils, System.DateUtils;

procedure TMyController.DoLogin(ctx: TWebContext);
begin

end;

procedure TMyController.Index(ctx: TWebContext);
begin
  Render('"Hello World" from a JWT protected resource');
end;

procedure TMyController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
var
  lJWT: TJWT;
  lAuthHeader: string;
  lToken: string;
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;

  if SameText(AActionName, 'DOLogin') and (Context.Request.HTTPMethod = httpPOST) then
  begin
    lJWT := TJWT.Create('mysecret');
    try
      lJWT.Claims.Issuer := 'dmvcframework app';
      lJWT.Claims.ExpirationTime := Now + OneHour;
      lJWT.Claims.NotBefore := Now - OneMinute * 5;
      lJWT.CustomClaims['username'] := 'dteti';
      Render(lJWT.GetToken);
      Exit;
    finally
      lJWT.Free;
    end;
  end;

  lJWT := TJWT.Create('mysecret');
  try
    lAuthHeader := Context.Request.Headers['Authentication'];
    if lAuthHeader.IsEmpty then
    begin
      Render(http_status.Unauthorized, 'Authentication Required');
      Handled := True;
      Exit;
    end;

    if lAuthHeader.StartsWith('bearer', True) then
    begin
      lToken := lAuthHeader.Remove(0, 'bearer'.Length).Trim;
    end;

    if not lJWT.IsValidToken(lToken) then
    begin
      Render(http_status.Unauthorized, 'Invalid Token, Authentication Required');
      Handled := True;
    end
    else
    begin
      lJWT.LoadToken(lToken);
      if lJWT.CustomClaims['username'].IsEmpty then
      begin
        Render(http_status.Unauthorized, 'Invalid Token, Authentication Required');
        Handled := True;
      end
      else
        Handled := False;
    end;
  finally
    lJWT.Free;
  end;

end;

end.
