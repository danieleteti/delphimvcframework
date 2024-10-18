unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/api')]
  TMyController = class(TMVCController) 
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/reversedstrings/($Value)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetReversedString(const Value: String);
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  end;

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
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

procedure TMyController.Index;
begin
  //use Context property to access to the HTTP request and response 
  Render('Hello DelphiMVCFramework World');
end;

procedure TMyController.GetReversedString(const Value: String);
begin
  Render(System.StrUtils.ReverseString(Value.Trim));
end;

procedure TMyController.OnAfterAction(Context: TWebContext; const AActionName: string); 
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;



{ TRedirectController }

procedure TRedirectController.DoRedirect;
begin
  Redirect('/swagger')
end;

procedure TRedirectController.DoRedirectIndex;
begin
  DoRedirect;
end;

end.
