unit REST.MainController;

interface

uses
  mvcframework,
  mvcframework.Commons,
  mvcframework.logger,
  generics.collections;

type

  { TBaseController = class abstract(TMVCController)
    strict private
    strict protected
    public

    end; }

  [MVCDoc('')]
  [MVCPath('/api')]
  TMainController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  public

  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

procedure TMainController.Index;
begin
  LogD('[TMainController] Index');
  Render('Hello DelphiMVCFramework World');
end;

// ------------------------------------------------------------------------------
procedure TMainController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMainController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
