unit TestServerControllerExceptionU;

interface

uses
  MVCFramework;

type

  [MVCPath('/exception/aftercreate')]
  TTestServerControllerExceptionAfterCreate = class(TMVCController)

  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;
  public
    [MVCPath('/nevercalled')]
    procedure NeverCalled(CTX: TWebContext);
  end;

  [MVCPath('/exception/beforedestroy')]
  TTestServerControllerExceptionBeforeDestroy = class(TMVCController)

  protected
    procedure MVCControllerAfterCreate; override;
    procedure MVCControllerBeforeDestroy; override;
  public
    [MVCPath('/nevercalled')]
    procedure NeverCalled(CTX: TWebContext);
  end;

implementation

uses
  System.SysUtils;

{ TTestServerControllerException }

procedure TTestServerControllerExceptionAfterCreate.MVCControllerAfterCreate;
begin
  inherited;
  raise Exception.Create('This is an exception raised in the MVCControllerAfterCreate');
end;

procedure TTestServerControllerExceptionAfterCreate.MVCControllerBeforeDestroy;
begin
  inherited;

end;

procedure TTestServerControllerExceptionAfterCreate.NeverCalled(CTX: TWebContext);
begin
  Render(500, 'This method should not be called...');
end;

{ TTestServerControllerExceptionBeforeDestroy }

procedure TTestServerControllerExceptionBeforeDestroy.MVCControllerAfterCreate;
begin
  inherited;

end;

procedure TTestServerControllerExceptionBeforeDestroy.MVCControllerBeforeDestroy;
begin
  inherited;
  raise Exception.Create('This is an exception raised in the MVCControllerBeforeDestroy');
end;

procedure TTestServerControllerExceptionBeforeDestroy.NeverCalled(CTX: TWebContext);
begin

end;

end.
