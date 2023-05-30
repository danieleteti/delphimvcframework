// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit TestServerControllerExceptionU;

interface

uses
  MVCFramework, System.SysUtils;

type

  EMyException = class(Exception)

  end;

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

  [MVCPath('/actionfilters/beforeaction')]
  TTestServerControllerActionFilters = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const aActionName: string;
      var Handled: Boolean); override;
  public
    [MVCPath('/alwayscalled')]
    procedure AlwaysCalled(CTX: TWebContext);
    [MVCPath('/nevercalled')]
    procedure NeverCalled(CTX: TWebContext);
  end;

implementation

uses
  MVCFramework.Commons;

{ TTestServerControllerException }

procedure TTestServerControllerExceptionAfterCreate.MVCControllerAfterCreate;
begin
  inherited;
  raise EMyException.Create('This is an exception raised in the MVCControllerAfterCreate');
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
  raise EMyException.Create('This is an exception raised in the MVCControllerBeforeDestroy');
end;

procedure TTestServerControllerExceptionBeforeDestroy.NeverCalled(CTX: TWebContext);
begin

end;

{ TTestServerControllerActionFilters }

procedure TTestServerControllerActionFilters.AlwaysCalled(CTX: TWebContext);
begin
  StatusCode := 200;
end;

procedure TTestServerControllerActionFilters.NeverCalled(CTX: TWebContext);
begin
  raise Exception.Create('Should never be called!');
end;

procedure TTestServerControllerActionFilters.OnBeforeAction(
  Context: TWebContext; const aActionName: string; var Handled: Boolean);
begin
  inherited;
  if SameText(aActionName, 'NeverCalled') then
  begin
    Context.Response.StatusCode := 404;
    Handled := True;
  end;
end;

end.
