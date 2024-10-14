unit HelpersU;

interface

uses
  System.Rtti, System.Bindings.EvalProtocol;

function MyHelper1(const Parameters: TArray<IValue>): TValue;
function MyHelper2(const Parameters: TArray<IValue>): TValue;


procedure WebStencilsProcessorConfigure;

implementation

uses
  System.SysUtils, MVCFramework.View.Renderers.WebStencils, System.Bindings.Methods, Web.Stencils;


function MyHelper1(const Parameters: TArray<IValue>): TValue;
begin
  Result := Parameters[0].GetValue.ToString +  ' (I''m The MyHelper1)';
end;

function MyHelper2(const Parameters: TArray<IValue>): TValue;
begin
  Result := Parameters[0].GetValue.ToString +  ' (I''m The MyHelper2)';
end;

procedure WebStencilsProcessorConfigure;
begin
  TBindingMethodsFactory.RegisterMethod(
   TMethodDescription.Create(
    MakeInvokable(function(Args: TArray<IValue>): IValue
    begin
      Result := TValueWrapper.Create(MyHelper1(Args));
    end),
    'MyHelper1', 'MyHelper1', '', True, 'MyHelper1 is just a sample', nil));


  TBindingMethodsFactory.RegisterMethod(
   TMethodDescription.Create(
    MakeInvokable(function(Args: TArray<IValue>): IValue
    begin
      Result := TValueWrapper.Create(MyHelper2(Args));
    end),
    'MyHelper2', 'MyHelper2', '', True, 'MyHelper2 is just a sample', nil));

  TMVCWebStencilsConfiguration.OnProcessorConfiguration :=
    procedure(const WebStencilsProcessor: TWebStencilsProcessor)
    begin
      //custom configuration for TWebStencilsProcessor (executed for each view)
    end;

end;

end.

