library mydll;

uses
  LoggerPro.GlobalLogger,
  MyThreadU in 'MyThreadU.pas';

var
  lObj: IMyInterface = nil;

procedure Init;
begin
  lObj := TMyObject.Create;
end;

procedure DeInit;
begin
  lObj := nil;
  ReleaseGlobalLogger; // This is required inside dll and ISAPI!!
end;

procedure DoSomething;
begin
  Log.Debug('This is a log message from the DLL', 'DLL');
end;

exports
  Init, DeInit, DoSomething;

begin

end.
