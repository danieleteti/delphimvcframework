unit MVCFramework.REPLCommandsHandlerU;

interface

uses
  IdHTTPWebBrokerBridge,
  MVCFramework;

type

{$SCOPEDENUMS ON}
  THandleCommandResult = (Continue, Break, Unknown);
  TMVCCustomREPLCommandsHandler = reference to function(const Value: string; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean)
    : THandleCommandResult;

function HandleCommand(const Value: string; const Server: TIdHTTPWebBrokerBridge;
  const CustomCommandsHandler: TMVCCustomREPLCommandsHandler = nil): THandleCommandResult;
procedure REPLEmit(const Value: string);

implementation

uses
  System.SysUtils,
  MVCFramework.Commons,
  MVCFramework.Logger;

procedure REPLEmit(const Value: string);
begin
  Write('#> ');
  WriteLn(Value);
end;

function HandleCommand(const Value: string; const Server: TIdHTTPWebBrokerBridge;
  const CustomCommandsHandler: TMVCCustomREPLCommandsHandler): THandleCommandResult;
var
  lTempCommandResult: THandleCommandResult;
  lHandled: Boolean;
begin
  Result := THandleCommandResult.Unknown;
  if Assigned(CustomCommandsHandler) then
  begin
    lTempCommandResult := CustomCommandsHandler(Value, Server, lHandled);
    if lHandled then
      Exit(lTempCommandResult);
  end;

  // Handling standard REPL commands
  if (Value = 'quit') or (Value = 'exit') then
  begin
    EnterInShutdownState;
    REPLEmit('Stop listening...');
    Server.StopListening;
    REPLEmit('Shutting down...');
    Result := THandleCommandResult.Break;
  end
  else if (Value = 'version') then
  begin
    REPLEmit(DMVCFRAMEWORK_VERSION);
    Result := THandleCommandResult.Continue;
  end
  else if (Value = 'stop') then
  begin
    if not Server.Active then
    begin
      REPLEmit('Server not running');
    end
    else
    begin
      REPLEmit('Stopping server...');
      Server.Active := False;
      REPLEmit('done!');
    end;
    Result := THandleCommandResult.Continue;
  end
  else if (Value = 'start') then
  begin
    if Server.Active then
    begin
      REPLEmit(Format('Server already running on port %d...', [Server.Bindings.DefaultPort]));
    end
    else
    begin
      REPLEmit(Format('Starting server on port %d...', [Server.Bindings.DefaultPort]));
      Server.Active := True;
      LogI('Server started on port ' + Server.Bindings.DefaultPort.ToString);
      REPLEmit('done!');
    end;
    Result := THandleCommandResult.Continue;
  end;
end;

end.
