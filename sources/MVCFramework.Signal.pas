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

unit MVCFramework.Signal;

interface

procedure WaitForTerminationSignal;

implementation

uses
  MVCFramework
{$IF Defined(MSWINDOWS)}
  ,WinAPI.Windows
{$ENDIF}
{$IF Defined(LINUX)}
  ,Posix.Signal
{$ENDIF}
  ,System.SysUtils
  ,System.SyncObjs;

var
  gEvent: TEvent = nil;

{$IF Defined(MSWINDOWS)}
function MSWindowsConsoleCtrlHandler(const dwCtrlType: DWORD): BOOL; stdcall;
begin
  // https://docs.microsoft.com/en-us/windows/console/handlerroutine
  if dwCtrlType in [CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT] then
  begin
    Assert(gEvent <> nil, 'WaitForTerminationSignal not called');
    gEvent.SetEvent;
  end;

  Result := True;
  {
    If the function handles the control signal, it should return TRUE.
    If it returns FALSE, the next handler function in the list of handlers
    for this process is used.
  }
end;
{$ENDIF}
{$IF Defined(LINUX)}

procedure HandleSignals(SignalNo: Integer); cdecl;
begin
  // SIGHUP	  1	Hang up detected on controlling terminal or death of controlling process
  // SIGINT	  2	Issued if the user sends an interrupt signal (Ctrl + C)
  // SIGQUIT	3	Issued if the user sends a quit signal (Ctrl + D)
  // SIGFPE	  8	Issued if an illegal mathematical operation is attempted
  // SIGKILL	9	If a process gets this signal it must quit immediately and will not perform any clean-up operations
  // SIGALRM 14	Alarm clock signal (used for timers)
  // SIGTERM 15	Software termination signal (sent by kill by default)
  if SignalNo in [SIGINT, SIGQUIT, SIGTERM] then
  begin
    Assert(gEvent <> nil, 'WaitForTerminationSignal not called');
    gEvent.SetEvent;
  end;
end;
{$ENDIF}

procedure InstallTerminationSignalHook;
begin
{$IF Defined(MSWindows)}
  SetConsoleCtrlHandler(@MSWindowsConsoleCtrlHandler, True { add } );
{$ENDIF}
{$IF Defined(LINUX)}
  Signal(SIGINT, @HandleSignals);
  Signal(SIGQUIT, @HandleSignals);
{$ENDIF}
end;

procedure WaitForTerminationSignal;
begin
  Assert(gEvent = nil, 'WaitForTerminationSignal already called');
  gEvent := TEvent.Create(nil, true, false,'');
  try
    InstallTerminationSignalHook;
    while gEvent.WaitFor(2000) <> TWaitResult.wrSignaled do
    begin
      if IsShuttingDown then
      begin
        Break;
      end;
    end;
  finally
    gEvent.Free;
  end;
end;

end.
