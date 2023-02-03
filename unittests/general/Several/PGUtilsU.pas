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

unit PGUtilsU;

interface

type
  TPGUtil = class
  private
    fPGHome: string;
    fPGDataDir: string;
    fInitDBExecutable: string;
    fPGCtlExecutable: String;
    fPGPort: UInt16;
    fCreateDBExecutable: string;
  public
    constructor Create(const PGHome, PGDataDir: String; const PGPort: UInt16);
    procedure InitDB;
    procedure StartPG;
    procedure CreateDatabase(const DatabaseName: String);
    procedure StopPG;
    procedure RemoveDataDir;
    function IsPGRunning: Boolean;
    property PGHome: String read fPGHome;
    property PGDataDir: String read fPGDataDir;
  end;

implementation

uses
  Winapi.Windows, System.IOUtils, System.SysUtils;

{ TPGUtil }

function SysExecute(const CommandLine: string; out StdOutput: String; const Work: string = 'C:\')
  : Cardinal;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array [0 .. 255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  StdOutput := '';
  with SA do
  begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine), nil, nil, True, 0, nil,
      PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            StdOutput := StdOutput + String(Buffer);
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
        GetExitCodeProcess(PI.hProcess, Result);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

function SysStartExecute(const CommandLine: string; const Work: string = 'C:\'): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  WorkDir: string;
  Handle: Boolean;
begin
  with SI do
  begin
    FillChar(SI, SizeOf(SI), 0);
    cb := SizeOf(SI);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
    hStdInput := 0;
    hStdOutput := 0;
    hStdError := 0;
  end;
  WorkDir := Work;
  Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine), nil, nil, True, 0, nil,
    PChar(WorkDir), SI, PI);
  Result := Handle;
end;

constructor TPGUtil.Create(const PGHome, PGDataDir: String; const PGPort: UInt16);
begin
  inherited Create;
  fPGHome := PGHome;
  fPGDataDir := PGDataDir;
  if fPGDataDir.Contains(' ') then
  begin
    raise Exception.Create('Cannot RUN test in a path with spaces');
  end;
  fPGPort := PGPort;
  fInitDBExecutable := TPath.Combine(fPGHome, 'bin\initdb.exe');
  fPGCtlExecutable := TPath.Combine(fPGHome, 'bin\pg_ctl.exe');
  fCreateDBExecutable := TPath.Combine(fPGHome, 'bin\createdb.exe');
end;

procedure TPGUtil.CreateDatabase(const DatabaseName: String);
var
  lParams: string;
  lOutput: string;
begin
  lParams := ' -p ' + fPGPort.ToString + ' ' + DatabaseName;
  if SysExecute(fCreateDBExecutable + lParams, lOutput) <> 0 then
  begin
    raise Exception.Create(lOutput);
  end;
end;

procedure TPGUtil.InitDB;
var
  lParams: string;
  lOutput: string;
begin
  lParams := ' -D ' + fPGDataDir +
    ' -E UTF8 --lc-collate=en_US.UTF8 --lc-ctype=en_US.UTF8 --locale=en_US';
  if SysExecute(fInitDBExecutable + lParams, lOutput) <> 0 then
  begin
    raise Exception.Create(lOutput);
  end;
end;

function TPGUtil.IsPGRunning: Boolean;
var
  lParams: string;
  lOutput: string;
begin
  lParams := ' -D ' + fPGDataDir + ' status';
  Result := SysExecute(fPGCtlExecutable + lParams, lOutput) = 0;
end;

procedure TPGUtil.RemoveDataDir;
begin
  StopPG;
  if TDirectory.Exists(fPGDataDir) then
  begin
    TDirectory.Delete(fPGDataDir, True);
  end;
end;

procedure TPGUtil.StartPG;
begin
  if not SysStartExecute(fPGCtlExecutable + ' -o "-F -p ' + fPGPort.ToString + '" -D ' + fPGDataDir
    + ' start') then
  begin
    raise Exception.Create('Cannot start postgresql');
  end;
end;

procedure TPGUtil.StopPG;
var
  lOutput: string;
begin
  SysExecute(fPGCtlExecutable + ' -D ' + fPGDataDir + ' stop -m smart', lOutput);
end;

end.
