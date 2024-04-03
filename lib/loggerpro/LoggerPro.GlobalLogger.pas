// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
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
// ***************************************************************************

unit LoggerPro.GlobalLogger;

interface

uses
  LoggerPro;
{ @abstract(The global logger. Just uses @link(Logger.GlobalLogger) and you can start to log using @code(Log) function.)
  The global logger is configured with a @link(TLoggerProFileAppender) using default settings.
}
function Log: ILogWriter;

{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads, so call this before unload DLL)
  Use this also in ISAPI dll. Check the @code(loggerproisapisample.dll) sample
}
procedure ReleaseGlobalLogger;

implementation

uses
  LoggerPro.FileAppender;

var
  _Logger: ILogWriter;
  _Lock: TObject = nil;
  _ShuttedDown: boolean = false;

function Log: ILogWriter;
begin
  if _Logger = nil then
  begin
    if not _ShuttedDown then
    begin
      TMonitor.Enter(_Lock);
      try
        if _Logger = nil then // double check
        begin
          _Logger := BuildLogWriter([TLoggerProFileAppender.Create]);
        end;
      finally
        TMonitor.Exit(_Lock);
      end;
    end;
  end;
  Result := _Logger;
end;

procedure ReleaseGlobalLogger;
begin
  if _Logger <> nil then
  begin
    TMonitor.Enter(_Lock);
    try
      if _Logger <> nil then // double check
      begin
        _Logger := nil;
        _ShuttedDown := True;
      end;
    finally
      TMonitor.Exit(_Lock);
    end;
  end;
end;

initialization

_Lock := TObject.Create;

finalization

_Lock.Free;

end.
