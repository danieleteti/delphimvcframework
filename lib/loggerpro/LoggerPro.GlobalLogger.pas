// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
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

{ @abstract(The global logger. Just use Log and start logging!)
  The global logger is configured with a TLoggerProFileAppender using default settings.
}
function Log: ILogWriter;

{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads)
  Call this before unloading a DLL. Also use in ISAPI dll.
  Check the loggerproisapisample.dll sample.
}
procedure ReleaseGlobalLogger;

implementation

uses
  LoggerPro.FileAppender;

var
  _Logger: ILogWriter = nil;
  _Lock: TObject = nil;
  _Finalized: Boolean = False;

function Log: ILogWriter;
begin
  // After finalization, don't create new loggers
  if _Finalized then
    Exit(nil);

  if _Logger = nil then
  begin
    TMonitor.Enter(_Lock);
    try
      if _Logger = nil then
      begin
        _Logger := BuildLogWriter([TLoggerProFileAppender.Create]);
      end;
    finally
      TMonitor.Exit(_Lock);
    end;
  end;
  Result := _Logger;
end;

procedure ReleaseGlobalLogger;
begin
  TMonitor.Enter(_Lock);
  try
    _Finalized := True;
    // Release the logger. Its destructor will wait for all
    // pending logs to be written before terminating.
    _Logger := nil;
  finally
    TMonitor.Exit(_Lock);
  end;
end;

initialization

_Lock := TObject.Create;

finalization

ReleaseGlobalLogger;
// Note: We don't free _Lock here because a thread might still be
// trying to acquire it. The OS will clean it up when the process exits.

end.
