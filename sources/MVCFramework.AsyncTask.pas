// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.AsyncTask;

interface

uses
  System.SysUtils,
  System.Threading;

type
  TAsyncBackgroundTask<T> = reference to function: T;
  TAsyncSuccessCallback<T> = reference to procedure(const TaskResult: T);
  TAsyncErrorCallback = reference to procedure(const E: Exception);
  TAsyncDefaultErrorCallback = reference to procedure(const E: Exception;
    const ExptAddress: Pointer);

  Async = class sealed
  public
    class function Run<T>(
      Task: TAsyncBackgroundTask<T>;
      Success: TAsyncSuccessCallback<T>;
      Error: TAsyncErrorCallback = nil): ITask;
  end;

var
  DefaultTaskErrorHandler: TAsyncDefaultErrorCallback = nil;

implementation

uses
  System.Classes;

{ Async }

class function Async.Run<T>(Task: TAsyncBackgroundTask<T>;
  Success: TAsyncSuccessCallback<T>;
  Error: TAsyncErrorCallback): ITask;
var
  LRes: T;
begin
  Result := TTask.Run(
    procedure
    var
      Ex: Pointer;
      ExceptionAddress: Pointer;
    begin
      Ex := nil;
      try
        LRes := Task();
        if Assigned(Success) then
        begin
          TThread.Queue(nil,
            procedure
            begin
              Success(LRes);
            end);
        end;
      except
        Ex := AcquireExceptionObject;
        ExceptionAddress := ExceptAddr;
        TThread.Queue(nil,
          procedure
          var
            LCurrException: Exception;
          begin
            LCurrException := Exception(Ex);
            try
              if Assigned(Error) then
                Error(LCurrException)
              else
                DefaultTaskErrorHandler(LCurrException, ExceptionAddress);
            finally
              FreeAndNil(LCurrException);
            end;
          end);
      end;
    end);
end;

initialization

DefaultTaskErrorHandler :=
    procedure(const E: Exception; const ExceptionAddress: Pointer)
  begin
    ShowException(E, ExceptionAddress);
  end;

end.
