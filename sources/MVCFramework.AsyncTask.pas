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

unit MVCFramework.AsyncTask;

interface

uses
  System.SysUtils,
  System.Threading;

type
  TMVCAsyncBackgroundTask<T> =  reference to function: T;
  TMVCAsyncSuccessCallback<T> = reference to procedure(const BackgroundTaskResult: T);
  TMVCAsyncErrorCallback = reference to procedure(const Expt: Exception);
  TMVCAsyncDefaultErrorCallback = reference to procedure(const Expt: Exception;
    const ExptAddress: Pointer);

  MVCAsync = class sealed
  public
    class function Run<T>(
      Task: TMVCAsyncBackgroundTask<T>;
      Success: TMVCAsyncSuccessCallback<T>;
      Error: TMVCAsyncErrorCallback = nil): ITask;
  end;

var
  gDefaultTaskErrorHandler: TMVCAsyncDefaultErrorCallback = nil;

implementation

{$I dmvcframework.inc}

uses
  System.Classes
  {$IF Defined(MOBILE)}
  , FMX.DialogService
  , System.UITypes
  , FMX.Dialogs
  {$ENDIF}
  ;

{ Async }

class function MVCAsync.Run<T>(
  Task: TMVCAsyncBackgroundTask<T>;
  Success: TMVCAsyncSuccessCallback<T>;
  Error: TMVCAsyncErrorCallback): ITask;
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
                gDefaultTaskErrorHandler(LCurrException, ExceptionAddress);
            finally
              FreeAndNil(LCurrException);
            end;
          end);
      end;
    end);
end;

initialization

gDefaultTaskErrorHandler :=
  procedure(const E: Exception; const ExceptionAddress: Pointer)
  begin
    {$IF Defined(MOBILE)}
      TDialogService.MessageDialog(Format('[%s] %s', [E.ClassName, E.Message]), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    {$ELSE}
      {TODO -oDanieleT -cGeneral : Should be better to inspect if stderr is available}
      if not (IsConsole or IsLibrary) then
      begin
        ShowException(E, ExceptionAddress);
      end
      else
      begin
        WriteLn(E.ClassName, ' ', E.Message);
      end;
    {$ENDIF}
  end;

end.
