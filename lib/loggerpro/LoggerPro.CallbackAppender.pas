// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
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

unit LoggerPro.CallbackAppender;

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Classes;

type
  { Callback signature for receiving log items }
  TLogItemCallback = reference to procedure(const aLogItem: TLogItem; const aFormattedMessage: string);

  { Callback signature for receiving only formatted message }
  TLogMessageCallback = reference to procedure(const aFormattedMessage: string);

  { @abstract(Appender that invokes a callback for each log item)
    Useful for:
    - Updating UI components (ListView, Memo, etc.) via TThread.Queue
    - Custom integrations
    - Testing
    - Real-time notifications
  }
  TLoggerProCallbackAppender = class(TLoggerProAppenderBase)
  private
    FCallback: TLogItemCallback;
    FSynchronizeToMainThread: Boolean;
  public
    { Creates a callback appender.
      @param aCallback The callback to invoke for each log item
      @param aSynchronizeToMainThread If true, callback is executed via TThread.Queue (safe for UI updates)
      @param aLogItemRenderer Optional custom renderer }
    constructor Create(aCallback: TLogItemCallback;
      aSynchronizeToMainThread: Boolean = False;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce; overload;

    { Creates a callback appender with a simple message-only callback.
      @param aCallback The callback to invoke with formatted message
      @param aSynchronizeToMainThread If true, callback is executed via TThread.Queue (safe for UI updates)
      @param aLogItemRenderer Optional custom renderer }
    constructor Create(aCallback: TLogMessageCallback;
      aSynchronizeToMainThread: Boolean = False;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce; overload;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;

    property SynchronizeToMainThread: Boolean read FSynchronizeToMainThread;
  end;

implementation

{ TLoggerProCallbackAppender }

constructor TLoggerProCallbackAppender.Create(aCallback: TLogItemCallback;
  aSynchronizeToMainThread: Boolean;
  aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  FCallback := aCallback;
  FSynchronizeToMainThread := aSynchronizeToMainThread;
end;

constructor TLoggerProCallbackAppender.Create(aCallback: TLogMessageCallback;
  aSynchronizeToMainThread: Boolean;
  aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  // Wrap simple callback into full callback
  FCallback := procedure(const aLogItem: TLogItem; const aFormattedMessage: string)
    begin
      aCallback(aFormattedMessage);
    end;
  FSynchronizeToMainThread := aSynchronizeToMainThread;
end;

procedure TLoggerProCallbackAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProCallbackAppender.TearDown;
begin
  inherited;
end;

procedure TLoggerProCallbackAppender.WriteLog(const aLogItem: TLogItem);
var
  lFormattedMessage: string;
  lClonedItem: TLogItem;
begin
  if not Assigned(FCallback) then
    Exit;

  lFormattedMessage := FormatLog(aLogItem);

  if FSynchronizeToMainThread then
  begin
    // Clone the item since original will be freed after WriteLog returns
    lClonedItem := aLogItem.Clone;
    TThread.Queue(nil,
      procedure
      begin
        try
          FCallback(lClonedItem, lFormattedMessage);
        finally
          lClonedItem.Free;
        end;
      end);
  end
  else
  begin
    FCallback(aLogItem, lFormattedMessage);
  end;
end;

end.
