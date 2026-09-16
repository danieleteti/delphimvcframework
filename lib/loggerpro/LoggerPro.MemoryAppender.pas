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

unit LoggerPro.MemoryAppender;

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs;

type
  { @abstract(A thread-safe ring buffer appender that keeps the last N log items in memory)
    Useful for:
    - Displaying recent logs in a UI (ListView, Memo)
    - Dumping logs on crash/error
    - Unit testing
    - In-app log viewer
  }
  TLoggerProMemoryRingBufferAppender = class(TLoggerProAppenderBase)
  private
    FLogItems: TList<TLogItem>;
    FMaxSize: Integer;
    FLock: TCriticalSection;
  public
    const DEFAULT_MAX_SIZE = 1000;

    constructor Create(aMaxSize: Integer = DEFAULT_MAX_SIZE; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    destructor Destroy; override;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;

    { Returns a copy of all log items currently in the buffer.
      Caller is responsible for freeing the returned list and its items. }
    function GetLogItems: TList<TLogItem>;

    { Returns a copy of log items filtered by tag.
      Caller is responsible for freeing the returned list and its items. }
    function GetLogItemsByTag(const aTag: string): TList<TLogItem>;

    { Returns a copy of log items filtered by log type.
      Caller is responsible for freeing the returned list and its items. }
    function GetLogItemsByType(aLogType: TLogType): TList<TLogItem>;

    { Returns formatted log strings for all items in buffer }
    function GetAsStringList: TArray<string>;

    { Returns the number of items currently in the buffer }
    function Count: Integer;

    { Clears all items from the buffer }
    procedure Clear;

    property MaxSize: Integer read FMaxSize;
  end;

implementation

{ TLoggerProMemoryRingBufferAppender }

constructor TLoggerProMemoryRingBufferAppender.Create(aMaxSize: Integer; aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  if aMaxSize < 1 then
    raise ELoggerPro.CreateFmt('MaxSize must be >= 1, got %d', [aMaxSize]);
  FMaxSize := aMaxSize;
  FLock := TCriticalSection.Create;
  FLogItems := TList<TLogItem>.Create;
end;

destructor TLoggerProMemoryRingBufferAppender.Destroy;
begin
  Clear;
  FLogItems.Free;
  FLock.Free;
  inherited;
end;

procedure TLoggerProMemoryRingBufferAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProMemoryRingBufferAppender.TearDown;
begin
  inherited;
end;

procedure TLoggerProMemoryRingBufferAppender.WriteLog(const aLogItem: TLogItem);
var
  lOldItem: TLogItem;
begin
  FLock.Enter;
  try
    // If buffer is full, remove oldest item
    while FLogItems.Count >= FMaxSize do
    begin
      lOldItem := FLogItems[0];
      FLogItems.Delete(0);
      lOldItem.Free;
    end;
    // Add clone of log item (original will be freed by logger)
    FLogItems.Add(aLogItem.Clone);
  finally
    FLock.Leave;
  end;
end;

function TLoggerProMemoryRingBufferAppender.GetLogItems: TList<TLogItem>;
var
  I: Integer;
begin
  Result := TList<TLogItem>.Create;
  FLock.Enter;
  try
    for I := 0 to FLogItems.Count - 1 do
      Result.Add(FLogItems[I].Clone);
  finally
    FLock.Leave;
  end;
end;

function TLoggerProMemoryRingBufferAppender.GetLogItemsByTag(const aTag: string): TList<TLogItem>;
var
  I: Integer;
begin
  Result := TList<TLogItem>.Create;
  FLock.Enter;
  try
    for I := 0 to FLogItems.Count - 1 do
      if FLogItems[I].LogTag = aTag then
        Result.Add(FLogItems[I].Clone);
  finally
    FLock.Leave;
  end;
end;

function TLoggerProMemoryRingBufferAppender.GetLogItemsByType(aLogType: TLogType): TList<TLogItem>;
var
  I: Integer;
begin
  Result := TList<TLogItem>.Create;
  FLock.Enter;
  try
    for I := 0 to FLogItems.Count - 1 do
      if FLogItems[I].LogType = aLogType then
        Result.Add(FLogItems[I].Clone);
  finally
    FLock.Leave;
  end;
end;

function TLoggerProMemoryRingBufferAppender.GetAsStringList: TArray<string>;
var
  I: Integer;
begin
  FLock.Enter;
  try
    SetLength(Result, FLogItems.Count);
    for I := 0 to FLogItems.Count - 1 do
      Result[I] := FormatLog(FLogItems[I]);
  finally
    FLock.Leave;
  end;
end;

function TLoggerProMemoryRingBufferAppender.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FLogItems.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerProMemoryRingBufferAppender.Clear;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := FLogItems.Count - 1 downto 0 do
      FLogItems[I].Free;
    FLogItems.Clear;
  finally
    FLock.Leave;
  end;
end;

end.
