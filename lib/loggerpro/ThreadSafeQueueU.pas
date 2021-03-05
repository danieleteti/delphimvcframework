// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2018 Daniele Teti
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

unit ThreadSafeQueueU;

interface

uses
  System.Generics.Collections,
  System.Types,
  System.SyncObjs, System.SysUtils;

type
  TThreadSafeQueue<T: class> = class
  private
    fPopTimeout: UInt64;
    fQueue: TObjectQueue<T>;
    fCriticalSection: TCriticalSection;
    // fSpinLock: TSpinLock;
    fEvent: TEvent;
    fMaxSize: UInt64;
    fShutDown: Boolean;
  public
    function Enqueue(const Item: T): Boolean;
    function Dequeue(out Item: T): TWaitResult; overload;
    function Dequeue(out QueueSize: UInt64; out Item: T): TWaitResult; overload;
    function Dequeue: T; overload;
    function QueueSize: UInt64;
    procedure DoShutDown;
    constructor Create(const MaxSize: UInt64; const PopTimeout: UInt64); overload; virtual;
    destructor Destroy; override;
  end;

  TMREWObjectList<T: class> = class(TObject)
  private
    fList: TObjectList<T>;
    fMREWSync: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function BeginRead: TObjectList<T>;
    procedure EndRead;
    function BeginWrite: TObjectList<T>;
    procedure EndWrite;
  end;


implementation

{ TThreadQueue<T> }

constructor TThreadSafeQueue<T>.Create(const MaxSize: UInt64; const PopTimeout: UInt64);
begin
  inherited Create;
  fShutDown := False;
  fMaxSize := MaxSize;
  fQueue := TObjectQueue<T>.Create(False);
  fCriticalSection := TCriticalSection.Create;
  // fSpinLock := TMonitor.Create(False);
  fEvent := TEvent.Create(nil, True, False, '');
  fPopTimeout := PopTimeout;
end;

function TThreadSafeQueue<T>.Dequeue(out Item: T): TWaitResult;
var
  lQueueSize: UInt64;
begin
  Result := Dequeue(lQueueSize, Item);
end;

function TThreadSafeQueue<T>.Dequeue: T;
begin
  Dequeue(Result);
end;

function TThreadSafeQueue<T>.Dequeue(out QueueSize: UInt64; out Item: T): TWaitResult;
var
  lWaitResult: TWaitResult;
begin
  if fShutDown then
    Exit(TWaitResult.wrAbandoned);
  lWaitResult := fEvent.WaitFor(fPopTimeout);
  if lWaitResult = TWaitResult.wrSignaled then
  begin
    if fShutDown then
      Exit(TWaitResult.wrAbandoned);
    fCriticalSection.Enter;
    try
      QueueSize := fQueue.Count;
      if QueueSize = 0 then
      begin
        fEvent.ResetEvent;
        Exit(TWaitResult.wrTimeout);
      end;
      Item := fQueue.Extract;
      Result := TWaitResult.wrSignaled;
    finally
      fCriticalSection.Leave;
    end;
  end
  else
  begin
    Item := default (T);
    Result := lWaitResult;
  end;
end;

destructor TThreadSafeQueue<T>.Destroy;
begin
  DoShutDown;
  fQueue.Free;
  fCriticalSection.Free;
  fEvent.Free;
  inherited;
end;

procedure TThreadSafeQueue<T>.DoShutDown;
begin
  fShutDown := True;
  fCriticalSection.Enter;
  try
    while fQueue.Count > 0 do
    begin
      fQueue.Extract.Free;
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

function TThreadSafeQueue<T>.Enqueue(const Item: T): Boolean;
const
  cRetryCount: Byte = 20;
var
  lCount: Integer;
begin
  if fShutDown then
    Exit(False);
  Result := False;

  lCount := 0;
  while lCount < cRetryCount do
  begin
    Sleep(lCount * lCount * lCount * 10);  //let's slow down the enqueue call using a cubic function
    fCriticalSection.Enter;
    try
      if fQueue.Count >= fMaxSize then
      begin
        Inc(lCount);
        Continue;
      end;
      fQueue.Enqueue(Item);
      Result := True;
      fEvent.SetEvent;
      Break;
    finally
      fCriticalSection.Leave;
    end;
  end;
end;

function TThreadSafeQueue<T>.QueueSize: UInt64;
begin
  fCriticalSection.Enter;
  try
    Result := fQueue.Count;
  finally
    fCriticalSection.Leave;
  end;

end;

{ TMREWObjectList<T> }

function TMREWObjectList<T>.BeginWrite: TObjectList<T>;
begin
  fMREWSync.BeginWrite;
  Result := fList;
end;

constructor TMREWObjectList<T>.Create;
begin
  inherited;
  fMREWSync := TMultiReadExclusiveWriteSynchronizer.Create;
  fList := TObjectList<T>.Create(true);
end;

destructor TMREWObjectList<T>.Destroy;
begin
  fMREWSync.Free;
  fList.Free;
  inherited;
end;

function TMREWObjectList<T>.BeginRead: TObjectList<T>;
begin
  fMREWSync.BeginRead;
  Result := fList;
end;

procedure TMREWObjectList<T>.EndRead;
begin
  fMREWSync.EndRead;
end;

procedure TMREWObjectList<T>.EndWrite;
begin
  fMREWSync.EndWrite;
end;



end.
