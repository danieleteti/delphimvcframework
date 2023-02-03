// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// The Initial Developer of the Original Code is Vivaticket S.p.A. https://www.vivaticket.com/
// The code has been fully donated to the DMVCFramework community without any charge nor rights.
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

unit MVCFramework.ObjectPool;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils;


const
  AVG_SAMPLES_COUNT = 10;
type
  EObjectPool = class(Exception)

  end;

  IObjectPool<T: class, constructor> = interface
    ['{0E79863D-D6F9-4426-9D80-F4C215233582}']
    function GetFromPool(const RaiseExceptionIfNotAvailable: Boolean = False): T;
    procedure ReleaseToPool(const Obj: T);
    function Size: UInt32;
  end;


  TCleanupThread<T: class, constructor> = class;
  TObjectPool<T: class, constructor> = class(TInterfacedObject, IObjectPool<T>)
  private
    fFactory: TFunc<T>;
    fMaxSize: Integer;
    fPool: TStack<T>;
    fSize: Integer;
    fShrinkTargetSize: Integer;
    fShrinkTriggerSize: Integer;
    fCleanupThread: TCleanupThread<T>;
    fLastGetFromPool: TDateTime;
    fOnResetState: TProc<T>;
  protected
    procedure Lock;
    procedure UnLock;
    procedure ShrinkPoolTo(const TargetSize: Integer);
  public
    constructor Create(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<T> = nil);
    destructor Destroy; override;
    function GetFromPool(const RaiseExceptionIfNotAvailable: Boolean = False): T;
    procedure ReleaseToPool(const Obj: T);
    function Size: UInt32;
    property OnResetState: TProc<T> read fOnResetState write fOnResetState;
  end;

  TCleanupThread<T: class, constructor> = class(TThread)
  private
    fObjectPool: TObjectPool<T>;
    type
      TPoolSizeSamples = array [0..AVG_SAMPLES_COUNT-1] of Integer;
    function GetAveragePoolSize(var SizeSamples: TPoolSizeSamples): Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ObjectPool: TObjectPool<T>);
  end;

  TPoolFactory = class
  public
    class function CreatePool<T: class, constructor>(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<T> = nil): IObjectPool<T>;
    class function CreateUnlimitedPool<T: class, constructor>(ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<T> = nil): IObjectPool<T>;
  end;


var
  GObjectPoolSamplingIntervalMS: UInt32 = 10000;

implementation

uses
  WinAPI.Windows;


{ TObjectPool<T> }

constructor TObjectPool<T>.Create(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<T>);
var
  i: Integer;
begin
  inherited Create;
  fOnResetState := nil;
  fLastGetFromPool := 0;
  fFactory := Factory;
  fMaxSize := MaxSize;
  fShrinkTargetSize := ShrinkTargetSize;
  fShrinkTriggerSize := ShrinkTriggerSize;
  fPool := TStack<T>.Create;
  fCleanupThread := nil;
  if fShrinkTriggerSize > 0 then
  begin
    fCleanupThread := TCleanupThread<T>.Create(Self);
  end;
end;

destructor TObjectPool<T>.Destroy;
begin
  if Assigned(fCleanupThread) then
  begin
    fCleanupThread.Terminate;
    fCleanupThread.Free;
  end;
  while fPool.Count > 0 do
    fPool.Pop.Free;
  FreeAndNil(fPool);
  inherited;
end;

function TObjectPool<T>.GetFromPool(const RaiseExceptionIfNotAvailable: Boolean): T;
begin
  MonitorEnter(Self);
  try
    fLastGetFromPool := Now();
    if fPool.Count > 0 then
    begin
      Exit(fPool.Pop);
    end;

    if (fMaxSize > 0) and (fSize >= fMaxSize) then
    begin
      if RaiseExceptionIfNotAvailable then
        raise EObjectPool.CreateFmt('Pool cannot provide an instance of %s', [T.ClassName]);
      Exit(nil);
    end;

    if Assigned(fFactory) then
    begin
      Result := fFactory()
    end
    else
    begin
      Result := T.Create;
    end;

    Inc(fSize);
  finally
    MonitorExit(Self);
  end;
end;

procedure TObjectPool<T>.Lock;
begin
  MonitorEnter(Self);
end;

procedure TObjectPool<T>.ReleaseToPool(const Obj: T);
begin
  if Assigned(fOnResetState) then
  begin
    fOnResetState(Obj);
  end;
  MonitorEnter(Self);
  try
    fPool.Push(Obj);
  finally
    MonitorExit(Self);
  end;
end;

procedure TObjectPool<T>.ShrinkPoolTo(const TargetSize: Integer);
begin
  MonitorEnter(Self);
  try
    while fSize > TargetSize do
    begin
      fPool.Pop.Free;
      Dec(fSize);
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TObjectPool<T>.Size: UInt32;
begin
  MonitorEnter(Self);
  try
    Result := fPool.Count;
  finally
    MonitorExit(Self);
  end;
end;

procedure TObjectPool<T>.UnLock;
begin
  MonitorExit(Self);
end;

constructor TCleanupThread<T>.Create(ObjectPool: TObjectPool<T>);
begin
  fObjectPool := ObjectPool;
  inherited Create(False);
end;

procedure TCleanupThread<T>.Execute;
var
  lTargetSize: Integer;
  lAvgSize: TPoolSizeSamples;
  lArrIndex: Integer;
  lSampleTick: Integer;
begin
  lArrIndex := 0;
  lSampleTick := 0;
  while not Terminated do
  begin
    Inc(lSampleTick);
    lArrIndex := lSampleTick mod AVG_SAMPLES_COUNT;
    lAvgSize[lArrIndex] := fObjectPool.Size;
    if (lAvgSize[lArrIndex] > 0) and (GetAveragePoolSize(lAvgSize) >= fObjectPool.fShrinkTriggerSize) then
    begin
      fObjectPool.Lock;
      try
        fObjectPool.ShrinkPoolTo(fObjectPool.fShrinkTargetSize);
        ZeroMemory(@lAvgSize, SizeOf(lAvgSize));
      finally
        fObjectPool.UnLock;
      end;
    end
    else
    begin
      Sleep(GObjectPoolSamplingIntervalMS);
      if lSampleTick = MaxInt  then
      begin
        lSampleTick := 0;
      end;
    end;
  end;
end;

function TCleanupThread<T>.GetAveragePoolSize(
  var SizeSamples: TPoolSizeSamples): Integer;
begin
  Result := 0;
  for var I := Low(TPoolSizeSamples) to High(TPoolSizeSamples) do
  begin
    Inc(Result, SizeSamples[I]);
  end;
  Result := Result div Length(SizeSamples);
end;

{ TPoolFactory }

class function TPoolFactory.CreatePool<T>(MaxSize, ShrinkTriggerSize,
  ShrinkTargetSize: Integer; const Factory: TFunc<T>): IObjectPool<T>;
begin
  Result := TObjectPool<T>.Create(MaxSize, ShrinkTriggerSize,
    ShrinkTargetSize, Factory);
end;

class function TPoolFactory.CreateUnlimitedPool<T>(ShrinkTriggerSize,
  ShrinkTargetSize: Integer; const Factory: TFunc<T>): IObjectPool<T>;
begin
  Result := CreatePool<T>(0, ShrinkTriggerSize,
    ShrinkTargetSize, Factory);
end;

end.
