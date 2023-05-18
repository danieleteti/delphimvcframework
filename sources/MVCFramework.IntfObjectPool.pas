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

unit MVCFramework.IntfObjectPool;

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

  IIntfObjectPool = interface
    ['{0E79863D-D6F9-4426-9D80-F4C215233582}']
    function GetFromPool(const RaiseExceptionIfNotAvailable: Boolean = False): IInterface;
    procedure ReleaseToPool(const Obj: IInterface);
    function Size: UInt32;
  end;


  TIntfCleanupThread<IInterface> = class;

  TIntfObjectPool = class(TInterfacedObject, IIntfObjectPool)
  private
    fFactory: TFunc<IInterface>;
    fMaxSize: Integer;
//    fPool : TInterfaceList;
    fPool : TList<IInterface>;
    fSize: Integer;
    fShrinkTargetSize: Integer;
    fShrinkTriggerSize: Integer;
    fCleanupThread: TIntfCleanupThread<IInterface>;
    fLastGetFromPool: TDateTime;
    fOnResetState: TProc<IInterface>;
  protected
    procedure Lock;
    procedure UnLock;
    procedure ShrinkPoolTo(const TargetSize: Integer);
  public
    constructor Create(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>);
    destructor Destroy; override;
    function GetFromPool(const RaiseExceptionIfNotAvailable: Boolean = False) : IInterface;
    procedure ReleaseToPool(const Obj: IInterface);
    function Size: UInt32;
//    property OnResetState: TProc<T> read fOnResetState write fOnResetState;
  end;

  TIntfCleanupThread<IInterface> = class(TThread)
  private
    fObjectPool: TIntfObjectPool;
    type
      TPoolSizeSamples = array [0..AVG_SAMPLES_COUNT-1] of Integer;
    function GetAveragePoolSize(var SizeSamples: TPoolSizeSamples): Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ObjectPool: TIntfObjectPool);
  end;

  TIntfPoolFactory = class
  public
    class function CreatePool(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>): IIntfObjectPool;
    class function CreateUnlimitedPool(ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>): IIntfObjectPool;
  end;


var
  GObjectPoolSamplingIntervalMS: UInt32 = 10000;

implementation

uses
  WinAPI.Windows;


{ TObjectPool<T> }

constructor TIntfObjectPool.Create(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>);
begin
  inherited Create;
  fOnResetState := nil;
  fLastGetFromPool := 0;
  fFactory := Factory;
  fMaxSize := MaxSize;
  fShrinkTargetSize := ShrinkTargetSize;
  fShrinkTriggerSize := ShrinkTriggerSize;
  //fPool := TInterfaceList.Create;
  fPool := TList<IInterface>.Create;
  fCleanupThread := nil;
  if fShrinkTriggerSize > 0 then
  begin
    fCleanupThread := TIntfCleanupThread<IInterface>.Create(Self);
  end;
end;

destructor TIntfObjectPool.Destroy;
begin
  if Assigned(fCleanupThread) then
  begin
    fCleanupThread.Terminate;
    fCleanupThread.Free;
  end;
  while fPool.Count > 0 do
    fPool.Delete(fPool.Count - 1);

  FreeAndNil(fPool);
  inherited;
end;

function TIntfObjectPool.GetFromPool(const RaiseExceptionIfNotAvailable: Boolean): IInterface;
begin
  MonitorEnter(Self);
  try
    fLastGetFromPool := Now();
    if fPool.Count > 0 then
    begin
      result := fPool.Items[fPool.Count - 1];
      fPool.Remove(result);
      exit
    end;

    if (fMaxSize > 0) and (fSize >= fMaxSize) then
    begin
      if RaiseExceptionIfNotAvailable then
        raise EObjectPool.CreateFmt('Pool cannot provide an instance of the interface',[] );
      Exit(nil);
    end;
    Result := fFactory();
    Inc(fSize);
  finally
    MonitorExit(Self);
  end;
end;

procedure TIntfObjectPool.Lock;
begin
  MonitorEnter(Self);
end;

procedure TIntfObjectPool.ReleaseToPool(const Obj: IInterface);
begin
//  if Assigned(fOnResetState) then
//  begin
////    fOnResetState(Obj);
//  end;
  MonitorEnter(Self);
  try
    fPool.Add(Obj);
  finally
    MonitorExit(Self);
  end;
end;

procedure TIntfObjectPool.ShrinkPoolTo(const TargetSize: Integer);
begin
  MonitorEnter(Self);
  try
    while fSize > TargetSize do
    begin
      fPool.Remove(fPool.Items[fPool.Count - 1]);
      Dec(fSize);
    end;
  finally
    MonitorExit(Self);
  end;
end;

function TIntfObjectPool.Size: UInt32;
begin
  MonitorEnter(Self);
  try
    Result := fPool.Count;
  finally
    MonitorExit(Self);
  end;
end;

procedure TIntfObjectPool.UnLock;
begin
  MonitorExit(Self);
end;

constructor TIntfCleanupThread<IInterface>.Create(ObjectPool: TIntfObjectPool);
begin
  fObjectPool := ObjectPool;
  inherited Create(False);
end;

procedure TIntfCleanupThread<IInterface>.Execute;
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

function TIntfCleanupThread<IInterface>.GetAveragePoolSize(
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

class function TIntfPoolFactory.CreatePool(MaxSize: Integer; ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>): IIntfObjectPool;
begin
  Result := TIntfObjectPool.Create(MaxSize, ShrinkTriggerSize,
    ShrinkTargetSize, Factory);
end;

class function TIntfPoolFactory.CreateUnlimitedPool(ShrinkTriggerSize, ShrinkTargetSize: Integer; const Factory: TFunc<IInterface>): IIntfObjectPool;
begin
  Result := CreatePool(0, ShrinkTriggerSize,
    ShrinkTargetSize, Factory);
end;

end.
