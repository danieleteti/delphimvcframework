// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit MVCFramework.Cache;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Generics.Collections,
  System.SyncObjs, MVCFramework.Commons, System.DateUtils;

const
  DEFAULT_CACHE_DURATION = OneMinute * 10;

type
  TMVCCacheItem = class
  private
    FCriticalSection: TCriticalSection;
    FValue: TValue;
    FTimeStamp: TDateTime;
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    property Value: TValue read GetValue write SetValue;
    property TimeStamp: TDateTime read FTimeStamp;
  end;

  TMVCCache = class
  private
    FStorage: TObjectDictionary<string, TMVCCacheItem>;
    FMREW: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    function SetValue(const AName: string; const AValue: TValue): TMVCCacheItem;
    procedure RemoveItem(const AName: string);
    function Contains(const AName: string; out AValue: TValue): Boolean;
    function ContainsItem(const AName: string; out AItem: TMVCCacheItem): Boolean;
    function GetValue(const AName: string): TValue;
    function ExecOnItemWithWriteLock(const AName: string; const AAction: TProc<TValue>): Boolean;
    procedure BeginWrite;
    procedure EndWrite;

  end;

{$IF Defined(CUSTOM_MANAGED_RECORDS)}
  TMVCObjectCache<T: class> = class;

  TMVCObjectCacheWriteTransaction<T: class> = record
  private
    fActive: Boolean;
    fParent: TMVCObjectCache<T>;
    constructor WithWriteLock(Parent: TMVCObjectCache<T>);
  public
    class operator Finalize(var Dest: TMVCObjectCacheWriteTransaction<T>);
    class operator Initialize (out Dest: TMVCObjectCacheWriteTransaction<T>);
    class operator Assign (var Dest: TMVCObjectCacheWriteTransaction<T>; const [ref] Src: TMVCObjectCacheWriteTransaction<T>);
    // Methods to use in "transaction"
    procedure SetValue(const AName: string; const AValue: T);
    procedure RemoveItem(const AName: string);
    function Contains(const AName: string; out AValue: T): Boolean;
    function GetValue(const AName: string): T;
    function Keys: TArray<String>;
  end;


  TMVCObjectCacheReadTransaction<T: class> = record
  private
    fActive: Boolean;
    fParent: TMVCObjectCache<T>;
    constructor WithReadLock(Parent: TMVCObjectCache<T>);
  public
    class operator Finalize(var Dest: TMVCObjectCacheReadTransaction<T>);
    class operator Initialize (out Dest: TMVCObjectCacheReadTransaction<T>);
    class operator Assign (var Dest: TMVCObjectCacheReadTransaction<T>; const [ref] Src: TMVCObjectCacheReadTransaction<T>);
    // Methods to use in "transaction"
    function Contains(const AName: string; out AValue: T): Boolean;
    function GetValue(const AName: string): T;
    function Keys: TArray<String>;
  end;


  TMVCObjectCache<T: class> = class
  private
    FStorage: TObjectDictionary<string, T>;
    FMREW: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    function WithSharedReadLock: TMVCObjectCacheReadTransaction<T>;
    function WithExclusiveWriteLock: TMVCObjectCacheWriteTransaction<T>;
  end;
{$ENDIF}

  TMVCCacheSingleton = class
  private
    class var Lock: TObject;
    class var SInstance: TMVCCache;
  protected
    class function GetInstance: TMVCCache; static;
  public
    class property Instance: TMVCCache read GetInstance;
    class constructor Create;
    class destructor Destroy;
  end;

  TMVCThreadedObjectCache<T: class> = class
  private
    fCS: TCriticalSection;
    fItems: TObjectDictionary<String, T>;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetValue(const Key: String; out Value: T): Boolean;
    procedure Add(const Key: String; const Value: T);
  end;

implementation

uses
  System.Math;

{ TCache }

function TMVCCache.SetValue(const AName: string; const AValue: TValue): TMVCCacheItem;
var
  lCacheItem: TMVCCacheItem;
  lValue: TValue;
begin
  lValue := AValue;

  FMREW.DoWithWriteLock(
    procedure
    var
      lItem: TMVCCacheItem;
    begin
      if FStorage.TryGetValue(AName, lItem) then
      begin
        // https://github.com/danieleteti/delphimvcframework/issues/825
        if lItem.Value.IsObject and ((lValue.IsObject and (lValue.AsObject <> lItem.Value.AsObject)) or not lValue.IsObject) then
        begin
          lItem.Value.AsObject.Free;
        end;
        lItem.Value := lValue;
      end
      else
      begin
        lCacheItem := TMVCCacheItem.Create;
        try
          lCacheItem.Value := lValue;
          FStorage.Add(AName, lCacheItem);
        except
          lCacheItem.Free;
          raise;
        end;
      end;
    end);
  Result := lCacheItem;
end;

procedure TMVCCache.EndWrite;
begin
  FMREW.EndWrite;
end;

function TMVCCache.ExecOnItemWithWriteLock(const AName: string; const AAction: TProc<TValue>): Boolean;
var
  lItem: TMVCCacheItem;
begin
  Result := False;
  FMREW.BeginWrite;
  try
    if FStorage.TryGetValue(AName, lItem) then
    begin
      Result := True;
      AAction(lItem.Value);
    end;
  finally
    FMREW.EndWrite;
  end;
end;

procedure TMVCCache.BeginWrite;
begin
  FMREW.BeginWrite;
end;

function TMVCCache.Contains(const AName: string; out AValue: TValue): Boolean;
var
  lItem: TMVCCacheItem;
begin
  Result := ContainsItem(AName, lItem);
  if Result then
    AValue := lItem.Value;
end;

function TMVCCache.ContainsItem(const AName: string; out AItem: TMVCCacheItem): Boolean;
var
  lItem: TMVCCacheItem;
  lRes: Boolean;
begin
  FMREW.DoWithReadLock(
    procedure
    begin
      lRes := FStorage.TryGetValue(AName, lItem);
    end);
  Result := lRes;
  if Result then
    AItem := lItem;
end;

constructor TMVCCache.Create;
begin
  inherited Create;
  FStorage := TObjectDictionary<string, TMVCCacheItem>.Create([doOwnsValues]);
  FMREW := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMVCCache.Destroy;
begin
  FMREW.Free;
  FStorage.Free;
  inherited;
end;

function TMVCCache.GetValue(const AName: string): TValue;
var
  lItem: TMVCCacheItem;
  lResult: TValue;
begin
  Result := TValue.Empty;
  FMREW.DoWithReadLock(
    procedure
    begin
      if FStorage.TryGetValue(AName, lItem) then
      begin
        lResult := lItem.Value;
      end;
    end);
  Result := lResult;
end;

procedure TMVCCache.RemoveItem(const AName: string);
begin
  FMREW.DoWithWriteLock(
    procedure
    var
      lItem: TMVCCacheItem;
    begin
      if FStorage.TryGetValue(AName, lItem) then
      begin
        if lItem.Value.IsObject then
        begin
          lItem.Value.AsObject.Free;
        end;
        FStorage.Remove(AName);
      end
    end);
end;

{ TMVCFrameworkCacheItem }

constructor TMVCCacheItem.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FValue := TValue.Empty;
  FTimeStamp := 0;
end;

destructor TMVCCacheItem.Destroy;
begin
  if FValue.IsObject then
  begin
    FValue.AsObject.Free;
  end;
  FCriticalSection.Free;
  inherited;
end;

function TMVCCacheItem.GetValue: TValue;
begin
  Result := FValue;
end;

procedure TMVCCacheItem.SetValue(const Value: TValue);
begin
  FValue := Value;
  FTimeStamp := Now;
end;

{ TMVCCacheSingleton }

class constructor TMVCCacheSingleton.Create;
begin
  SInstance := nil;
  Lock := TObject.Create;
end;

class destructor TMVCCacheSingleton.Destroy;
begin
  FreeAndNil(SInstance);
  FreeAndNil(Lock);
end;

class function TMVCCacheSingleton.GetInstance: TMVCCache;
begin
  if not Assigned(SInstance) then
  begin
    TMonitor.Enter(Lock);
    try
      if not Assigned(SInstance) then // doublecheck here
      begin
        SInstance := TMVCCache.Create;
      end;
    finally
      TMonitor.Exit(Lock);
    end;
  end;
  Result := SInstance;
end;

{ TMVCThreadedObjectCache<T> }

procedure TMVCThreadedObjectCache<T>.Add(const Key: String; const Value: T);
begin
  fCS.Enter;
  try
    fItems.Add(Key, Value);
  finally
    fCS.Leave;
  end;
end;

constructor TMVCThreadedObjectCache<T>.Create;
begin
  inherited;
  fCS := TCriticalSection.Create;
  fItems := TObjectDictionary<String, T>.Create([doOwnsValues]);
end;

destructor TMVCThreadedObjectCache<T>.Destroy;
begin
  fItems.Free;
  fCS.Free;
  inherited;
end;

function TMVCThreadedObjectCache<T>.TryGetValue(const Key: String; out Value: T): Boolean;
begin
  fCS.Enter;
  try
    Result := fItems.TryGetValue(Key, Value);
  finally
    fCS.Leave;
  end;
end;

{ TMVCObjectCache<T> }

{$IF Defined(CUSTOM_MANAGED_RECORDS)}

constructor TMVCObjectCache<T>.Create;
begin
  inherited Create;
  FStorage := TObjectDictionary<String, T>.Create([doOwnsValues]);
  FMREW := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMVCObjectCache<T>.Destroy;
begin
  FStorage.Free;
  FMREW.Free;
  inherited;
end;

function TMVCObjectCache<T>.WithSharedReadLock: TMVCObjectCacheReadTransaction<T>;
begin
  Result := TMVCObjectCacheReadTransaction<T>.WithReadLock(Self);
end;

function TMVCObjectCache<T>.WithExclusiveWriteLock: TMVCObjectCacheWriteTransaction<T>;
begin
  Result := TMVCObjectCacheWriteTransaction<T>.WithWriteLock(Self);
end;

{ TMVCObjectCacheWriteTransaction<T> }

function TMVCObjectCacheWriteTransaction<T>.Contains(const AName: string; out AValue: T): Boolean;
begin
  Result := self.fParent.FStorage.TryGetValue(AName, AValue);
end;

function TMVCObjectCacheWriteTransaction<T>.GetValue(const AName: string): T;
begin
  if not Contains(AName, Result) then
  begin
    raise EMVCException.Create('Key not available in cache - HINT: use Contains if you are not sure the item in in the cache');
  end;
end;

class operator TMVCObjectCacheWriteTransaction<T>.Initialize(out Dest: TMVCObjectCacheWriteTransaction<T>);
begin
  Dest.fActive := False;
  Dest.fParent := nil;
end;

class operator TMVCObjectCacheWriteTransaction<T>.Finalize(var Dest: TMVCObjectCacheWriteTransaction<T>);
begin
  if Dest.fActive then
  begin
    Dest.fParent.FMREW.EndWrite;
  end;
end;

class operator TMVCObjectCacheWriteTransaction<T>.Assign(var Dest: TMVCObjectCacheWriteTransaction<T>;  const [ref] Src: TMVCObjectCacheWriteTransaction<T>);
begin
  if Src.fActive then
  begin
    raise Exception.Create('Transaction item cannot be re-assigned');
  end;
  Dest.fActive := True;
  Dest.fParent := Src.fParent;
  Dest.fParent.FMREW.BeginWrite;
end;

function TMVCObjectCacheWriteTransaction<T>.Keys: TArray<String>;
begin
  Result := fParent.FStorage.Keys.ToArray;
end;

procedure TMVCObjectCacheWriteTransaction<T>.RemoveItem(const AName: string);
begin
  Self.fParent.FStorage.Remove(AName);
end;

procedure TMVCObjectCacheWriteTransaction<T>.SetValue(const AName: string; const AValue: T);
begin
  Self.fParent.FStorage.AddOrSetValue(AName, AValue);
end;

constructor TMVCObjectCacheWriteTransaction<T>.WithWriteLock(Parent: TMVCObjectCache<T>);
begin
  fParent := Parent;
end;

{ TMVCObjectCacheReadTransaction<T> }

function TMVCObjectCacheReadTransaction<T>.Contains(const AName: string; out AValue: T): Boolean;
begin
  Result := self.fParent.FStorage.TryGetValue(AName, AValue);
end;

class operator TMVCObjectCacheReadTransaction<T>.Initialize(out Dest: TMVCObjectCacheReadTransaction<T>);
begin
  Dest.fActive := False;
  Dest.fParent := nil;
end;

class operator TMVCObjectCacheReadTransaction<T>.Assign(var Dest: TMVCObjectCacheReadTransaction<T>;
  const [ref] Src: TMVCObjectCacheReadTransaction<T>);
begin
  if Src.fActive then
  begin
    raise Exception.Create('Transaction item cannot be re-assigned');
  end;
  Dest.fActive := True;
  Dest.fParent := Src.fParent;
  Dest.fParent.FMREW.BeginRead;
end;

class operator TMVCObjectCacheReadTransaction<T>.Finalize(var Dest: TMVCObjectCacheReadTransaction<T>);
begin
  if Dest.fActive then
  begin
    Dest.fParent.FMREW.EndRead;
  end;
end;

function TMVCObjectCacheReadTransaction<T>.GetValue(const AName: string): T;
begin
  if not Contains(AName, Result) then
  begin
    raise EMVCException.Create('Key not available in cache - HINT: use Contains if you are not sure the item in in the cache');
  end;
end;

function TMVCObjectCacheReadTransaction<T>.Keys: TArray<String>;
begin
  Result := fParent.FStorage.Keys.ToArray;
end;

constructor TMVCObjectCacheReadTransaction<T>.WithReadLock(Parent: TMVCObjectCache<T>);
begin
  fParent := Parent;
end;
{$ENDIF}

end.
