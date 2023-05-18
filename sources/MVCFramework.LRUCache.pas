// *************************************************************************** }
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
// ***************************************************************************

unit MVCFramework.LRUCache;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TMVCLRUCacheAction = reference to function(const Key: string): Boolean;

  TMVCLRUCache<T: class> = class
  private type
    TMVCLRUCacheItem = class
    public
      Key: string;
      Value: T;
      constructor Create(const Key: string; const Item: T);
      destructor Destroy; override;
    end;
  private
    fCache: TObjectList<TMVCLRUCacheItem>;
    fCapacity: Integer;
  public
    constructor Create(const Capacity: Integer); virtual;
    destructor Destroy; override;
    function Contains(const Key: string; out ItemIndex: UInt64): Boolean;
    procedure Put(const Key: string; const Item: T);
    function TryGet(const Key: string; out Item: T): Boolean;
    procedure RemoveIf(const Action: TMVCLRUCacheAction);
    procedure Clear;
    function Size: UInt32;
    procedure Lock;
    procedure UnLock;
  end;

implementation

uses
  MVCFramework.Logger;

{ TMVCLRUCache }

procedure TMVCLRUCache<T>.Clear;
begin
  fCache.Clear;
end;

function TMVCLRUCache<T>.Contains(const Key: string; out ItemIndex: UInt64): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to fCache.Count - 1 do
  begin
    if Key = fCache[I].Key then
    begin
      ItemIndex := I;
      Exit(True);
    end;
  end;
end;

function TMVCLRUCache<T>.Size: UInt32;
begin
  Result := fCache.Count;
end;

constructor TMVCLRUCache<T>.Create(const Capacity: Integer);
begin
  inherited Create;
  fCapacity := Capacity;
  fCache := TObjectList<TMVCLRUCacheItem>.Create(True);
end;

destructor TMVCLRUCache<T>.Destroy;
begin
  fCache.Free;
  inherited;
end;

procedure TMVCLRUCache<T>.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TMVCLRUCache<T>.Put(const Key: string; const Item: T);
begin
  Lock;
  try
    if fCache.Count = fCapacity then
    begin
	    fCache.Delete(fCache.Count - 1);
    end;
    fCache.Insert(0, TMVCLRUCacheItem.Create(Key, Item));
  finally
    UnLock;
  end;
end;

procedure TMVCLRUCache<T>.RemoveIf(const Action: TMVCLRUCacheAction);
var
  I: Integer;
  lIndex: Integer;
  lCacheSize: Integer;
begin
  lIndex := 0;
  lCacheSize := fCache.Count;
  while lIndex < lCacheSize do
  begin
    if Action(fCache[lIndex].Key) then
    begin
      fCache.Delete(lIndex);
    end
    else
    begin
      Inc(lIndex);
    end;
    lCacheSize := fCache.Count;
  end;
end;

function TMVCLRUCache<T>.TryGet(const Key: string; out Item: T): Boolean;
var
  lItemIndex: UInt64;
  lCacheItem: TMVCLRUCacheItem;
begin
  Result := Contains(Key, lItemIndex);
  if Result { and (lItemIndex <> 0) } then
  begin
    if lItemIndex > 0 then
    begin
      fCache.Exchange(lItemIndex, 0);
    end;
    //
    //
    // if lItemIndex = 0 then
    // begin
    // lCacheItem := fCache[0];
    // end
    // else
    // begin
    // lCacheItem := fCache.Extract(fCache[lItemIndex]);
    // fCache.Insert(0, lCacheItem);
    // end;
    lCacheItem := fCache[0];
    Item := lCacheItem.Value;
  end;
end;

procedure TMVCLRUCache<T>.UnLock;
begin
  TMonitor.Exit(Self);
end;

{ TMVCLRUCache<T>.TMVCLRUCacheItem<T> }

constructor TMVCLRUCache<T>.TMVCLRUCacheItem.Create(const Key: string; const Item: T);
begin
  inherited Create;
  Self.Key := Key;
  Self.Value := Item;
end;

destructor TMVCLRUCache<T>.TMVCLRUCacheItem.Destroy;
begin
  Value.Free;
  Value := nil;
  inherited;
end;

end.
