// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti,
  System.SyncObjs, MVCFramework.Commons, System.DateUtils;

const
  DEFAULT_CACHE_DURATION = OneMinute * 10;

type
  TMVCCacheItem = class
  private
    FCriticalSection: TCriticalSection;
    FValue: TValue;
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    property Value: TValue read GetValue write SetValue;
  end;

  TMVCCache = class sealed
  private
    FStorage: TObjectDictionary<string, TMVCCacheItem>;
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetValue(const AName: string; const AValue: TValue);
    function Contains(const AName: string; out AValue: TValue): Boolean;
    function GetValue(const AName: string): TValue;
    function ExecOnItem(const AName: string; const AAction: TProc<TValue>): Boolean;
  end;

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

implementation

uses
  System.Math;

{ TCache }

procedure TMVCCache.SetValue(const AName: string; const AValue: TValue);
var
  lValue: TValue;
begin
  lValue := AValue;

  FCriticalSection.DoWithLock(
    procedure
    var
      lItem: TMVCCacheItem;
    begin
      if FStorage.TryGetValue(AName, lItem) then
      begin
        lItem.Value := lValue;
      end
      else
      begin
        lItem := TMVCCacheItem.Create;
        try
          lItem.Value := lValue;
          FStorage.Add(AName, lItem);
        except
          lItem.Free;
          raise;
        end;
      end;
    end);
end;

function TMVCCache.ExecOnItem(const AName: string; const AAction: TProc<TValue>): Boolean;
var
  lItem: TMVCCacheItem;
begin
  Result := False;
  FCriticalSection.Enter;
  try
    if FStorage.TryGetValue(AName, lItem) then
    begin
      Result := True;
      AAction(lItem.Value);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TMVCCache.Contains(const AName: string; out AValue: TValue): Boolean;
var
  lValue: TMVCCacheItem;
  lRes: Boolean;
begin
  FCriticalSection.DoWithLock(
    procedure
    begin
      lRes := FStorage.TryGetValue(AName, lValue);
    end);
  Result := lRes;
  if Result then
    AValue := lValue.Value;
end;

constructor TMVCCache.Create;
begin
  inherited Create;
  FStorage := TObjectDictionary<string, TMVCCacheItem>.Create([doOwnsValues]);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TMVCCache.Destroy;
begin
  FCriticalSection.Free;
  FStorage.Free;
  inherited;
end;

function TMVCCache.GetValue(const AName: string): TValue;
var
  lItem: TMVCCacheItem;
  lResult: TValue;
begin
  Result := TValue.Empty;
  FCriticalSection.DoWithLock(
    procedure
    begin
      if FStorage.TryGetValue(AName, lItem) then
      begin
        lResult := lItem.Value;
      end;
    end);
  Result := lResult;
end;

{ TMVCFrameworkCacheItem }

constructor TMVCCacheItem.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FValue := TValue.Empty;
end;

destructor TMVCCacheItem.Destroy;
begin
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

end.
