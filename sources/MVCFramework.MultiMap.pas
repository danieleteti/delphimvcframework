// ***************************************************************************
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
// *************************************************************************** }

unit MVCFramework.MultiMap;

interface

uses
  System.Generics.Collections;

type
  IMVCMultiMap<TKey, TVal> = interface
    ['{8D762DCE-3DB0-42BB-973C-68C49997606D}']
    procedure Add(const Key: TKey; const Value: TVal);
    function GetItems(const Key: TKey): TList<TVal>;
    function Contains(const Key: TKey): Boolean;
    function Keys: TArray<TKey>;
    procedure Remove(const Key: String);
    procedure Clear;
  end;

  IMVCInterfaceMultiMap<TVal: IInterface> = interface(IMVCMultiMap<String, TVal>)
    ['{90534C2B-7E93-4907-81CE-4CF6CAE63D50}']

  end;

  TMVCInterfaceMultiMap<TVal: IInterface> = class(TInterfacedObject, IMVCInterfaceMultiMap<TVal>)
  private
    FMultiMap: TObjectDictionary<String, TList<TVal>>;
  public
    constructor Create(OwnsValues: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Add(const Key: String; const Value: TVal);
    function GetItems(const Key: String): TList<TVal>;
    function Contains(const Key: String): Boolean;
    function Keys: TArray<String>;
    procedure Remove(const Key: String);
    procedure Clear;
  end;

  IMVCObjectMultiMap<TVal: class> = interface(IMVCMultiMap<String, TVal>)
    ['{1F6A6826-0759-488A-AEFC-A068AD5BB5DE}']

  end;

  TMVCObjectMultiMap<TVal: class> = class(TInterfacedObject, IMVCObjectMultiMap<TVal>)
  private
    FMultiMap: TObjectDictionary<String, TObjectList<TVal>>;
  public
    constructor Create(OwnsValues: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Add(const Key: String; const Value: TVal);
    function GetItems(const Key: String): TList<TVal>;
    function Contains(const Key: String): Boolean;
    function Keys: TArray<String>;
    procedure Remove(const Key: String);
    procedure Clear;
  end;

implementation

uses
  System.SysUtils;

{ TMVCMultiMap<TKey, TVal> }

procedure TMVCObjectMultiMap<TVal>.Add(const Key: String; const Value: TVal);
var
  lList: TObjectList<TVal>;
begin
  if not FMultiMap.TryGetValue(Key, lList) then
  begin
    lList := TObjectList<TVal>.Create(True);
    FMultiMap.Add(Key, lList);
  end;
  lList.Add(Value);
end;

procedure TMVCObjectMultiMap<TVal>.Clear;
begin
  FMultiMap.Clear;
end;

function TMVCObjectMultiMap<TVal>.Contains(const Key: String): Boolean;
begin
  Result := FMultiMap.ContainsKey(Key);
end;

constructor TMVCObjectMultiMap<TVal>.Create(OwnsValues: Boolean);
var
  lDictOwnerships: TDictionaryOwnerships;
begin
  inherited Create;
  lDictOwnerships := [];
  if OwnsValues then
    lDictOwnerships := lDictOwnerships + [doOwnsValues];

  FMultiMap := TObjectDictionary < String, TObjectList < TVal >>.Create(lDictOwnerships);
end;

destructor TMVCObjectMultiMap<TVal>.Destroy;
begin
  FMultiMap.Free;
  inherited;
end;

function TMVCObjectMultiMap<TVal>.GetItems(const Key: String): TList<TVal>;
var
  lOutput: TObjectList<TVal>;
begin
  Result := nil;
  if FMultiMap.TryGetValue(Key, lOutput) then
    Result := lOutput;
end;

function TMVCObjectMultiMap<TVal>.Keys: TArray<String>;
begin
  Result := FMultiMap.Keys.ToArray;
end;

procedure TMVCObjectMultiMap<TVal>.Remove(const Key: String);
begin
  FMultiMap.Remove(Key);
end;

{ TMVCInterfaceMultiMap<TVal> }

procedure TMVCInterfaceMultiMap<TVal>.Add(const Key: String; const Value: TVal);
var
  lList: TList<TVal>;
begin
  if not FMultiMap.TryGetValue(Key, lList) then
  begin
    lList := TList<TVal>.Create;
    FMultiMap.Add(Key, lList);
  end;
  lList.Add(Value);
end;

procedure TMVCInterfaceMultiMap<TVal>.Clear;
begin
  FMultiMap.Clear;
end;

function TMVCInterfaceMultiMap<TVal>.Contains(const Key: String): Boolean;
begin
  Result := FMultiMap.ContainsKey(Key);
end;

constructor TMVCInterfaceMultiMap<TVal>.Create(OwnsValues: Boolean);
var
  lDictOwnerships: TDictionaryOwnerships;
begin
  inherited Create;
  lDictOwnerships := [];
  if OwnsValues then
    lDictOwnerships := lDictOwnerships + [doOwnsValues];
  FMultiMap := TObjectDictionary < String, TList < TVal >>.Create(lDictOwnerships);
end;

destructor TMVCInterfaceMultiMap<TVal>.Destroy;
begin
  FMultiMap.Free;
  inherited;
end;

function TMVCInterfaceMultiMap<TVal>.GetItems(const Key: String): TList<TVal>;
var
  lOutput: TList<TVal>;
begin
  Result := nil;
  if FMultiMap.TryGetValue(Key, lOutput) then
    Result := lOutput;
end;

function TMVCInterfaceMultiMap<TVal>.Keys: TArray<String>;
begin
  Result := FMultiMap.Keys.ToArray;
end;

procedure TMVCInterfaceMultiMap<TVal>.Remove(const Key: String);
begin
  FMultiMap.Remove(Key);
end;

end.
