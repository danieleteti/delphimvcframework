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

unit MVCFramework.MultiMap;

{$I dmvcframework.inc}

interface

uses
  System.Generics.Collections;

type

  IMVCMultiMap<TKey, TVal> = interface
    ['{8D762DCE-3DB0-42BB-973C-68C49997606D}']
    procedure Add(const AKey: TKey; const AValue: TVal);
    function GetItems(const AKey: TKey): TList<TVal>;
    function Contains(const AKey: TKey): Boolean;
    function Keys: TArray<TKey>;
    procedure Remove(const AKey: String);
    procedure Clear;
  end;

  IMVCInterfaceMultiMap<TVal: IInterface> = interface(IMVCMultiMap<String, TVal>)
    ['{90534C2B-7E93-4907-81CE-4CF6CAE63D50}']
  end;

  TMVCInterfaceMultiMap<TVal: IInterface> = class(TInterfacedObject, IMVCInterfaceMultiMap<TVal>)
  private
    FMultiMap: TObjectDictionary<String, TList<TVal>>;
  public
    constructor Create(AOwnsValues: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Add(const AKey: String; const AValue: TVal);
    function GetItems(const AKey: String): TList<TVal>;
    function Contains(const AKey: String): Boolean;
    function Keys: TArray<String>;
    procedure Remove(const AKey: String);
    procedure Clear;
  end;

  IMVCObjectMultiMap<TVal: class> = interface(IMVCMultiMap<String, TVal>)
    ['{1F6A6826-0759-488A-AEFC-A068AD5BB5DE}']
  end;

  TMVCObjectMultiMap<TVal: class> = class(TInterfacedObject, IMVCObjectMultiMap<TVal>)
  private
    FMultiMap: TObjectDictionary<String, TObjectList<TVal>>;
  public
    constructor Create(AOwnsValues: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Add(const AKey: String; const AValue: TVal);
    function GetItems(const AKey: String): TList<TVal>;
    function Contains(const AKey: String): Boolean;
    function Keys: TArray<String>;
    procedure Remove(const AKey: String);
    procedure Clear;
  end;

implementation

uses
  System.SysUtils;

{ TMVCObjectMultiMap<TKey, TVal> }

procedure TMVCObjectMultiMap<TVal>.Add(const AKey: String; const AValue: TVal);
var
  lList: TObjectList<TVal>;
begin
  if not FMultiMap.TryGetValue(AKey, lList) then
  begin
    lList := TObjectList<TVal>.Create(True);
    FMultiMap.Add(AKey, lList);
  end;
  lList.Add(AValue);
end;

procedure TMVCObjectMultiMap<TVal>.Clear;
begin
  FMultiMap.Clear;
end;

function TMVCObjectMultiMap<TVal>.Contains(const AKey: String): Boolean;
begin
  Result := FMultiMap.ContainsKey(AKey);
end;

constructor TMVCObjectMultiMap<TVal>.Create(AOwnsValues: Boolean);
var
  lDictOwnerships: TDictionaryOwnerships;
begin
  inherited Create;
  lDictOwnerships := [];
  if AOwnsValues then
    lDictOwnerships := lDictOwnerships + [doOwnsValues];

  FMultiMap := TObjectDictionary < String, TObjectList < TVal >>.Create(lDictOwnerships);
end;

destructor TMVCObjectMultiMap<TVal>.Destroy;
begin
  FMultiMap.Free;
  inherited;
end;

function TMVCObjectMultiMap<TVal>.GetItems(const AKey: String): TList<TVal>;
var
  lOutput: TObjectList<TVal>;
begin
  Result := nil;
  if FMultiMap.TryGetValue(AKey, lOutput) then
    Result := lOutput;
end;

function TMVCObjectMultiMap<TVal>.Keys: TArray<String>;
begin
  Result := FMultiMap.Keys.ToArray;
end;

procedure TMVCObjectMultiMap<TVal>.Remove(const AKey: String);
begin
  FMultiMap.Remove(AKey);
end;

{ TMVCInterfaceMultiMap<TVal> }

procedure TMVCInterfaceMultiMap<TVal>.Add(const AKey: String; const AValue: TVal);
var
  lList: TList<TVal>;
begin
  if not FMultiMap.TryGetValue(AKey, lList) then
  begin
    lList := TList<TVal>.Create;
    FMultiMap.Add(AKey, lList);
  end;
  lList.Add(AValue);
end;

procedure TMVCInterfaceMultiMap<TVal>.Clear;
begin
  FMultiMap.Clear;
end;

function TMVCInterfaceMultiMap<TVal>.Contains(const AKey: String): Boolean;
begin
  Result := FMultiMap.ContainsKey(AKey);
end;

constructor TMVCInterfaceMultiMap<TVal>.Create(AOwnsValues: Boolean);
var
  lDictOwnerships: TDictionaryOwnerships;
begin
  inherited Create;
  lDictOwnerships := [];
  if AOwnsValues then
    lDictOwnerships := lDictOwnerships + [doOwnsValues];
  FMultiMap := TObjectDictionary < String, TList < TVal >>.Create(lDictOwnerships);
end;

destructor TMVCInterfaceMultiMap<TVal>.Destroy;
begin
  FMultiMap.Free;
  inherited;
end;

function TMVCInterfaceMultiMap<TVal>.GetItems(const AKey: String): TList<TVal>;
var
  lOutput: TList<TVal>;
begin
  Result := nil;
  if FMultiMap.TryGetValue(AKey, lOutput) then
    Result := lOutput;
end;

function TMVCInterfaceMultiMap<TVal>.Keys: TArray<String>;
begin
  Result := FMultiMap.Keys.ToArray;
end;

procedure TMVCInterfaceMultiMap<TVal>.Remove(const AKey: String);
begin
  FMultiMap.Remove(AKey);
end;

end.
