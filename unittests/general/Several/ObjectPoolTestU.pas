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

unit ObjectPoolTestU;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTObjectPool = class
  public
    [Test]
    procedure TestFixedSize;
    [Test]
    procedure TestGrowing;
    [Test]
    procedure TestFactory;
    [Test]
    procedure TestMultiThreading;
    [Test]
    procedure TestRaiseExceptionWhenPoolIsEmpty;
    [Test]
    procedure TestNotRaiseExceptionWhenPoolIsEmpty;
  end;

implementation

uses
  System.Generics.Collections, MVCFramework.ObjectPool, System.SysUtils, System.Classes, SyncObjs;

var
  GNumReleased: integer;

type
  TPoolObject = class
  strict private
    FValue: integer;
  public
    constructor Create; overload;
    constructor Create(value: integer); overload;
    destructor Destroy; override;
    Function GetValue : integer;
    property value: integer read FValue;
  end;

  { TTestTObjectPool }

procedure TTestTObjectPool.TestFixedSize;
var
  i: integer;
  lObjs: array [1 .. 5] of TPoolObject;
  lPool: IObjectPool<TPoolObject>;
  lRpt: integer;
begin
  lPool := TPoolFactory.CreatePool<TPoolObject>(3,3,0);
  for lRpt := 1 to 3 do
  begin
    for i := Low(lObjs) to High(lObjs) do
      lObjs[i] := lPool.GetFromPool;

    for i := 1 to 3 do
      Assert.IsTrue(nil <> lObjs[i]);
    for i := 4 to 5 do
      Assert.IsTrue(nil = lObjs[i]);

    Assert.IsTrue(lObjs[1] <> lObjs[2]);
    Assert.IsTrue(lObjs[1] <> lObjs[3]);
    Assert.IsTrue(lObjs[2] <> lObjs[3]);

    GNumReleased := 0;

    for i := 1 to 3 do
      lPool.ReleaseToPool(lObjs[i]);

    Assert.AreEqual(0, GNumReleased);
  end;

  lPool := nil;

  Assert.AreEqual(3, GNumReleased);
end;

procedure TTestTObjectPool.TestGrowing;
var
  i: integer;
  lObjs: array [1 .. 5] of TPoolObject;
  lPool: IObjectPool<TPoolObject>;
  lRpt: integer;
  j: integer;
begin
  lPool := TPoolFactory.CreateUnlimitedPool<TPoolObject>(0,0);

  for lRpt := 1 to 3 do
  begin
    for i := Low(lObjs) to High(lObjs) do
    begin
      lObjs[i] := lPool.GetFromPool;
    end;

    for i := 1 to 5 do
    begin
      Assert.IsTrue(nil <> lObjs[i]);
      for j := i + 1 to 5 do
      begin
        Assert.IsTrue(lObjs[i] <> lObjs[j]);
      end;
    end;

    GNumReleased := 0;

    for i := 1 to 5 do
    begin
      lPool.ReleaseToPool(lObjs[i]);
    end;

    Assert.AreEqual(0, GNumReleased);
  end;
  lPool := nil; //actually releases internal instances
  Assert.AreEqual(5, GNumReleased);
end;

procedure TTestTObjectPool.TestMultiThreading;
var
  lPool: IObjectPool<TPoolObject>;
  i: integer;
  lCount: Int64;
  lMax: Int64;
const
  POOL_SIZE = 10;
begin
  lPool := TPoolFactory.CreatePool<TPoolObject>(POOL_SIZE, 10, 0);
  lCount := 100;
  lMax := lCount;
  for i := 1 to lMax do
  begin
    TThread.CreateAnonymousThread(
      procedure
      var
        j: integer;
        lObj: TPoolObject;
      begin
        for j := 1 to 2 do
        begin
          repeat
            Sleep(1);
            lObj := lPool.GetFromPool;
          until Assigned(lObj);
          try
            Sleep(1);
          finally
            lPool.ReleaseToPool(lObj);
          end;
        end;
        TInterlocked.Decrement(lCount);
      end).Start;
  end;

  while TInterlocked.Read(lCount) > 0 do
  begin
    TThread.Sleep(100);
  end;

  Assert.AreEqual(POOL_SIZE, lPool.Size);
end;

procedure TTestTObjectPool.TestNotRaiseExceptionWhenPoolIsEmpty;
var
  lPool: IObjectPool<TPoolObject>;
  lObj1, lObj2: TPoolObject;
begin
  lObj1 := nil;
  lObj2 := nil;
  lPool := TObjectPool<TPoolObject>.Create(2,0,0);
  try
    lObj1 := lPool.GetFromPool(False);
    lObj2 := lPool.GetFromPool(False);
    Assert.IsNull(lPool.GetFromPool(False));
  finally
    lPool.ReleaseToPool(lObj1);
    lPool.ReleaseToPool(lObj2);
  end;
end;

procedure TTestTObjectPool.TestRaiseExceptionWhenPoolIsEmpty;
var
  lPool: IObjectPool<TPoolObject>;
  lObj1, lObj2: TPoolObject;
begin
  lObj1 := nil;
  lObj2 := nil;
  lPool := TObjectPool<TPoolObject>.Create(2,0,0);
  try
    lObj1 := lPool.GetFromPool(True);
    lObj2 := lPool.GetFromPool(True);
    Assert.WillRaise(procedure
      begin
        lPool.GetFromPool(True);
      end, EObjectPool);
  finally
    lPool.ReleaseToPool(lObj1);
    lPool.ReleaseToPool(lObj2);
  end;
end;

procedure TTestTObjectPool.TestFactory;
var
  lPoolObject: TPoolObject;
  lPool: TObjectPool<TPoolObject>;
  lCustomFactory: TFunc<TPoolObject>;
begin
  lPool := TObjectPool<TPoolObject>.Create(0, 0, 0);
  try
    lPoolObject := lPool.GetFromPool;
    Assert.AreEqual(1, lPoolObject.value);
    lPool.ReleaseToPool(lPoolObject); //dteti 26/10/2022
  finally
    FreeAndNil(lPool);
  end;

  lPool := TObjectPool<TPoolObject>.Create(3, 0, 0);
  try
    lPoolObject := lPool.GetFromPool;
    Assert.AreEqual(1, lPoolObject.Getvalue);
    lPool.ReleaseToPool(lPoolObject); //dteti 26/10/2022
  finally
    FreeAndNil(lPool);
  end;

  lCustomFactory := function: TPoolObject
    begin
      Result := TPoolObject.Create(79);
    end;

  lPool := TObjectPool<TPoolObject>.Create(0, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool;
    Assert.AreEqual(79, lPoolObject.value);
    lPool.ReleaseToPool(lPoolObject); //dteti 26/10/2022
  finally
    FreeAndNil(lPool);
  end;

  lPool := TObjectPool<TPoolObject>.Create(3, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool;
    Assert.AreEqual(79, lPoolObject.value);
    lPool.ReleaseToPool(lPoolObject); //dteti 26/10/2022
  finally
    FreeAndNil(lPool);
  end;
end;

{ TPoolObject }

constructor TPoolObject.Create;
begin
  Create(1);
end;

constructor TPoolObject.Create(value: integer);
begin
  inherited Create;
  FValue := value;
end;

destructor TPoolObject.Destroy;
begin
  Inc(GNumReleased);
  inherited;
end;

function TPoolObject.GetValue: integer;
begin
  result := value;
end;

initialization

GObjectPoolSamplingIntervalMS := 1000;
TDUnitX.RegisterTestFixture(TTestTObjectPool);

end.
