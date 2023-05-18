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

unit IntfObjectPoolTestU;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTIntfObjectPool = class
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
  System.Generics.Collections, MVCFramework.IntfObjectPool, System.SysUtils, System.Classes, SyncObjs;

var
  GNumReleased: integer;

type
  IPoolObject = Interface
    ['{CA1012E7-5639-48A9-B26B-D94218C28508}']
  End;

type
  TPoolObject = class(TInterfacedObject, IPoolObject)
  strict private
    FValue: integer;
  public
    constructor Create; overload;
    constructor Create(value: integer); overload;
    destructor Destroy; override;
    property value: integer read FValue;
  end;

type
  TCustomInterfaceCreator = TFunc<IInterface>;

  { TTestTObjectPool }

procedure TTestTIntfObjectPool.TestFixedSize;
var
  i: integer;
  lObjs: array [1 .. 5] of IPoolObject;
  lPool: IIntfObjectPool;
  lRpt: integer;
  lCustomFactory: TCustomInterfaceCreator;
begin
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfPoolFactory.CreatePool(3, 3, 0, lCustomFactory);
  for lRpt := 1 to 3 do
  begin
    for i := Low(lObjs) to High(lObjs) do
      lObjs[i] := lPool.GetFromPool as IPoolObject;

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

  // lPool := nil;

  // Assert.AreEqual(3, GNumReleased);
end;

procedure TTestTIntfObjectPool.TestGrowing;
var
  i: integer;
  lObjs: array [1 .. 5] of IPoolObject;
  lPool: IIntfObjectPool;
  lRpt: integer;
  j: integer;
  lCustomFactory: TCustomInterfaceCreator;
begin
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfPoolFactory.CreateUnlimitedPool(0, 0, lCustomFactory);

  for lRpt := 1 to 3 do
  begin
    for i := Low(lObjs) to High(lObjs) do
    begin
      lObjs[i] := lPool.GetFromPool as IPoolObject;
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
  // lPool := nil; //actually releases internal instances
  // Assert.AreEqual(5, GNumReleased);
end;

procedure TTestTIntfObjectPool.TestMultiThreading;
var
  lPool: IIntfObjectPool;
  i: integer;
  lCount: Int64;
  lMax: Int64;
  lCustomFactory: TCustomInterfaceCreator;
const
  POOL_SIZE = 10;
begin
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfPoolFactory.CreatePool(POOL_SIZE, 10, 0, lCustomFactory);
  lCount := 100;
  lMax := lCount;
  for i := 1 to lMax do
  begin
    TThread.CreateAnonymousThread(
      procedure
      var
        j: integer;
        lObj: IPoolObject;
      begin
        for j := 1 to 2 do
        begin
          repeat
            Sleep(1);
            lObj := lPool.GetFromPool as IPoolObject;
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

  lPool := nil;
end;

procedure TTestTIntfObjectPool.TestNotRaiseExceptionWhenPoolIsEmpty;
var
  lPool: IIntfObjectPool;
  lObj1, lObj2: IPoolObject;
  lCustomFactory: TCustomInterfaceCreator;
begin
  lObj1 := nil;
  lObj2 := nil;
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfObjectPool.Create(2, 0, 0, lCustomFactory);
  try
    lObj1 := lPool.GetFromPool(False) as IPoolObject;
    lObj2 := lPool.GetFromPool(False) as IPoolObject;
    Assert.IsNull(lPool.GetFromPool(False));
  finally
    lPool.ReleaseToPool(lObj1);
    lPool.ReleaseToPool(lObj2);
  end;

  lPool := nil;
end;

procedure TTestTIntfObjectPool.TestRaiseExceptionWhenPoolIsEmpty;
var
  lPool: IIntfObjectPool;
  lObj1, lObj2: IPoolObject;
  lCustomFactory: TCustomInterfaceCreator;
begin
  lObj1 := nil;
  lObj2 := nil;
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfObjectPool.Create(2, 0, 0, lCustomFactory);
  try
    lObj1 := lPool.GetFromPool(True) as IPoolObject;
    lObj2 := lPool.GetFromPool(True) as IPoolObject;
    Assert.WillRaise(
      procedure
      begin
        lPool.GetFromPool(True);
      end, EObjectPool);
  finally
    lPool.ReleaseToPool(lObj1);
    lPool.ReleaseToPool(lObj2);
  end;
  lPool := nil;
end;

procedure TTestTIntfObjectPool.TestFactory;
var
  lPoolObject: IPoolObject;
  lPool: TIntfObjectPool;
  lCustomFactory: TCustomInterfaceCreator;
begin
  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create();
    end;
  lPool := TIntfObjectPool.Create(0, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool as IPoolObject;
    Assert.AreEqual(1, TPoolObject(lPoolObject).value);
  finally
    FreeAndNil(lPool);
  end;

  lPool := TIntfObjectPool.Create(3, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool as IPoolObject;
    Assert.AreEqual(1, TPoolObject(lPoolObject).value);
  finally
    FreeAndNil(lPool);
  end;

  lCustomFactory := function: IInterface
    begin
      Result := TPoolObject.Create(79);
    end;

  lPool := TIntfObjectPool.Create(0, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool as IPoolObject;
    Assert.AreEqual(79, TPoolObject(lPoolObject).value);
  finally
    FreeAndNil(lPool);
  end;

  lPool := TIntfObjectPool.Create(3, 0, 0, lCustomFactory);
  try
    lPoolObject := lPool.GetFromPool as IPoolObject;
    Assert.AreEqual(79, TPoolObject(lPoolObject).value);
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

initialization

GObjectPoolSamplingIntervalMS := 1000;
TDUnitX.RegisterTestFixture(TTestTIntfObjectPool);

end.
