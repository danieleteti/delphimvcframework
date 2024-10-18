// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

unit InjectorTestU;

interface

uses
  DUnitX.TestFramework, MVCFramework.Container,
  MVCFramework.Serializer.JsonDataObjects;

type

  [TestFixture]
  TTestContainer = class
  public
    [Test]
    procedure TestNotBuiltContainer;
    [Test]
    procedure TestUnknownService;
    [Test]
    procedure TestTransient;
    [Test]
    procedure TestTransientWithDelegate;
    [Test]
    procedure TestSingleton;
    [Test]
    procedure TestSingletonWithDelegate;
    [Test]
    procedure TestSingletonPerRequest;
    [Test]
    procedure TestSingletonPerRequestWithDelegate;
    [Test]
    procedure TestCascadeConstructorInjection;
  end;


  IServiceA = interface
    ['{B6C5EAD8-9008-4200-BF33-E3DE5C8A2320}']
  end;

  IServiceB = interface
    ['{8418244D-8AEC-4567-A21E-3F4ECD07E227}']
  end;

  IServiceC = interface
    ['{A9E5FD77-87FD-4C9C-91BA-79556252DAAD}']
    function GetServiceA: IServiceA;
    function GetServiceB: IServiceB;
  end;

  TServiceA = class(TInterfacedObject, IServiceA)

  end;

  TServiceB = class(TInterfacedObject, IServiceB)

  end;

  TServiceAB = class(TInterfacedObject, IServiceA, IServiceB)

  end;

  TServiceC = class(TInterfacedObject, IServiceC)
  private
    fServiceA: IServiceA;
    fServiceB: IServiceB;
  protected
    function GetServiceA: IServiceA;
    function GetServiceB: IServiceB;
  public
    constructor Create(ServiceA: IServiceA; ServiceB: IServiceB);
  end;

implementation

uses
  System.Generics.Collections, MVCFramework.IntfObjectPool, System.SysUtils, System.Classes, SyncObjs,
  MVCFramework.Serializer.Intf;

{ TTestContainer }

procedure TTestContainer.TestCascadeConstructorInjection;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(TServiceA, IServiceA);
  lCont.RegisterType(TServiceB, IServiceB, TRegistrationType.SingletonPerRequest);
  lCont.RegisterType(TServiceC, IServiceC);
  lCont.Build;

  // 1° "request"
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceC)) as IServiceC;
  Assert.IsNotNull(l0.GetServiceA);
  Assert.IsNotNull(l0.GetServiceB);

  // resolve another "IServiceC" in the same request - ServiceB is rtSingletonPerRequest
  var l01 := lResolver.Resolve(TypeInfo(IServiceC)) as IServiceC;
  Assert.IsNotNull(l0.GetServiceA);
  Assert.IsNotNull(l0.GetServiceB);
  Assert.AreNotEqual(l0.GetServiceA, l01.GetServiceA);
  Assert.AreEqual(l0.GetServiceB, l01.GetServiceB);

  // 2° "request"
  lResolver := NewServiceContainerResolver(lCont);
  var l1 := lResolver.Resolve(TypeInfo(IServiceC)) as IServiceC;
  Assert.IsNotNull(l0.GetServiceA);
  Assert.IsNotNull(l0.GetServiceB);
  Assert.AreNotEqual(l0.GetServiceA, l1.GetServiceA);
  Assert.AreNotEqual(l0.GetServiceB, l1.GetServiceB);
end;

procedure TTestContainer.TestNotBuiltContainer;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(TServiceA, IServiceA);
  var lResolver := NewServiceContainerResolver(lCont);
  Assert.WillRaise(
    procedure
    begin
      var l0 := lResolver.Resolve(TypeInfo(IServiceA));
    end, EMVCContainerError);
end;

procedure TTestContainer.TestSingleton;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(TServiceA, IServiceA, TRegistrationType.Singleton);
  lCont.RegisterType(TServiceA, IServiceA, TRegistrationType.Singleton, 'Svc1');
  lCont.Build;

  // 1° Request
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreEqual(l2, l3);

  // 2° Request
  lResolver := NewServiceContainerResolver(lCont);
  var l10 := lResolver.Resolve(TypeInfo(IServiceA));
  var l11 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l10, l11);
  Assert.AreEqual(l0, l10);
  Assert.AreEqual(l1, l11);
end;

procedure TTestContainer.TestSingletonPerRequest;
begin
  var lCont := NewMVCServiceContainer
          .RegisterType(TServiceA, IServiceA, TRegistrationType.SingletonPerRequest)
          .RegisterType(TServiceA, IServiceA, TRegistrationType.SingletonPerRequest, 'Svc1');
  lCont.Build;

  // 1° "request"
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreEqual(l2, l3);

  // 2° "request"
  lResolver := NewServiceContainerResolver(lCont);
  var l00 := lResolver.Resolve(TypeInfo(IServiceA));
  var l10 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l00, l10);
  Assert.AreNotEqual(l0, l00);
  Assert.AreNotEqual(l1, l10);
end;


procedure TTestContainer.TestSingletonPerRequestWithDelegate;
begin
  var lCont := NewMVCServiceContainer
          .RegisterType(function : TInterfacedObject
                        begin
                          Result := TServiceA.Create
                        end, IServiceA, TRegistrationType.SingletonPerRequest)
          .RegisterType(function : TInterfacedObject
                        begin
                          Result := TServiceA.Create
                        end, IServiceA, TRegistrationType.SingletonPerRequest, 'Svc1');
  lCont.Build;

  // 1° "request"
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreEqual(l2, l3);

  // 2° "request"
  lResolver := NewServiceContainerResolver(lCont);
  var l00 := lResolver.Resolve(TypeInfo(IServiceA));
  var l10 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l00, l10);
  Assert.AreNotEqual(l0, l00);
  Assert.AreNotEqual(l1, l10);
end;

procedure TTestContainer.TestSingletonWithDelegate;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(function : TInterfacedObject
                     begin
                       Result := TServiceA.Create
                     end, IServiceA, TRegistrationType.Singleton);
  lCont.RegisterType(function : TInterfacedObject
                     begin
                       Result := TServiceA.Create
                     end, IServiceA, TRegistrationType.Singleton, 'Svc1');
  lCont.Build;

  // 1° Request
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreEqual(l2, l3);

  // 2° Request
  lResolver := NewServiceContainerResolver(lCont);
  var l10 := lResolver.Resolve(TypeInfo(IServiceA));
  var l11 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreEqual(l10, l11);
  Assert.AreEqual(l0, l10);
  Assert.AreEqual(l1, l11);
end;

procedure TTestContainer.TestTransient;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(TServiceA, IServiceA);
  lCont.RegisterType(TServiceA, IServiceA, TRegistrationType.Transient, 'Svc1');
  lCont.Build;
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreNotEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreNotEqual(l2, l3);
end;

procedure TTestContainer.TestTransientWithDelegate;
begin
  var lCont := NewMVCServiceContainer;
  lCont.RegisterType(function : TInterfacedObject
                     begin
                       Result := TServiceA.Create
                     end, IServiceA);
  lCont.RegisterType(function : TInterfacedObject
                     begin
                       Result := TServiceA.Create
                     end, IServiceA, TRegistrationType.Transient, 'Svc1');
  lCont.Build;
  var lResolver := NewServiceContainerResolver(lCont);
  var l0 := lResolver.Resolve(TypeInfo(IServiceA));
  var l1 := lResolver.Resolve(TypeInfo(IServiceA));
  Assert.AreNotEqual(l0, l1);
  var l2 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  var l3 := lResolver.Resolve(TypeInfo(IServiceA), 'Svc1');
  Assert.AreNotEqual(l2, l3);
end;

procedure TTestContainer.TestUnknownService;
begin
  var lCont := NewMVCServiceContainer;
  Assert.WillRaise(
    procedure
    begin
      lCont.RegisterType(TServiceA, IServiceB);
    end, EMVCContainerErrorUnknownService);

  Assert.WillRaise(
    procedure
    begin
      lCont.RegisterType(TMVCJsonDataObjectsSerializer, IServiceB);
    end, EMVCContainerErrorUnknownService);

  Assert.WillRaise(
    procedure
    begin
      lCont.RegisterType(TServiceA, IMVCSerializer);
    end, EMVCContainerErrorUnknownService);
end;

{ TServiceC }

constructor TServiceC.Create(ServiceA: IServiceA; ServiceB: IServiceB);
begin
  inherited Create;
  fServiceA := ServiceA;
  fServiceB := ServiceB;
end;

function TServiceC.GetServiceA: IServiceA;
begin
  Result := fServiceA;
end;

function TServiceC.GetServiceB: IServiceB;
begin
  Result := fServiceB;
end;

initialization

TDUnitX.RegisterTestFixture(TTestContainer);

end.
