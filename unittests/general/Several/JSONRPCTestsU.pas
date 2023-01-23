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

unit JSONRPCTestsU;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestJSONRPC = class(TObject)
  public
    [Test]
    procedure TestRequestWithArrayParameters;
    [Test]
    procedure TestRequestWithNamedParameters;
    [Test]
    procedure TestRequestWithMixedParamaters;
    [Test]
    procedure TestRequestWithNoParameters;
    [Test]
    procedure TestRequestWithMalformedJSON;
    [Test]
    procedure TestNotificationWithNoParameters;
    [Test]
    procedure TestIssue595;
  end;



implementation

{ TJSONRPCTest }

uses MVCFramework.JSONRPC, BusinessObjectsU, System.SysUtils;

procedure TTestJSONRPC.TestIssue595;
begin
  Assert.WillRaise(
    procedure
    begin
      raise EMVCJSONRPCRemoteException.Create(100, 'ErrMessage');
    end,
    EMVCJSONRPCRemoteException);
end;

procedure TTestJSONRPC.TestNotificationWithNoParameters;
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create; // FromString('{"jsonrpc": "2.0", "method": "subtract"}');
  lReq.Method := 'subtract';
  Assert.AreEqual(TJSONRPCRequestType.Notification, lReq.RequestType);
  Assert.AreEqual(0, lReq.Params.Count);
  Assert.AreEqual('subtract', lReq.Method);
end;

procedure TTestJSONRPC.TestRequestWithArrayParameters;
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.Params.Add(42);
  lReq.Params.Add(23);
  lReq.RequestID := 1;
  Assert.AreEqual(1, lReq.RequestID.AsInteger);
  Assert.AreEqual(42, lReq.Params[0].AsInteger);
  Assert.AreEqual(23, lReq.Params[1].AsInteger);
  Assert.AreEqual('subtract', lReq.Method);
end;

procedure TTestJSONRPC.TestRequestWithMalformedJSON;
begin
  Assert.WillRaise(
    procedure
    var
      lReq: IJSONRPCRequest;
    begin
      lReq := TJSONRPCRequest.Create;
      lReq.AsJSONString := '{"jsonrpc": "2.0", this is wrong}';
    end, EMVCJSONRPCParseError);
end;

procedure TTestJSONRPC.TestRequestWithMixedParamaters;
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  Assert.WillRaise(
    procedure
    begin
      lReq.Params.AddByName('par1', 42);
      lReq.Params.Add(42);
    end, EMVCJSONRPCException);

  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  Assert.WillRaise(
    procedure
    begin
      lReq.Params.Add(42);
      lReq.Params.AddByName('par1', 42);
    end, EMVCJSONRPCException);
end;

procedure TTestJSONRPC.TestRequestWithNamedParameters;
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.Params.AddByName('par1', 42);
  lReq.Params.AddByName('PAR2', 23);
  lReq.RequestID := 1;
  Assert.AreEqual(1, lReq.RequestID.AsInteger);
  Assert.AreEqual('par1', lReq.Params.ItemsName[0]);
  Assert.AreEqual('par2', lReq.Params.ItemsName[1]);
  Assert.AreEqual('subtract', lReq.Method);
end;

procedure TTestJSONRPC.TestRequestWithNoParameters;
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.AsJSONString := '{"jsonrpc": "2.0", "method": "subtract", "id": 1}';
  Assert.AreEqual(1, lReq.RequestID.AsInteger);
  Assert.AreEqual(0, lReq.Params.Count);
  Assert.AreEqual('subtract', lReq.Method);
  Assert.AreEqual(TJSONRPCRequestType.Request, lReq.RequestType);
end;

initialization

TDUnitX.RegisterTestFixture(TTestJSONRPC);

finalization

end.
