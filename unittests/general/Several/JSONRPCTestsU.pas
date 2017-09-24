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

unit JSONRPCTestsU;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestJSONRPC = class(TObject)
  public
    [Test]
    procedure TestSimpleRequestMessage;
  end;

implementation

{ TJSONRPCTest }

uses MVCFramework.JSONRPC;

procedure TTestJSONRPC.TestSimpleRequestMessage;
var
  lReq: TMVCJSONRPCRequest;
begin
  lReq := TMVCJSONRPCRequest.LoadFromString('{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}');
  try
    Assert.AreEqual(1, lReq.ID);
    Assert.AreEqual(42, lReq.Params.I[0]);
    Assert.AreEqual(23, lReq.Params.I[1]);
    Assert.AreEqual('subtract', lReq.Method);
    Assert.IsFalse(lReq.IsNotification);
  finally
    lReq.Free;
  end;

  lReq := TMVCJSONRPCRequest.LoadFromString('{"jsonrpc": "2.0", "method": "subtract", "id": 1}');
  try
    Assert.AreEqual(1, lReq.ID);
    Assert.IsNull(lReq.Params);
    Assert.AreEqual('subtract', lReq.Method);
    Assert.IsFalse(lReq.IsNotification);
  finally
    lReq.Free;
  end;

  lReq := TMVCJSONRPCRequest.LoadFromString('{"jsonrpc": "2.0", "method": "subtract"}');
  try
    Assert.IsTrue(lReq.IsNotification);
    Assert.IsNull(lReq.Params);
    Assert.AreEqual('subtract', lReq.Method);
  finally
    lReq.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestJSONRPC);

finalization

end.
