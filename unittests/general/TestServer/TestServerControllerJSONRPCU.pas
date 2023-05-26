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

unit TestServerControllerJSONRPCU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.JSONRPC, JsonDataObjects,
  BusinessObjectsU;

type
  TTestJSONRPCController = class(TMVCJSONRPCController)
  public
    [MVCInheritable]
    function Subtract(Value1, Value2: Int64): Integer;
    [MVCInheritable]
    procedure MyNotify;
    [MVCInheritable]
    function MyRequest: Boolean;
    [MVCInheritable]
    function Add(Value1, Value2, Value3: Int64): TJsonObject;
    [MVCInheritable]
    function GetListFromTo(aFrom, aTo: Int64): TJsonArray;
    [MVCInheritable]
    function MultiplyString(aString: string; Multiplier: Int64): string;

  end;

  [MVCJSONRPCAllowGET]
  TTestJSONRPCControllerWithGet = class(TTestJSONRPCController)

  end;

  TTestJSONRPCClass = class(TObject)
  public
    [MVCInheritable]
    function Subtract(Value1, Value2: Int64): Integer;
    [MVCInheritable]
    procedure MyNotify;
    [MVCInheritable]
    function Add(Value1, Value2, Value3: Int64): TJsonObject;
    [MVCInheritable]
    function GetListFromTo(aFrom, aTo: Int64): TJsonArray;
    [MVCInheritable]
    function MultiplyString(aString: string; Multiplier: Int64): string;
    [MVCInheritable]
    function AddTimeToDateTime(aDateTime: TDateTime; aTime: TTime): TDateTime;

    //exceptions
    [MVCInheritable]
    function DoError(MyObj: TPerson): TPerson;

    //objects support
    [MVCInheritable]
    function HandlingObjects(MyObj: TPerson): TPerson;


    //enums support
    [MVCInheritable]
    function ProcessEnums(Value1: TEnumTest; Value2: TEnumTest): TEnumTest;

    //sets support
    [MVCInheritable]
    function ProcessSets(Value1: TSetOfEnumTest; Value2: TEnumTest): TSetOfEnumTest;

    //records support
    [MVCInheritable]
    function GetSingleRecord: TSimpleRecord;
    [MVCInheritable]
    function GetArrayOfRecords: TArray<TSimpleRecord>;
    [MVCInheritable]
    function EchoSingleRecord(const SimpleRecord: TSimpleRecord): TSimpleRecord;
    [MVCInheritable]
    function GetSingleComplexRecord: TComplexRecord;
    [MVCInheritable]
    function EchoSingleComplexRecord(const ComplexRecord: TComplexRecord): TComplexRecord;
    [MVCInheritable]
    function EchoArrayOfRecords(const ComplexRecordArray: TComplexRecordArray): TComplexRecordArray;

    //issues
    [MVCInheritable]
    function GetTCustomer_ISSUE648: TCustomerIssue648;
  end;

  [MVCJSONRPCAllowGET]
  TTestJSONRPCClassWithGET = class(TTestJSONRPCClass)

  end;

  TTestJSONRPCHookClass = class(TObject)
  private
    fJSONReq: TJsonObject;
    fHistory: string;
    fJSONRPCKind: TJSONRPCRequestType;
  public
    procedure OnBeforeRoutingHook(const Context: TWebContext; const JSON: TJsonObject);
    procedure OnBeforeCallHook(const Context: TWebContext; const JSON: TJsonObject);
    procedure OnAfterCallHook(const Context: TWebContext; const JSON: TJsonObject);
    [MVCInheritable]
    function error_OnBeforeRoutingHook: Boolean;
    [MVCInheritable]
    function error_OnBeforeCallHook: Boolean;
    [MVCInheritable]
    function error_OnAfterCallHook: Boolean;
    [MVCInheritable]
    procedure Notif1;
    [MVCInheritable]
    procedure NotifWithError;
    [MVCInheritable]
    function Request1: string;
    [MVCInheritable]
    function RequestWithError: string;
  end;

  [MVCJSONRPCAllowGET]
  TTestJSONRPCHookClassWithGet = class(TTestJSONRPCHookClass)

  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils;

{ TTestJSONRPCController }

function TTestJSONRPCController.Add(Value1, Value2, Value3: Int64): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['res'] := Value1 + Value2 + Value3;
end;

function TTestJSONRPCController.GetListFromTo(aFrom, aTo: Int64): TJsonArray;
var
  I: Cardinal;
begin
  Result := TJsonArray.Create;
  for I := aFrom to aTo do
    Result.Add(I);
end;

function TTestJSONRPCClass.GetSingleComplexRecord: TComplexRecord;
begin
  Result := TComplexRecord.Create;
end;

function TTestJSONRPCClass.GetSingleRecord: TSimpleRecord;
begin
  Result := TSimpleRecord.Create;
end;

function TTestJSONRPCClass.GetTCustomer_ISSUE648: TCustomerIssue648;
begin
  Result.Id := 155;
  Result.Added := Now;
  Result.Name := 'Daniele Teti';
  Result.ExpirationDate := Now + 7;
  Result.MaxUpdateDate.Clear;
  Result.AppVersion.Clear;
  Result.Activated.Clear;
end;

function TTestJSONRPCClass.HandlingObjects(MyObj: TPerson): TPerson;
begin
  Result := TPerson.Create;
  Result.ID := MyObj.ID;
  Result.FirstName := MyObj.FirstName;
  Result.LastName := MyObj.LastName;
  Result.DOB := MyObj.DOB;
  Result.Married := MyObj.Married;
end;

function TTestJSONRPCController.MultiplyString(aString: string; Multiplier: Int64): string;
var
  I: Integer;
begin
  Result := aString;
  for I := 2 to Multiplier do
  begin
    Result := Result + aString;
  end;
end;

procedure TTestJSONRPCController.MyNotify;
begin
  // this is a notify with no parameters and no result code
  Self.ClassName;
end;

function TTestJSONRPCController.MyRequest: Boolean;
begin
  Result := True;
end;

function TTestJSONRPCController.Subtract(Value1, Value2: Int64): Integer;
begin
  Result := Value1 - Value2;
end;

{ TTestJSONRPCClass }

function TTestJSONRPCClass.Add(Value1, Value2, Value3: Int64): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.I['res'] := Value1 + Value2 + Value3;
end;

function TTestJSONRPCClass.AddTimeToDateTime(aDateTime: TDateTime; aTime: TTime): TDateTime;
begin
  Result := aDateTime + aTime;
end;

function TTestJSONRPCClass.DoError(MyObj: TPerson): TPerson;
begin
  raise Exception.Create('BOOOM!! (TTestJSONRPCClass.DoError)');
end;

function TTestJSONRPCClass.EchoArrayOfRecords(
  const ComplexRecordArray: TComplexRecordArray): TComplexRecordArray;
begin
  Result := ComplexRecordArray;
end;

function TTestJSONRPCClass.EchoSingleComplexRecord(
  const ComplexRecord: TComplexRecord): TComplexRecord;
begin
  Result := ComplexRecord;
end;

function TTestJSONRPCClass.EchoSingleRecord(
  const SimpleRecord: TSimpleRecord): TSimpleRecord;
begin
  Result := SimpleRecord;
end;

function TTestJSONRPCClass.GetArrayOfRecords: TArray<TSimpleRecord>;
begin
  SetLength(Result, 3);
  Result[0] := TSimpleRecord.Create;
  Result[1] := TSimpleRecord.Create;
  Result[2] := TSimpleRecord.Create;

  Result[0].IntegerProperty := 0;
  Result[1].IntegerProperty := 1;
  Result[2].IntegerProperty := 2;
end;

function TTestJSONRPCClass.GetListFromTo(aFrom, aTo: Int64): TJsonArray;
var
  I: Cardinal;
begin
  Result := TJsonArray.Create;
  for I := aFrom to aTo do
    Result.Add(I);
end;

function TTestJSONRPCClass.MultiplyString(aString: string; Multiplier: Int64): string;
var
  I: Integer;
begin
  Result := aString;
  for I := 2 to Multiplier do
  begin
    Result := Result + aString;
  end;
end;

procedure TTestJSONRPCClass.MyNotify;
begin
  // this is a notify with no parameters and no result code
  Self.ClassName;
end;

function TTestJSONRPCClass.ProcessEnums(Value1, Value2: TEnumTest): TEnumTest;
begin
  Result := TEnumTest((Ord(Value1) + Ord(Value2)) mod 3);
end;

function TTestJSONRPCClass.ProcessSets(Value1: TSetOfEnumTest;
  Value2: TEnumTest): TSetOfEnumTest;
begin
  Include(Value1, Value2);
  Result := Value1;
end;

function TTestJSONRPCClass.Subtract(Value1, Value2: Int64): Integer;
begin
  Result := Value1 - Value2;
end;

{ TTestJSONRPCHookClass }

function TTestJSONRPCHookClass.error_OnAfterCallHook: Boolean;
begin
  // do nothing
  Result := True;
end;

function TTestJSONRPCHookClass.error_OnBeforeCallHook: Boolean;
begin
  // do nothing
  Result := True;
end;

function TTestJSONRPCHookClass.error_OnBeforeRoutingHook: Boolean;
begin
  // do nothing
  Result := True;
end;

procedure TTestJSONRPCHookClass.Notif1;
begin
  // do nothing
end;

procedure TTestJSONRPCHookClass.NotifWithError;
begin
  raise Exception.Create('BOOM NOTIF');
end;

procedure TTestJSONRPCHookClass.OnAfterCallHook(const Context: TWebContext; const JSON: TJsonObject);
begin
  try
    if SameText(fJSONReq.S['method'], 'error_OnAfterCallHook') then
      raise Exception.Create('error_OnAfterCallHook');

    fHistory := fHistory + '|OnAfterCallHook';

    // do nothing
    if fJSONRPCKind = TJSONRPCRequestType.Request then
    begin
      Assert(Assigned(JSON));
      LogD('TTestJSONRPCHookClass.OnAfterCallHook: ' + JSON.ToJSON());
    end
    else
    begin
      if Assigned(JSON) then
        Assert(JSON.Contains('error'), 'ERROR! Notification has a response but is not an error');
      LogD('TTestJSONRPCHookClass.OnAfterCallHook: Param is nil');
    end;
    if Assigned(JSON) then
      if JSON.Contains('error') then
        fHistory := fHistory + '|error';
    Context.Response.CustomHeaders.Values['x-history'] := fHistory;
  finally
    FreeAndNil(fJSONReq);
  end;
end;

procedure TTestJSONRPCHookClass.OnBeforeCallHook(const Context: TWebContext; const JSON: TJsonObject);
begin
  if SameText(JSON.S['method'], 'error_OnBeforeCallHook') then
    raise Exception.Create('error_OnBeforeCallHook');

  fHistory := fHistory + '|OnBeforeCallHook';
  Assert(Assigned(JSON), 'JSON not assigned in OnBeforeCallHook');
  LogD('TTestJSONRPCHookClass.OnBeforeCallHook: ' + JSON.ToJSON());
end;

procedure TTestJSONRPCHookClass.OnBeforeRoutingHook(const Context: TWebContext; const JSON: TJsonObject);
begin
  fJSONReq := JSON.Clone as TJsonObject;

  if SameText(JSON.S['method'], 'error_OnBeforeRoutingHook') then
    raise Exception.Create('error_OnBeforeRoutingHook');

  fHistory := 'OnBeforeRoutingHook';
  // do nothing
  Assert(Assigned(JSON), 'JSON not assigned in OnBeforeRoutingHook');
  LogD('TTestJSONRPCHookClass.OnBeforeRoutingHook: ' + JSON.ToJSON());
  if JSON.Contains('id') then
    fJSONRPCKind := TJSONRPCRequestType.Request
  else
    fJSONRPCKind := TJSONRPCRequestType.Notification;
end;

function TTestJSONRPCHookClass.Request1: string;
begin
  Result := 'empty';
end;

function TTestJSONRPCHookClass.RequestWithError: string;
begin
  raise Exception.Create('BOOM REQUEST');
end;

end.
