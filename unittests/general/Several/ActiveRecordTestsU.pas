// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit ActiveRecordTestsU;

interface

uses
  DUnitX.TestFramework, FireDAC.Comp.Client, FireDAC.ConsoleUI.Wait, FireDAC.VCLUI.Wait;

type

  [TestFixture]
  TTestActiveRecord = class(TObject)
  private
    fConnection: TFDConnection;
    procedure LoadData;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestCRUD;
    [Test]
    procedure TestCRUDStringPK;
    [Test]
    procedure TestSelectWithExceptions;
    [Test]
    procedure TestStore;
    [Test]
    procedure TestLifeCycle;
    [Test]
    procedure TestRQL;
    [Test]
    procedure TestMultiThreading;
    [Test]
    procedure TestNullables;
  end;

implementation

uses
  System.Classes, System.IOUtils, BOs, MVCFramework.ActiveRecord,
  System.SysUtils, System.Threading, System.Generics.Collections, Data.DB;

const
  CON_DEF_NAME = 'SQLITECONNECTION';

var
  GDBFileName: string = '';

procedure CreateSqlitePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    GDBFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'sqlitetest.db');
    LParams.Add('Database=' + GDBFileName);
    LParams.Add('OpenMode=CreateUTF8');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(CON_DEF_NAME, 'SQLite', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecord.TestCRUD;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>());
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.ID := -1; { don't be fooled by the default! }
    lCustomer.Insert;
    lID := lCustomer.ID;
    Assert.AreEqual(1, lID);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert.IsFalse(lCustomer.Code.HasValue);
    Assert.IsFalse(lCustomer.Rating.HasValue);
    lCustomer.Code := '1234';
    lCustomer.Rating := 3;
    lCustomer.Note := lCustomer.Note + 'noteupdated';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert.AreEqual('1234', lCustomer.Code.Value);
    Assert.AreEqual(3, lCustomer.Rating.Value);
    Assert.AreEqual('note1noteupdated', lCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.Value);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.AreEqual(1, lCustomer.ID);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    lCustomer.Delete;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], False);
  Assert.IsNull(lCustomer);

end;

procedure TTestActiveRecord.TestCRUDStringPK;
var
  lCustomer: TCustomerWithCode;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomerWithCode>());
  lCustomer := TCustomerWithCode.Create;
  try
    lCustomer.Code := '1000';
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.Insert;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000');
  try
    Assert.IsFalse(lCustomer.Rating.HasValue);
    lCustomer.Rating := 3;
    lCustomer.Note := lCustomer.Note + 'noteupdated';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000');
  try
    Assert.AreEqual('1000', lCustomer.Code);
    Assert.AreEqual(3, lCustomer.Rating.Value);
    Assert.AreEqual('note1noteupdated', lCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.Value);
    Assert.AreEqual('Rome, IT', lCustomer.City);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000');
  try
    lCustomer.Delete;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000', False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithCode>('code = ?', ['1000'], [ftString], False);
  Assert.IsNull(lCustomer);
end;

procedure TTestActiveRecord.TestLifeCycle;
var
  lCustomer: TCustomerWithLF;
  lID: Integer;
begin
  lCustomer := TCustomerWithLF.Create;
  try
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.Insert;
    Assert.AreEqual
      ('OnValidation|OnBeforeInsert|OnBeforeInsertOrUpdate|OnBeforeExecuteSQL|MapObjectToParams|OnAfterInsert|OnAfterInsertOrUpdate',
      lCustomer.GetHistory);
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithLF>(lID);
  try
    Assert.AreEqual('OnBeforeLoad|MapDatasetToObject|OnAfterLoad', lCustomer.GetHistory);
    lCustomer.ClearHistory;
    lCustomer.City := 'XXX';
    lCustomer.Update;
    Assert.AreEqual
      ('OnValidation|OnBeforeUpdate|OnBeforeInsertOrUpdate|OnBeforeExecuteSQL|MapObjectToParams|OnAfterUpdate|OnAfterInsertOrUpdate',
      lCustomer.GetHistory);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithLF>('id = ?', [lID]);
  try
    Assert.AreEqual('OnBeforeLoad|MapDatasetToObject|OnAfterLoad', lCustomer.GetHistory);
    lCustomer.ClearHistory;
    lCustomer.Delete;
    Assert.AreEqual('OnValidation|OnBeforeDelete|OnBeforeExecuteSQL|MapObjectToParams|OnAfterDelete',
      lCustomer.GetHistory);
  finally
    lCustomer.Free;
  end;
end;

procedure TTestActiveRecord.TestMultiThreading;
begin
  LoadData;
  Assert.AreEqual(Trunc(20 * 30), TMVCActiveRecord.Count(TCustomerWithLF));
end;

procedure TTestActiveRecord.TestNullables;
var
  lTest: TNullablesTest;
begin
  TMVCActiveRecord.DeleteAll(TNullablesTest);

  lTest := TNullablesTest.Create();
  try
    lTest.f_int2 := 2;
    lTest.f_int4 := 4;
    lTest.f_int8 := 8;
    lTest.f_blob := TStringStream.Create('Hello World');
    lTest.Insert;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [2]);
  try
    Assert.IsTrue(lTest.f_int2.HasValue);
    Assert.IsTrue(lTest.f_int4.HasValue);
    Assert.IsTrue(lTest.f_int8.HasValue);
    Assert.IsFalse(lTest.f_string.HasValue);
    Assert.IsFalse(lTest.f_bool.HasValue);
    Assert.IsFalse(lTest.f_date.HasValue);
    Assert.IsFalse(lTest.f_time.HasValue);
    Assert.IsFalse(lTest.f_datetime.HasValue);
    Assert.IsFalse(lTest.f_float4.HasValue);
    Assert.IsFalse(lTest.f_float8.HasValue);
    Assert.IsFalse(lTest.f_bool.HasValue);
    Assert.IsNotNull(lTest);
    lTest.f_int2 := lTest.f_int2.Value + 2;
    lTest.f_int4 := lTest.f_int4.Value + 4;
    lTest.f_int8 := lTest.f_int8.Value + 8;
    lTest.f_blob.Free;
    lTest.f_blob := nil;
    lTest.Update;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [4]);
  try
    Assert.IsTrue(lTest.f_int2.ValueOrDefault = 4);
    Assert.IsTrue(lTest.f_int4.ValueOrDefault = 8);
    Assert.IsTrue(lTest.f_int8.ValueOrDefault = 16);
    Assert.IsFalse(lTest.f_string.HasValue);
    Assert.IsFalse(lTest.f_bool.HasValue);
    Assert.IsFalse(lTest.f_date.HasValue);
    Assert.IsFalse(lTest.f_time.HasValue);
    Assert.IsFalse(lTest.f_datetime.HasValue);
    Assert.IsFalse(lTest.f_float4.HasValue);
    Assert.IsFalse(lTest.f_float8.HasValue);
    Assert.IsFalse(lTest.f_bool.HasValue);
    Assert.IsFalse(Assigned(lTest.f_blob), 'Blob contains a value when should not');
    TMVCActiveRecord.DeleteRQL(TNullablesTest, 'eq(f_int2,4)');
  finally
    lTest.Free;
  end;

  Assert.IsNull(TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = 4', [], False));

  lTest := TNullablesTest.Create;
  try
    lTest.f_int2 := 2;
    lTest.f_int4 := 4;
    lTest.f_int8 := 8;
    lTest.f_string := 'Hello World';
    lTest.f_bool := True;
    lTest.f_date := EncodeDate(2020, 02, 01);
    lTest.f_time := EncodeTime(12, 24, 36, 0);
    lTest.f_datetime := Now;
    lTest.f_float4 := 1234.5678;
    lTest.f_float8 := 12345678901234567890.0123456789;
    lTest.f_currency := 1234567890.1234;
    lTest.Insert;
  finally
    lTest.Free;
  end;
end;

procedure TTestActiveRecord.TestRQL;
var
  lCustomers: TObjectList<TCustomer>;
const
  RQL1 = 'or(eq(City, "Rome"),eq(City, "London"))';
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TCustomer));
  LoadData;
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, MAXINT);
  try
    Assert.AreEqual(240, lCustomers.Count);
    for var lCustomer in lCustomers do
    begin
      Assert.IsMatch('^(Rome|London)$', lCustomer.City);
    end;
  finally
    lCustomers.Free;
  end;
  TMVCActiveRecord.DeleteRQL(TCustomer, RQL1);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>(RQL1));
end;

procedure TTestActiveRecord.TestSelectWithExceptions;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  lID := 1000;
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, False);
  try
    if Assigned(lCustomer) then
    begin
      lCustomer.Delete;
    end;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], False);
  Assert.IsNull(lCustomer);

  Assert.WillRaise(
    procedure
    begin
      TMVCActiveRecord.GetByPK<TCustomer>(lID, True);
    end, EMVCActiveRecordNotFound);

  Assert.WillRaise(
    procedure
    begin
      TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], True);
    end, EMVCActiveRecordNotFound);

  Assert.WillRaise(
    procedure
    begin
      TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], True);
    end, EMVCActiveRecordNotFound);

  Assert.WillRaise(
    procedure
    begin
      TMVCActiveRecord.GetFirstByWhere<TCustomer>('id = ?', [lID], [ftInteger], True);
    end, EMVCActiveRecordNotFound);

  Assert.WillRaise(
    procedure
    begin
      TMVCActiveRecord.GetFirstByWhere<TCustomer>('id = ?', [lID], True);
    end, EMVCActiveRecordNotFound);

end;

procedure TTestActiveRecord.TestStore;
var
  lCustomer: TCustomerWithNullablePK;
  lID: Integer;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomerWithNullablePK>());
  lCustomer := TCustomerWithNullablePK.Create;
  try
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.Store; { pk is not set, so it should do an insert }
    lID := lCustomer.ID;
    Assert.AreEqual(1, lID);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithNullablePK>(lID);
  try
    Assert.IsFalse(lCustomer.Code.HasValue);
    Assert.IsFalse(lCustomer.Rating.HasValue);
    lCustomer.Code := '1234';
    lCustomer.Rating := 3;
    lCustomer.Note := lCustomer.Note + 'noteupdated';
    lCustomer.Store; { pk is set, so it should do an update }
    Assert.AreEqual<Int64>(1, lCustomer.ID.Value);
  finally
    lCustomer.Free;
  end;

end;

procedure TTestActiveRecord.LoadData;
var
  lTasks: TArray<ITask>;
  lProc: TProc;
const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');
begin
  TMVCActiveRecord.DeleteRQL(TCustomer, 'in(City,["Rome","New York","London","Melbourne","Berlin"])');
  lProc := procedure
    var
      lCustomer: TCustomer;
      I: Integer;
    begin
      ActiveRecordConnectionsRegistry.AddDefaultConnection(TFDConnection.Create(nil), True);
      try
        ActiveRecordConnectionsRegistry.GetCurrent.ConnectionDefName := CON_DEF_NAME;
        for I := 1 to 30 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
            lCustomer.City := Cities[I mod Length(Cities)];
            lCustomer.CompanyName := Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
              CompanySuffix[Random(high(CompanySuffix) + 1)]]);
            lCustomer.Note := Stuff[I mod Length(Stuff)];
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
      end;
    end;

  lTasks := [
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc),
    TTask.Run(lProc)];
  TTask.WaitForAll(lTasks);
end;

procedure TTestActiveRecord.Setup;
begin
  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := CON_DEF_NAME;

  if FDManager.ConnectionDefs.FindConnectionDef(CON_DEF_NAME) = nil then
  begin
    CreateSqlitePrivateConnDef(True);
    if TFile.Exists(GDBFileName) then
    begin
      TFile.Delete(GDBFileName);
    end;

    fConnection.Open;
    for var lSQL in SQLs do
    begin
      fConnection.ExecSQL(lSQL);
    end;
  end
  else
  begin
    fConnection.Open;
  end;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(fConnection);
  TMVCActiveRecord.DeleteAll(TCustomer);
end;

procedure TTestActiveRecord.Teardown;
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection();
  fConnection.Close;
  FreeAndNil(fConnection);
end;

initialization

TDUnitX.RegisterTestFixture(TTestActiveRecord);

finalization

end.
