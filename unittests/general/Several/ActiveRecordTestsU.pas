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
  DUnitX.TestFramework, FireDAC.Comp.Client, FireDAC.ConsoleUI.Wait, FireDAC.VCLUI.Wait,
  PGUtilsU;

const
  PG_PORT = 5555;

type
  TTestActiveRecordBase = class(TObject)
  protected
    fConnection: TFDConnection;
    fConDefName: string;
    procedure CreatePrivateConnDef(AIsPooled: boolean); virtual; abstract;
    procedure LoadData; virtual;
    procedure AfterDataLoad; virtual; abstract;
    procedure InternalSetupFixture; virtual;
  public
    [SetupFixture]
    procedure SetupFixturePG;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestCRUD;
    [Test]
    procedure TestCRUDWithTableChange;
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
    procedure TestRQLWithDateTime;
    [Test]
    procedure TestRQLLimit;
    [Test]
    procedure TestIssue424;
    [Test]
    procedure TestMultiThreading;
    [Test]
    procedure TestNullables;
  end;

  [TestFixture]
  TTestActiveRecordSQLite = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup; virtual;
  end;

  [TestFixture]
  TTestActiveRecordFirebird = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup;
  end;

  [TestFixture]
  TTestActiveRecordPostgreSQL = class(TTestActiveRecordBase)
  private
    fPGUtil: TPGUtil;
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
    procedure InternalSetupFixture; override;
  public
    [TearDownFixture]
    procedure TearDownFixture;
    [Setup]
    procedure Setup;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes, System.IOUtils, BOs, MVCFramework.ActiveRecord,
  System.SysUtils, System.Threading, System.Generics.Collections, Data.DB,
  FireDAC.Stan.Intf, ShellAPI, Winapi.Windows;

const
  _CON_DEF_NAME_SQLITE = 'SQLITECONNECTION';
  _CON_DEF_NAME_FIREBIRD = 'FIREBIRDCONNECTION';
  _CON_DEF_NAME_POSTGRESQL = 'POSTGRESQLCONNECTION';

var
  GDBFileName: string = '';
  SQLiteFileName: string = 'sqlitetest.db';
  GDBTemplateFileName: string = '';
  GPGIsInitialized: boolean = false;

procedure TTestActiveRecordSQLite.AfterDataLoad;
begin
  { TODO -oDanieleT -cGeneral : Hot to reset a sqlite autoincrement field? }
  // https://sqlite.org/fileformat2.html#seqtab
  // https://stackoverflow.com/questions/5586269/how-can-i-reset-a-autoincrement-sequence-number-in-sqlite/14298431
  // TMVCActiveRecord.CurrentConnection.ExecSQL('delete from sqlite_sequence where name=''customers''');
  // TMVCActiveRecord.CurrentConnection.ExecSQL('delete from sqlite_sequence where name=''customers2''');
  TMVCActiveRecord.CurrentConnection.ExecSQL('drop table if exists sqlite_sequence');
end;

procedure TTestActiveRecordSQLite.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    SQLiteFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), SQLiteFileName);
    LParams.Add('Database=' + SQLiteFileName);
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
    FDManager.AddConnectionDef(fConDefName, 'SQLite', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordBase.TestCRUD;
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
    lCustomer.CreationTime := Time;
    lCustomer.CreationDate := Date;
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
    Assert.IsTrue(lCustomer.CreationTime.HasValue);
    Assert.IsTrue(lCustomer.CreationDate.HasValue);
    lCustomer.Code := '1234';
    lCustomer.Rating := 3;
    lCustomer.Note := lCustomer.Note + 'noteupdated';
    lCustomer.CreationTime.Clear;
    lCustomer.CreationDate.Clear;
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
    Assert.IsFalse(lCustomer.CreationTime.HasValue);
    Assert.IsFalse(lCustomer.CreationDate.HasValue);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    lCustomer.Delete;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, false);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], false);
  Assert.IsNull(lCustomer);

end;

procedure TTestActiveRecordBase.TestCRUDStringPK;
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000', false);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithCode>('code = ?', ['1000'],
    [ftString], false);
  Assert.IsNull(lCustomer);
end;

procedure TTestActiveRecordBase.TestCRUDWithTableChange;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>());
  AfterDataLoad;
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.CreationTime := Time;
    lCustomer.CreationDate := Date;
    lCustomer.ID := -1; { don't be fooled by the default! }
    lCustomer.Insert;
    lID := lCustomer.ID;
    Assert.AreEqual(1, lID);
  finally
    lCustomer.Free;
  end;

  // the same changing tablename

  lCustomer := TCustomer.Create;
  try
    Assert.AreEqual('customers', lCustomer.TableName);
    lCustomer.TableName := 'customers2';
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.CreationTime := Time;
    lCustomer.CreationDate := Date;
    lCustomer.ID := -1; { don't be fooled by the default! }
    lCustomer.Insert;
    lID := lCustomer.ID;
    Assert.AreEqual(1, lID);
    Assert.IsTrue(lCustomer.LoadByPK(lID));
  finally
    lCustomer.Free;
  end;
end;

{ https://github.com/danieleteti/delphimvcframework/issues/424 }
procedure TTestActiveRecordBase.TestIssue424;
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
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 20);
  try
    Assert.AreEqual(20, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 1);
  try
    Assert.AreEqual(1, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, -1);
  try
    Assert.AreEqual(240, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestLifeCycle;
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
    Assert.AreEqual
      ('OnValidation|OnBeforeDelete|OnBeforeExecuteSQL|MapObjectToParams|OnAfterDelete',
      lCustomer.GetHistory);
  finally
    lCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMultiThreading;
begin
  LoadData;
  Assert.AreEqual(Trunc(20 * 30), TMVCActiveRecord.Count(TCustomerWithLF));
end;

procedure TTestActiveRecordBase.TestNullables;
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

  Assert.IsNull(TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = 4', [], false));

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
    // lTest.f_currency := 1234567890.1234;
    lTest.Insert;
  finally
    lTest.Free;
  end;
end;

procedure TTestActiveRecordBase.TestRQL;
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

procedure TTestActiveRecordBase.TestRQLLimit;
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

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 10);
  try
    Assert.AreEqual(10, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 0);
  try
    Assert.AreEqual(0, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  TMVCActiveRecord.DeleteRQL(TCustomer, RQL1);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>(RQL1));
end;

procedure TTestActiveRecordBase.TestRQLWithDateTime;
var
  lCustomers: TObjectList<TCustomer>;
const
  RQL1 = 'and(and(gt(CreationDate, "2010-10-01"),le(CreationDate, "2022-12-31")),' +
    'and(gt(CreationTime, "00:00:00"),le(CreationTime, "08:00:00")))';
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TCustomer));
  LoadData;
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, MAXINT);
  try
    Assert.AreEqual(140, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
  TMVCActiveRecord.DeleteRQL(TCustomer, RQL1);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>(RQL1));
end;

procedure TTestActiveRecordBase.TestSelectWithExceptions;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
  lID := 1000;
  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, false);
  try
    if Assigned(lCustomer) then
    begin
      lCustomer.Delete;
    end;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, false);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], false);
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

procedure TTestActiveRecordBase.TestStore;
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
    Assert.AreEqual(1, lID, 'ID should be 1 but it is ' + lID.ToString);
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

procedure TTestActiveRecordBase.InternalSetupFixture;
begin
  // do nothing
end;

procedure TTestActiveRecordBase.LoadData;
var
  lTasks: TArray<ITask>;
  lProc: TProc;
const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');
begin
  TMVCActiveRecord.DeleteRQL(TCustomer,
    'in(City,["Rome","New York","London","Melbourne","Berlin"])');
  lProc := procedure
    var
      lCustomer: TCustomer;
      I: Integer;
    begin
      ActiveRecordConnectionsRegistry.AddDefaultConnection(TFDConnection.Create(nil), True);
      try
        ActiveRecordConnectionsRegistry.GetCurrent.ConnectionDefName := fConDefName;
        for I := 1 to 30 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
            lCustomer.City := Cities[I mod Length(Cities)];
            lCustomer.CompanyName :=
              Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
              CompanySuffix[Random(high(CompanySuffix) + 1)]]);
            lCustomer.Note := Stuff[I mod Length(Stuff)];
            lCustomer.CreationTime := EncodeTime(I mod 23, I, 60 - 1, 0);
            lCustomer.CreationDate := EncodeDate(2020 - I, (I mod 12) + 1, (I mod 27) + 1);
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveDefaultConnection;
      end;
    end;
  AfterDataLoad;

  lTasks := [TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
    TTask.Run(lProc)];
  TTask.WaitForAll(lTasks);
end;

procedure TTestActiveRecordBase.SetupFixturePG;
begin
  InternalSetupFixture;
end;

procedure TTestActiveRecordSQLite.Setup;
begin
  fConDefName := _CON_DEF_NAME_SQLITE;
  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;

  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
    if TFile.Exists(SQLiteFileName) then
    begin
      TFile.Delete(SQLiteFileName);
    end;

    fConnection.Open;
    for var lSQL in SQLs_SQLITE do
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
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('delete from customers2');
  AfterDataLoad;
end;

procedure TTestActiveRecordBase.Teardown;
begin
  ActiveRecordConnectionsRegistry.RemoveDefaultConnection();
  fConnection.Close;
  FreeAndNil(fConnection);
end;

{ TTestActiveRecordFirebird }

procedure TTestActiveRecordFirebird.AfterDataLoad;
begin
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers alter column id restart');
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers2 alter column id restart');
end;

procedure TTestActiveRecordFirebird.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
  lDriver: IFDStanDefinition;
begin
  lDriver := FDManager.DriverDefs.Add;
  lDriver.Name := 'FBEMBEDDED';
  lDriver.AsString['BaseDriverID'] := 'FB';
  lDriver.AsString['DriverID'] := 'FBEMBEDDED';
  lDriver.AsString['VendorLib'] := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
    'firebird\fbclient.dll');
  lDriver.Apply;

  LParams := TStringList.Create;
  try
    GDBFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebirdtest.fdb');
    GDBTemplateFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)),
      'firebirdtest_template.fdb');
    LParams.Add('Database=' + GDBFileName);
    LParams.Add('user_name=sysdba');
    LParams.Add('password=masterkey');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(fConDefName, 'FBEMBEDDED', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordFirebird.Setup;
begin
  fConDefName := _CON_DEF_NAME_FIREBIRD;
  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;

  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
    if TFile.Exists(GDBFileName) then
    begin
      TFile.Delete(GDBFileName);
    end;

    TFile.Copy(GDBTemplateFileName, GDBFileName);

    fConnection.Open;
    for var lSQL in SQLs_FIREBIRD do
    begin
      fConnection.ExecSQL(lSQL);
    end;
  end
  else
  begin
    fConnection.Open;
  end;
  fConnection.Close;
  fConnection.Open;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(fConnection);
  TMVCActiveRecord.DeleteAll(TCustomer);
  TMVCActiveRecord.CurrentConnection.ExecSQL('delete from customers2');
  AfterDataLoad;
end;

{ TTestActiveRecordPostgreSQL }

procedure TTestActiveRecordPostgreSQL.AfterDataLoad;
begin
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers alter column id restart');
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers2 alter column id restart');
end;

constructor TTestActiveRecordPostgreSQL.Create;
var
  lPGHome, lDataDir: String;
begin
  inherited;
  lPGHome := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'pgsql');
  lDataDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'pgsql\testdatadir');
  fPGUtil := TPGUtil.Create(lPGHome, lDataDir, PG_PORT);
end;

procedure TTestActiveRecordPostgreSQL.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
  lDriver: IFDStanDefinition;
begin
  lDriver := FDManager.DriverDefs.Add;
  lDriver.Name := 'PG';
  // lDriver.AsString['BaseDriverID'] := 'PG';
  lDriver.AsString['DriverID'] := 'PG';
  lDriver.AsString['VendorLib'] := TPath.Combine(fPGUtil.PGHome, 'libpq.dll');
  lDriver.Apply;

  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecordtest');
    LParams.Add('Port=' + PG_PORT.ToString);
    // LParams.Add('user_name=sysdba');
    // LParams.Add('password=masterkey');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(fConDefName, 'PG', LParams);
  finally
    LParams.Free;
  end;
end;

destructor TTestActiveRecordPostgreSQL.Destroy;
begin
  try
    fPGUtil.StopPG;
  except
    // do nothing
  end;
  fPGUtil.Free;
  inherited;
end;

procedure TTestActiveRecordPostgreSQL.InternalSetupFixture;
begin
  fPGUtil.RemoveDataDir;
  fPGUtil.InitDB;
  fPGUtil.StartPG;
  fPGUtil.CreateDatabase('activerecordtest');
end;

procedure TTestActiveRecordPostgreSQL.Setup;
var
  lInitDBStructure: boolean;
begin
  lInitDBStructure := false;

  if not GPGIsInitialized then
  begin
    FDManager.CloseConnectionDef(_CON_DEF_NAME_POSTGRESQL);
    fPGUtil.StopPG;
    fPGUtil.RemoveDataDir;
    lInitDBStructure := True;
    InternalSetupFixture;
    GPGIsInitialized := True;
  end;

  fConDefName := _CON_DEF_NAME_POSTGRESQL;
  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
  end;

  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;
  fConnection.Open;
  if lInitDBStructure then
  begin
    for var lSQL in SQLs_POSTGRESQL do
    begin
      fConnection.ExecSQL(lSQL);
    end;
  end;

  fConnection.Close;
  fConnection.Open;

  ActiveRecordConnectionsRegistry.AddDefaultConnection(fConnection);
  TMVCActiveRecord.DeleteAll(TCustomer);
  TMVCActiveRecord.CurrentConnection.ExecSQL('delete from customers2');
  AfterDataLoad;
end;

procedure TTestActiveRecordPostgreSQL.TearDownFixture;
begin
  FDManager.CloseConnectionDef(_CON_DEF_NAME_POSTGRESQL);
  fPGUtil.StopPG;
  GPGIsInitialized := false;
end;

initialization

TDUnitX.RegisterTestFixture(TTestActiveRecordSQLite);
TDUnitX.RegisterTestFixture(TTestActiveRecordFirebird);
TDUnitX.RegisterTestFixture(TTestActiveRecordPostgreSQL);

finalization

end.
