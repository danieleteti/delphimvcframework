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
    procedure LoadData(const JustAFew: boolean = False); virtual;
    procedure AfterDataLoad; virtual; abstract;
    procedure InternalSetupFixture; virtual;
    function CreateACustomer(CompanyName: String; Rating: Integer): Integer; overload;
    function CreateACustomer(CompanyName: String; City: String; Rating: Integer): Integer; overload;
  public
    [SetupFixture]
    procedure SetupFixture;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestCRUD;
    [Test]
    procedure TestRefresh;
    [Test]
    procedure Test_ISSUE485;
    [Test]
    procedure TestDeleteIfNotFound;
    [Test]
    procedure TestUpdateIfNotFound;
    [Test]
    procedure TestCRUDWithSpaces;
    [Test]
    procedure TestCRUDWithGUID;
    [Test]
    procedure TestCRUDWithTableChange;
    [Test]
    procedure TestCRUDStringPK;
    [Test]
    procedure TestSelectWithExceptions;
    [Test]
    procedure TestNamedQuerySQL;
    [Test]
    procedure TestNamedQuerySQLByBackEnd;
    [Test]
    procedure TestStore;
    [Test]
    procedure TestLifeCycle;
    [Test]
    procedure TestRQL;
    [Test]
    procedure TestNamedQueryRQL;
    [Test]
    procedure TestNamedQueryRQLWithExceptions;
    [Test]
    procedure TestRQLWithMVCNameAsAttribute;
    [Test]
    procedure TestRQLWithBoolean;
    [Test]
    procedure TestRQLWithDateTime;
    [Test]
    procedure TestRQLWithGUID;
    [Test]
    procedure TestRQLLimit;
    [Test]
    procedure TestIssue424;
    [Test]
    procedure TestMultiThreading;
    [Test]
    procedure TestNullables;
    [Test]
    procedure TestMergeWhenNewRecords;
    [Test]
    procedure TestMergeWhenNewDeletedRecords;
    [Test]
    procedure TestMergeWhenChangedRecords;
    [Test]
    procedure TestMergeWhenMixedRecords;
    { default filtering }
    [Test]
    procedure TestDefaultFilteringSelectByRQL;
    [Test]
    procedure TestDefaultFilteringSelectOneByRQL;
    [Test]
    procedure TestDefaultFilteringCount;
    [Test]
    procedure TestDefaultFilteringCountByRQL;
    [Test]
    procedure TestDefaultFilteringDeleteByRQL;
    [Test]
    procedure TestDefaultFilteringDelete;
    [Test]
    procedure TestDefaultFilteringGetByPK;
    { partitioning }
    [Test]
    procedure TestPartitioningCRUD;
    [Test]
    procedure TestPartitioningSelectByWhere;
    [Test]
    procedure TestPartitioningSelectByRQL;
    [Test]
    procedure TestPartitioningSelectOneByRQL;
    [Test]
    procedure TestPartitioningCount;
    [Test]
    procedure TestPartitioningCountByRQL;
    [Test]
    procedure TestPartitioningDeleteByRQL;
    [Test]
    procedure TestPartitioningDelete;
    [Test]
    procedure TestPartitioningGetByPK;
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
  FireDAC.Stan.Intf, ShellAPI, Winapi.Windows, MVCFramework.Logger,
  MVCFramework.Nullables;

const
  _CON_DEF_NAME_SQLITE = 'SQLITECONNECTION';
  _CON_DEF_NAME_FIREBIRD = 'FIREBIRDCONNECTION';
  _CON_DEF_NAME_POSTGRESQL = 'POSTGRESQLCONNECTION';

var
  GDBFileName: string = '';
  SQLiteFileName: string = 'sqlitetest.db';
  GDBTemplateFileName: string = '';
  GPGIsInitialized: boolean = False;

procedure TTestActiveRecordSQLite.AfterDataLoad;
begin
  { TODO -oDanieleT -cGeneral : Hot to reset a sqlite autoincrement field? }
  // https://sqlite.org/fileformat2.html#seqtab
  // https://stackoverflow.com/questions/5586269/how-can-i-reset-a-autoincrement-sequence-number-in-sqlite/14298431
  TMVCActiveRecord.CurrentConnection.ExecSQL('delete from sqlite_sequence where name=''customers''');
  TMVCActiveRecord.CurrentConnection.ExecSQL('delete from sqlite_sequence where name=''customers2''');
  TMVCActiveRecord.CurrentConnection.ExecSQL('delete from sqlite_sequence where name=''customers with spaces''');
  // TMVCActiveRecord.CurrentConnection.ExecSQL('drop table if exists sqlite_sequence');
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
    Assert.AreEqual(1, lCustomer.ID.Value);
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID, False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomer>('id = ?', [lID], [ftInteger], False);
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithCode>('1000', False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithCode>('code = ?', ['1000'], [ftString], False);
  Assert.IsNull(lCustomer);
end;

procedure TTestActiveRecordBase.TestCRUDWithGUID;
var
  lCustomer: TCustomerWithGUID;
  lGUID: TGUID;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomerWithGUID>());
  lCustomer := TCustomerWithGUID.Create;
  try
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.City := 'Rome, IT';
    lCustomer.Note := 'note1';
    lCustomer.GUID := TGUID.NewGuid;
    lCustomer.OtherGUID := TGUID.NewGuid;
    lCustomer.Insert;
    lGUID := lCustomer.GUID;
    Assert.IsFalse(lGUID.IsEmpty);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lGUID);
  try
    Assert.IsFalse(lCustomer.Code.HasValue);
    Assert.IsFalse(lCustomer.Rating.HasValue);
    lCustomer.Code := '1234';
    lCustomer.Rating := 3;
    lCustomer.OtherGUID := TGUID.NewGuid;
    lCustomer.Note := lCustomer.Note + 'noteupdated';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lGUID);
  try
    Assert.AreEqual('1234', lCustomer.Code.Value);
    Assert.AreEqual(3, lCustomer.Rating.Value);
    Assert.AreEqual('note1noteupdated', lCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.Value);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.IsFalse(lCustomer.GUID.Value.IsEmpty);
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lGUID);
  try
    lCustomer.Delete;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithGUID>(lGUID, False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithGUID>('idguid = ?', [lGUID.ToString], [ftGuid], False);
  Assert.IsNull(lCustomer);
end;

procedure TTestActiveRecordBase.TestCRUDWithSpaces;
var
  lCustomer: TCustomerWithSpaces;
  lID: Integer;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomerWithSpaces>());
  lCustomer := TCustomerWithSpaces.Create;
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID);
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID);
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

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID);
  try
    lCustomer.Delete;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithSpaces>(lID, False);
  Assert.IsNull(lCustomer);

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithSpaces>('"id with spaces" = ?', [lID], [ftInteger], False);
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

procedure TTestActiveRecordBase.TestDefaultFilteringCount;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 1);
  CreateACustomer('Jack', 2);
  CreateACustomer('John', 3);
  CreateACustomer('Scott', 4);
  CreateACustomer('Bruce', 5);
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TGoodCustomer>);
  Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TBadCustomer>);
  TMVCActiveRecord.DeleteRQL(TBadCustomer, 'eq(rating,1)');
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TBadCustomer>);
  TMVCActiveRecord.DeleteRQL(TBadCustomer, 'eq(rating,2)');
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TBadCustomer>);
end;

procedure TTestActiveRecordBase.TestDefaultFilteringCountByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 1);
  CreateACustomer('Jack', 2);
  CreateACustomer('John', 3);
  CreateACustomer('Scott', 4);
  CreateACustomer('Bruce', 5);
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TGoodCustomer>('eq(CompanyName,"Scott")'));
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TBadCustomer>('eq(CompanyName,"Scott")'));
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TBadCustomer>('in(CompanyName,["Daniele","Scott"])'));
end;

procedure TTestActiveRecordBase.TestDefaultFilteringDeleteByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 1);
  CreateACustomer('Jack', 2);
  CreateACustomer('John', 3);
  CreateACustomer('Scott', 4);
  CreateACustomer('Bruce', 5);
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TGoodCustomer>);
  Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TBadCustomer>);
  TMVCActiveRecord.DeleteRQL(TBadCustomer, 'eq(rating,1)');
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TBadCustomer>);
  TMVCActiveRecord.DeleteRQL(TBadCustomer, 'eq(rating,2)');
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TBadCustomer>);
end;

procedure TTestActiveRecordBase.TestDefaultFilteringDelete;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 1);
  CreateACustomer('Jack', 2);
  CreateACustomer('John', 3);
  CreateACustomer('Scott', 4);
  CreateACustomer('Bruce', 5);
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TGoodCustomer>);
  Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TBadCustomer>);

  TMVCActiveRecord.DeleteAll(TGoodCustomer);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TGoodCustomer>);
  Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TBadCustomer>);

  TMVCActiveRecord.DeleteAll(TBadCustomer);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TGoodCustomer>);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TBadCustomer>);

  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>);
end;

procedure TTestActiveRecordBase.TestDefaultFilteringGetByPK;
var
  lIDBad, lIDGood: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  lIDBad := CreateACustomer('Daniele', 1);
  lIDGood := CreateACustomer('Jack', 5);

  var lAGoodCustomer := TMVCActiveRecord.GetByPK<TGoodCustomer>(lIDGood, False);
  try
    Assert.IsNotNull(lAGoodCustomer);
  finally
    lAGoodCustomer.Free;
  end;

  lAGoodCustomer := TMVCActiveRecord.GetByPK<TGoodCustomer>(lIDBad, False);
  try
    Assert.IsNull(lAGoodCustomer);
  finally
    lAGoodCustomer.Free;
  end;

  var lCustomer := TBadCustomer.Create;
  try
    lCustomer.LoadByPK(lIDBad);
    lCustomer.Rating := 5;
    lCustomer.Store;
    Assert.IsFalse(lCustomer.LoadByPK(lIDBad)); { this customer is not "bad" anymore }
  finally
    lCustomer.Free;
  end;

  var lCustomer1 := TGoodCustomer.Create;
  try
    lCustomer1.LoadByPK(lIDGood);
    lCustomer1.Rating := 1;
    lCustomer1.Store;
    Assert.IsFalse(lCustomer1.LoadByPK(lIDGood)); { this customer is not "good" anymore }
  finally
    lCustomer1.Free;
  end;
end;

procedure TTestActiveRecordBase.TestDefaultFilteringSelectByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  for var I := 1 to 5 do
  begin
    CreateACustomer('Company' + I.ToString, I);
  end;

  var lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('', 10);
  try
    Assert.AreEqual(2, lGoodCustomers.Count);
  finally
    lGoodCustomers.Free;
  end;

  lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('sort(+CompanyName)', 10);
  try
    Assert.AreEqual('Company4', lGoodCustomers[0].CompanyName.Value);
    Assert.AreEqual('Company5', lGoodCustomers[1].CompanyName.Value);
  finally
    lGoodCustomers.Free;
  end;

  lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('eq(Rating,5);sort(+CompanyName)', 10);
  try
    Assert.AreEqual('Company5', lGoodCustomers[0].CompanyName.Value);
  finally
    lGoodCustomers.Free;
  end;

  lGoodCustomers := TMVCActiveRecord.SelectRQL<TGoodCustomer>('lt(Rating,4);sort(+CompanyName)', 10);
  try
    Assert.AreEqual(0, lGoodCustomers.Count);
  finally
    lGoodCustomers.Free;
  end;


end;

procedure TTestActiveRecordBase.TestDefaultFilteringSelectOneByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  for var I := 1 to 5 do
  begin
    CreateACustomer('Company' + I.ToString, I);
  end;

  var
  lGoodCustomer := TMVCActiveRecord.SelectOneByRQL<TGoodCustomer>('eq(rating,5)', False);
  try
    Assert.IsNotNull(lGoodCustomer);
  finally
    lGoodCustomer.Free;
  end;

  lGoodCustomer := TMVCActiveRecord.SelectOneByRQL<TGoodCustomer>('eq(rating,1)', False);
  try
    Assert.IsNull(lGoodCustomer);
  finally
    lGoodCustomer.Free;
  end;

  var
  lImpossibileCustomer := TMVCActiveRecord.SelectOneByRQL<TGoodCustomer>('eq(rating,1)', False);
  try
    Assert.IsNull(lImpossibileCustomer);
  finally
    lImpossibileCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.TestDeleteIfNotFound;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
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
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert.WillNotRaise(
      procedure
      begin
        lCustomer.Delete(True);
      end, EMVCActiveRecordNotFound);

    Assert.WillNotRaise(
      procedure
      begin
        lCustomer.Delete(False);
      end, EMVCActiveRecordNotFound);

    Assert.WillRaise(
      procedure
      begin
        lCustomer.Delete(True);
      end, EMVCActiveRecordNotFound);

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
    Assert.AreEqual('OnValidation|OnBeforeDelete|OnBeforeExecuteSQL|MapObjectToParams|OnAfterDelete',
      lCustomer.GetHistory);
  finally
    lCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMergeWhenChangedRecords;
var
  lCustomer: TCustomer;
  lCustomers: TObjectList<TCustomer>;
  lCustomersChanges: TObjectList<TCustomer>;
  lInserted, lUpdated, lDeleted: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  LoadData(True);
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    lCustomersChanges := TObjectList<TCustomer>.Create(True);
    try
      for var lCust in lCustomers do
      begin
        lCustomer := lCust.Clone;
        lCustomer.Rating := 10;
        lCustomersChanges.Add(lCustomer);
      end;

      // calculate the unit-of-work to merge the lists
      lInserted := 0;
      lUpdated := 0;
      lDeleted := 0;
      TMVCActiveRecord.Merge<TCustomer>(lCustomers, lCustomersChanges).Apply(
        procedure(const Customer: TCustomer; const EntityAction: TMVCEntityAction; var Handled: boolean)
        begin
          Handled := False;
          case EntityAction of
            eaCreate:
              begin
                LogI('Inserting Customer : ' + Customer.ToString);
                Inc(lInserted);
              end;
            eaUpdate:
              begin
                LogI('Updating Customer  : ' + Customer.ToString);
                Inc(lUpdated);
              end;
            eaDelete:
              begin
                LogI('Deleting Customer  : ' + Customer.ToString);
                Inc(lDeleted);
              end;
          end;
        end);
    finally
      lCustomersChanges.Free;
    end;
  finally
    lCustomers.Free;
  end;

  Assert.AreEqual(0, lInserted);
  Assert.AreEqual(30, lUpdated);
  Assert.AreEqual(0, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual(30, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,10)', 1000);
  try
    Assert.AreEqual(30, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMergeWhenMixedRecords;
var
  lCustomer: TCustomer;
  lCustomers: TObjectList<TCustomer>;
  lCustomersChanges: TObjectList<TCustomer>;
  lInserted, lUpdated, lDeleted: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  LoadData(True);
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    lCustomersChanges := TObjectList<TCustomer>.Create(True);
    try
      // these 2 customers will be updated
      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.ID := lCustomers[0].ID;
      lCustomer.Code := 'C8765';
      lCustomer.CompanyName := '(changed) Company1';
      lCustomer.City := '(changed) City';
      lCustomer.Rating := 2;

      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.ID := lCustomers[1].ID;
      lCustomer.Code := lCustomers[1].Code;
      lCustomer.CompanyName := '(changed) Company2';
      lCustomer.City := '(changed) City';
      lCustomer.Rating := 2;

      // these 2 customer will be created
      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C9898';
      lCustomer.CompanyName := '(new) Company3';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 3;

      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C2343';
      lCustomer.CompanyName := '(new) Company4';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 3;

      // these 2 customer will remain the same but will be updated
      lCustomer := TCustomer.Create;
      lCustomer.Assign(lCustomers[2]);
      lCustomersChanges.Add(lCustomer);

      lCustomer := TCustomer.Create;
      lCustomer.Assign(lCustomers[3]);
      lCustomersChanges.Add(lCustomer);

      // all the other customers will be deleted

      // calculate the unit-of-work to merge the lists
      lInserted := 0;
      lUpdated := 0;
      lDeleted := 0;
      TMVCActiveRecord.Merge<TCustomer>(lCustomers, lCustomersChanges).Apply(
        procedure(const Customer: TCustomer; const EntityAction: TMVCEntityAction; var Handled: boolean)
        begin
          Handled := False;
          case EntityAction of
            eaCreate:
              begin
                LogI('Inserting Customer : ' + Customer.ToString);
                Inc(lInserted);
              end;
            eaUpdate:
              begin
                LogI('Updating Customer  : ' + Customer.ToString);
                Inc(lUpdated);
              end;
            eaDelete:
              begin
                LogI('Deleting Customer  : ' + Customer.ToString);
                Inc(lDeleted);
              end;
          end;
        end);
    finally
      lCustomersChanges.Free;
    end;
  finally
    lCustomers.Free;
  end;

  Assert.AreEqual(2, lInserted);
  Assert.AreEqual(4, lUpdated);
  Assert.AreEqual(26, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual(6, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,3)', 1000);
  try
    Assert.AreEqual(2, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,2)', 1000);
  try
    Assert.AreEqual(2, lCustomers.Count, 'Customers not updated correctly');
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    Assert.AreEqual(2, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMergeWhenNewDeletedRecords;
var
  lCustomers: TObjectList<TCustomer>;
  lCustomersChanges: TObjectList<TCustomer>;
  lInserted, lUpdated, lDeleted, lTotCustomers: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  LoadData(True);
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    lCustomersChanges := TObjectList<TCustomer>.Create(True);
    try
      lTotCustomers := lCustomersChanges.Count;
      lInserted := 0;
      lUpdated := 0;
      lDeleted := 0;
      // calculate the unit-of-work to merge the lists
      TMVCActiveRecord.Merge<TCustomer>(lCustomers, lCustomersChanges).Apply(
        procedure(const Customer: TCustomer; const EntityAction: TMVCEntityAction; var Handled: boolean)
        begin
          Handled := False;
          case EntityAction of
            eaCreate:
              begin
                LogI('Inserting Customer : ' + Customer.ToString);
                Inc(lInserted);
              end;
            eaUpdate:
              begin
                LogI('Updating Customer  : ' + Customer.ToString);
                Inc(lUpdated);
              end;
            eaDelete:
              begin
                LogI('Deleting Customer  : ' + Customer.ToString);
                Inc(lDeleted);
              end;
          end;
        end);
    finally
      lCustomersChanges.Free;
    end;
  finally
    lCustomers.Free;
  end;

  Assert.AreEqual(0, lInserted);
  Assert.AreEqual(0, lUpdated);
  Assert.AreEqual(30, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual(lTotCustomers, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMergeWhenNewRecords;
var
  lCustomer: TCustomer;
  lCustomers: TObjectList<TCustomer>;
  lCustomersChanges: TObjectList<TCustomer>;
  lInserted, lUpdated, lDeleted, lTotCustomers: Integer;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  LoadData(True);
  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    lCustomersChanges := TObjectList<TCustomer>.Create(True);
    try
      for var lCust in lCustomers do
      begin
        lCustomersChanges.Add(lCust.Clone);
      end;

      // these 2 customer will be created
      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C9898';
      lCustomer.CompanyName := '(new) Company3';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 3;

      lCustomer := TCustomer.Create;
      lCustomersChanges.Add(lCustomer);
      lCustomer.Code := 'C2343';
      lCustomer.CompanyName := '(new) Company4';
      lCustomer.City := '(new) New City2';
      lCustomer.Rating := 3;

      lTotCustomers := lCustomersChanges.Count;

      lInserted := 0;
      lUpdated := 0;
      lDeleted := 0;
      // calculate the unit-of-work to merge the lists
      TMVCActiveRecord.Merge<TCustomer>(lCustomers, lCustomersChanges).Apply(
        procedure(const Customer: TCustomer; const EntityAction: TMVCEntityAction; var Handled: boolean)
        begin
          Handled := False;
          case EntityAction of
            eaCreate:
              begin
                LogI('Inserting Customer : ' + Customer.ToString);
                Inc(lInserted);
              end;
            eaUpdate:
              begin
                LogI('Updating Customer  : ' + Customer.ToString);
                Inc(lUpdated);
              end;
            eaDelete:
              begin
                LogI('Deleting Customer  : ' + Customer.ToString);
                Inc(lDeleted);
              end;
          end;
        end);
    finally
      lCustomersChanges.Free;
    end;
  finally
    lCustomers.Free;
  end;

  Assert.AreEqual(2, lInserted);
  Assert.AreEqual(30, lUpdated);
  Assert.AreEqual(0, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual(lTotCustomers, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    Assert.AreEqual(lTotCustomers - 2, lCustomers.Count, 'Some customer changed when should not change');
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,3)', 1000);
  try
    Assert.AreEqual(2, lCustomers.Count, 'Some customer changed when should not change');
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestMultiThreading;
begin
  LoadData;
  Assert.AreEqual(Trunc(20 * 30), TMVCActiveRecord.Count(TCustomerWithLF));
end;

procedure TTestActiveRecordBase.TestNamedQueryRQL;
var
  lCustomers: TObjectList<TCustomer>;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TCustomer));
  LoadData;
  lCustomers := TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>('CityRomeOrLondon', [], MAXINT);
  try
    Assert.AreEqual(240, lCustomers.Count);
    for var lCustomer in lCustomers do
    begin
      Assert.IsMatch('^(Rome|London)$', lCustomer.City);
    end;
  finally
    lCustomers.Free;
  end;
  TMVCActiveRecord.DeleteRQLByNamedQuery<TCustomer>('CityRomeOrLondon', []);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.CountRQLByNamedQuery<TCustomer>('CityRomeOrLondon', []));
end;

procedure TTestActiveRecordBase.TestNamedQueryRQLWithExceptions;
begin
  Assert.WillRaiseWithMessage(
  procedure
  begin
    TMVCActiveRecord.SelectRQLByNamedQuery<TCustomer>('WrongQueryName', [1,2,3], MAXINT);
  end, nil, 'NamedRQLQuery not found: WrongQueryName');

  Assert.WillRaiseWithMessage(
  procedure
  begin
    TMVCActiveRecord.DeleteRQLByNamedQuery<TCustomer>('WrongQueryName', []);
  end, nil, 'NamedRQLQuery not found: WrongQueryName');
end;

procedure TTestActiveRecordBase.TestNamedQuerySQL;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TCustomer));
  LoadData;
  var lCustomers := TMVCActiveRecord.SelectByNamedQuery<TCustomer>('ByTwoCities', ['Rome', 'London'], [ftString, ftString]);
  try
    Assert.AreEqual(240, lCustomers.Count);
    for var lCustomer in lCustomers do
    begin
      Assert.IsMatch('^(Rome|London)$', lCustomer.City);
    end;
  finally
    lCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestNamedQuerySQLByBackEnd;
begin
  var lList := TMVCActiveRecord.SelectByNamedQuery<TDummyEntity>('get_backend_name', [],[]);
  try
    Assert.AreEqual(1, lList.Count);
    Assert.AreEqual(lList.First.GetBackEnd, lList.First.BackEndName);
  finally
    lList.Free;
  end;
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
    lTest.f_int4 := lTest.f_int4.Value + 4;
    lTest.f_int8 := lTest.f_int8.Value + 8;
    lTest.f_blob.Free;
    lTest.f_blob := nil;
    lTest.Update;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [2]);
  try
    Assert.IsTrue(lTest.f_int2.ValueOrDefault = 2);
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
    TMVCActiveRecord.DeleteRQL(TNullablesTest, 'eq(f_int2,2)');
  finally
    lTest.Free;
  end;

  Assert.IsNull(TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = 2', [], False));

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

procedure TTestActiveRecordBase.TestPartitioningCount;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'Rome', 2);
  CreateACustomer('John', 'New York', 3);
  CreateACustomer('Scott', 'Milan', 4);
  CreateACustomer('Bruce', 'Tokyo', 5);
  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TRomeBasedCustomer>);
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TNewYorkBasedCustomer>);
end;

procedure TTestActiveRecordBase.TestPartitioningCountByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'Rome', 2);
  CreateACustomer('John', 'New York', 3);
  CreateACustomer('Scott', 'Milan', 4);
  CreateACustomer('Bruce', 'Tokyo', 5);
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TRomeBasedCustomer>('ge(rating,2)'));
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TNewYorkBasedCustomer>('gt(rating,4)'));
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TNewYorkBasedCustomer>('contains(CompanyName,"a")'));
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TNewYorkBasedCustomer>('contains(CompanyName,"h")'));
end;

procedure TTestActiveRecordBase.TestPartitioningCRUD;
var
  lRMCustomer: TRomeBasedCustomer;
  lNYCustomer: TNewYorkBasedCustomer;
  lIDRome: Integer;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TRomeBasedCustomer>());
  lRMCustomer := TRomeBasedCustomer.Create;
  try
    lRMCustomer.CompanyName := 'bit Time Professionals';
    lRMCustomer.Note := 'note1';
    lRMCustomer.Insert;
    lIDRome := lRMCustomer.ID;
  finally
    lRMCustomer.Free;
  end;

  lNYCustomer := TNewYorkBasedCustomer.Create;
  try
    lNYCustomer.CompanyName := 'bit Time Professionals NY';
    lRMCustomer.Note := 'note2';
    lNYCustomer.Insert;
  finally
    lNYCustomer.Free;
  end;

  lRMCustomer := TMVCActiveRecord.GetByPK<TRomeBasedCustomer>(lIDRome);
  try
    Assert.IsFalse(lRMCustomer.Code.HasValue);
    lRMCustomer.Code := '1234';
    lRMCustomer.Note := lRMCustomer.Note + 'noteupdated';
    lRMCustomer.Update;
  finally
    lRMCustomer.Free;
  end;

  lRMCustomer := TMVCActiveRecord.GetByPK<TRomeBasedCustomer>(lIDRome);
  try
    Assert.AreEqual('1234', lRMCustomer.Code.Value);
    Assert.AreEqual('note1noteupdated', lRMCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lRMCustomer.CompanyName.Value);
    Assert.AreEqual(1, lRMCustomer.ID.Value);
  finally
    lRMCustomer.Free;
  end;

  lRMCustomer := TMVCActiveRecord.GetByPK<TRomeBasedCustomer>(lIDRome);
  try
    lRMCustomer.Delete;
  finally
    lRMCustomer.Free;
  end;

  lRMCustomer := TMVCActiveRecord.GetByPK<TRomeBasedCustomer>(lIDRome, False);
  Assert.IsNull(lRMCustomer);

  lRMCustomer := TMVCActiveRecord.GetOneByWhere<TRomeBasedCustomer>('id = ?', [lIDRome], [ftInteger], False);
  Assert.IsNull(lRMCustomer);
end;

procedure TTestActiveRecordBase.TestPartitioningDelete;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'Rome', 2);
  CreateACustomer('Bruce', 'Tokyo', 3);
  CreateACustomer('John', 'New York', 4);
  var lID5 := CreateACustomer('Scott', 'New York', 5);

  var lGoodNewYorkCustomer := TMVCActiveRecord.GetByPK<TNewYorkBasedGoodCustomer>(lID5);
  try
    lGoodNewYorkCustomer.Delete;
    Assert.Pass;
  finally
    lGoodNewYorkCustomer.Free;
  end;

  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count(TNewYorkBasedCustomer));
  TMVCActiveRecord.DeleteAll(TNewYorkBasedGoodCustomer);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TNewYorkBasedGoodCustomer));
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count(TNewYorkBasedCustomer));
end;

procedure TTestActiveRecordBase.TestPartitioningDeleteByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'Rome', 2);
  CreateACustomer('Bruce', 'Tokyo', 3);
  CreateACustomer('John', 'New York', 4);
  CreateACustomer('Scott', 'New York', 5);

  Assert.AreEqual(Int64(2), TMVCActiveRecord.Count(TNewYorkBasedCustomer));
  TMVCActiveRecord.DeleteRQL(TNewYorkBasedCustomer, 'eq(CompanyName,"John")');
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count(TNewYorkBasedCustomer));
  TMVCActiveRecord.DeleteRQL(TNewYorkBasedCustomer, 'eq(CompanyName,"John")');
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count(TNewYorkBasedCustomer));
  Assert.AreEqual(Int64(1), TMVCActiveRecord.Count(TNewYorkBasedGoodCustomer));
end;

procedure TTestActiveRecordBase.TestPartitioningGetByPK;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  var
  lID1 := CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'Rome', 2);
  CreateACustomer('Bruce', 'Tokyo', 3);
  CreateACustomer('John', 'New York', 4);
  var
  lID5 := CreateACustomer('Scott', 'New York', 5);

  var
  lRomeCustomer := TMVCActiveRecord.GetByPK<TRomeBasedCustomer>(lID1);
  try
    Assert.IsNotNull(lRomeCustomer);
  finally
    lRomeCustomer.Free;
  end;

  var
  lNYCustomer := TMVCActiveRecord.GetByPK<TNewYorkBasedCustomer>(lID1, False);
  try
    Assert.IsNull(lNYCustomer);
  finally
    lNYCustomer.Free;
  end;

  var
  lNYGoodCustomer := TMVCActiveRecord.GetByPK<TNewYorkBasedGoodCustomer>(lID5, False);
  try
    Assert.IsNotNull(lNYGoodCustomer);
  finally
    lNYGoodCustomer.Free;
  end;

  lNYGoodCustomer := TMVCActiveRecord.GetByPK<TNewYorkBasedGoodCustomer>(lID1, False);
  try
    Assert.IsNull(lNYGoodCustomer);
  finally
    lNYGoodCustomer.Free;
  end;

end;

procedure TTestActiveRecordBase.TestPartitioningSelectByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Rome Company 1', 'Rome', 5);
  CreateACustomer('Rome Company 2', 'Rome', 2);
  CreateACustomer('New York 1', 'New York', 1);
  CreateACustomer('Toyko 1', 'Tokyo', 4);

  var
  lRomeCustomers := TMVCActiveRecord.SelectRQL<TRomeBasedCustomer>('', 10);
  try
    Assert.AreEqual(2, lRomeCustomers.Count);
  finally
    lRomeCustomers.Free;
  end;

  lRomeCustomers := TMVCActiveRecord.SelectRQL<TRomeBasedCustomer>('sort(+CompanyName)', 10);
  try
    Assert.AreEqual('Rome Company 1', lRomeCustomers[0].CompanyName.Value);
    Assert.AreEqual('Rome Company 2', lRomeCustomers[1].CompanyName.Value);
  finally
    lRomeCustomers.Free;
  end;

  lRomeCustomers := TMVCActiveRecord.SelectRQL<TRomeBasedCustomer>('eq(Rating,5);sort(+CompanyName)', 10);
  try
    Assert.AreEqual(1, lRomeCustomers.Count);
    Assert.AreEqual('Rome Company 1', lRomeCustomers[0].CompanyName.Value);
  finally
    lRomeCustomers.Free;
  end;

  lRomeCustomers := TMVCActiveRecord.SelectRQL<TRomeBasedCustomer>('lt(Rating,2);sort(+CompanyName)', 10);
  try
    Assert.AreEqual(0, lRomeCustomers.Count);
  finally
    lRomeCustomers.Free;
  end;
end;

procedure TTestActiveRecordBase.TestPartitioningSelectByWhere;
begin
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TRomeBasedCustomer>());
  CreateACustomer('Daniele', 'Rome', 1);
  CreateACustomer('Jack', 'New York', 1);
  var
  lRomeBasedCustomers := TMVCActiveRecord.Where<TRomeBasedCustomer>('city = ?', ['New York'], [ftString]);
  try
    Assert.AreEqual(0, lRomeBasedCustomers.Count);
  finally
    lRomeBasedCustomers.Free;
  end;

  lRomeBasedCustomers := TMVCActiveRecord.Where<TRomeBasedCustomer>('description = ?', ['Daniele'], [ftString]);
  try
    Assert.AreEqual(1, lRomeBasedCustomers.Count);
  finally
    lRomeBasedCustomers.Free;
  end;

end;

procedure TTestActiveRecordBase.TestPartitioningSelectOneByRQL;
begin
  TMVCActiveRecord.DeleteAll(TCustomer);
  CreateACustomer('Rome Company 1', 'Rome', 5);
  CreateACustomer('Rome Company 2', 'Rome', 2);
  CreateACustomer('New York 1', 'New York', 5);
  CreateACustomer('Toyko 1', 'Tokyo', 4);

  var lRomeCustomer := TMVCActiveRecord.SelectOneByRQL<TRomeBasedCustomer>('contains(CompanyName,"1")');
  try
    Assert.IsNotNull(lRomeCustomer);
  finally
    lRomeCustomer.Free;
  end;

  lRomeCustomer := TMVCActiveRecord.SelectOneByRQL<TRomeBasedCustomer>('eq(Rating,5);sort(+CompanyName)');
  try
    Assert.AreEqual('Rome Company 1', lRomeCustomer.CompanyName.Value);
  finally
    lRomeCustomer.Free;
  end;

  TMVCActiveRecord.DeleteAll(TRomeBasedCustomer);

  lRomeCustomer := TMVCActiveRecord.SelectOneByRQL<TRomeBasedCustomer>('eq(Rating,5);sort(+CompanyName)', False);
  try
    Assert.IsNull(lRomeCustomer);
  finally
    lRomeCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.TestRefresh;
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
    lCustomer.CompanyName.Clear;
    lCustomer.City := '';
    lCustomer.Note := '';
    lCustomer.CreationTime := 0;
    lCustomer.CreationDate := 0;
    lCustomer.Refresh;
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.ValueOrDefault);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.AreEqual('note1', lCustomer.Note);
  finally
    lCustomer.Free;
  end;

  lCustomer := TCustomer.Create;
  try
    lCustomer.ID := lID;
    lCustomer.Refresh;
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.ValueOrDefault);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.AreEqual('note1', lCustomer.Note);
  finally
    lCustomer.Free;
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

procedure TTestActiveRecordBase.TestRQLWithBoolean;
const
  RQL1 = 'or(eq(City, "Rome"),eq(City, "London"))';
var
  lBoolTable, lBoolValue2: TBoolTest;
begin
  TMVCActiveRecord.DeleteAll(TBoolTest);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TBoolTest));
  lBoolTable := TBoolTest.Create;
  try
    lBoolTable.BoolValue := True;
    lBoolTable.Store;
    lBoolValue2 := TMVCActiveRecord.SelectOneByRQL<TBoolTest>('eq(boolvalue, true)');
    try
      Assert.AreEqual(True, lBoolValue2.BoolValue);
    finally
      lBoolValue2.Free;
    end;
    lBoolTable.BoolValue := False;
    lBoolTable.Store;

    lBoolValue2 := TMVCActiveRecord.SelectOneByRQL<TBoolTest>('eq(boolvalue, false)');
    try
      Assert.AreEqual(False, lBoolValue2.BoolValue);
    finally
      lBoolValue2.Free;
    end;

    Assert.IsNull(TMVCActiveRecord.SelectOneByRQL<TBoolTest>('eq(boolvalue, true)', False));
  finally
    lBoolTable.Free;
  end;
  // LoadData;
  // lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, MAXINT);
  // try
  // Assert.AreEqual(240, lCustomers.Count);
  // for var lCustomer in lCustomers do
  // begin
  // Assert.IsMatch('^(Rome|London)$', lCustomer.City);
  // end;
  // finally
  // lCustomers.Free;
  // end;
  // TMVCActiveRecord.DeleteRQL(TCustomer, RQL1);
  // Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomer>(RQL1));
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

procedure TTestActiveRecordBase.TestRQLWithGUID;
var
  lCustomers: TObjectList<TCustomerWithGUID>;
  lCust: TCustomerWithGUID;
const
  RQL1 = 'and(eq(idguid, "{81778CF0-BFF8-474B-991B-ABFB225AE377}"), eq(otherguid, "{31531A9E-3D24-4DEA-A9CD-B803DF186DE6}"))';
begin
  TMVCActiveRecord.DeleteAll(TCustomerWithGUID);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count(TCustomerWithGUID));
  LoadData;

  lCust := TCustomerWithGUID.Create;
  try
    lCust.GUID := StringToGUID('{81778CF0-BFF8-474B-991B-ABFB225AE377}');
    lCust.OtherGUID := StringToGUID('{31531A9E-3D24-4DEA-A9CD-B803DF186DE6}');
    lCust.Insert;
  finally
    lCust.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomerWithGUID>(RQL1, MAXINT);
  try
    Assert.AreEqual(1, lCustomers.Count);
  finally
    lCustomers.Free;
  end;
  TMVCActiveRecord.DeleteRQL(TCustomerWithGUID, RQL1);
  Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TCustomerWithGUID>(RQL1));
end;

procedure TTestActiveRecordBase.TestRQLWithMVCNameAsAttribute;
var
  lCustomers: TObjectList<TCustomer>;
const
  //this RQL contains aliases defined using MVCNameAs attribute
  RQL1 = 'and(or(eq(CityName, "Rome"),eq(City, "London")),ne(CustomerCode,"INVALID"))';
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

procedure TTestActiveRecordBase.TestSelectWithExceptions;
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

procedure TTestActiveRecordBase.TestUpdateIfNotFound;
var
  lCustomer: TCustomer;
  lID: Integer;
begin
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
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    lCustomer.Update;
    lCustomer.Delete;

    Assert.WillNotRaise(
      procedure
      begin
        lCustomer.Update(False);
      end, EMVCActiveRecordNotFound);

    Assert.WillRaise(
      procedure
      begin
        lCustomer.Update(True);
      end, EMVCActiveRecordNotFound);

  finally
    lCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.Test_ISSUE485;
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
    Assert.IsTrue(lCustomer.CompanyName.HasValue);
    lCustomer.CompanyName.Clear;
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert.IsFalse(lCustomer.CompanyName.HasValue);
    lCustomer.CompanyName := 'bit Time Professionals';
    lCustomer.Update;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomer>(lID);
  try
    Assert.IsTrue(lCustomer.CompanyName.HasValue);
  finally
    lCustomer.Free;
  end;
end;

function TTestActiveRecordBase.CreateACustomer(CompanyName: String; Rating: Integer): Integer;
begin
  Result := CreateACustomer(CompanyName, CompanyName + 'City', Rating);
end;

function TTestActiveRecordBase.CreateACustomer(CompanyName, City: String; Rating: Integer): Integer;
var
  lCustomer: TCustomer;
begin
  lCustomer := TCustomer.Create;
  try
    lCustomer.CompanyName := CompanyName;
    lCustomer.City := City;
    lCustomer.Rating := Rating;
    lCustomer.Insert;
    Result := lCustomer.ID;
  finally
    lCustomer.Free;
  end;
end;

procedure TTestActiveRecordBase.InternalSetupFixture;
begin
  // do nothing
end;

procedure TTestActiveRecordBase.LoadData(const JustAFew: boolean);
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
      // ActiveRecordConnectionsRegistry.AddDefaultConnection(TFDConnection.Create(nil), True);
      ActiveRecordConnectionsRegistry.AddConnection('load', TFDConnection.Create(nil), True);
      try
        ActiveRecordConnectionsRegistry.SetCurrent('load');
        ActiveRecordConnectionsRegistry.GetCurrent.ConnectionDefName := fConDefName;
        for I := 1 to 30 do
        begin
          lCustomer := TCustomer.Create;
          try
            lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
            lCustomer.City := Cities[I mod Length(Cities)];
            lCustomer.CompanyName := Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
              CompanySuffix[Random(high(CompanySuffix) + 1)]]);
            lCustomer.Note := Stuff[I mod Length(Stuff)];
            lCustomer.Rating := 1;
            lCustomer.CreationTime := EncodeTime(I mod 23, I, 60 - 1, 0);
            lCustomer.CreationDate := EncodeDate(2020 - I, (I mod 12) + 1, (I mod 27) + 1);
            lCustomer.Insert;
          finally
            lCustomer.Free;
          end;
        end;
      finally
        ActiveRecordConnectionsRegistry.RemoveConnection('load');
      end;
    end;
  AfterDataLoad;

  if JustAFew then
  begin
    lProc();
    ActiveRecordConnectionsRegistry.SetCurrent('default');
  end
  else
  begin
    lTasks := [TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
      TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
      TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc),
      TTask.Run(lProc), TTask.Run(lProc), TTask.Run(lProc)];
    TTask.WaitForAll(lTasks);
  end;
end;

procedure TTestActiveRecordBase.SetupFixture;
begin
//  ActiveRecordTableMapRegistry.FlushCache;
  LogI('** Setup Fixture: ' + ClassName);
  InternalSetupFixture;
end;

procedure TTestActiveRecordSQLite.Setup;
begin
  LogI('** Setup Test: ' + ClassName);
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
  if not Assigned(FDManager.DriverDefs.FindDefinition('FBEMBEDDED')) then
  begin
    lDriver := FDManager.DriverDefs.Add;
    lDriver.Name := 'FBEMBEDDED';
    lDriver.AsString['BaseDriverID'] := 'FB';
    lDriver.AsString['DriverID'] := 'FBEMBEDDED';
    lDriver.AsString['VendorLib'] := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebird\fbclient.dll');
    lDriver.Apply;
  end;

  LParams := TStringList.Create;
  try
    GDBFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebirdtest.fdb');
    GDBTemplateFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebirdtest_template.fdb');
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
  LogI('** Setup Test: ' + ClassName);
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
  lPGHome := TPath.Combine(TPath.GetDirectoryName(TPath.GetDirectoryName(ParamStr(0))), 'pgsql');
  lDataDir := TPath.Combine(lPGHome, 'testdatadir');
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
  //lDriver.AsString['VendorLib'] := TPath.Combine(fPGUtil.PGHome, 'libpq.dll');
  lDriver.Apply;

  LParams := TStringList.Create;
  try
    LParams.Add('Database=activerecordtest');
    LParams.Add('Port=' + PG_PORT.ToString);

    // https://quality.embarcadero.com/browse/RSP-19755?jql=text%20~%20%22firedac%20guid%22
    LParams.Add('GUIDEndian=Big');
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
  LogI('** Setup Test: ' + ClassName);
  lInitDBStructure := False;

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
  GPGIsInitialized := False;
end;

initialization

TDUnitX.RegisterTestFixture(TTestActiveRecordSQLite);
TDUnitX.RegisterTestFixture(TTestActiveRecordFirebird);
TDUnitX.RegisterTestFixture(TTestActiveRecordPostgreSQL);

finalization

end.
