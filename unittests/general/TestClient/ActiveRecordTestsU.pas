// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
  PGUtilsU, MariaDBUtilsU;

const
  PG_PORT = 5555;
  MARIADB_PORT = 3399;
  ORACLE_PORT = 1523;
  MSSQL_PORT = 1433;

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
    procedure TestCRUDStringPK;
    [Test]
    procedure TestSelectWithExceptions;
    [Test]
    procedure TestNamedQuerySQL;
    [Test]
    procedure TestTryGetNamedQuery;
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
    // https://github.com/danieleteti/delphimvcframework/issues/909
    [Test]
    procedure TestNullablesRefreshResetsToNull;
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
    { validation - attribute based }
    [Test]
    procedure TestValidation_InsertFailsWhenDescriptionEmpty;
    [Test]
    procedure TestValidation_InsertFailsWhenDescriptionTooLong;
    [Test]
    procedure TestValidation_InsertFailsWhenEmailInvalid;
    [Test]
    procedure TestValidation_InsertFailsWhenRatingOutOfRange;
    [Test]
    procedure TestValidation_ExceptionMessageListsFailingFields;
    [Test]
    procedure TestValidation_InsertSucceedsWhenAllValid;
    [Test]
    procedure TestValidation_UpdateFailsWhenInvalid;
    [Test]
    procedure TestValidation_ValidateMethodCanBeCalledDirectly;
    [Test]
    procedure TestValidation_PKRequiredSkippedOnInsert;
    [Test]
    procedure TestValidation_OnValidateMethodIsInvoked;
    { audit columns }
    [Test]
    procedure TestAudit_CreatedAtFilledOnInsert;
    [Test]
    procedure TestAudit_UpdatedAtMatchesCreatedAtOnInsert;
    [Test]
    procedure TestAudit_UpdatedAtChangesOnUpdate;
    [Test]
    procedure TestAudit_CreatedAtUnchangedOnUpdate;
    [Test]
    procedure TestAudit_CreatedByFromThreadLocal;
    [Test]
    procedure TestAudit_UpdatedByFollowsThreadUser;
    [Test]
    procedure TestAudit_NoUserLeavesFieldsNull;
    [Test]
    procedure TestAudit_WrongDateTimeFieldTypeFailsFast;
    [Test]
    procedure TestAudit_WrongStringFieldTypeFailsFast;
    [Test]
    procedure TestAudit_CurrentUserIsThreadIsolated;
    { SelectUnidirectionalDataSet — signature tripwire }
    [Test]
    procedure TestSelectUnidirectionalDataSetSignature;
    // Composite (multi-column) primary key: full CRUD via LoadByPKs/GetByPKs and
    // the guards that steer single-value PK APIs to their *PKs counterparts.
    [Test]
    procedure TestCompositePK_CRUD;
    [Test]
    procedure TestCompositePK_Guards;
    // Heterogeneous key: string column + integer column.
    [Test]
    procedure TestCompositePK_Heterogeneous;
    // Composite key through the Repository pattern (IMVCRepository<T>).
    [Test]
    procedure TestCompositePK_Repository;
    // --- Exhaustive composite-PK coverage (each test owns a unique table) ---
    // #1 three-column PK (INTEGER, INTEGER, VARCHAR): full CRUD + Count.
    [Test]
    procedure TestCompositePK_ThreeColumn;
    // #2 GUID column inside a composite key (GUID + INTEGER).
    [Test]
    procedure TestCompositePK_GUIDColumn;
    // #3 nullable PK columns (NullableInt64 + NullableString), values set.
    [Test]
    procedure TestCompositePK_NullableColumns;
    // #4 foVersion alongside a composite PK: increment + stale-update rejection.
    [Test]
    procedure TestCompositePK_VersionColumn;
    // #5 duplicate full composite key must raise on the second Insert.
    [Test]
    procedure TestCompositePK_DuplicateKeyRaises;
    // #6 GetByPKs not-found: raises with True, returns nil with False.
    [Test]
    procedure TestCompositePK_NotFoundBehavior;
    // #7 wrong value count on GetByPKs/LoadByPKs/SetPKs must raise (no DB).
    [Test]
    procedure TestCompositePK_WrongValueCountRaises;
    // #8 GetPKs/SetPKs round-trip, and GetPKs after LoadByPKs equals the key.
    [Test]
    procedure TestCompositePK_GetSetPKsRoundTrip;
    // #9 two foAutoGenerated PK columns must be rejected at map build (no DB).
    [Test]
    procedure TestCompositePK_TwoAutogenRejected;
    // #10 Where / SelectRQL / Count on a composite entity return correct rows.
    [Test]
    procedure TestCompositePK_WhereAndRQL;
    // #11 DeleteAll / DeleteRQL on a composite entity empty the table.
    [Test]
    procedure TestCompositePK_DeleteAllAndDeleteRQL;
    // #12 PKAsJSONArray escapes quote/backslash in a string PK column.
    [Test]
    procedure TestCompositePK_PKAsJSONArrayEscaping;
    // #13 the foRefresh fallback (RefreshFromDB) must filter on every PK column.
    [Test]
    procedure TestCompositePK_RefreshFromDB;
    // #14 PrimaryKeyIsAutogenerated must really switch the behaviour it claims.
    [Test]
    procedure TestPrimaryKeyIsAutogenerated_RoundTrip;
  end;

  [TestFixture]
  TTestActiveRecordSQLite = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup; virtual;
    // Regression: an autogenerated Int64 PK whose DB value exceeds 32-bit MaxInt
    // must round-trip intact after Insert (previously truncated via AsInteger).
    [Test]
    procedure TestInt64AutogenPKBeyondMaxInt32;
  end;

  [TestFixture]
  TTestActiveRecordFirebird = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup;
    // Regression (real BIGINT column): autogenerated Int64 PK above 32-bit MaxInt
    // must round-trip intact through both Insert read-back and a SELECT (GetByPK).
    [Test]
    procedure TestInt64AutogenPKBeyondMaxInt32;
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
    // Regression (real BIGINT column): autogenerated Int64 PK above 32-bit MaxInt
    // must round-trip intact through both Insert read-back (RETURNING) and a
    // SELECT (GetByPK). Unlike SQLite, PG maps BIGINT to ftLargeInt end-to-end.
    [Test]
    procedure TestInt64AutogenPKBeyondMaxInt32;
    // Composite PK where one key column is DB-autogenerated (SERIAL) and the
    // other is a natural INTEGER: proves the autogen column is read back via
    // RETURNING into the object after Insert even when it lives inside a
    // composite key, and that GetByPKs/Update/Delete address the right row.
    [Test]
    procedure TestCompositePK_AutogenColumn;
    // Serves a real (PostgreSQL) dataset through the server's streaming writer:
    // forward-only cursor on the server + TMVCJSONArrayWriter, so neither the
    // whole dataset nor the whole JSON is buffered. Uses PostgreSQL (not SQLite)
    // so a strict type system can't mask serialization issues.
    [Test]
    procedure TestStreamedDataSet_NotBufferedEndToEnd;
    // Serves a real (PostgreSQL) dataset through the server's CHUNKED streaming
    // writer (Transfer-Encoding: chunked, no Content-Length, keep-alive on Indy
    // Direct / HTTP.sys). The classic WebBroker backend can't chunk a self-emitted
    // body so the framework fails cleanly (5xx) before any byte is written.
    [Test]
    procedure TestStreamedDataSetChunked_EndToEnd;
  end;

  // MariaDB runs from the copy bundled under TestClient\mariadb, started on its
  // own port with its own data directory, exactly like the PostgreSQL fixture
  // above. It never touches a MariaDB or MySQL installed on the machine.
  [TestFixture]
  TTestActiveRecordMariaDB = class(TTestActiveRecordBase)
  private
    fMariaDBUtil: TMariaDBUtil;
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

{$IF Defined(TEST_ORACLE)}
  // Oracle needs a server this repository cannot bundle, so the fixture is
  // compiled only when TEST_ORACLE is defined. Start the container it expects
  // with:
  //
  //   docker run -d --name dmvc-oracle -p 1523:1521 -e ORACLE_PASSWORD=masterkey
  //     -e APP_USER=guida -e APP_USER_PASSWORD=guida gvenzl/oracle-free:slim
  //
  // It is ready when the log line "DATABASE IS READY TO USE!" appears. The
  // Oracle Instant Client must also be reachable: put its folder on the PATH,
  // since oci.dll loads its own dependencies from the executable's directory.
  [TestFixture]
  TTestActiveRecordOracle = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup;
  end;
{$ENDIF}

{$IF Defined(TEST_MSSQL)}
  // Same reasoning as Oracle above: compiled only when TEST_MSSQL is defined.
  // Start the container it expects with:
  //
  //   docker run -d --name dmvc-mssql -p 1433:1433 -e ACCEPT_EULA=Y
  //     -e "MSSQL_SA_PASSWORD=Masterkey!2026" -e MSSQL_PID=Developer
  //     mcr.microsoft.com/mssql/server:2022-latest
  //
  // It is ready when the log says "SQL Server is now ready for client
  // connections". Microsoft's ODBC driver 17 or 18 must be installed; version
  // 18 encrypts by default, hence TrustServerCertificate in the connection.
  [TestFixture]
  TTestActiveRecordMSSQL = class(TTestActiveRecordBase)
  protected
    procedure AfterDataLoad; override;
    procedure CreatePrivateConnDef(AIsPooled: boolean); override;
  public
    [Setup]
    procedure Setup;
  end;
{$ENDIF}

implementation

uses
  System.Classes, System.IOUtils, BOs, MVCFramework.ActiveRecord,
  System.SysUtils, System.Rtti, System.Threading, System.Generics.Collections, Data.DB,
  System.DateUtils, System.SyncObjs,
  FireDAC.Stan.Intf, ShellAPI, Winapi.Windows, MVCFramework.Logger,
  MVCFramework.Nullables, MVCFramework.Validation, MVCFramework.Repository,
  MVCFramework.RESTClient, MVCFramework.RESTClient.Intf, JsonDataObjects,
  TestConstsU;

type
  // Dedicated entity for the Int64-autogen-PK regression test. Its table is
  // created/dropped by the test itself, so it never touches the shared schema.
  [MVCTable('bigint_pk_test')]
  TBigIntPKEntity = class(TMVCActiveRecord)
  private
    [MVCTableField('id', [foPrimaryKey, foAutoGenerated])]
    fID: Int64;
    [MVCTableField('descr')]
    fDescr: string;
  public
    property ID: Int64 read fID write fID;
    property Descr: string read fDescr write fDescr;
  end;

  // Heterogeneous composite primary key: a string column + an integer column.
  // Proves PK columns need not share a type.
  [MVCTable('ar_doc_lines')]
  TARDocLine = class(TMVCActiveRecord)
  private
    [MVCTableField('doc_code', [foPrimaryKey])]
    fDocCode: string;
    [MVCTableField('line_no', [foPrimaryKey])]
    fLineNo: Integer;
    [MVCTableField('descr')]
    fDescr: NullableString;
  public
    property DocCode: string read fDocCode write fDocCode;
    property LineNo: Integer read fLineNo write fLineNo;
    property Descr: NullableString read fDescr write fDescr;
  end;

  // Same shape as TARUserRole but a distinct table, so the Repository test never
  // collides with the CRUD test on Firebird (in-test DDL drops are not guaranteed
  // to commit before the next test's CREATE).
  [MVCTable('ar_repo_roles')]
  TARRepoRole = class(TMVCActiveRecord)
  private
    [MVCTableField('user_id', [foPrimaryKey])]
    fUserID: Integer;
    [MVCTableField('role_id', [foPrimaryKey])]
    fRoleID: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property UserID: Integer read fUserID write fUserID;
    property RoleID: Integer read fRoleID write fRoleID;
    property Note: NullableString read fNote write fNote;
  end;

  // Natural composite primary key (two columns, no auto-generation): the common
  // junction-table shape. Its table is created/dropped by the test itself.
  [MVCTable('ar_user_roles')]
  TARUserRole = class(TMVCActiveRecord)
  private
    [MVCTableField('user_id', [foPrimaryKey])]
    fUserID: Integer;
    [MVCTableField('role_id', [foPrimaryKey])]
    fRoleID: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property UserID: Integer read fUserID write fUserID;
    property RoleID: Integer read fRoleID write fRoleID;
    property Note: NullableString read fNote write fNote;
  end;

  // Single-column PK, used only by TestPrimaryKeyIsAutogenerated_RoundTrip:
  // that setter edits the table map, which is built once per class and shared
  // by every instance, so this class must not be shared with another test.
  // InsertSQL exposes what the generator would emit, which is how the test
  // checks the flag actually changed something instead of just reading back.
  [MVCTable('ar_autogen_flag')]
  TARAutogenFlag = class(TMVCActiveRecord)
  private
    [MVCTableField('id', [foPrimaryKey])]
    fID: Integer;
    [MVCTableField('descr')]
    fDescr: NullableString;
  public
    function InsertSQL: string;
    property ID: Integer read fID write fID;
    property Descr: NullableString read fDescr write fDescr;
  end;

  // foRefresh column alongside a composite primary key. RefreshFromDB is the
  // fallback used by engines without RETURNING/OUTPUT (MySQL, MariaDB, Oracle);
  // it is protected, so this descendant exposes it to the test.
  [MVCTable('ar_refresh_ck')]
  TARRefreshCK = class(TMVCActiveRecord)
  private
    [MVCTableField('tenant', [foPrimaryKey])]
    fTenant: Integer;
    [MVCTableField('doc_id', [foPrimaryKey])]
    fDocID: Integer;
    [MVCTableField('descr')]
    fDescr: NullableString;
    [MVCTableField('norm', [foRefresh, foDoNotInsert, foDoNotUpdate])]
    fNorm: NullableString;
  public
    procedure CallRefreshFromDB;
    property Tenant: Integer read fTenant write fTenant;
    property DocID: Integer read fDocID write fDocID;
    property Descr: NullableString read fDescr write fDescr;
    property Norm: NullableString read fNorm write fNorm;
  end;

  // Composite primary key where one key column is DB-autogenerated (SERIAL /
  // IDENTITY) and the other is a natural integer. Exercises RETURNING read-back
  // of the autogen column when it lives inside a composite key. Table is
  // created/dropped by the test itself.
  [MVCTable('ar_invoice_lines')]
  TARInvoiceLine = class(TMVCActiveRecord)
  private
    [MVCTableField('line_id', [foPrimaryKey, foAutoGenerated])]
    fLineID: Int64;
    [MVCTableField('invoice_no', [foPrimaryKey])]
    fInvoiceNo: Integer;
    [MVCTableField('descr')]
    fDescr: NullableString;
  public
    property LineID: Int64 read fLineID write fLineID;
    property InvoiceNo: Integer read fInvoiceNo write fInvoiceNo;
    property Descr: NullableString read fDescr write fDescr;
  end;

  // === Exhaustive composite-PK entities (one table each; see the tests) ===

  // #1 Three-column composite PK: INTEGER, INTEGER, VARCHAR.
  [MVCTable('ar_ck_triple')]
  TARTriple = class(TMVCActiveRecord)
  private
    [MVCTableField('tenant_id', [foPrimaryKey])]
    fTenantID: Integer;
    [MVCTableField('dept_id', [foPrimaryKey])]
    fDeptID: Integer;
    [MVCTableField('code', [foPrimaryKey])]
    fCode: string;
    [MVCTableField('descr')]
    fDescr: NullableString;
  public
    property TenantID: Integer read fTenantID write fTenantID;
    property DeptID: Integer read fDeptID write fDeptID;
    property Code: string read fCode write fCode;
    property Descr: NullableString read fDescr write fDescr;
  end;

  // #2 GUID + INTEGER composite PK (GUID column type is DB-specific in the DDL).
  [MVCTable('ar_ck_guid')]
  TARGuidKey = class(TMVCActiveRecord)
  private
    [MVCTableField('gid', [foPrimaryKey])]
    fGID: NullableTGUID;
    [MVCTableField('seq', [foPrimaryKey])]
    fSeq: Integer;
    [MVCTableField('payload')]
    fPayload: NullableString;
  public
    property GID: NullableTGUID read fGID write fGID;
    property Seq: Integer read fSeq write fSeq;
    property Payload: NullableString read fPayload write fPayload;
  end;

  // #3 Nullable PK columns (NullableInt64 + NullableString), both with values.
  [MVCTable('ar_ck_null')]
  TARNullKey = class(TMVCActiveRecord)
  private
    [MVCTableField('k1', [foPrimaryKey])]
    fK1: NullableInt64;
    [MVCTableField('k2', [foPrimaryKey])]
    fK2: NullableString;
    [MVCTableField('val')]
    fVal: NullableString;
  public
    property K1: NullableInt64 read fK1 write fK1;
    property K2: NullableString read fK2 write fK2;
    property Val: NullableString read fVal write fVal;
  end;

  // #4 Composite PK + a foVersion column (optimistic locking).
  [MVCTable('ar_ck_ver')]
  TARVerKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
    [MVCTableField('objversion', [foVersion])]
    fObjVersion: Integer;
  public
    property A: Integer read fA write fA;
    property B: Integer read fB write fB;
    property Note: NullableString read fNote write fNote;
    property ObjVersion: Integer read fObjVersion write fObjVersion;
  end;

  // #5 Duplicate-key test entity.
  [MVCTable('ar_ck_dup')]
  TARDupKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property A: Integer read fA write fA;
    property B: Integer read fB write fB;
    property Note: NullableString read fNote write fNote;
  end;

  // #6 Not-found behaviour test entity.
  [MVCTable('ar_ck_nf')]
  TARNfKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property A: Integer read fA write fA;
    property B: Integer read fB write fB;
    property Note: NullableString read fNote write fNote;
  end;

  // #8 GetPKs/SetPKs round-trip entity (heterogeneous: INTEGER + VARCHAR).
  [MVCTable('ar_ck_pks')]
  TARPksKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: string;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property A: Integer read fA write fA;
    property B: string read fB write fB;
    property Note: NullableString read fNote write fNote;
  end;

  // #9 Two foAutoGenerated PK columns: illegal, must raise at map build.
  [MVCTable('ar_ck_twoautogen')]
  TARTwoAutogen = class(TMVCActiveRecord)
  private
    [MVCTableField('id1', [foPrimaryKey, foAutoGenerated])]
    fID1: Int64;
    [MVCTableField('id2', [foPrimaryKey, foAutoGenerated])]
    fID2: Int64;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property ID1: Int64 read fID1 write fID1;
    property ID2: Int64 read fID2 write fID2;
    property Note: NullableString read fNote write fNote;
  end;

  // #10 Where / SelectRQL / Count entity (extra filterable INTEGER column).
  [MVCTable('ar_ck_rql')]
  TARRqlKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: Integer;
    [MVCTableField('qty')]
    fQty: Integer;
  public
    property A: Integer read fA write fA;
    property B: Integer read fB write fB;
    property Qty: Integer read fQty write fQty;
  end;

  // #11 DeleteAll / DeleteRQL entity.
  [MVCTable('ar_ck_del')]
  TARDelKey = class(TMVCActiveRecord)
  private
    [MVCTableField('a', [foPrimaryKey])]
    fA: Integer;
    [MVCTableField('b', [foPrimaryKey])]
    fB: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property A: Integer read fA write fA;
    property B: Integer read fB write fB;
    property Note: NullableString read fNote write fNote;
  end;

  // #12 PKAsJSONArray escaping: a string PK column carrying quote/backslash.
  [MVCTable('ar_ck_json')]
  TARJsonKey = class(TMVCActiveRecord)
  private
    [MVCTableField('skey', [foPrimaryKey])]
    fSKey: string;
    [MVCTableField('n', [foPrimaryKey])]
    fN: Integer;
    [MVCTableField('note')]
    fNote: NullableString;
  public
    property SKey: string read fSKey write fSKey;
    property N: Integer read fN write fN;
    property Note: NullableString read fNote write fNote;
  end;

  // In-memory UnitOfWork test (no DB): Merge + Apply with Handled:=True.
  [TestFixture]
  TTestUnitOfWorkMerge = class(TObject)
  public
    [Test]
    procedure RegisterUpdateDoesNotTruncateInt64PK;
  end;

const
  _CON_DEF_NAME_SQLITE = 'SQLITECONNECTION';
  _CON_DEF_NAME_FIREBIRD = 'FIREBIRDCONNECTION';
  _CON_DEF_NAME_POSTGRESQL = 'POSTGRESQLCONNECTION';
  _CON_DEF_NAME_MARIADB = 'MARIADBCONNECTION';
  _CON_DEF_NAME_ORACLE = 'ORACLECONNECTION';
  _CON_DEF_NAME_MSSQL = 'MSSQLCONNECTION';

var
  GDBFileName: string = '';
  SQLiteFileName: string = 'sqlitetest.db';
  GDBTemplateFileName: string = '';
  GPGIsInitialized: boolean = False;
  GMariaDBIsInitialized: boolean = False;

procedure TTestUnitOfWorkMerge.RegisterUpdateDoesNotTruncateInt64PK;
var
  lCurrent, lNew: TObjectList<TCustomerWithNullablePK>;
  lDeleted, lUpdated: TList<Int64>;
  lHiPK: Int64;
  lExec: IMVCMultiExecutor<TCustomerWithNullablePK>;
  lE1, lE2, lN1: TCustomerWithNullablePK;
begin
  // Two current rows whose Int64 PKs share the same low 32 bits (5 and 2^32+5).
  // Merge(delete+update): the hi-PK row is updated (must be removed from the
  // delete set); the pk=5 row is not in the new list (must stay deleted).
  // RegisterUpdate must match on the full Int64, not a 32-bit-truncated value.
  lHiPK := Int64($100000000) + 5; // 4294967301
  lCurrent := TObjectList<TCustomerWithNullablePK>.Create(True);
  lNew := TObjectList<TCustomerWithNullablePK>.Create(True);
  lDeleted := TList<Int64>.Create;
  lUpdated := TList<Int64>.Create;
  try
    lE2 := TCustomerWithNullablePK.Create; lE2.ID := 5; lCurrent.Add(lE2);
    lE1 := TCustomerWithNullablePK.Create; lE1.ID := lHiPK; lCurrent.Add(lE1);
    lN1 := TCustomerWithNullablePK.Create; lN1.ID := lHiPK; lNew.Add(lN1);

    lExec := TMVCActiveRecord.Merge<TCustomerWithNullablePK>(lCurrent, lNew);
    lExec.Apply(
      procedure(const Obj: TCustomerWithNullablePK; const Action: TMVCEntityAction; var Handled: Boolean)
      begin
        Handled := True; // never touch a database
        if Action = eaDelete then
          lDeleted.Add(Obj.ID)
        else if Action = eaUpdate then
          lUpdated.Add(Obj.ID);
      end);
    lExec := nil; // release the UnitOfWork (non-owning lists) before freeing entities

    Assert.IsTrue(lUpdated.Contains(lHiPK), 'the hi-PK entity must be scheduled for UPDATE');
    Assert.IsTrue(lDeleted.Contains(5), 'the pk=5 entity (not in new list) must remain scheduled for DELETE');
    Assert.IsFalse(lDeleted.Contains(lHiPK), 'the updated hi-PK entity must NOT also be deleted');
  finally
    lDeleted.Free;
    lUpdated.Free;
    lNew.Free;
    lCurrent.Free;
  end;
end;

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
    Assert.AreEqual<Integer>(1, lID);
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
    Assert.AreEqual<Integer>(3, lCustomer.Rating.Value);
    Assert.AreEqual('note1noteupdated', lCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.Value);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.AreEqual<Integer>(1, lCustomer.ID.Value);
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
    Assert.AreEqual<Integer>(3, lCustomer.Rating.Value);
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
    Assert.AreEqual<Integer>(3, lCustomer.Rating.Value);
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
    Assert.AreEqual<Integer>(1, lID);
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
    Assert.AreEqual<Integer>(3, lCustomer.Rating.Value);
    Assert.AreEqual('note1noteupdated', lCustomer.Note);
    Assert.AreEqual('bit Time Professionals', lCustomer.CompanyName.Value);
    Assert.AreEqual('Rome, IT', lCustomer.City);
    Assert.AreEqual<Integer>(1, lCustomer.ID);
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
    Assert.AreEqual<Integer>(2, lGoodCustomers.Count);
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
    Assert.AreEqual<Integer>(0, lGoodCustomers.Count);
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 20);
  try
    Assert.AreEqual<Integer>(20, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 1);
  try
    Assert.AreEqual<Integer>(1, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, -1);
  try
    Assert.AreEqual<Integer>(240, lCustomers.Count);
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
      lCustomer.GetHistory, 'step1');
    lID := lCustomer.ID;
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetByPK<TCustomerWithLF>(lID);
  try
    Assert.AreEqual('OnBeforeExecuteSQL|OnBeforeLoad|MapDatasetToObject|OnAfterLoad',
      lCustomer.GetHistory, 'step2');
    lCustomer.ClearHistory;
    lCustomer.City := 'XXX';
    lCustomer.Update;
    Assert.AreEqual
      ('OnValidation|OnBeforeUpdate|OnBeforeInsertOrUpdate|OnBeforeExecuteSQL|MapObjectToParams|OnAfterUpdate|OnAfterInsertOrUpdate',
      lCustomer.GetHistory, 'step3');
  finally
    lCustomer.Free;
  end;

  lCustomer := TMVCActiveRecord.GetOneByWhere<TCustomerWithLF>('id = ?', [lID]);
  try
    Assert.AreEqual('OnBeforeLoad|MapDatasetToObject|OnAfterLoad',
      lCustomer.GetHistory, 'step4');
    lCustomer.ClearHistory;
    lCustomer.Delete;
    Assert.AreEqual('OnValidation|OnBeforeDelete|OnBeforeExecuteSQL|MapObjectToParams|OnAfterDelete',
      lCustomer.GetHistory, 'step5');
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

  Assert.AreEqual<Integer>(0, lInserted);
  Assert.AreEqual<Integer>(30, lUpdated);
  Assert.AreEqual<Integer>(0, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual<Integer>(30, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,10)', 1000);
  try
    Assert.AreEqual<Integer>(30, lCustomers.Count);
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

  Assert.AreEqual<Integer>(2, lInserted);
  Assert.AreEqual<Integer>(4, lUpdated);
  Assert.AreEqual<Integer>(26, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual<Integer>(6, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,3)', 1000);
  try
    Assert.AreEqual<Integer>(2, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,2)', 1000);
  try
    Assert.AreEqual<Integer>(2, lCustomers.Count, 'Customers not updated correctly');
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    Assert.AreEqual<Integer>(2, lCustomers.Count);
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

  Assert.AreEqual<Integer>(0, lInserted);
  Assert.AreEqual<Integer>(0, lUpdated);
  Assert.AreEqual<Integer>(30, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual<Integer>(lTotCustomers, lCustomers.Count);
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

  Assert.AreEqual<Integer>(2, lInserted);
  Assert.AreEqual<Integer>(30, lUpdated);
  Assert.AreEqual<Integer>(0, lDeleted);

  lCustomers := TMVCActiveRecord.All<TCustomer>;
  try
    Assert.AreEqual<Integer>(lTotCustomers, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,1)', 1000);
  try
    Assert.AreEqual<Integer>(lTotCustomers - 2, lCustomers.Count, 'Some customer changed when should not change');
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('eq(rating,3)', 1000);
  try
    Assert.AreEqual<Integer>(2, lCustomers.Count, 'Some customer changed when should not change');
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
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
    Assert.AreEqual<Integer>(1, lList.Count);
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

procedure TTestActiveRecordBase.TestNullablesRefreshResetsToNull;
var
  lTest: TNullablesTest;
begin
  TMVCActiveRecord.DeleteAll(TNullablesTest);

  lTest := TNullablesTest.Create;
  try
    lTest.f_int2 := 2; // just the PK, all the other columns stay NULL on the row
    lTest.Insert;
  finally
    lTest.Free;
  end;

  lTest := TMVCActiveRecord.GetFirstByWhere<TNullablesTest>('f_int2 = ?', [2]);
  try
    // dirty the in-memory copy, then discard the edits: a NULL column must
    // clear the nullable, not leave the previous value in place (issue #909)
    lTest.f_int4 := 4;
    lTest.f_int8 := 8;
    lTest.f_string := 'dirty';
    lTest.f_bool := True;
    lTest.f_date := EncodeDate(2020, 02, 01);
    lTest.f_time := EncodeTime(12, 24, 36, 0);
    lTest.f_datetime := Now;
    lTest.f_float4 := 1234.5678;
    lTest.f_float8 := 12345678901234567890.0123456789;
    lTest.Refresh;
    Assert.IsFalse(lTest.f_int4.HasValue, 'f_int4 not cleared by Refresh');
    Assert.IsFalse(lTest.f_int8.HasValue, 'f_int8 not cleared by Refresh');
    Assert.IsFalse(lTest.f_string.HasValue, 'f_string not cleared by Refresh');
    Assert.IsFalse(lTest.f_bool.HasValue, 'f_bool not cleared by Refresh');
    Assert.IsFalse(lTest.f_date.HasValue, 'f_date not cleared by Refresh');
    Assert.IsFalse(lTest.f_time.HasValue, 'f_time not cleared by Refresh');
    Assert.IsFalse(lTest.f_datetime.HasValue, 'f_datetime not cleared by Refresh');
    Assert.IsFalse(lTest.f_float4.HasValue, 'f_float4 not cleared by Refresh');
    Assert.IsFalse(lTest.f_float8.HasValue, 'f_float8 not cleared by Refresh');
  finally
    lTest.Free;
  end;

  TMVCActiveRecord.DeleteAll(TNullablesTest);
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
    Assert.AreEqual<Integer>(1, lRMCustomer.ID.Value);
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
    Assert.AreEqual<Integer>(2, lRomeCustomers.Count);
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
    Assert.AreEqual<Integer>(1, lRomeCustomers.Count);
    Assert.AreEqual('Rome Company 1', lRomeCustomers[0].CompanyName.Value);
  finally
    lRomeCustomers.Free;
  end;

  lRomeCustomers := TMVCActiveRecord.SelectRQL<TRomeBasedCustomer>('lt(Rating,2);sort(+CompanyName)', 10);
  try
    Assert.AreEqual<Integer>(0, lRomeCustomers.Count);
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
    Assert.AreEqual<Integer>(0, lRomeBasedCustomers.Count);
  finally
    lRomeBasedCustomers.Free;
  end;

  lRomeBasedCustomers := TMVCActiveRecord.Where<TRomeBasedCustomer>('description = ?', ['Daniele'], [ftString]);
  try
    Assert.AreEqual<Integer>(1, lRomeBasedCustomers.Count);
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
    Assert.AreEqual<Integer>(1, lID);
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
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
  // Assert.AreEqual<Integer>(240, lCustomers.Count);
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
    for var lCustomer in lCustomers do
    begin
      Assert.IsMatch('^(Rome|London)$', lCustomer.City);
    end;
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 10);
  try
    Assert.AreEqual<Integer>(10, lCustomers.Count);
  finally
    lCustomers.Free;
  end;

  lCustomers := TMVCActiveRecord.SelectRQL<TCustomer>(RQL1, 0);
  try
    Assert.AreEqual<Integer>(0, lCustomers.Count);
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
    Assert.AreEqual<Integer>(140, lCustomers.Count);
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
    Assert.AreEqual<Integer>(1, lCustomers.Count);
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
    Assert.AreEqual<Integer>(240, lCustomers.Count);
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
    Assert.AreEqual<Integer>(1, lID, 'ID should be 1 but it is ' + lID.ToString);
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

procedure TTestActiveRecordBase.TestTryGetNamedQuery;
var
  lTmpSQLQueryWithName: TSQLQueryWithName;
  lTmpRQLQueryWithName: TRQLQueryWithName;
begin
  Assert.IsTrue(TMVCActiveRecord.TryGetSQLQuery<TCustomer>('ByTwoCities', lTmpSQLQueryWithName));
  Assert.AreEqual('ByTwoCities', lTmpSQLQueryWithName.Name);
  Assert.IsNotEmpty(lTmpSQLQueryWithName.SQLText);
  Assert.IsEmpty(lTmpSQLQueryWithName.BackEnd);
  Assert.IsFalse(TMVCActiveRecord.TryGetSQLQuery<TCustomer>('DO_NOT_EXISTS', lTmpSQLQueryWithName));

  Assert.IsTrue(TMVCActiveRecord.TryGetRQLQuery<TCustomer>('CityRomeOrLondon', lTmpRQLQueryWithName));
  Assert.AreEqual('CityRomeOrLondon', lTmpRQLQueryWithName.Name);
  Assert.IsNotEmpty(lTmpRQLQueryWithName.RQLText);
  Assert.IsFalse(TMVCActiveRecord.TryGetRQLQuery<TCustomer>('DO_NOT_EXISTS', lTmpRQLQueryWithName));
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
    Assert.AreEqual<Integer>(1, lID);
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
      lConn: TFDConnection;
    begin
      // ActiveRecordConnectionsRegistry.AddDefaultConnection(TFDConnection.Create(nil), True);
      lConn := TFDConnection.Create(nil);
      ActiveRecordConnectionsRegistry.AddConnection('load', lConn, True);
      try
        lConn.ConnectionDefName := fConDefName;
        ActiveRecordConnectionsRegistry.SetCurrent('load');
        //ActiveRecordConnectionsRegistry.GetCurrent.ConnectionDefName := fConDefName;
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
  { RemoveDefaultConnection raises when the connection is missing, and DUnitX
    attributes a Teardown error to the test that just passed: the report then
    blames a healthy test with a message about a connection. Seen for real on
    2026-08-23, never reproduced since. Leave this behind so the next
    occurrence carries the thread it happened on instead of just a message. }
  if ActiveRecordConnectionsRegistry.GetCurrent(False) = nil then
    LogE(Format('%s.Teardown: no default connection on TID=%d (current name is [%s])',
      [ClassName, TThread.CurrentThread.ThreadID,
       ActiveRecordConnectionsRegistry.GetCurrentConnectionName(False)]));
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

{ TTestActiveRecordMariaDB }

constructor TTestActiveRecordMariaDB.Create;
var
  lHome, lDataDir: String;
begin
  inherited;
  lHome := TPath.Combine(TPath.GetDirectoryName(TPath.GetDirectoryName(ParamStr(0))), 'mariadb');
  lDataDir := TPath.Combine(lHome, 'testdatadir');
  fMariaDBUtil := TMariaDBUtil.Create(lHome, lDataDir, MARIADB_PORT);
end;

destructor TTestActiveRecordMariaDB.Destroy;
begin
  try
    fMariaDBUtil.StopDB;
  except
    // do nothing
  end;
  fMariaDBUtil.Free;
  inherited;
end;

procedure TTestActiveRecordMariaDB.AfterDataLoad;
begin
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers auto_increment = 1');
  TMVCActiveRecord.CurrentConnection.ExecSQL('alter table customers2 auto_increment = 1');
end;

procedure TTestActiveRecordMariaDB.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
  lDriver: IFDStanDefinition;
begin
  { VendorLib points at the bundled client library on purpose: without it
    FireDAC would load whatever libmariadb.dll happens to be on the PATH, and
    the tests would stop being independent from the machine they run on. }
  lDriver := FDManager.DriverDefs.FindDefinition('MySQL');
  if not Assigned(lDriver) then
  begin
    lDriver := FDManager.DriverDefs.Add;
    lDriver.Name := 'MySQL';
    lDriver.AsString['BaseDriverID'] := 'MySQL';
  end;
  lDriver.AsString['VendorLib'] := fMariaDBUtil.ClientLib;
  lDriver.Apply;

  LParams := TStringList.Create;
  try
    LParams.Add('Server=127.0.0.1');
    LParams.Add('Port=' + MARIADB_PORT.ToString);
    LParams.Add('Database=activerecordtest');
    LParams.Add('User_Name=root');
    LParams.Add('CharacterSet=utf8mb4');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(fConDefName, 'MySQL', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordMariaDB.InternalSetupFixture;
begin
  fMariaDBUtil.RemoveDataDir;
  fMariaDBUtil.InitDB;
  fMariaDBUtil.StartDB;
  fMariaDBUtil.CreateDatabase('activerecordtest');
end;

procedure TTestActiveRecordMariaDB.Setup;
var
  lInitDBStructure: boolean;
begin
  LogI('** Setup Test: ' + ClassName);
  lInitDBStructure := False;

  fConDefName := _CON_DEF_NAME_MARIADB;
  if not GMariaDBIsInitialized then
  begin
    FDManager.CloseConnectionDef(_CON_DEF_NAME_MARIADB);
    lInitDBStructure := True;
    InternalSetupFixture;
    GMariaDBIsInitialized := True;
  end;

  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
  end;

  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;
  fConnection.Open;
  if lInitDBStructure then
  begin
    for var lSQL in SQLs_MARIADB do
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

procedure TTestActiveRecordMariaDB.TearDownFixture;
begin
  FDManager.CloseConnectionDef(_CON_DEF_NAME_MARIADB);
  fMariaDBUtil.StopDB;
  GMariaDBIsInitialized := False;
end;

{$IF Defined(TEST_ORACLE)}

{ TTestActiveRecordOracle }

procedure TTestActiveRecordOracle.AfterDataLoad;
begin
  // nothing to reset: the identity columns are recreated with the schema
end;

procedure TTestActiveRecordOracle.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    { EZConnect goes in Database: the Oracle driver has no Server/Port of its
      own. The service name is the one the container publishes. }
    LParams.Add('DriverID=Ora');
    LParams.Add('Database=localhost:' + ORACLE_PORT.ToString + '/FREEPDB1');
    LParams.Add('User_Name=guida');
    LParams.Add('Password=guida');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(fConDefName, 'Ora', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordOracle.Setup;
var
  lSQL: string;
begin
  LogI('** Setup Test: ' + ClassName);
  fConDefName := _CON_DEF_NAME_ORACLE;
  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
  end;
  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;
  fConnection.Open;
  { Oracle has no DROP TABLE IF EXISTS, so every drop is attempted and ignored. }
  for lSQL in ['customers', 'customers2', 'customers_with_code', 'nullables_test',
    '"customers with spaces"', 'booltest', 'customers_with_guid',
    'validated_customers', 'audited_customers'] do
  begin
    try
      fConnection.ExecSQL('drop table ' + lSQL + ' cascade constraints purge');
    except
      // the table was not there
    end;
  end;
  for lSQL in SQLs_ORACLE do
  begin
    fConnection.ExecSQL(lSQL);
  end;
  ActiveRecordConnectionsRegistry.AddDefaultConnection(fConnection);
  AfterDataLoad;
end;
{$ENDIF}

{$IF Defined(TEST_MSSQL)}

{ TTestActiveRecordMSSQL }

procedure TTestActiveRecordMSSQL.AfterDataLoad;
begin
  // nothing to reset: the identity columns are recreated with the schema
end;

procedure TTestActiveRecordMSSQL.CreatePrivateConnDef(AIsPooled: boolean);
var
  LParams: TStringList;
begin
  LParams := TStringList.Create;
  try
    LParams.Add('DriverID=MSSQL');
    LParams.Add('Server=localhost');
    LParams.Add('Port=' + MSSQL_PORT.ToString);
    LParams.Add('Database=guida');
    LParams.Add('User_Name=sa');
    LParams.Add('Password=Masterkey!2026');
    { ODBC Driver 18 encrypts by default and the container serves a
      self-signed certificate. }
    LParams.Add('ODBCAdvanced=TrustServerCertificate=yes');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(fConDefName, 'MSSQL', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordMSSQL.Setup;
var
  lSQL: string;
begin
  LogI('** Setup Test: ' + ClassName);
  fConDefName := _CON_DEF_NAME_MSSQL;
  if FDManager.ConnectionDefs.FindConnectionDef(fConDefName) = nil then
  begin
    CreatePrivateConnDef(True);
  end;
  fConnection := TFDConnection.Create(nil);
  fConnection.ConnectionDefName := fConDefName;
  fConnection.Open;
  for lSQL in ['customers', 'customers2', 'customers_with_code', 'nullables_test',
    '"customers with spaces"', 'booltest', 'customers_with_guid',
    'validated_customers', 'audited_customers'] do
  begin
    fConnection.ExecSQL('drop table if exists ' + lSQL);
  end;
  for lSQL in SQLs_MSSQL do
  begin
    fConnection.ExecSQL(lSQL);
  end;
  ActiveRecordConnectionsRegistry.AddDefaultConnection(fConnection);
  AfterDataLoad;
end;
{$ENDIF}

// ===========================================================================
// Validation / Audit tests
//
// These tests exercise the attribute-based validators and the MVCAudit*
// attributes added on top of TMVCActiveRecord. They are declared on the
// shared base class so they run across SQLite, Firebird and PostgreSQL.
// ===========================================================================

function BuildValidValidatedCustomer: TValidatedCustomer;
begin
  Result := TValidatedCustomer.Create;
  Result.Description := 'Test';
  Result.Email := 'user@example.com';
  Result.Rating := 3;
end;

procedure TTestActiveRecordBase.TestValidation_InsertFailsWhenDescriptionEmpty;
var
  lCust: TValidatedCustomer;
begin
  lCust := TValidatedCustomer.Create;
  try
    // Description left null on purpose: MVCRequired must report it as missing
    lCust.Email := 'user@example.com';
    lCust.Rating := 3;
    Assert.WillRaise(
      procedure
      begin
        lCust.Insert;
      end,
      EMVCStorageValidationException);
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_InsertFailsWhenDescriptionTooLong;
var
  lCust: TValidatedCustomer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Description := 'this string is definitely longer than ten chars';
    Assert.WillRaise(
      procedure
      begin
        lCust.Insert;
      end,
      EMVCStorageValidationException);
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_InsertFailsWhenEmailInvalid;
var
  lCust: TValidatedCustomer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Email := 'not-an-email';
    Assert.WillRaise(
      procedure
      begin
        lCust.Insert;
      end,
      EMVCStorageValidationException);
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_InsertFailsWhenRatingOutOfRange;
var
  lCust: TValidatedCustomer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Rating := 99;
    Assert.WillRaise(
      procedure
      begin
        lCust.Insert;
      end,
      EMVCStorageValidationException);
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_ExceptionMessageListsFailingFields;
var
  lCust: TValidatedCustomer;
  lCaught: Boolean;
begin
  // A single glance at e.Message should reveal WHICH fields failed, without
  // forcing the caller to iterate ValidationErrors. We set two invalid
  // fields (Email and Rating) on an otherwise valid entity and look at the
  // resulting exception text.
  lCaught := False;
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Email := 'not-an-email';
    lCust.Rating := 99;
    try
      lCust.Validate(eaCreate);
    except
      on E: EMVCStorageValidationException do
      begin
        lCaught := True;
        Assert.Contains(E.Message, 'Email',
          'Exception message should mention the failing "Email" field');
        Assert.Contains(E.Message, 'Rating',
          'Exception message should mention the failing "Rating" field');
      end;
    end;
    Assert.IsTrue(lCaught, 'EMVCStorageValidationException was expected but was not raised');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_InsertSucceedsWhenAllValid;
var
  lCust: TValidatedCustomer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Insert;
    Assert.IsTrue(lCust.ID.HasValue, 'Expected autogenerated ID to be set after Insert');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_UpdateFailsWhenInvalid;
var
  lCust: TValidatedCustomer;
  lId: Integer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Insert;
    lId := lCust.ID.Value;
  finally
    lCust.Free;
  end;

  lCust := TMVCActiveRecord.GetByPK<TValidatedCustomer>(lId);
  try
    lCust.Email := 'still-not-an-email';
    Assert.WillRaise(
      procedure
      begin
        lCust.Update;
      end,
      EMVCStorageValidationException);
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_ValidateMethodCanBeCalledDirectly;
var
  lCust: TValidatedCustomer;
begin
  lCust := BuildValidValidatedCustomer;
  try
    lCust.Email := 'invalid';
    Assert.WillRaise(
      procedure
      begin
        lCust.Validate(eaCreate);
      end,
      EMVCStorageValidationException,
      'Direct call to Validate should raise with bad email');

    lCust.Email := 'ok@example.com';
    lCust.Validate(eaCreate); // must not raise
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_PKRequiredSkippedOnInsert;
var
  lCust: TValidatedCustomer;
begin
  // The PK "fID" carries MVCRequired intentionally. On INSERT with foAutoGenerated
  // the validator MUST be skipped; on UPDATE it would still fail if ID is null,
  // but we only assert the insert-path here.
  lCust := BuildValidValidatedCustomer;
  try
    Assert.IsFalse(lCust.ID.HasValue, 'ID must be null before Insert');
    lCust.Insert;
    Assert.IsTrue(lCust.ID.HasValue,
      'Insert must succeed and autogenerate ID even though MVCRequired decorates the PK');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestValidation_OnValidateMethodIsInvoked;
var
  lCust: TValidatedCustomerWithOnValidate;
begin
  lCust := TValidatedCustomerWithOnValidate.Create;
  try
    lCust.Description := 'FORBIDDEN';
    lCust.Email := 'user@example.com';
    Assert.WillRaise(
      procedure
      begin
        lCust.Validate(eaCreate);
      end,
      EMVCStorageValidationException,
      'OnValidate override must emit an error for the forbidden value');

    lCust.Description := 'OK';
    lCust.Validate(eaCreate); // must not raise - OnValidate produces no error
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_CreatedAtFilledOnInsert;
var
  lCust: TAuditedCustomer;
  lBefore, lAfter: TDateTime;
begin
  lCust := TAuditedCustomer.Create;
  try
    lCust.Description := 'Some description';
    lBefore := Now - (1 / 86400);
    lCust.Insert;
    lAfter := Now + (1 / 86400);
    Assert.IsTrue(lCust.CreatedAt.HasValue, 'CreatedAt must be set after Insert');
    Assert.IsTrue((lCust.CreatedAt.Value >= lBefore) and (lCust.CreatedAt.Value <= lAfter),
      'CreatedAt must be within the insert window');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_UpdatedAtMatchesCreatedAtOnInsert;
var
  lCust: TAuditedCustomer;
begin
  lCust := TAuditedCustomer.Create;
  try
    lCust.Description := 'Some description';
    lCust.Insert;
    Assert.IsTrue(lCust.UpdatedAt.HasValue, 'UpdatedAt must be set after Insert');
    Assert.IsTrue(SameDateTime(lCust.CreatedAt.Value, lCust.UpdatedAt.Value),
      'UpdatedAt must equal CreatedAt on Insert');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_UpdatedAtChangesOnUpdate;
var
  lCust: TAuditedCustomer;
  lId: Integer;
  lInsertedAt: TDateTime;
begin
  lCust := TAuditedCustomer.Create;
  try
    lCust.Description := 'Some description';
    lCust.Insert;
    lId := lCust.ID.Value;
    lInsertedAt := lCust.UpdatedAt.Value;
  finally
    lCust.Free;
  end;

  Sleep(1100); // ensure the clock has advanced past one-second TIMESTAMP granularity

  lCust := TMVCActiveRecord.GetByPK<TAuditedCustomer>(lId);
  try
    lCust.Description := 'Changed';
    lCust.Update;
    Assert.IsTrue(lCust.UpdatedAt.Value > lInsertedAt,
      'UpdatedAt must be refreshed on Update');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_CreatedAtUnchangedOnUpdate;
var
  lCust: TAuditedCustomer;
  lId: Integer;
  lOriginalCreatedAt: TDateTime;
begin
  lCust := TAuditedCustomer.Create;
  try
    lCust.Description := 'Some description';
    lCust.Insert;
    lId := lCust.ID.Value;
    lOriginalCreatedAt := lCust.CreatedAt.Value;
  finally
    lCust.Free;
  end;

  Sleep(1100);

  lCust := TMVCActiveRecord.GetByPK<TAuditedCustomer>(lId);
  try
    lCust.Description := 'Changed';
    lCust.Update;
    Assert.IsTrue(SameDateTime(lCust.CreatedAt.Value, lOriginalCreatedAt),
      'CreatedAt must NOT change on Update');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_CreatedByFromThreadLocal;
var
  lCust: TAuditedCustomer;
begin
  TMVCActiveRecord.SetCurrentUser('alice');
  try
    lCust := TAuditedCustomer.Create;
    try
      lCust.Description := 'with user';
      lCust.Insert;
      Assert.AreEqual('alice', String(lCust.CreatedBy),
        'CreatedBy must come from the per-thread user');
      Assert.AreEqual('alice', String(lCust.UpdatedBy),
        'UpdatedBy must also be set on Insert');
    finally
      lCust.Free;
    end;
  finally
    TMVCActiveRecord.SetCurrentUser('');
  end;
end;

procedure TTestActiveRecordBase.TestAudit_UpdatedByFollowsThreadUser;
var
  lCust: TAuditedCustomer;
  lId: Integer;
begin
  // First Insert under user "bob", then Update under user "carol": UpdatedBy
  // must reflect the user active on the THREAD AT THE MOMENT of the save,
  // while CreatedBy must remain the one captured at Insert time.
  TMVCActiveRecord.SetCurrentUser('bob');
  try
    lCust := TAuditedCustomer.Create;
    try
      lCust.Description := 'created by bob';
      lCust.Insert;
      lId := lCust.ID.Value;
      Assert.AreEqual('bob', String(lCust.CreatedBy));
      Assert.AreEqual('bob', String(lCust.UpdatedBy));
    finally
      lCust.Free;
    end;

    TMVCActiveRecord.SetCurrentUser('carol');
    lCust := TMVCActiveRecord.GetByPK<TAuditedCustomer>(lId);
    try
      lCust.Description := 'touched by carol';
      lCust.Update;
      Assert.AreEqual('bob', String(lCust.CreatedBy),
        'CreatedBy must NOT change on Update');
      Assert.AreEqual('carol', String(lCust.UpdatedBy),
        'UpdatedBy must follow the thread-local user at Update time');
    finally
      lCust.Free;
    end;
  finally
    TMVCActiveRecord.SetCurrentUser('');
  end;
end;

procedure TTestActiveRecordBase.TestAudit_NoUserLeavesFieldsNull;
var
  lCust: TAuditedCustomer;
begin
  TMVCActiveRecord.SetCurrentUser('');
  lCust := TAuditedCustomer.Create;
  try
    lCust.Description := 'no user';
    lCust.Insert;
    Assert.IsFalse(lCust.CreatedBy.HasValue,
      'When no user is set on this thread, *_by columns must remain null');
    Assert.IsFalse(lCust.UpdatedBy.HasValue,
      'When no user is set on this thread, *_by columns must remain null');
    // Dates are still filled (they don't need a user)
    Assert.IsTrue(lCust.CreatedAt.HasValue, 'CreatedAt must still be filled');
  finally
    lCust.Free;
  end;
end;

procedure TTestActiveRecordBase.TestAudit_WrongDateTimeFieldTypeFailsFast;
begin
  // The failure must happen the first time the entity is instantiated
  // (inside the constructor's InitTableInfo), NOT later on Insert/Update.
  Assert.WillRaise(
    procedure
    var
      lBad: TBadAuditTimeType;
    begin
      lBad := TBadAuditTimeType.Create;
      lBad.Free;
    end,
    EMVCActiveRecord,
    'MVCAuditCreatedAt on a NullableTTime field must raise at InitTableInfo time');
end;

procedure TTestActiveRecordBase.TestAudit_WrongStringFieldTypeFailsFast;
begin
  Assert.WillRaise(
    procedure
    var
      lBad: TBadAuditByType;
    begin
      lBad := TBadAuditByType.Create;
      lBad.Free;
    end,
    EMVCActiveRecord,
    'MVCAuditCreatedBy on an Integer field must raise at InitTableInfo time');
end;

procedure TTestActiveRecordBase.TestAudit_CurrentUserIsThreadIsolated;
var
  lMain: string;
  lWorker: string;
  lEvent: TEvent;
  lTask: ITask;
begin
  // Thread MAIN sets user "pippo", thread WORKER sets user "pluto" at the
  // same time. After the worker has set its value, MAIN re-reads its own:
  // it must still be "pippo". No lock, no global, no bleed-through.
  TMVCActiveRecord.SetCurrentUser('');
  lEvent := TEvent.Create(nil, True, False, '');
  try
    TMVCActiveRecord.SetCurrentUser('pippo');

    lTask := TTask.Run(
      procedure
      begin
        TMVCActiveRecord.SetCurrentUser('pluto');
        lWorker := TMVCActiveRecord.GetCurrentUser;
        lEvent.SetEvent;
      end);
    lEvent.WaitFor(INFINITE);

    lMain := TMVCActiveRecord.GetCurrentUser;

    Assert.AreEqual('pippo', lMain,
      'Main thread must keep its own user after the worker changed its own');
    Assert.AreEqual('pluto', lWorker,
      'Worker thread must see the user it set, independent of the main thread');

    lTask.Wait;
  finally
    lEvent.Free;
    TMVCActiveRecord.SetCurrentUser('');
  end;
end;

procedure TTestActiveRecordBase.TestSelectUnidirectionalDataSetSignature;

  procedure AssertBackwardScrollIsForbidden(const ADS: TDataSet; const AMsg: string);
  var
    lRaised: Boolean;
  begin
    { A truly unidirectional dataset cannot scroll backwards: once the
      cursor has advanced, calling Prior must raise. Prior is used here
      instead of First because FireDAC silently tolerates First on a
      unidirectional cursor (no-op), while Prior unambiguously requires
      backward scrolling and is rejected. }
    ADS.Next;
    lRaised := False;
    try
      ADS.Prior;
    except
      lRaised := True;
    end;
    Assert.IsTrue(lRaised, AMsg);
  end;

var
  lDS: TDataSet;
begin
  { Signature tripwire for SelectUnidirectionalDataSet. Exercises both
    overloads, checks IsUniDirectional, and proves the dataset is really
    forward-only by showing that First raises after scrolling. }

  { Seed a few rows so the cursor can actually advance. }
  CreateACustomer('UT-Unidirectional-1', 1);
  CreateACustomer('UT-Unidirectional-2', 2);
  CreateACustomer('UT-Unidirectional-3', 3);

  { Overload 1: SQL + Params }
  lDS := TMVCActiveRecord.SelectUnidirectionalDataSet(
    'SELECT id FROM customers ORDER BY id', []);
  try
    Assert.IsTrue(lDS.IsUniDirectional,
      'Overload 1 must return an unidirectional dataset');
    AssertBackwardScrollIsForbidden(lDS,
      'Overload 1: Prior must raise on a unidirectional dataset after scrolling');
  finally
    lDS.Free;
  end;

  { Overload 2: SQL + Params + ParamTypes }
  lDS := TMVCActiveRecord.SelectUnidirectionalDataSet(
    'SELECT id FROM customers WHERE rating >= ? ORDER BY id', [1], [ftInteger]);
  try
    Assert.IsTrue(lDS.IsUniDirectional,
      'Overload 2 must return an unidirectional dataset');
    AssertBackwardScrollIsForbidden(lDS,
      'Overload 2: Prior must raise on a unidirectional dataset after scrolling');
  finally
    lDS.Free;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_CRUD;
var
  lE, lLoaded: TARUserRole;
begin
  // Portable two-column-PK table (SQLite / Firebird / PostgreSQL). Created and
  // dropped by the test so it never touches the shared schema.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_user_roles');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_user_roles (user_id INTEGER NOT NULL, role_id INTEGER NOT NULL, ' +
    'note VARCHAR(200), PRIMARY KEY(user_id, role_id))');
  try
    lE := TARUserRole.Create;
    try
      lE.UserID := 1; lE.RoleID := 42; lE.Note := 'admin'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARUserRole.Create;
    try
      lE.UserID := 1; lE.RoleID := 43; lE.Note := 'editor'; lE.Insert;
    finally
      lE.Free;
    end;

    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARUserRole>(),
      'two composite-key rows inserted');

    // GetByPKs (found): both key columns must match
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([1, 42]);
    try
      Assert.AreEqual(1, lLoaded.UserID);
      Assert.AreEqual(42, lLoaded.RoleID);
      Assert.AreEqual('admin', lLoaded.Note.Value);
    finally
      lLoaded.Free;
    end;

    // GetByPKs (missing key, RaiseExceptionIfNotFound = False)
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([9, 9], False);
    Assert.IsTrue(lLoaded = nil, 'GetByPKs must return nil for a missing composite key');

    // LoadByPKs + Update
    lLoaded := TARUserRole.Create;
    try
      Assert.IsTrue(lLoaded.LoadByPKs([1, 43]), 'LoadByPKs finds the row');
      lLoaded.Note := 'writer';
      lLoaded.Update;
    finally
      lLoaded.Free;
    end;
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([1, 43]);
    try
      Assert.AreEqual('writer', lLoaded.Note.Value, 'Update persisted on the composite-key row');
    finally
      lLoaded.Free;
    end;

    // Delete addresses both key columns: the sibling (1,43) must survive
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([1, 42]);
    try
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;
    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARUserRole>(),
      'only the addressed composite-key row was deleted');
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([1, 42], False);
    Assert.IsTrue(lLoaded = nil, 'the deleted row is gone');
    lLoaded := TMVCActiveRecord.GetByPKs<TARUserRole>([1, 43], False);
    Assert.IsTrue(lLoaded <> nil, 'the sibling row is untouched');
    lLoaded.Free;

    // Order-independent reload: set the key by PROPERTY NAME (deliberately out of
    // declaration order), then Load. No positional array involved.
    lLoaded := TARUserRole.Create;
    try
      lLoaded.RoleID := 43;
      lLoaded.UserID := 1;
      Assert.IsTrue(lLoaded.Load, 'Load returns True on a hit');
      Assert.AreEqual('writer', lLoaded.Note.Value, 'Load populated by the current composite key');
      // Refresh discards a local-only edit and re-syncs with the DB row.
      lLoaded.Note := 'local-only';
      Assert.IsTrue(lLoaded.Refresh, 'Refresh returns True on a hit');
      Assert.AreEqual('writer', lLoaded.Note.Value, 'Refresh reloaded the composite-key row');

      // Fail-loud: a composite key that matches no row must raise, never sit stale.
      lLoaded.RoleID := 999;
      Assert.IsFalse(lLoaded.Load(False), 'Load(False) returns False on a missing row');
      Assert.IsFalse(lLoaded.Refresh(False), 'Refresh(False) returns False on a missing row');
      Assert.WillRaise(
        procedure begin lLoaded.Load end,
        EMVCActiveRecordNotFound, 'Load raises on a missing composite key');
      Assert.WillRaise(
        procedure begin lLoaded.Refresh end,
        EMVCActiveRecordNotFound, 'Refresh raises on a missing composite key');
    finally
      lLoaded.Free;
    end;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_user_roles');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_Repository;
var
  lRepo: IMVCRepository<TARRepoRole>;
  lE, lLoaded: TARRepoRole;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_repo_roles');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_repo_roles (user_id INTEGER NOT NULL, role_id INTEGER NOT NULL, ' +
    'note VARCHAR(200), PRIMARY KEY(user_id, role_id))');
  try
    lRepo := TMVCRepository<TARRepoRole>.Create;

    lE := TARRepoRole.Create;
    try
      lE.UserID := 1; lE.RoleID := 42; lE.Note := 'admin';
      lRepo.Insert(lE);
    finally
      lE.Free;
    end;

    Assert.IsTrue(lRepo.Exists([1, 42]), 'Exists finds the composite-key row');
    Assert.IsFalse(lRepo.Exists([9, 9]), 'Exists returns false for a missing composite key');

    lLoaded := lRepo.GetByPKs([1, 42]);
    try
      Assert.AreEqual('admin', lLoaded.Note.Value);
      lRepo.Delete(lLoaded);
    finally
      lLoaded.Free;
    end;

    Assert.IsFalse(lRepo.Exists([1, 42]), 'row deleted through the repository');

    // Single-value repo lookups steer to the *PKs variants, like the AR layer.
    Assert.WillRaise(procedure begin lRepo.GetByPK(Int64(1)) end, EMVCActiveRecord,
      'repository GetByPK(scalar) must raise on a composite-key entity');
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_repo_roles');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_ThreeColumn;
var
  lE, lLoaded: TARTriple;
begin
  // Three-column composite PK (INTEGER, INTEGER, VARCHAR). Portable on all 3 DBs.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_triple');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_triple (tenant_id INTEGER NOT NULL, dept_id INTEGER NOT NULL, ' +
    'code VARCHAR(30) NOT NULL, descr VARCHAR(200), PRIMARY KEY(tenant_id, dept_id, code))');
  try
    lE := TARTriple.Create;
    try
      lE.TenantID := 1; lE.DeptID := 10; lE.Code := 'A'; lE.Descr := 'first'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARTriple.Create;
    try
      lE.TenantID := 1; lE.DeptID := 10; lE.Code := 'B'; lE.Descr := 'second'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARTriple.Create;
    try
      lE.TenantID := 1; lE.DeptID := 20; lE.Code := 'A'; lE.Descr := 'third'; lE.Insert;
    finally
      lE.Free;
    end;

    Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TARTriple>(), 'three 3-column-key rows inserted');

    // GetByPKs round-trip: all three columns must match precisely.
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 10, 'B']);
    try
      Assert.AreEqual(1, lLoaded.TenantID);
      Assert.AreEqual(10, lLoaded.DeptID);
      Assert.AreEqual('B', lLoaded.Code);
      Assert.AreEqual('second', lLoaded.Descr.Value);
    finally
      lLoaded.Free;
    end;

    // LoadByPKs + Update a non-key field.
    lLoaded := TARTriple.Create;
    try
      Assert.IsTrue(lLoaded.LoadByPKs([1, 20, 'A']), 'LoadByPKs finds the 3-column key');
      lLoaded.Descr := 'third-updated';
      lLoaded.Update;
    finally
      lLoaded.Free;
    end;
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 20, 'A']);
    try
      Assert.AreEqual('third-updated', lLoaded.Descr.Value, 'Update persisted on the 3-column-key row');
    finally
      lLoaded.Free;
    end;

    // Delete exactly one: the two rows sharing (1,10,*) and (1,*,A) must survive
    // except the one addressed.
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 10, 'A']);
    try
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;
    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARTriple>(), 'only the addressed 3-column row was deleted');
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 10, 'A'], False);
    Assert.IsTrue(lLoaded = nil, 'the deleted 3-column row is gone');
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 10, 'B'], False);
    Assert.IsTrue(lLoaded <> nil, 'sibling (1,10,B) survives');
    lLoaded.Free;
    lLoaded := TMVCActiveRecord.GetByPKs<TARTriple>([1, 20, 'A'], False);
    Assert.IsTrue(lLoaded <> nil, 'sibling (1,20,A) survives');
    lLoaded.Free;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_triple');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_GUIDColumn;
var
  lE, lLoaded: TARGuidKey;
  lGUID: TGUID;
  lGuidColType: string;
begin
  // GUID column inside a composite key (GUID + INTEGER). The GUID column type is
  // DB-specific: PostgreSQL has a native UUID type; SQLite/Firebird store it as
  // text (mirror TCustomerWithGUID in BOs.pas). GUID values cross array-of-const
  // as their string form (a TGUID record cannot be a TVarRec).
  if ActiveRecordConnectionsRegistry.GetCurrentBackend = TMVCActiveRecordBackEnd.PostgreSQL then
    lGuidColType := 'UUID'
  else
    lGuidColType := 'VARCHAR(38)';
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_guid');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_guid (gid ' + lGuidColType + ' NOT NULL, seq INTEGER NOT NULL, ' +
    'payload VARCHAR(200), PRIMARY KEY(gid, seq))');
  try
    lGUID := StringToGUID('{2C3A1E90-5B77-4E2B-9E0A-1234567890AB}');
    lE := TARGuidKey.Create;
    try
      lE.GID := lGUID; lE.Seq := 1; lE.Payload := 'g1'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARGuidKey.Create;
    try
      lE.GID := lGUID; lE.Seq := 2; lE.Payload := 'g2'; lE.Insert;
    finally
      lE.Free;
    end;

    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARGuidKey>());

    // GetByPKs([theGuid, n]): the GUID goes across as a string (its ToString form).
    lLoaded := TMVCActiveRecord.GetByPKs<TARGuidKey>([lGUID.ToString, 2]);
    try
      Assert.AreEqual(lGUID.ToString, lLoaded.GID.Value.ToString, 'GUID PK round-trips intact');
      Assert.AreEqual(2, lLoaded.Seq);
      Assert.AreEqual('g2', lLoaded.Payload.Value);
    finally
      lLoaded.Free;
    end;

    // LoadByPKs + Delete one sibling; the other (same GUID, other seq) survives.
    lLoaded := TARGuidKey.Create;
    try
      Assert.IsTrue(lLoaded.LoadByPKs([lGUID.ToString, 1]));
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;
    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARGuidKey>());
    lLoaded := TMVCActiveRecord.GetByPKs<TARGuidKey>([lGUID.ToString, 1], False);
    Assert.IsTrue(lLoaded = nil, 'deleted (guid,1) is gone');
    lLoaded := TMVCActiveRecord.GetByPKs<TARGuidKey>([lGUID.ToString, 2], False);
    Assert.IsTrue(lLoaded <> nil, 'sibling (guid,2) survives');
    lLoaded.Free;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_guid');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_NullableColumns;
var
  lE, lLoaded: TARNullKey;
begin
  // Both PK columns are Nullable (NullableInt64 + NullableString) with values set.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_null');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_null (k1 BIGINT NOT NULL, k2 VARCHAR(30) NOT NULL, ' +
    'val VARCHAR(200), PRIMARY KEY(k1, k2))');
  try
    lE := TARNullKey.Create;
    try
      lE.K1 := Int64(100); lE.K2 := 'x'; lE.Val := 'first'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARNullKey.Create;
    try
      lE.K1 := Int64(100); lE.K2 := 'y'; lE.Val := 'second'; lE.Insert;
    finally
      lE.Free;
    end;

    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARNullKey>());

    lLoaded := TMVCActiveRecord.GetByPKs<TARNullKey>([100, 'x']);
    try
      Assert.AreEqual(Int64(100), lLoaded.K1.Value);
      Assert.AreEqual('x', lLoaded.K2.Value);
      Assert.AreEqual('first', lLoaded.Val.Value);
      lLoaded.Val := 'first-updated';
      lLoaded.Update;
    finally
      lLoaded.Free;
    end;
    lLoaded := TMVCActiveRecord.GetByPKs<TARNullKey>([100, 'x']);
    try
      Assert.AreEqual('first-updated', lLoaded.Val.Value, 'Update persisted on the nullable-key row');
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;
    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARNullKey>(), 'one nullable-key row deleted');
    lLoaded := TMVCActiveRecord.GetByPKs<TARNullKey>([100, 'y'], False);
    Assert.IsTrue(lLoaded <> nil, 'sibling nullable-key row survives');
    lLoaded.Free;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_null');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_VersionColumn;
var
  lE, lStale: TARVerKey;
  lFresh: TARVerKey;
  lV1, lV2: Integer;
begin
  // foVersion optimistic locking on a composite-PK entity: Update bumps the
  // version; a stale in-memory version must be rejected on Update.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_ver');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_ver (a INTEGER NOT NULL, b INTEGER NOT NULL, note VARCHAR(200), ' +
    'objversion INTEGER NOT NULL, PRIMARY KEY(a, b))');
  try
    lE := TARVerKey.Create;
    try
      lE.A := 1; lE.B := 2; lE.Note := 'v0'; lE.Insert;
      lV1 := lE.ObjVersion;
    finally
      lE.Free;
    end;

    // Load, update once: version must increment.
    lFresh := TMVCActiveRecord.GetByPKs<TARVerKey>([1, 2]);
    try
      lFresh.Note := 'v1';
      lFresh.Update;
      lV2 := lFresh.ObjVersion;
    finally
      lFresh.Free;
    end;
    Assert.IsTrue(lV2 > lV1, 'foVersion must increment on Update (composite PK): ' +
      IntToStr(lV1) + ' -> ' + IntToStr(lV2));

    // A stale copy still holding the old version must fail its Update.
    lStale := TMVCActiveRecord.GetByPKs<TARVerKey>([1, 2]);
    try
      // Force the in-memory version back to the pre-update value.
      lStale.ObjVersion := lV1;
      lStale.Note := 'stale-write';
      Assert.WillRaise(
        procedure begin lStale.Update end,
        EMVCActiveRecordVersionedItemNotFound,
        'a stale-version Update on a composite-PK entity must be rejected');
    finally
      lStale.Free;
    end;

    // The rejected write must not have changed the row.
    lFresh := TMVCActiveRecord.GetByPKs<TARVerKey>([1, 2]);
    try
      Assert.AreEqual('v1', lFresh.Note.Value, 'stale write must not have persisted');
    finally
      lFresh.Free;
    end;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_ver');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_DuplicateKeyRaises;
var
  lE: TARDupKey;
begin
  // Inserting a second row with the same full composite key must raise (PK/unique
  // violation from the DB). The concrete exception class is driver-specific, so
  // assert only that some exception is raised.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_dup');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_dup (a INTEGER NOT NULL, b INTEGER NOT NULL, note VARCHAR(200), ' +
    'PRIMARY KEY(a, b))');
  try
    lE := TARDupKey.Create;
    try
      lE.A := 1; lE.B := 1; lE.Note := 'orig'; lE.Insert;
    finally
      lE.Free;
    end;
    Assert.WillRaise(
      procedure
      var lDup: TARDupKey;
      begin
        lDup := TARDupKey.Create;
        try
          lDup.A := 1; lDup.B := 1; lDup.Note := 'dup'; lDup.Insert;
        finally
          lDup.Free;
        end;
      end,
      nil,
      'inserting a duplicate composite key must raise');
    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARDupKey>(), 'the duplicate was not persisted');
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_dup');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_NotFoundBehavior;
var
  lLoaded: TARNfKey;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_nf');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_nf (a INTEGER NOT NULL, b INTEGER NOT NULL, note VARCHAR(200), ' +
    'PRIMARY KEY(a, b))');
  try
    // RaiseExceptionIfNotFound = True (default) on a missing key.
    Assert.WillRaise(
      procedure begin TMVCActiveRecord.GetByPKs<TARNfKey>([7, 8]).Free end,
      EMVCActiveRecordNotFound,
      'GetByPKs must raise EMVCActiveRecordNotFound for a missing composite key');
    // RaiseExceptionIfNotFound = False returns nil.
    lLoaded := TMVCActiveRecord.GetByPKs<TARNfKey>([7, 8], False);
    Assert.IsTrue(lLoaded = nil, 'GetByPKs(..., False) returns nil for a missing composite key');
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_nf');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_WrongValueCountRaises;
begin
  // Value-count guards fire from the RTTI-built table map before any DB access,
  // so no table is needed. TARTriple has THREE PK columns.
  // GetByPKs<T> creates the instance then LoadByPKs, which raises on a count mismatch.
  Assert.WillRaise(
    procedure begin TMVCActiveRecord.GetByPKs<TARTriple>([1, 2]).Free end,
    EMVCActiveRecord,
    'GetByPKs with too few values must raise');
  Assert.WillRaise(
    procedure begin TMVCActiveRecord.GetByPKs<TARTriple>([1, 2, 'A', 99]).Free end,
    EMVCActiveRecord,
    'GetByPKs with too many values must raise');

  Assert.WillRaise(
    procedure
    var lE: TARTriple;
    begin
      lE := TARTriple.Create;
      try
        lE.LoadByPKs([1, 2]); // 2 values, 3 columns
      finally
        lE.Free;
      end;
    end,
    EMVCActiveRecord,
    'LoadByPKs with too few values must raise');

  Assert.WillRaise(
    procedure
    var lE: TARTriple;
    begin
      lE := TARTriple.Create;
      try
        lE.SetPKs([1, 2, 'A', 99]); // 4 values, 3 columns
      finally
        lE.Free;
      end;
    end,
    EMVCActiveRecord,
    'SetPKs with too many values must raise');
end;

procedure TTestActiveRecordBase.TestCompositePK_GetSetPKsRoundTrip;
var
  lE, lLoaded: TARPksKey;
  lPKs: TArray<TValue>;
begin
  // SetPKs then GetPKs (no DB), then GetPKs after a real LoadByPKs equals the key.
  // Heterogeneous key: INTEGER + VARCHAR.
  lE := TARPksKey.Create;
  try
    lE.SetPKs([5, 'hello']);
    lPKs := lE.GetPKs;
    // Integer(): on Win64 Length() of a dynamic array is NativeInt (Int64), which
    // would make Assert.AreEqual fail to unify with the Integer literal.
    Assert.AreEqual(2, Integer(Length(lPKs)), 'GetPKs returns one value per PK column');
    Assert.AreEqual(5, lPKs[0].AsInteger, 'GetPKs[0] matches SetPKs');
    Assert.AreEqual('hello', lPKs[1].AsString, 'GetPKs[1] matches SetPKs');
    Assert.AreEqual(5, lE.A, 'SetPKs wrote the integer PK field');
    Assert.AreEqual('hello', lE.B, 'SetPKs wrote the string PK field');
  finally
    lE.Free;
  end;

  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_pks');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_pks (a INTEGER NOT NULL, b VARCHAR(30) NOT NULL, note VARCHAR(200), ' +
    'PRIMARY KEY(a, b))');
  try
    lE := TARPksKey.Create;
    try
      lE.A := 5; lE.B := 'hello'; lE.Note := 'row'; lE.Insert;
    finally
      lE.Free;
    end;

    lLoaded := TARPksKey.Create;
    try
      Assert.IsTrue(lLoaded.LoadByPKs([5, 'hello']), 'LoadByPKs finds the heterogeneous key');
      lPKs := lLoaded.GetPKs;
      Assert.AreEqual(5, lPKs[0].AsInteger, 'GetPKs after LoadByPKs equals key col 0');
      Assert.AreEqual('hello', lPKs[1].AsString, 'GetPKs after LoadByPKs equals key col 1');
    finally
      lLoaded.Free;
    end;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_pks');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_TwoAutogenRejected;
begin
  // An entity declaring two [foPrimaryKey, foAutoGenerated] columns is illegal.
  // The table map is built on first use (here, at Create), so instantiation must
  // raise. DB-independent: the guard fires during map construction, before any I/O.
  Assert.WillRaise(
    procedure begin TARTwoAutogen.Create.Free end,
    EMVCActiveRecord,
    'an entity with two auto-generated PK columns must be rejected at map build');
end;

procedure TTestActiveRecordBase.TestCompositePK_WhereAndRQL;
var
  lList: TObjectList<TARRqlKey>;
  lE: TARRqlKey;
  I: Integer;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_rql');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_rql (a INTEGER NOT NULL, b INTEGER NOT NULL, qty INTEGER NOT NULL, ' +
    'PRIMARY KEY(a, b))');
  try
    for I := 1 to 5 do
    begin
      lE := TARRqlKey.Create;
      try
        lE.A := 1; lE.B := I; lE.Qty := I * 10; lE.Insert;
      finally
        lE.Free;
      end;
    end;

    // Where: parameterized SQL filter round-trips the whole composite entity.
    lList := TMVCActiveRecord.Where<TARRqlKey>('qty >= ?', [30]);
    try
      Assert.AreEqual<Integer>(3, lList.Count, 'Where returns rows qty>=30 (b in 3,4,5)');
      // Every returned entity must carry both PK columns populated by the SELECT.
      for lE in lList do
      begin
        Assert.AreEqual(1, lE.A, 'PK column a materialized from SELECT');
        Assert.IsTrue(lE.B >= 3, 'PK column b materialized from SELECT');
      end;
    finally
      lList.Free;
    end;

    // SelectRQL + Count with an RQL filter over a composite entity.
    lList := TMVCActiveRecord.SelectRQL<TARRqlKey>('gt(qty,20)', MAXINT);
    try
      Assert.AreEqual<Integer>(3, lList.Count, 'SelectRQL gt(qty,20) returns 3 rows');
    finally
      lList.Free;
    end;
    Assert.AreEqual(Int64(3), TMVCActiveRecord.Count<TARRqlKey>('gt(qty,20)'), 'RQL Count matches');
    Assert.AreEqual(Int64(5), TMVCActiveRecord.Count<TARRqlKey>(), 'unfiltered Count matches');
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_rql');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_DeleteAllAndDeleteRQL;
var
  lE: TARDelKey;
  I: Integer;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_del');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_del (a INTEGER NOT NULL, b INTEGER NOT NULL, note VARCHAR(200), ' +
    'PRIMARY KEY(a, b))');
  try
    for I := 1 to 4 do
    begin
      lE := TARDelKey.Create;
      try
        lE.A := 1; lE.B := I; lE.Note := 'n' + IntToStr(I); lE.Insert;
      finally
        lE.Free;
      end;
    end;
    Assert.AreEqual(Int64(4), TMVCActiveRecord.Count<TARDelKey>());

    // DeleteRQL removes the matching subset and returns the count.
    Assert.AreEqual(Int64(2), TMVCActiveRecord.DeleteRQL(TARDelKey, 'le(b,2)'),
      'DeleteRQL le(b,2) removes 2 composite-key rows');
    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARDelKey>(), 'two rows remain after DeleteRQL');

    // DeleteAll empties the table.
    Assert.AreEqual(Int64(2), TMVCActiveRecord.DeleteAll(TARDelKey), 'DeleteAll removes the rest');
    Assert.AreEqual(Int64(0), TMVCActiveRecord.Count<TARDelKey>(), 'table is empty after DeleteAll');
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_del');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_PKAsJSONArrayEscaping;
const
  TRICKY = 'A"B\C'; // one double-quote, one backslash
var
  lE, lLoaded: TARJsonKey;
  lJSON: string;
  lParsed: TJsonArray;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_json');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_ck_json (skey VARCHAR(50) NOT NULL, n INTEGER NOT NULL, note VARCHAR(200), ' +
    'PRIMARY KEY(skey, n))');
  try
    lE := TARJsonKey.Create;
    try
      lE.SKey := TRICKY; lE.N := 7; lE.Note := 'tricky'; lE.Insert;
    finally
      lE.Free;
    end;

    lLoaded := TMVCActiveRecord.GetByPKs<TARJsonKey>([TRICKY, 7]);
    try
      // PKAsJSONArray must produce VALID JSON with the quote and backslash escaped.
      lJSON := lLoaded.PKAsJSONArray;
      lParsed := TJsonBaseObject.Parse(lJSON) as TJsonArray;
      try
        Assert.AreEqual<Integer>(2, lParsed.Count, 'PKAsJSONArray has one element per PK column');
        Assert.AreEqual(TRICKY, lParsed.S[0], 'the escaped string PK round-trips through JSON parse');
        Assert.AreEqual(7, lParsed.I[1], 'the integer PK is a bare JSON number');
      finally
        lParsed.Free;
      end;
    finally
      lLoaded.Free;
    end;

    // The same tricky string must still address the row through GetByPKs.
    lLoaded := TMVCActiveRecord.GetByPKs<TARJsonKey>([TRICKY, 7]);
    try
      Assert.AreEqual(TRICKY, lLoaded.SKey, 'tricky string PK round-trips through GetByPKs');
      Assert.AreEqual('tricky', lLoaded.Note.Value);
    finally
      lLoaded.Free;
    end;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_ck_json');
    except
    end;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_Guards;
var
  lE: TARUserRole;
begin
  // No table needed: every guard fires from the RTTI-built table map before any
  // database access, so this test is DB-independent.
  lE := TARUserRole.Create;
  try
    lE.UserID := 1;
    lE.RoleID := 2;
    Assert.IsTrue(lE.HasCompositePK, 'entity must report a composite PK');
    // Single-value PK APIs are meaningless on a composite key: they must steer
    // the caller to the *PKs variants rather than silently use only column [0].
    Assert.WillRaise(procedure begin lE.GetPK end, EMVCActiveRecord,
      'GetPK must raise on a composite key');
    Assert.WillRaise(procedure begin lE.SetPK(1) end, EMVCActiveRecord,
      'SetPK must raise on a composite key');
    Assert.WillRaise(procedure begin lE.LoadByPK(1) end, EMVCActiveRecord,
      'single-value LoadByPK must raise on a composite key');
    Assert.WillRaise(procedure begin lE.LoadByPKs([1]) end, EMVCActiveRecord,
      'LoadByPKs must raise when the value count does not match the PK column count');
    // Store/IsNew answer "is the key assigned?", which on a natural composite key
    // is always yes and says nothing about the row: deciding Insert-or-Update from
    // it would also hide a read-then-write race.
    Assert.WillRaise(procedure begin lE.IsNew end, EMVCActiveRecord,
      'IsNew must raise on a composite key');
    Assert.WillRaise(procedure begin lE.IsPersisted end, EMVCActiveRecord,
      'IsPersisted must raise on a composite key');
    Assert.WillRaise(procedure begin lE.Store end, EMVCActiveRecord,
      'Store must raise on a composite key');
    // Reading the flag must stay answerable on every shape, because
    // serialization reads it: it reports whether the key HAS a generated
    // column, wherever that column sits. This key has none.
    Assert.IsFalse(lE.PrimaryKeyIsAutogenerated,
      'no column of this composite key is declared foAutoGenerated');
    // Writing it is the direction that cannot guess which column is meant.
    Assert.WillRaise(procedure begin lE.PrimaryKeyIsAutogenerated := True end, EMVCActiveRecord,
      'writing PrimaryKeyIsAutogenerated must raise on a composite key');
  finally
    lE.Free;
  end;
end;

procedure TARRefreshCK.CallRefreshFromDB;
begin
  RefreshFromDB;
end;

function TARAutogenFlag.InsertSQL: string;
begin
  Result := SQLGenerator.CreateInsertSQL(fTableMap, Self);
end;

procedure TTestActiveRecordBase.TestPrimaryKeyIsAutogenerated_RoundTrip;
var
  lE: TARAutogenFlag;
begin
  // No table needed: this is about the map the generator reads, not about rows.
  lE := TARAutogenFlag.Create;
  try
    // The flag lives in the class-wide table map, so a failure halfway through
    // would leave it set for the next engine's run of this same test. Restore
    // it whatever happens, or one real failure turns into three confusing ones.
    try
      Assert.IsFalse(lE.PrimaryKeyIsAutogenerated, 'the column is not declared foAutoGenerated');
      Assert.IsTrue(lE.InsertSQL.Contains(':id'), 'a plain key column is written by the INSERT');

      lE.PrimaryKeyIsAutogenerated := True;
      Assert.IsTrue(lE.PrimaryKeyIsAutogenerated, 'the flag must read back as it was set');
      // The point of the test. Setting the flag used to write only the legacy
      // mirror, leaving untouched the map Insert actually reads: the property
      // answered True while nothing whatsoever had changed.
      Assert.IsFalse(lE.InsertSQL.Contains(':id'),
        'an auto-generated key column must be left out of the INSERT');

      lE.PrimaryKeyIsAutogenerated := False;
      Assert.IsFalse(lE.PrimaryKeyIsAutogenerated, 'the flag must read back as it was set');
      Assert.IsTrue(lE.InsertSQL.Contains(':id'), 'switching it back must restore the column');
    finally
      lE.PrimaryKeyIsAutogenerated := False;
    end;
  finally
    lE.Free;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_RefreshFromDB;
var
  lE: TARRefreshCK;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_refresh_ck');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_refresh_ck (tenant INTEGER NOT NULL, doc_id INTEGER NOT NULL, ' +
    'descr VARCHAR(200), norm VARCHAR(50), PRIMARY KEY(tenant, doc_id))');
  // Decoy row: same first PK column, different second one. A WHERE built on
  // column [0] alone would refresh "norm" from this row instead of the right one.
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'INSERT INTO ar_refresh_ck (tenant, doc_id, descr, norm) VALUES (7, 1, ''decoy'', ''WRONG'')');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'INSERT INTO ar_refresh_ck (tenant, doc_id, descr, norm) VALUES (7, 2, ''real'', ''RIGHT'')');

  lE := TARRefreshCK.Create;
  try
    lE.Tenant := 7;
    lE.DocID := 2;
    // Before the composite-aware rewrite this raised, because RefreshFromDB
    // asked GetPK for the key value and GetPK refuses a composite one.
    lE.CallRefreshFromDB;
    Assert.AreEqual('RIGHT', lE.Norm.ValueOrDefault,
      'RefreshFromDB must match on every PK column, not just the first');
  finally
    lE.Free;
  end;
end;

procedure TTestActiveRecordBase.TestCompositePK_Heterogeneous;
var
  lE, lLoaded: TARDocLine;
begin
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_doc_lines');
  except
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_doc_lines (doc_code VARCHAR(50) NOT NULL, line_no INTEGER NOT NULL, ' +
    'descr VARCHAR(200), PRIMARY KEY(doc_code, line_no))');
  try
    lE := TARDocLine.Create;
    try
      lE.DocCode := 'INV-001'; lE.LineNo := 1; lE.Descr := 'first'; lE.Insert;
    finally
      lE.Free;
    end;
    lE := TARDocLine.Create;
    try
      lE.DocCode := 'INV-001'; lE.LineNo := 2; lE.Descr := 'second'; lE.Insert;
    finally
      lE.Free;
    end;

    Assert.AreEqual(Int64(2), TMVCActiveRecord.Count<TARDocLine>());

    // GetByPKs with a string value AND an integer value (heterogeneous key)
    lLoaded := TMVCActiveRecord.GetByPKs<TARDocLine>(['INV-001', 2]);
    try
      Assert.AreEqual('INV-001', lLoaded.DocCode);
      Assert.AreEqual(2, lLoaded.LineNo);
      Assert.AreEqual('second', lLoaded.Descr.Value);
    finally
      lLoaded.Free;
    end;

    // LoadByPKs (string, integer)
    lLoaded := TARDocLine.Create;
    try
      Assert.IsTrue(lLoaded.LoadByPKs(['INV-001', 1]));
      Assert.AreEqual('first', lLoaded.Descr.Value);
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;

    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARDocLine>(),
      'delete addressed the (string,int) key precisely; the sibling survives');
    lLoaded := TMVCActiveRecord.GetByPKs<TARDocLine>(['INV-001', 1], False);
    Assert.IsTrue(lLoaded = nil);
    lLoaded := TMVCActiveRecord.GetByPKs<TARDocLine>(['INV-001', 2], False);
    Assert.IsTrue(lLoaded <> nil);
    lLoaded.Free;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE ar_doc_lines');
    except
    end;
  end;
end;

procedure TTestActiveRecordSQLite.TestInt64AutogenPKBeyondMaxInt32;
const
  SEED_ID = Int64(5000000000); // > 32-bit MaxInt (2147483647)
var
  lEntity: TBigIntPKEntity;
  lExpectedID: Int64;
begin
  // SQLite INTEGER PRIMARY KEY is a 64-bit rowid. Seeding an explicit large id
  // makes the next autogenerated rowid SEED_ID+1, so the INSERT read-back (via
  // last_insert_rowid, an ftLargeInt) must carry a PK above 32-bit MaxInt.
  // Pre-fix the read-back used TField.AsInteger and truncated it to 32 bits.
  // NOTE: we only assert the Insert read-back. We deliberately do NOT round-trip
  // via a SELECT here: FireDAC maps a SQLite "INTEGER" column to a 32-bit
  // ftInteger field, truncating on SELECT before the ORM sees it — a driver
  // column-type-mapping quirk (real BIGINT columns on PG/Firebird map to
  // ftLargeInt and are unaffected), not the framework defect under test.
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS bigint_pk_test');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE bigint_pk_test (id INTEGER PRIMARY KEY, descr VARCHAR(100))');
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
      'INSERT INTO bigint_pk_test (id, descr) VALUES (' + IntToStr(SEED_ID) + ', ''seed'')');
    lExpectedID := SEED_ID + 1;

    lEntity := TBigIntPKEntity.Create;
    try
      lEntity.Descr := 'big';
      lEntity.Insert; // autogenerated PK is read back here (the fixed code path)
      Assert.AreEqual<Int64>(lExpectedID, lEntity.ID,
        'Int64 autogenerated PK truncated/incorrect on Insert read-back');
    finally
      lEntity.Free;
    end;
  finally
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS bigint_pk_test');
  end;
end;

procedure TTestActiveRecordPostgreSQL.TestInt64AutogenPKBeyondMaxInt32;
var
  lEntity: TBigIntPKEntity;
  lLoaded: TBigIntPKEntity;
  lInsertedID: Int64;
begin
  // PostgreSQL BIGINT IDENTITY maps to ftLargeInt on both the INSERT ... RETURNING
  // read-back and on a subsequent SELECT, so unlike the SQLite case we assert the
  // full round-trip. Two pre-fix defects are covered here: (1) the RETURNING
  // read-back used TField.AsInteger and truncated the PK to 32 bits; (2) GetByPK<T>
  // (Int64) hinted ftInteger, which on PG (native-UUID engine) overflowed the param
  // conversion for values above 32-bit MaxInt. We assert the value is above MaxInt
  // (proving no truncation) and that the SELECT round-trip preserves it exactly.
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS bigint_pk_test');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE bigint_pk_test (id BIGINT GENERATED BY DEFAULT AS IDENTITY (START WITH 5000000000) PRIMARY KEY, descr VARCHAR(100))');
  try
    lEntity := TBigIntPKEntity.Create;
    try
      lEntity.Descr := 'big';
      lEntity.Insert; // autogenerated PK read back via RETURNING (the fixed path)
      lInsertedID := lEntity.ID;
      Assert.IsTrue(lInsertedID > Int64(MaxInt),
        'Int64 autogenerated PK truncated on Insert read-back (PG RETURNING): ' + lInsertedID.ToString);

      lLoaded := TMVCActiveRecord.GetByPK<TBigIntPKEntity>(lInsertedID);
      try
        Assert.AreEqual<Int64>(lInsertedID, lLoaded.ID,
          'Int64 PK truncated/incorrect on GetByPK round-trip');
        Assert.AreEqual('big', lLoaded.Descr);
      finally
        lLoaded.Free;
      end;
    finally
      lEntity.Free;
    end;
  finally
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS bigint_pk_test');
  end;
end;

procedure TTestActiveRecordPostgreSQL.TestCompositePK_AutogenColumn;
var
  lE, lLoaded: TARInvoiceLine;
  lLineID1, lLineID2: Int64;
begin
  // The PK is (line_id, invoice_no): line_id is SERIAL (autogenerated), invoice_no
  // is a natural INTEGER the caller sets. On Insert the framework must read the
  // autogenerated line_id back into the object via RETURNING even though it is only
  // one column of a composite key.
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS ar_invoice_lines');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE ar_invoice_lines (line_id SERIAL, invoice_no INTEGER NOT NULL, ' +
    'descr VARCHAR(200), PRIMARY KEY(line_id, invoice_no))');
  try
    lE := TARInvoiceLine.Create;
    try
      lE.InvoiceNo := 100; lE.Descr := 'first'; lE.Insert;
      lLineID1 := lE.LineID;
    finally
      lE.Free;
    end;
    Assert.IsTrue(lLineID1 <> 0, 'autogenerated line_id must be read back after Insert (row 1)');

    lE := TARInvoiceLine.Create;
    try
      lE.InvoiceNo := 100; lE.Descr := 'second'; lE.Insert;
      lLineID2 := lE.LineID;
    finally
      lE.Free;
    end;
    Assert.IsTrue(lLineID2 <> 0, 'autogenerated line_id must be read back after Insert (row 2)');
    Assert.AreNotEqual(lLineID1, lLineID2, 'each row must get a distinct autogenerated line_id');

    // GetByPKs round-trips the composite key (autogen column + natural column).
    lLoaded := TMVCActiveRecord.GetByPKs<TARInvoiceLine>([lLineID1, 100]);
    try
      Assert.AreEqual<Int64>(lLineID1, lLoaded.LineID);
      Assert.AreEqual(100, lLoaded.InvoiceNo);
      Assert.AreEqual('first', lLoaded.Descr.Value);
      lLoaded.Descr := 'first-updated';
      lLoaded.Update;
    finally
      lLoaded.Free;
    end;

    lLoaded := TMVCActiveRecord.GetByPKs<TARInvoiceLine>([lLineID1, 100]);
    try
      Assert.AreEqual('first-updated', lLoaded.Descr.Value, 'Update persisted on the composite autogen-key row');
      lLoaded.Delete;
    finally
      lLoaded.Free;
    end;

    // Delete addresses both key columns: the sibling row (distinct line_id) survives.
    Assert.AreEqual(Int64(1), TMVCActiveRecord.Count<TARInvoiceLine>(),
      'only the addressed composite-key row was deleted');
    lLoaded := TMVCActiveRecord.GetByPKs<TARInvoiceLine>([lLineID1, 100], False);
    Assert.IsTrue(lLoaded = nil, 'the deleted row is gone');
    lLoaded := TMVCActiveRecord.GetByPKs<TARInvoiceLine>([lLineID2, 100], False);
    Assert.IsTrue(lLoaded <> nil, 'the sibling autogen row is untouched');
    lLoaded.Free;
  finally
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS ar_invoice_lines');
  end;
end;

procedure TTestActiveRecordPostgreSQL.TestStreamedDataSet_NotBufferedEndToEnd;
const
  ROW_COUNT = 5000;
var
  lRESTClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
  lBase: TJsonBaseObject;
  lArr: TJsonArray;
  lStreamed: Boolean;
begin
  // Seed a real PostgreSQL table the server streams back. generate_series keeps
  // seeding a single fast statement even for thousands of rows.
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS streamed_people');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE streamed_people (id INTEGER PRIMARY KEY, first_name VARCHAR(50), last_name VARCHAR(50))');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'INSERT INTO streamed_people (id, first_name, last_name) ' +
    'SELECT g, ''First'' || g, ''Last'' || g FROM generate_series(1, ' + IntToStr(ROW_COUNT) + ') g');
  try
    lRESTClient := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 8888);
    lResp := lRESTClient.Get('/streameddataset');
    Assert.AreEqual<Integer>(200, lResp.StatusCode);

    // On an Indy-based backend the dataset is streamed through TMVCJSONArrayWriter;
    // on HTTP.sys the server falls back to a buffered render and flags it.
    lStreamed := lResp.HeaderValue('X-DMVC-Streaming') <> 'fallback';
    if lStreamed then
      // The streaming writer can't know the body size up front, so it emits no
      // Content-Length (it ends the body by closing the connection). A present
      // Content-Length would mean the whole JSON had been buffered to measure it.
      Assert.AreEqual('', lResp.HeaderValue('Content-Length'),
        'a streamed response must not carry a Content-Length header');

    lBase := TJsonBaseObject.Parse(lResp.Content);
    try
      Assert.IsTrue(lBase is TJsonArray, 'streamed response is not a JSON array');
      lArr := TJsonArray(lBase);
      Assert.AreEqual<Integer>(ROW_COUNT, lArr.Count, 'streamed array element count mismatch');
      // Spot-check first and last elements survived the streaming round-trip intact.
      Assert.AreEqual<Integer>(1, lArr.O[0].I['id']);
      Assert.AreEqual('First1', lArr.O[0].S['first_name']);
      Assert.AreEqual<Integer>(ROW_COUNT, lArr.O[ROW_COUNT - 1].I['id']);
      Assert.AreEqual('Last' + IntToStr(ROW_COUNT), lArr.O[ROW_COUNT - 1].S['last_name']);
    finally
      lBase.Free;
    end;
  finally
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS streamed_people');
  end;
end;

procedure TTestActiveRecordPostgreSQL.TestStreamedDataSetChunked_EndToEnd;
const
  ROW_COUNT = 10000;
var
  lRESTClient: IMVCRESTClient;
  lResp: IMVCRESTResponse;
  lBase: TJsonBaseObject;
  lArr: TJsonArray;
begin
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS streamed_people');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE streamed_people (id INTEGER PRIMARY KEY, first_name VARCHAR(50), last_name VARCHAR(50))');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'INSERT INTO streamed_people (id, first_name, last_name) ' +
    'SELECT g, ''First'' || g, ''Last'' || g FROM generate_series(1, ' + IntToStr(ROW_COUNT) + ') g');
  try
    lRESTClient := TMVCRESTClient.New.BaseURL(TEST_SERVER_ADDRESS, 8888);
    // Bounded read timeout: a streamed response that never signals end-of-body
    // would otherwise hang the suite. With a correct EOF this completes in well
    // under a second; a timeout here IS a failure (no longer excused).
    lRESTClient.ReadTimeout(15000);
    lResp := lRESTClient.Get('/streameddatasetchunked');

    if lResp.StatusCode <> 200 then
    begin
      // Unsupported backend (classic WebBroker): CreateChunkedWriter raises BEFORE
      // any byte -> a clean error response (no partial body).
      Assert.IsTrue(lResp.StatusCode >= 500,
        'unsupported backend must fail cleanly, got ' + IntToStr(lResp.StatusCode));
      Exit;
    end;

    // Supported streaming backend (Indy Direct = chunked + keep-alive; HTTP.sys =
    // close-delimited). Either way the body size is unknown up front, so there is
    // NO Content-Length (a present one would mean the body had been buffered).
    Assert.AreEqual('', lResp.HeaderValue('Content-Length'),
      'a streamed response must not carry a Content-Length header');

    lBase := TJsonBaseObject.Parse(lResp.Content);
    try
      Assert.IsTrue(lBase is TJsonArray, 'streamed response is not a JSON array');
      lArr := TJsonArray(lBase);
      Assert.AreEqual<Integer>(ROW_COUNT, lArr.Count, 'streamed array element count mismatch');
      Assert.AreEqual<Integer>(1, lArr.O[0].I['id']);
      Assert.AreEqual('First1', lArr.O[0].S['first_name']);
      Assert.AreEqual<Integer>(ROW_COUNT, lArr.O[ROW_COUNT - 1].I['id']);
      Assert.AreEqual('Last' + IntToStr(ROW_COUNT), lArr.O[ROW_COUNT - 1].S['last_name']);
    finally
      lBase.Free;
    end;

    // The endpoint is repeatable: a second request returns 200 — on Indy Direct
    // over the kept-alive connection, on HTTP.sys over a fresh connection (the
    // stream is close-delimited there). A hang or broken state would fail here.
    lResp := lRESTClient.Get('/streameddatasetchunked');
    Assert.AreEqual<Integer>(200, lResp.StatusCode,
      'streamed endpoint not repeatable (second request failed)');
  finally
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE IF EXISTS streamed_people');
  end;
end;

procedure TTestActiveRecordFirebird.TestInt64AutogenPKBeyondMaxInt32;
var
  lEntity: TBigIntPKEntity;
  lLoaded: TBigIntPKEntity;
  lInsertedID: Int64;
begin
  // Firebird BIGINT IDENTITY maps to ftLargeInt end-to-end, so the full round-trip
  // (Insert read-back via RETURNING + GetByPK SELECT) must preserve a PK above
  // 32-bit MaxInt. Pre-fix the RETURNING read-back truncated via TField.AsInteger.
  // The exact post-seed value depends on RESTART semantics, so we assert it is
  // above MaxInt (no truncation) and that the round-trip preserves it exactly.
  try
    ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE bigint_pk_test');
  except
    // table may not exist on first run; Firebird has no DROP TABLE IF EXISTS pre-FB4
  end;
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'CREATE TABLE bigint_pk_test (id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY, descr VARCHAR(100))');
  ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL(
    'ALTER TABLE bigint_pk_test ALTER COLUMN id RESTART WITH 5000000000');
  try
    lEntity := TBigIntPKEntity.Create;
    try
      lEntity.Descr := 'big';
      lEntity.Insert; // autogenerated PK read back via RETURNING (the fixed path)
      lInsertedID := lEntity.ID;
      Assert.IsTrue(lInsertedID > Int64(MaxInt),
        'Int64 autogenerated PK truncated on Insert read-back (FB RETURNING): ' + lInsertedID.ToString);

      lLoaded := TMVCActiveRecord.GetByPK<TBigIntPKEntity>(lInsertedID);
      try
        Assert.AreEqual<Int64>(lInsertedID, lLoaded.ID,
          'Int64 PK truncated/incorrect on GetByPK round-trip');
        Assert.AreEqual('big', lLoaded.Descr);
      finally
        lLoaded.Free;
      end;
    finally
      lEntity.Free;
    end;
  finally
    try
      ActiveRecordConnectionsRegistry.GetCurrent.ExecSQL('DROP TABLE bigint_pk_test');
    except
      // best-effort cleanup
    end;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestActiveRecordSQLite);
TDUnitX.RegisterTestFixture(TTestActiveRecordFirebird);
TDUnitX.RegisterTestFixture(TTestActiveRecordPostgreSQL);
TDUnitX.RegisterTestFixture(TTestActiveRecordMariaDB);
{$IF Defined(TEST_ORACLE)}
TDUnitX.RegisterTestFixture(TTestActiveRecordOracle);
{$ENDIF}
{$IF Defined(TEST_MSSQL)}
TDUnitX.RegisterTestFixture(TTestActiveRecordMSSQL);
{$ENDIF}
TDUnitX.RegisterTestFixture(TTestUnitOfWorkMerge);

finalization

end.
