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
// ***************************************************************************

unit EntGenTestsU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.Classes,
  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.DApt,
  FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  EntGen.Core;

type
  // Tests for the entity generator. Targeted at PostgreSQL because:
  //   1. Existing dev environment is already set up against the
  //      `activerecorddb` PG instance the rest of the test suite uses.
  //   2. PG returns full metadata (computed columns included) through
  //      FireDAC's TFDMetaInfoQuery — SQLite's driver omits them, so the
  //      `caCalculated`-based detection cannot be exercised there.
  //   3. PG enforces strict typing, no SQLite-style affinity surprises.
  //
  // Each [Test] starts from a clean private table named
  // `entgen_test_refreshable`, runs the generator with the requested
  // config, and asserts on the generated Pascal source.
  [TestFixture]
  TTestEntGenFieldOptions_PG = class
  strict private
    FConn: TFDConnection;
    procedure ExecIgnoreErrors(const ASQL: string);
    procedure CreateTableWithComputedColumn;
    procedure CreateTableWithoutComputedColumn;
    function GenerateSource(AConfig: TEntGenConfig): string;
    function MakeBaseConfig: TEntGenConfig;
  public
    [SetUp]    procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure TestReadOnlyColumnEmitsFoReadOnly;
    [Test] procedure TestRefreshColumnEmitsFoRefresh;
    [Test] procedure TestColumnInBothListsEmitsBothOptions;
    [Test] procedure TestUndeclaredColumnGetsNeitherOption;
    [Test] procedure TestPlainColumnUnaffected;
    [Test] procedure TestReadOnlyColumnSkipsRequiredAndMaxLength;
    [Test] procedure TestRefreshOnlyColumnKeepsRequiredAndMaxLength;
    [Test] procedure TestPKAndReadOnlyColumnCoexist;
    [Test] procedure TestColumnLookupIsCaseInsensitive;
  end;

  // Audit-column auto-detection tests. Same SQLite-vs-PG argument as for
  // the field-options fixture — PG is needed to round-trip the schema
  // metadata reliably.
  [TestFixture]
  TTestEntGenAutoAudit_PG = class
  strict private
    FConn: TFDConnection;
    procedure ExecIgnoreErrors(const ASQL: string);
    procedure CreateAuditTable(const AColumnTypes: string);
    function GenerateSource(AConfig: TEntGenConfig): string;
    function MakeBaseConfig: TEntGenConfig;
  public
    [SetUp]    procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure TestCreatedAtEmitsMVCAuditCreatedAt;
    [Test] procedure TestUpdatedAtEmitsMVCAuditUpdatedAt;
    [Test] procedure TestCreatedByEmitsMVCAuditCreatedBy;
    [Test] procedure TestUpdatedByEmitsMVCAuditUpdatedBy;
    [Test] procedure TestAuditColumnSkipsRequiredAndMaxLength;
    [Test] procedure TestNoAutoAuditFlagSkipsAllAuditAttributes;
    [Test] procedure TestCustomAuditNameIsHonoured;
    [Test] procedure TestWrongTypeOnAuditNameIsSkipped;
  end;

  // Soft-delete auto-detection tests. PG used for the same reasons as
  // the other PG fixtures (strict typing, real metadata).
  [TestFixture]
  TTestEntGenAutoSoftDelete_PG = class
  strict private
    FConn: TFDConnection;
    procedure ExecIgnoreErrors(const ASQL: string);
    procedure CreateTable(const AColumnTypes: string);
    function GenerateSource(AConfig: TEntGenConfig): string;
    function MakeBaseConfig: TEntGenConfig;
  public
    [SetUp]    procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure TestTimestampDeletedAtEmitsMVCSoftDeleted;
    [Test] procedure TestFlagIsDeletedEmitsMVCSoftDeleted;
    [Test] procedure TestFlagDeletedEmitsMVCSoftDeleted;
    [Test] procedure TestNonNullableTimestampIsSkipped;
    [Test] procedure TestWrongTypeOnTimestampNameIsSkipped;
    [Test] procedure TestSoftDeleteColumnSkipsRequiredAndMaxLength;
    [Test] procedure TestNoAutoSoftDeleteFlagSkipsAttribute;
    [Test] procedure TestCustomTimestampNameIsHonoured;
    [Test] procedure TestCustomFlagNameIsHonoured;
    [Test] procedure TestTimestampWinsOverFlagWhenBothPresent;
  end;

implementation

uses
  Data.DB,
  FireDAC.Phys, FireDAC.Phys.Intf,
  FDConnectionConfigU;

const
  TEST_TABLE_REFRESHABLE = 'entgen_test_refreshable';
  TEST_TABLE_PLAIN       = 'entgen_test_plain';
  TEST_TABLE_AUDIT       = 'entgen_test_audit';
  TEST_TABLE_SOFTDEL     = 'entgen_test_softdel';

{ TTestEntGenFieldOptions_PG }

procedure TTestEntGenFieldOptions_PG.SetUp;
begin
  // FDManager.AddConnectionDef raises on duplicate names: only register
  // the connection definition the first time around.
  if not FDManager.IsConnectionDef(CON_DEF_NAME) then
    CreatePostgresqlPrivateConnDef(False);
  FConn := TFDConnection.Create(nil);
  FConn.ConnectionDefName := CON_DEF_NAME;
  try
    FConn.Connected := True;
  except
    on E: Exception do
    begin
      FConn.Free;
      FConn := nil;
      Assert.Fail(
        'PostgreSQL connection failed (looking for `activerecorddb` on ' +
        'localhost as user `postgres`/`postgres`). Skipping the entity ' +
        'generator test fixture. Underlying error: ' + E.Message);
    end;
  end;
  // Always start clean — leftovers from a previous failed run would
  // poison the test.
  ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_REFRESHABLE);
  ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_PLAIN);
end;

procedure TTestEntGenFieldOptions_PG.TearDown;
begin
  if FConn <> nil then
  begin
    ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_REFRESHABLE);
    ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_PLAIN);
    FConn.Free;
  end;
end;

procedure TTestEntGenFieldOptions_PG.ExecIgnoreErrors(const ASQL: string);
begin
  // Some statements (DROP TABLE on first run) raise — ignore them so the
  // setup/teardown can be idempotent without try/except cluttering the
  // test bodies.
  try
    FConn.ExecSQL(ASQL);
  except
    // intentional swallow: setup/teardown idempotency
  end;
end;

procedure TTestEntGenFieldOptions_PG.CreateTableWithComputedColumn;
begin
  // PostgreSQL 12+ syntax. FireDAC reports `upper_name` with caCalculated
  // set, which is the marker the generator's auto-detection looks for.
  FConn.ExecSQL(
    'CREATE TABLE ' + TEST_TABLE_REFRESHABLE + ' (' +
    '  id BIGSERIAL PRIMARY KEY, ' +
    '  name VARCHAR(50) NOT NULL, ' +
    '  upper_name VARCHAR(50) GENERATED ALWAYS AS (UPPER(name)) STORED)');
end;

procedure TTestEntGenFieldOptions_PG.CreateTableWithoutComputedColumn;
begin
  FConn.ExecSQL(
    'CREATE TABLE ' + TEST_TABLE_PLAIN + ' (' +
    '  id BIGSERIAL PRIMARY KEY, ' +
    '  name VARCHAR(50) NOT NULL)');
end;

function TTestEntGenFieldOptions_PG.MakeBaseConfig: TEntGenConfig;
begin
  // The Tier 1 auto-* flags default to True in production. Tests that
  // want to isolate a single behaviour switch the others off below.
  // The ReadOnlyColumns / RefreshColumns lists start empty; tests that
  // need them populate them explicitly.
  Result := Default(TEntGenConfig);
  Result.Schema := 'public';
  Result.NameCase := ncLowerCase;
  Result.FieldNameFormat := fnPascalCase;
  Result.GenerateMapping := False;
  Result.ClassAsAbstract := False;
  Result.AutoRequired := True;
  Result.AutoMaxLength := True;
  Result.ReadOnlyColumns := [];
  Result.RefreshColumns := [];
end;

function TTestEntGenFieldOptions_PG.GenerateSource(AConfig: TEntGenConfig): string;
var
  lGen: TMVCEntityGenerator;
begin
  lGen := TMVCEntityGenerator.Create(FConn);
  try
    Result := lGen.Generate(AConfig);
  finally
    lGen.Free;
  end;
end;

procedure TTestEntGenFieldOptions_PG.TestReadOnlyColumnEmitsFoReadOnly;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  CreateTableWithComputedColumn;
  // Generate ONLY the test table — without this filter the generator
  // would walk every other table in `activerecorddb` and produce a
  // huge string we'd have to grep through.
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  // Strip MVCRequired / MVCMaxLength noise so the assertion below can
  // anchor on the [MVCTableField('upper_name', ...)] line directly.
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foReadOnly])]',
    'Column listed in READONLY_COLUMNS must be emitted with foReadOnly. ' +
    'Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenFieldOptions_PG.TestRefreshColumnEmitsFoRefresh;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.RefreshColumns := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foRefresh])]',
    'Column listed in REFRESH_COLUMNS must be emitted with foRefresh. ' +
    'Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenFieldOptions_PG.TestColumnInBothListsEmitsBothOptions;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // A column may appear in both lists; the resulting set must contain
  // both options in declaration order (foReadOnly first, foRefresh second).
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  lCfg.RefreshColumns  := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foReadOnly, foRefresh])]',
    'Column in both lists must carry both options. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenFieldOptions_PG.TestUndeclaredColumnGetsNeitherOption;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // No entries -> the column is treated as a regular writable column
  // even though the DB actually declares it as GENERATED ALWAYS. By
  // design: the generator does not auto-detect, the user opts in.
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [];
  lCfg.RefreshColumns := [];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.IsFalse(lSrc.Contains('foReadOnly'),
    'foReadOnly must NOT appear when the column is not in READONLY_COLUMNS');
  Assert.IsFalse(lSrc.Contains('foRefresh'),
    'foRefresh must NOT appear when the column is not in REFRESH_COLUMNS');
  Assert.Contains(lSrc, '[MVCTableField(''upper_name'')]',
    'Plain MVCTableField expected. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenFieldOptions_PG.TestPlainColumnUnaffected;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // No declarations -> no foReadOnly / foRefresh slipping in. Sanity
  // check that the new code path does not regress plain tables.
  CreateTableWithoutComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_PLAIN];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.IsFalse(lSrc.Contains('foReadOnly'),
    'foReadOnly must not appear on a plain table');
  Assert.IsFalse(lSrc.Contains('foRefresh'),
    'foRefresh must not appear on a plain table');
  Assert.Contains(lSrc, '[MVCTableField(''name'')]', 'Plain field expected');
end;

procedure TTestEntGenFieldOptions_PG.TestReadOnlyColumnSkipsRequiredAndMaxLength;
var
  lCfg: TEntGenConfig;
  lSrc: string;
  lFoReadOnlyPos: Integer;
  lUpperNameFieldPos: Integer;
  lBetween: string;
begin
  // The user can't write a foReadOnly column, so [MVCRequired] /
  // [MVCMaxLength] would be wasted instructions — assert they are NOT
  // emitted between the [MVCTableField] line and the field declaration.
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  // Keep AutoRequired and AutoMaxLength enabled -> they should still
  // skip the read-only column.

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foReadOnly])]');

  lFoReadOnlyPos := lSrc.IndexOf('[MVCTableField(''upper_name'', [foReadOnly])]');
  lUpperNameFieldPos := lSrc.IndexOf('fUpperName:', lFoReadOnlyPos);
  Assert.IsTrue(lUpperNameFieldPos > lFoReadOnlyPos,
    'Could not locate the fUpperName declaration after its [MVCTableField]');

  lBetween := lSrc.Substring(lFoReadOnlyPos, lUpperNameFieldPos - lFoReadOnlyPos);
  Assert.IsFalse(lBetween.Contains('[MVCRequired]'),
    'foReadOnly column must NOT carry [MVCRequired]. Block was:'#13#10 + lBetween);
  Assert.IsFalse(lBetween.Contains('[MVCMaxLength('),
    'foReadOnly column must NOT carry [MVCMaxLength]. Block was:'#13#10 + lBetween);
end;

procedure TTestEntGenFieldOptions_PG.TestRefreshOnlyColumnKeepsRequiredAndMaxLength;
var
  lCfg: TEntGenConfig;
  lSrc: string;
  lFoRefreshPos: Integer;
  lUpperNameFieldPos: Integer;
  lBetween: string;
begin
  // foRefresh by itself does NOT mark the column as read-only; the user
  // still writes it, the DB just re-reads it after the write. So
  // [MVCRequired] (NOT NULL is set on upper_name in our DDL) and
  // [MVCMaxLength] must still be emitted.
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.RefreshColumns := [TEST_TABLE_REFRESHABLE + '.name'];
  // Keep AutoRequired and AutoMaxLength enabled.

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''name'', [foRefresh])]');

  lFoRefreshPos := lSrc.IndexOf('[MVCTableField(''name'', [foRefresh])]');
  lUpperNameFieldPos := lSrc.IndexOf('fName:', lFoRefreshPos);
  Assert.IsTrue(lUpperNameFieldPos > lFoRefreshPos,
    'Could not locate the fName declaration after its [MVCTableField]');

  lBetween := lSrc.Substring(lFoRefreshPos, lUpperNameFieldPos - lFoRefreshPos);
  Assert.IsTrue(lBetween.Contains('[MVCRequired]'),
    'foRefresh-only column must KEEP [MVCRequired] - the user still writes it');
  Assert.IsTrue(lBetween.Contains('[MVCMaxLength('),
    'foRefresh-only column must KEEP [MVCMaxLength] - the user still writes it');
end;

procedure TTestEntGenFieldOptions_PG.TestPKAndReadOnlyColumnCoexist;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // Two MVCTableField lines: PK with foPrimaryKey/foAutoGenerated,
  // read-only column with foReadOnly. They must co-exist.
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [TEST_TABLE_REFRESHABLE + '.upper_name'];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''id'', [foPrimaryKey, foAutoGenerated])]',
    'PK line missing or wrong');
  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foReadOnly])]',
    'Read-only column line missing or wrong');
end;

procedure TTestEntGenFieldOptions_PG.TestColumnLookupIsCaseInsensitive;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // The user might paste table.column pairs with varying case (DB schemas
  // sometimes carry uppercase identifiers). Match must be SameText-style.
  CreateTableWithComputedColumn;
  lCfg := MakeBaseConfig;
  lCfg.Tables := [TEST_TABLE_REFRESHABLE];
  lCfg.ReadOnlyColumns := [UpperCase(TEST_TABLE_REFRESHABLE) + '.UPPER_NAME'];
  lCfg.AutoRequired := False;
  lCfg.AutoMaxLength := False;

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCTableField(''upper_name'', [foReadOnly])]',
    'Column lookup must be case-insensitive. Source was:'#13#10 + lSrc);
end;

{ TTestEntGenAutoAudit_PG }

procedure TTestEntGenAutoAudit_PG.SetUp;
begin
  if not FDManager.IsConnectionDef(CON_DEF_NAME) then
    CreatePostgresqlPrivateConnDef(False);
  FConn := TFDConnection.Create(nil);
  FConn.ConnectionDefName := CON_DEF_NAME;
  try
    FConn.Connected := True;
  except
    on E: Exception do
    begin
      FConn.Free;
      FConn := nil;
      Assert.Fail('PostgreSQL connection failed: ' + E.Message);
    end;
  end;
  ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_AUDIT);
end;

procedure TTestEntGenAutoAudit_PG.TearDown;
begin
  if FConn <> nil then
  begin
    ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_AUDIT);
    FConn.Free;
  end;
end;

procedure TTestEntGenAutoAudit_PG.ExecIgnoreErrors(const ASQL: string);
begin
  try
    FConn.ExecSQL(ASQL);
  except
    // intentional swallow
  end;
end;

procedure TTestEntGenAutoAudit_PG.CreateAuditTable(const AColumnTypes: string);
begin
  // AColumnTypes is the comma-separated list of column declarations
  // beyond the PK (e.g. 'created_at TIMESTAMP NOT NULL, updated_by VARCHAR(50)').
  // Lets each test pick exactly the columns it cares about.
  FConn.ExecSQL(
    'CREATE TABLE ' + TEST_TABLE_AUDIT + ' (' +
    '  id BIGSERIAL PRIMARY KEY, ' + AColumnTypes + ')');
end;

function TTestEntGenAutoAudit_PG.MakeBaseConfig: TEntGenConfig;
begin
  Result := Default(TEntGenConfig);
  Result.Schema := 'public';
  Result.NameCase := ncLowerCase;
  Result.FieldNameFormat := fnPascalCase;
  Result.GenerateMapping := False;
  Result.ClassAsAbstract := False;
  Result.Tables := [TEST_TABLE_AUDIT];
  Result.AutoRequired := True;
  Result.AutoMaxLength := True;
  // Audit defaults — match the canonical names.
  Result.AutoAudit := True;
  Result.AuditCreatedAtName := 'created_at';
  Result.AuditUpdatedAtName := 'updated_at';
  Result.AuditCreatedByName := 'created_by';
  Result.AuditUpdatedByName := 'updated_by';
end;

function TTestEntGenAutoAudit_PG.GenerateSource(AConfig: TEntGenConfig): string;
var
  lGen: TMVCEntityGenerator;
begin
  lGen := TMVCEntityGenerator.Create(FConn);
  try
    Result := lGen.Generate(AConfig);
  finally
    lGen.Free;
  end;
end;

procedure TTestEntGenAutoAudit_PG.TestCreatedAtEmitsMVCAuditCreatedAt;
var
  lSrc: string;
begin
  CreateAuditTable('created_at TIMESTAMP NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCAuditCreatedAt]',
    '[MVCAuditCreatedAt] expected. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenAutoAudit_PG.TestUpdatedAtEmitsMVCAuditUpdatedAt;
var
  lSrc: string;
begin
  CreateAuditTable('updated_at TIMESTAMP');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCAuditUpdatedAt]',
    '[MVCAuditUpdatedAt] expected. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenAutoAudit_PG.TestCreatedByEmitsMVCAuditCreatedBy;
var
  lSrc: string;
begin
  CreateAuditTable('created_by VARCHAR(100)');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCAuditCreatedBy]',
    '[MVCAuditCreatedBy] expected. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenAutoAudit_PG.TestUpdatedByEmitsMVCAuditUpdatedBy;
var
  lSrc: string;
begin
  CreateAuditTable('updated_by VARCHAR(100)');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCAuditUpdatedBy]',
    '[MVCAuditUpdatedBy] expected. Source was:'#13#10 + lSrc);
end;

procedure TTestEntGenAutoAudit_PG.TestAuditColumnSkipsRequiredAndMaxLength;
var
  lSrc: string;
  lAuditPos, lFieldPos: Integer;
  lBetween: string;
begin
  // NOT NULL VARCHAR(50) audit column: without the audit detection the
  // generator would have emitted [MVCRequired] (NOT NULL) and
  // [MVCMaxLength(50)] (bounded VARCHAR). With audit on, both are skipped.
  CreateAuditTable('created_by VARCHAR(50) NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);

  lAuditPos := lSrc.IndexOf('[MVCAuditCreatedBy]');
  Assert.IsTrue(lAuditPos >= 0, '[MVCAuditCreatedBy] missing');
  lFieldPos := lSrc.IndexOf('fCreatedBy:', lAuditPos);
  Assert.IsTrue(lFieldPos > lAuditPos, 'fCreatedBy declaration not found');

  lBetween := lSrc.Substring(lAuditPos, lFieldPos - lAuditPos);
  Assert.IsFalse(lBetween.Contains('[MVCRequired]'),
    'Audit column must NOT carry [MVCRequired]. Block was:'#13#10 + lBetween);
  Assert.IsFalse(lBetween.Contains('[MVCMaxLength('),
    'Audit column must NOT carry [MVCMaxLength]. Block was:'#13#10 + lBetween);
end;

procedure TTestEntGenAutoAudit_PG.TestNoAutoAuditFlagSkipsAllAuditAttributes;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  CreateAuditTable(
    'created_at TIMESTAMP, updated_at TIMESTAMP, ' +
    'created_by VARCHAR(50), updated_by VARCHAR(50)');
  lCfg := MakeBaseConfig;
  lCfg.AutoAudit := False;

  lSrc := GenerateSource(lCfg);

  Assert.IsFalse(lSrc.Contains('[MVCAuditCreatedAt]'),
    'AutoAudit=False must NOT emit [MVCAuditCreatedAt]');
  Assert.IsFalse(lSrc.Contains('[MVCAuditUpdatedAt]'),
    'AutoAudit=False must NOT emit [MVCAuditUpdatedAt]');
  Assert.IsFalse(lSrc.Contains('[MVCAuditCreatedBy]'),
    'AutoAudit=False must NOT emit [MVCAuditCreatedBy]');
  Assert.IsFalse(lSrc.Contains('[MVCAuditUpdatedBy]'),
    'AutoAudit=False must NOT emit [MVCAuditUpdatedBy]');
end;

procedure TTestEntGenAutoAudit_PG.TestCustomAuditNameIsHonoured;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // Schema uses non-canonical column names (`row_created` instead of
  // `created_at`). The user redirects the AUDIT_CREATED_AT_NAME and the
  // generator must still recognise the column.
  CreateAuditTable('row_created TIMESTAMP NOT NULL');
  lCfg := MakeBaseConfig;
  lCfg.AuditCreatedAtName := 'row_created';

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCAuditCreatedAt]',
    '[MVCAuditCreatedAt] must follow the configured name. Source:'#13#10 + lSrc);
end;

procedure TTestEntGenAutoAudit_PG.TestWrongTypeOnAuditNameIsSkipped;
var
  lSrc: string;
begin
  // Column literally named `created_at` but typed as INT — the generator
  // must NOT emit the audit attribute (would only blow up at runtime).
  CreateAuditTable('created_at INT NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);

  Assert.IsFalse(lSrc.Contains('[MVCAuditCreatedAt]'),
    'Wrong-typed column must not carry [MVCAuditCreatedAt]');
  // The column is still emitted as a normal field — and since the type
  // is non-string the AutoMaxLength branch doesn't fire either.
  Assert.Contains(lSrc, 'fCreatedAt:',
    'The column must still appear in the generated entity');
end;

{ TTestEntGenAutoSoftDelete_PG }

procedure TTestEntGenAutoSoftDelete_PG.SetUp;
begin
  if not FDManager.IsConnectionDef(CON_DEF_NAME) then
    CreatePostgresqlPrivateConnDef(False);
  FConn := TFDConnection.Create(nil);
  FConn.ConnectionDefName := CON_DEF_NAME;
  try
    FConn.Connected := True;
  except
    on E: Exception do
    begin
      FConn.Free;
      FConn := nil;
      Assert.Fail('PostgreSQL connection failed: ' + E.Message);
    end;
  end;
  ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_SOFTDEL);
end;

procedure TTestEntGenAutoSoftDelete_PG.TearDown;
begin
  if FConn <> nil then
  begin
    ExecIgnoreErrors('DROP TABLE IF EXISTS ' + TEST_TABLE_SOFTDEL);
    FConn.Free;
  end;
end;

procedure TTestEntGenAutoSoftDelete_PG.ExecIgnoreErrors(const ASQL: string);
begin
  try
    FConn.ExecSQL(ASQL);
  except
    // intentional swallow
  end;
end;

procedure TTestEntGenAutoSoftDelete_PG.CreateTable(const AColumnTypes: string);
begin
  FConn.ExecSQL(
    'CREATE TABLE ' + TEST_TABLE_SOFTDEL + ' (' +
    '  id BIGSERIAL PRIMARY KEY, ' + AColumnTypes + ')');
end;

function TTestEntGenAutoSoftDelete_PG.MakeBaseConfig: TEntGenConfig;
begin
  Result := Default(TEntGenConfig);
  Result.Schema := 'public';
  Result.NameCase := ncLowerCase;
  Result.FieldNameFormat := fnPascalCase;
  Result.GenerateMapping := False;
  Result.ClassAsAbstract := False;
  Result.Tables := [TEST_TABLE_SOFTDEL];
  Result.AutoRequired := True;
  Result.AutoMaxLength := True;
  Result.AutoSoftDelete := True;
  Result.SoftDeleteTimestampNames := ['deleted_at'];
  Result.SoftDeleteFlagNames := ['is_deleted', 'deleted'];
end;

function TTestEntGenAutoSoftDelete_PG.GenerateSource(AConfig: TEntGenConfig): string;
var
  lGen: TMVCEntityGenerator;
begin
  lGen := TMVCEntityGenerator.Create(FConn);
  try
    Result := lGen.Generate(AConfig);
  finally
    lGen.Free;
  end;
end;

procedure TTestEntGenAutoSoftDelete_PG.TestTimestampDeletedAtEmitsMVCSoftDeleted;
var
  lSrc: string;
begin
  // PG TIMESTAMP nullable -> Delphi NullableTDateTime
  CreateTable('deleted_at TIMESTAMP NULL');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCSoftDeleted]',
    '[MVCSoftDeleted] must be emitted on the timestamp column. Source:'#13#10 + lSrc);
  Assert.Contains(lSrc, 'fDeletedAt: NullableTDateTime',
    'Field must be NullableTDateTime');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestFlagIsDeletedEmitsMVCSoftDeleted;
var
  lSrc: string;
begin
  CreateTable('is_deleted BOOLEAN NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCSoftDeleted]',
    '[MVCSoftDeleted] must be emitted on the flag column. Source:'#13#10 + lSrc);
  Assert.Contains(lSrc, 'fIsDeleted: Boolean', 'Field must be Boolean');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestFlagDeletedEmitsMVCSoftDeleted;
var
  lSrc: string;
begin
  // The default flag list includes both `is_deleted` and `deleted` —
  // the latter is a frequent alias.
  CreateTable('deleted BOOLEAN NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.Contains(lSrc, '[MVCSoftDeleted]',
    '[MVCSoftDeleted] must be emitted on the `deleted` flag column');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestNonNullableTimestampIsSkipped;
var
  lSrc: string;
begin
  // NOT NULL TIMESTAMP -> Delphi TDateTime (not NullableTDateTime). The
  // framework REQUIRES NullableTDateTime for timestamp soft-delete (plain
  // TDateTime defaults to 1899-12-30 - never matches the auto-filter).
  // Generator must skip with warning.
  CreateTable('deleted_at TIMESTAMP NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.IsFalse(lSrc.Contains('[MVCSoftDeleted]'),
    'NOT NULL timestamp must NOT carry [MVCSoftDeleted]');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestWrongTypeOnTimestampNameIsSkipped;
var
  lSrc: string;
begin
  CreateTable('deleted_at INT');
  lSrc := GenerateSource(MakeBaseConfig);
  Assert.IsFalse(lSrc.Contains('[MVCSoftDeleted]'),
    'INT-typed deleted_at must NOT carry [MVCSoftDeleted]');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestSoftDeleteColumnSkipsRequiredAndMaxLength;
var
  lSrc: string;
  lAttribPos, lFieldPos: Integer;
  lBetween: string;
begin
  // NOT NULL flag column: without the soft-delete detection, [MVCRequired]
  // would fire (Boolean is non-string so no MaxLength). With detection,
  // [MVCRequired] is suppressed.
  CreateTable('is_deleted BOOLEAN NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);

  lAttribPos := lSrc.IndexOf('[MVCSoftDeleted]');
  Assert.IsTrue(lAttribPos >= 0, '[MVCSoftDeleted] missing');
  lFieldPos := lSrc.IndexOf('fIsDeleted:', lAttribPos);
  Assert.IsTrue(lFieldPos > lAttribPos, 'fIsDeleted declaration not found');

  lBetween := lSrc.Substring(lAttribPos, lFieldPos - lAttribPos);
  Assert.IsFalse(lBetween.Contains('[MVCRequired]'),
    'Soft-delete column must NOT carry [MVCRequired]. Block was:'#13#10 + lBetween);
end;

procedure TTestEntGenAutoSoftDelete_PG.TestNoAutoSoftDeleteFlagSkipsAttribute;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  CreateTable('deleted_at TIMESTAMP NULL, is_deleted BOOLEAN');
  lCfg := MakeBaseConfig;
  lCfg.AutoSoftDelete := False;

  lSrc := GenerateSource(lCfg);

  Assert.IsFalse(lSrc.Contains('[MVCSoftDeleted]'),
    'AutoSoftDelete=False must NOT emit [MVCSoftDeleted]');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestCustomTimestampNameIsHonoured;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  // Schema uses `archived_at` instead of the canonical `deleted_at`.
  CreateTable('archived_at TIMESTAMP NULL');
  lCfg := MakeBaseConfig;
  lCfg.SoftDeleteTimestampNames := ['archived_at'];

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCSoftDeleted]',
    '[MVCSoftDeleted] must follow the configured timestamp name');
  Assert.Contains(lSrc, 'fArchivedAt:');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestCustomFlagNameIsHonoured;
var
  lCfg: TEntGenConfig;
  lSrc: string;
begin
  CreateTable('is_archived BOOLEAN NOT NULL');
  lCfg := MakeBaseConfig;
  lCfg.SoftDeleteFlagNames := ['is_archived'];

  lSrc := GenerateSource(lCfg);

  Assert.Contains(lSrc, '[MVCSoftDeleted]',
    '[MVCSoftDeleted] must follow the configured flag name');
  Assert.Contains(lSrc, 'fIsArchived:');
end;

procedure TTestEntGenAutoSoftDelete_PG.TestTimestampWinsOverFlagWhenBothPresent;
var
  lSrc: string;
  lTimestampPos, lFlagPos, lAttrPos: Integer;
begin
  // Pathological table with both candidates. Timestamp must win, the
  // flag column must NOT also carry [MVCSoftDeleted].
  CreateTable('deleted_at TIMESTAMP NULL, is_deleted BOOLEAN NOT NULL');
  lSrc := GenerateSource(MakeBaseConfig);

  // Sanity: only one [MVCSoftDeleted] in the whole emitted source.
  lAttrPos := lSrc.IndexOf('[MVCSoftDeleted]');
  Assert.IsTrue(lAttrPos >= 0, '[MVCSoftDeleted] missing entirely');
  Assert.AreEqual(-1, lSrc.IndexOf('[MVCSoftDeleted]', lAttrPos + 1),
    'Only ONE [MVCSoftDeleted] must be emitted (timestamp wins)');

  // Locate where [MVCSoftDeleted] sits — should be next to fDeletedAt,
  // not fIsDeleted. We do a positional check.
  lTimestampPos := lSrc.IndexOf('fDeletedAt:');
  lFlagPos := lSrc.IndexOf('fIsDeleted:');
  Assert.IsTrue(lAttrPos < lTimestampPos,
    '[MVCSoftDeleted] must precede fDeletedAt');
  Assert.IsTrue(lAttrPos < lFlagPos,
    '[MVCSoftDeleted] must NOT be between fDeletedAt and fIsDeleted ' +
    '(it would land on the flag column instead of the timestamp)');
  // Soundness: distance from attr to fDeletedAt must be smaller than
  // distance to fIsDeleted, i.e. the attribute belongs to the field
  // immediately following it.
  Assert.IsTrue((lTimestampPos - lAttrPos) < (lFlagPos - lAttrPos),
    'The single [MVCSoftDeleted] must be adjacent to fDeletedAt');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestEntGenFieldOptions_PG);
  TDUnitX.RegisterTestFixture(TTestEntGenAutoAudit_PG);
  TDUnitX.RegisterTestFixture(TTestEntGenAutoSoftDelete_PG);

end.
