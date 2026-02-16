/// <summary>
/// SQL-file-based database migration runner.
/// Reads versioned SQL files from the Migrations directory, compares them
/// against the schema_migrations table, and applies any pending migrations
/// inside individual transactions.
/// </summary>
unit MigrationServiceU;

interface

uses
  FireDAC.Comp.Client;

type
  /// <summary>
  /// Applies pending SQL migrations to a PostgreSQL database.
  /// Migration files must follow the naming convention V###_Description.sql
  /// (for example V001_InitialSchema.sql) and reside in the Migrations
  /// directory next to the application binary.
  /// </summary>
  TMigrationService = class
  public
    /// <summary>
    /// Scan the Migrations directory for SQL files whose version exceeds
    /// the current database version and execute them in order.
    /// Each file runs inside its own transaction; on failure the
    /// transaction is rolled back and the error is re-raised.
    /// </summary>
    class procedure ApplyMigrations(const AConnection: TFDConnection);
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.IOUtils,
  System.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  MVCFramework.Logger;

resourcestring
  SLogMigrationsDir         = 'Migration directory: %s';
  SLogMigrationsDirMissing  = 'Migrations directory not found -- skipping migrations';
  SLogCurrentVersion        = 'Current schema version: %d';
  SLogNoMigrationsFound     = 'No migration files found';
  SLogNoPending             = 'Database schema is up to date';
  SLogApplyingMigration     = 'Applying migration V%d: %s';
  SLogMigrationApplied      = 'Migration V%d applied successfully';
  SLogMigrationFailed       = 'Migration V%d failed: %s';
  SLogSchemaTableCreated    = 'Created schema_migrations table';

/// <summary>
/// Extract the integer version number from a migration filename.
/// Expected format: V###_Description.sql  (e.g. V001_InitialSchema.sql -> 1)
/// Returns -1 when the filename does not match the expected pattern.
/// </summary>
function ExtractVersion(const AFileName: string): Integer;
var
  LName: string;
  LUnderscorePos: Integer;
  LVersionStr: string;
begin
  Result := -1;
  LName := TPath.GetFileNameWithoutExtension(AFileName);
  if (Length(LName) < 2) or (LName[1] <> 'V') then
    Exit;

  LUnderscorePos := Pos('_', LName);
  if LUnderscorePos < 3 then
    Exit;

  LVersionStr := Copy(LName, 2, LUnderscorePos - 2);
  Result := StrToIntDef(LVersionStr, -1);
end;

/// <summary>
/// Extract the human-readable description from a migration filename.
/// V001_InitialSchema.sql -> "InitialSchema"
/// </summary>
function ExtractDescription(const AFileName: string): string;
var
  LName: string;
  LUnderscorePos: Integer;
begin
  LName := TPath.GetFileNameWithoutExtension(AFileName);
  LUnderscorePos := Pos('_', LName);
  if LUnderscorePos > 0 then
    Result := Copy(LName, LUnderscorePos + 1, MaxInt)
  else
    Result := LName;
end;

{ TMigrationService }

class procedure TMigrationService.ApplyMigrations(const AConnection: TFDConnection);
var
  LMigrationsDir: string;
  LFiles: TStringDynArray;
  LSorted: TList<string>;
  LCurrentVersion: Integer;
  LScalarResult: Variant;
  LVersion: Integer;
  LDescription: string;
  LSQL: string;
  LFile: string;
begin
  // Determine the Migrations directory relative to the executable
  LMigrationsDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'Migrations');
  LogI(Format(SLogMigrationsDir, [LMigrationsDir]));

  if not TDirectory.Exists(LMigrationsDir) then
  begin
    LogW(SLogMigrationsDirMissing);
    Exit;
  end;

  // Ensure the schema_migrations tracking table exists
  AConnection.ExecSQL(
    'CREATE TABLE IF NOT EXISTS schema_migrations (' +
    '  version INTEGER PRIMARY KEY, ' +
    '  description VARCHAR(255) NOT NULL, ' +
    '  applied_at TIMESTAMP NOT NULL DEFAULT NOW()' +
    ')');
  LogI(SLogSchemaTableCreated);

  // Read current schema version
  LScalarResult := AConnection.ExecSQLScalar(
    'SELECT COALESCE(MAX(version), 0) FROM schema_migrations');
  if VarIsNull(LScalarResult) or VarIsEmpty(LScalarResult) then
    LCurrentVersion := 0
  else
    LCurrentVersion := LScalarResult;
  LogI(Format(SLogCurrentVersion, [LCurrentVersion]));

  // Collect SQL files matching the V###_*.sql pattern
  LFiles := TDirectory.GetFiles(LMigrationsDir, 'V*.sql');
  if Length(LFiles) = 0 then
  begin
    LogI(SLogNoMigrationsFound);
    Exit;
  end;

  // Sort files by extracted version number
  LSorted := TList<string>.Create;
  try
    for LFile in LFiles do
      if ExtractVersion(LFile) > 0 then
        LSorted.Add(LFile);

    LSorted.Sort(
      TComparer<string>.Construct(
        function(const Left, Right: string): Integer
        begin
          Result := ExtractVersion(Left) - ExtractVersion(Right);
        end));

    // Check whether any work is needed
    if (LSorted.Count = 0) or (ExtractVersion(LSorted.Last) <= LCurrentVersion) then
    begin
      LogI(SLogNoPending);
      Exit;
    end;

    // Apply each pending migration
    for LFile in LSorted do
    begin
      LVersion := ExtractVersion(LFile);
      if LVersion <= LCurrentVersion then
        Continue;

      LDescription := ExtractDescription(LFile);
      LogI(Format(SLogApplyingMigration, [LVersion, LDescription]));

      AConnection.StartTransaction;
      try
        LSQL := TFile.ReadAllText(LFile, TEncoding.UTF8);
        AConnection.ExecSQL(LSQL);

        AConnection.ExecSQL(
          'INSERT INTO schema_migrations (version, description) VALUES (:ver, :desc)',
          [LVersion, LDescription]);

        AConnection.Commit;
        LogI(Format(SLogMigrationApplied, [LVersion]));
      except
        on E: Exception do
        begin
          AConnection.Rollback;
          LogE(Format(SLogMigrationFailed, [LVersion, E.Message]));
          raise;
        end;
      end;
    end;
  finally
    LSorted.Free;
  end;
end;

end.
