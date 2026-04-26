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

unit EntGen.CLIMain;

interface

procedure RunCLI;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils,
  System.Generics.Collections, System.Generics.Defaults,
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.DApt, FireDAC.Comp.Client,
  LoggerPro, LoggerPro.Builder,
  MVCFramework.Console,
  EntGen.Core;

function IsInteractiveConsole: Boolean;
{$IFDEF MSWINDOWS}
var
  lHandle: THandle;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  lHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  Result := (lHandle <> 0) and (lHandle <> INVALID_HANDLE_VALUE) and
    (GetFileType(lHandle) = FILE_TYPE_CHAR);
{$ELSE}
  Result := IsTerminalCapable;
{$ENDIF}
end;

procedure FlushStdout;
begin
  try
    Flush(Output);
  except
    { Output might be closed or not a file (e.g. service) - ignore. }
  end;
end;

const
  APP_VERSION = '1.2.0';

  { Maps env-file keys (UPPER_SNAKE_CASE) to FireDAC canonical parameter names. }
  CONNECTION_PARAM_MAP: array[0..7, 0..1] of string = (
    ('DRIVER_ID',    'DriverID'),
    ('SERVER',       'Server'),
    ('PORT',         'Port'),
    ('DATABASE',     'Database'),
    ('USER_NAME',    'User_Name'),
    ('PASSWORD',     'Password'),
    ('CHARACTERSET', 'CharacterSet'),
    ('OSAUTHENTIC',  'OSAuthent')
  );

type
  TEnvConfig = TDictionary<string, string>;

procedure PrintBanner;
begin
  MVCFramework.Console.WriteLine('');
  MVCFramework.Console.WriteHeader(
    Format(' DMVCFramework Entity Generator CLI  v%s ', [APP_VERSION]),
    80, TConsoleColor.Cyan);
  MVCFramework.Console.WriteLine(
    'Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine('');
end;

procedure PrintKeyValue(const AKey, AValue: string);
begin
  MVCFramework.Console.WriteColoredText('  ' + AKey + ': ', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine(AValue, TConsoleColor.Cyan);
end;

procedure PrintUsage;
begin
  PrintBanner;
  MVCFramework.Console.WriteLine('Usage:', TConsoleColor.White);
  MVCFramework.Console.WriteLine(
    '  mvcentgen --config <file.env> [--connection <name>] [--output <file.pas>] [--log <file>]',
    TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('');
  MVCFramework.Console.WriteLine('Options:', TConsoleColor.White);
  MVCFramework.Console.WriteLine('  --config <file>        Path to .env configuration file (required)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --connection <name>    FireDAC connection definition name (overrides config)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --output <file>        Output .pas file path (overrides config)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --log <file>           Also write structured log to file', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --verbose              Include Debug-level messages (e.g. skipped tables)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --no-color             Disable ANSI colored console output', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --no-auto-required           Skip [MVCRequired] on NOT NULL non-PK columns (default: emit)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --no-auto-maxlength          Skip [MVCMaxLength(N)] on bounded VARCHAR columns (default: emit)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --no-auto-audit              Skip [MVCAudit*] on convention-named audit columns (default: emit)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --no-auto-soft-delete        Skip [MVCSoftDeleted] on convention-named columns (default: emit)', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('  --help                 Show this help message', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine('');
  MVCFramework.Console.WriteLine('Config file format (.env):', TConsoleColor.White);
  MVCFramework.Console.WriteLine(
    '  CONNECTION_DEF=MyConnection     # or set DRIVER_ID + SERVER + DATABASE + ...',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  SCHEMA=public                   # optional',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  OUTPUT_FILE=EntitiesU.pas',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  NAME_CASE=LowerCase             # LowerCase|UpperCase|CamelCase|PascalCase|SnakeCase|AsIs',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  FIELD_NAME_FORMAT=AsIs          # AsIs|PascalCase',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  GENERATE_MAPPING=true',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  CLASS_AS_ABSTRACT=false',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  TABLES=                         # optional: exact name, wildcard (TBL_*), regex (/^pattern$/)',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  EXCLUDE_TABLES=',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUTO_REQUIRED=true              # default: emit [MVCRequired] on NOT NULL non-PK columns',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUTO_MAXLENGTH=true             # default: emit [MVCMaxLength(N)] on bounded VARCHAR columns',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  READONLY_COLUMNS=tbl.col,tbl.col2  # CSV of table.column pairs to emit with foReadOnly',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  REFRESH_COLUMNS=tbl.col,tbl.col2   # CSV of table.column pairs to emit with foRefresh',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUTO_AUDIT=true                    # default: emit [MVCAudit*] on convention-named columns',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUDIT_CREATED_AT_NAME=created_at   # column name to recognize as MVCAuditCreatedAt',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUDIT_UPDATED_AT_NAME=updated_at   # column name to recognize as MVCAuditUpdatedAt',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUDIT_CREATED_BY_NAME=created_by   # column name to recognize as MVCAuditCreatedBy',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUDIT_UPDATED_BY_NAME=updated_by   # column name to recognize as MVCAuditUpdatedBy',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  AUTO_SOFT_DELETE=true              # default: emit [MVCSoftDeleted] on convention-named columns',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  SOFT_DELETE_TIMESTAMP_NAME=deleted_at  # CSV; column type must be NullableTDateTime',
    TConsoleColor.DarkGray);
  MVCFramework.Console.WriteLine(
    '  SOFT_DELETE_FLAG_NAME=is_deleted,deleted  # CSV; column type must be Boolean',
    TConsoleColor.DarkGray);
end;

function ParseEnvFile(const AFileName: string): TEnvConfig;
var
  lLines: TArray<string>;
  lLine, lKey, lValue: string;
  lEqPos: Integer;
  I: Integer;
begin
  Result := TEnvConfig.Create;
  if not TFile.Exists(AFileName) then
    raise Exception.CreateFmt('Config file not found: %s', [AFileName]);

  lLines := TFile.ReadAllLines(AFileName, TEncoding.UTF8);
  for I := 0 to Length(lLines) - 1 do
  begin
    lLine := lLines[I];
    if lLine.Trim.IsEmpty or lLine.TrimLeft.StartsWith('#') then
      Continue;
    lEqPos := lLine.IndexOf('=');
    if lEqPos < 1 then
      Continue;
    lKey := lLine.Substring(0, lEqPos).Trim.ToUpper;
    lValue := lLine.Substring(lEqPos + 1).Trim;
    { Strip surrounding quotes if present }
    if (lValue.Length >= 2) and
       ((lValue.StartsWith('"') and lValue.EndsWith('"')) or
        (lValue.StartsWith('''') and lValue.EndsWith(''''))) then
      lValue := lValue.Substring(1, lValue.Length - 2);
    Result.AddOrSetValue(lKey, lValue);
  end;
end;

function GetEnvValue(const AConfig: TEnvConfig; const AKey: string;
  const ADefault: string = ''): string;
begin
  if not AConfig.TryGetValue(AKey.ToUpper, Result) then
    Result := ADefault;
end;

function GetEnvBool(const AConfig: TEnvConfig; const AKey: string;
  const ADefault: Boolean = False): Boolean;
var
  S: string;
begin
  S := GetEnvValue(AConfig, AKey, '').ToLower;
  if S.IsEmpty then
    Exit(ADefault);
  Result := (S = 'true') or (S = '1') or (S = 'yes');
end;

function ParseNameCase(const AValue: string): TEntGenNameCase;
var
  S: string;
begin
  S := AValue.ToLower;
  if S = 'uppercase' then Result := ncUpperCase
  else if S = 'camelcase' then Result := ncCamelCase
  else if S = 'pascalcase' then Result := ncPascalCase
  else if S = 'snakecase' then Result := ncSnakeCase
  else if S = 'asis' then Result := ncAsIs
  else Result := ncLowerCase;
end;

function ParseFieldNameFormat(const AValue: string): TEntGenFieldNameFormat;
begin
  if AValue.ToLower = 'pascalcase' then
    Result := fnPascalCase
  else
    Result := fnAsIs;
end;

function SplitCSV(const AValue: string): TArray<string>;
var
  lList: TStringList;
  I: Integer;
begin
  if AValue.Trim.IsEmpty then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  lList := TStringList.Create;
  try
    lList.StrictDelimiter := True;
    lList.Delimiter := ',';
    lList.DelimitedText := AValue;
    SetLength(Result, lList.Count);
    for I := 0 to lList.Count - 1 do
      Result[I] := lList[I].Trim;
  finally
    lList.Free;
  end;
end;

function GetCmdSwitch(const AName: string; out AValue: string): Boolean;
begin
  { Support both -name and --name }
  Result := FindCmdLineSwitch(AName, AValue, True, [clstValueAppended, clstValueNextParam]);
  if not Result then
    Result := FindCmdLineSwitch('-' + AName, AValue, True, [clstValueAppended, clstValueNextParam]);
end;

function HasCmdSwitch(const AName: string): Boolean;
begin
  { Support both -name and --name }
  Result := FindCmdLineSwitch(AName, True);
  if not Result then
    Result := FindCmdLineSwitch('-' + AName, True);
end;

function BuildTableClassMap(const AEnv: TEnvConfig): TEntGenTableClassMap;
var
  lPair: TPair<string, string>;
  lTableName: string;
begin
  Result := nil;
  for lPair in AEnv do
  begin
    if lPair.Key.StartsWith('MAP_') and (not lPair.Value.IsEmpty) then
    begin
      if Result = nil then
        Result := TEntGenTableClassMap.Create(TIStringComparer.Ordinal);
      lTableName := lPair.Key.Substring(4); // strip MAP_ prefix
      Result.AddOrSetValue(lTableName, lPair.Value);
    end;
  end;
end;

procedure ConfigureConnection(AConnection: TFDConnection; const AEnv: TEnvConfig;
  const AConnectionOverride: string);
var
  lConnDef, lEnvKey, lFdKey, lValue: string;
  lPair: TPair<string, string>;
  I: Integer;
begin
  lConnDef := AConnectionOverride;
  if lConnDef.IsEmpty then
    lConnDef := GetEnvValue(AEnv, 'CONNECTION_DEF');

  if not lConnDef.IsEmpty then
  begin
    { Use named connection definition }
    FDManager.LoadConnectionDefFile;
    AConnection.ConnectionDefName := lConnDef;
  end
  else
  begin
    { Build connection from individual params }
    if GetEnvValue(AEnv, 'DRIVER_ID').IsEmpty then
      raise Exception.Create('Either CONNECTION_DEF or DRIVER_ID must be specified in config');
    AConnection.Params.Clear;
    for I := Low(CONNECTION_PARAM_MAP) to High(CONNECTION_PARAM_MAP) do
    begin
      lEnvKey := CONNECTION_PARAM_MAP[I, 0];
      lFdKey := CONNECTION_PARAM_MAP[I, 1];
      lValue := GetEnvValue(AEnv, lEnvKey);
      if not lValue.IsEmpty then
        AConnection.Params.AddPair(lFdKey, lValue);
    end;
    { Pass any additional FireDAC params (FD_*) verbatim after the prefix. }
    for lPair in AEnv do
    begin
      if lPair.Key.StartsWith('FD_') and (not lPair.Value.IsEmpty) then
        AConnection.Params.AddPair(lPair.Key.Substring(3), lPair.Value);
    end;
  end;
  AConnection.Params.AddPair('ExtendedMetadata', 'True');
end;

function BuildLogger(const ALogFile: string; AVerbose, AUseColors: Boolean): ILogWriter;
var
  lBuilder: ILoggerProBuilder;
  lDefaultLevel: TLogType;
begin
  if AVerbose then
    lDefaultLevel := TLogType.Debug
  else
    lDefaultLevel := TLogType.Info;

  lBuilder := LoggerProBuilder.WithDefaultMinimumLevel(lDefaultLevel);

  if AUseColors then
    lBuilder := lBuilder
      .WriteToConsole
        .WithColors
        .WithUTF8Output
        .Done
  else
    lBuilder := lBuilder
      .WriteToSimpleConsole
        .WithUTF8Output
        .Done;

  if not ALogFile.IsEmpty then
    lBuilder := lBuilder
      .WriteToFile
        .WithLogsFolder(TPath.GetDirectoryName(TPath.GetFullPath(ALogFile)))
        .WithFileBaseName(TPath.GetFileNameWithoutExtension(ALogFile))
        .WithMaxBackupFiles(5)
        .WithMaxFileSizeInKB(5120)
        .Done;

  Result := lBuilder.Build;
end;

procedure PrintResultBanner(const AResult: TEntGenResult; const AOutputFile: string);
begin
  MVCFramework.Console.WriteLine('');
  MVCFramework.Console.WriteColoredText('Entities generated: ', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine(IntToStr(AResult.GeneratedCount), TConsoleColor.Yellow);
  MVCFramework.Console.WriteColoredText('Tables skipped:     ', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine(IntToStr(AResult.SkippedCount), TConsoleColor.DarkGray);
  MVCFramework.Console.WriteColoredText('Output file:        ', TConsoleColor.Gray);
  MVCFramework.Console.WriteLine(AOutputFile, TConsoleColor.Cyan);
  MVCFramework.Console.WriteLine('');
  MVCFramework.Console.WriteSuccess('Done!');
end;

procedure RunCLI;
var
  lConfigFile, lConnectionOverride, lOutputFile, lLogFile: string;
  lEnv: TEnvConfig;
  lConnection: TFDConnection;
  lGenerator: TMVCEntityGenerator;
  lConfig: TEntGenConfig;
  lLogger: ILogWriter;
  lUseColors, lVerbose: Boolean;
  lSpinner: ISpinner;
  lGeneratedOk: Boolean;
  lResult: TEntGenResult;
begin
  lUseColors := (not HasCmdSwitch('no-color')) and IsInteractiveConsole;
  lVerbose := HasCmdSwitch('verbose') or HasCmdSwitch('v');

  if lUseColors then
    MVCFramework.Console.EnableANSIColorConsole;

  if HasCmdSwitch('help') or HasCmdSwitch('h') or (ParamCount = 0) then
  begin
    PrintUsage;
    Exit;
  end;

  PrintBanner;

  if not GetCmdSwitch('config', lConfigFile) then
  begin
    MVCFramework.Console.WriteError('--config parameter is required');
    MVCFramework.Console.WriteLine('');
    PrintUsage;
    ExitCode := 1;
    Exit;
  end;

  if not GetCmdSwitch('log', lLogFile) then
    lLogFile := '';

  { Resolve relative path based on current directory }
  if not TPath.IsPathRooted(lConfigFile) then
    lConfigFile := TPath.Combine(GetCurrentDir, lConfigFile);

  PrintKeyValue('Config file', lConfigFile);
  try
    lEnv := ParseEnvFile(lConfigFile);
  except
    on E: Exception do
    begin
      MVCFramework.Console.WriteError(E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;

  lGeneratedOk := False;
  try
    if not GetCmdSwitch('connection', lConnectionOverride) then
      lConnectionOverride := '';
    if not GetCmdSwitch('output', lOutputFile) then
      lOutputFile := GetEnvValue(lEnv, 'OUTPUT_FILE', 'EntitiesU.pas');

    { Resolve relative output path based on current directory }
    if not TPath.IsPathRooted(lOutputFile) then
      lOutputFile := TPath.Combine(GetCurrentDir, lOutputFile);

    { Build config }
    lConfig.Schema := GetEnvValue(lEnv, 'SCHEMA');
    lConfig.NameCase := ParseNameCase(GetEnvValue(lEnv, 'NAME_CASE', 'LowerCase'));
    lConfig.FieldNameFormat := ParseFieldNameFormat(GetEnvValue(lEnv, 'FIELD_NAME_FORMAT', 'AsIs'));
    lConfig.GenerateMapping := GetEnvBool(lEnv, 'GENERATE_MAPPING', False);
    lConfig.ClassAsAbstract := GetEnvBool(lEnv, 'CLASS_AS_ABSTRACT', False);
    lConfig.Tables := SplitCSV(GetEnvValue(lEnv, 'TABLES'));
    lConfig.ExcludeTables := SplitCSV(GetEnvValue(lEnv, 'EXCLUDE_TABLES'));
    lConfig.TableClassMap := BuildTableClassMap(lEnv);
    // Auto-validation (Tier 1): ON by default — the generator infers
    // [MVCRequired] / [MVCMaxLength] from FireDAC schema metadata. Users can
    // opt out per-flag via --no-auto-required / --no-auto-maxlength on the
    // command line, or via AUTO_REQUIRED=false / AUTO_MAXLENGTH=false in the
    // env file. Precedence: CLI --no-* wins, then env var, then default (True).
    lConfig.AutoRequired := GetEnvBool(lEnv, 'AUTO_REQUIRED', True);
    if HasCmdSwitch('no-auto-required') then
      lConfig.AutoRequired := False;
    lConfig.AutoMaxLength := GetEnvBool(lEnv, 'AUTO_MAXLENGTH', True);
    if HasCmdSwitch('no-auto-maxlength') then
      lConfig.AutoMaxLength := False;
    // READONLY_COLUMNS / REFRESH_COLUMNS — CSV of "table.column" pairs.
    // Each entry on READONLY_COLUMNS adds foReadOnly to that field; each
    // on REFRESH_COLUMNS adds foRefresh. A column may appear in both.
    lConfig.ReadOnlyColumns := SplitCSV(GetEnvValue(lEnv, 'READONLY_COLUMNS'));
    lConfig.RefreshColumns  := SplitCSV(GetEnvValue(lEnv, 'REFRESH_COLUMNS'));

    // Audit-column auto-detection (Tier 2 — by NAME). Default ON; the
    // canonical column names are the .env / CLI-overridable defaults
    // below. Opt out per-feature with --no-auto-audit.
    lConfig.AutoAudit := GetEnvBool(lEnv, 'AUTO_AUDIT', True);
    if HasCmdSwitch('no-auto-audit') then
      lConfig.AutoAudit := False;
    lConfig.AuditCreatedAtName := GetEnvValue(lEnv, 'AUDIT_CREATED_AT_NAME', 'created_at');
    lConfig.AuditUpdatedAtName := GetEnvValue(lEnv, 'AUDIT_UPDATED_AT_NAME', 'updated_at');
    lConfig.AuditCreatedByName := GetEnvValue(lEnv, 'AUDIT_CREATED_BY_NAME', 'created_by');
    lConfig.AuditUpdatedByName := GetEnvValue(lEnv, 'AUDIT_UPDATED_BY_NAME', 'updated_by');

    // Soft-delete auto-detection (Tier 2 — by NAME). Default ON. Two
    // CSV lists for the two storage modes; type validation enforces the
    // expected Delphi types. Override per-name with --no-auto-soft-delete.
    lConfig.AutoSoftDelete := GetEnvBool(lEnv, 'AUTO_SOFT_DELETE', True);
    if HasCmdSwitch('no-auto-soft-delete') then
      lConfig.AutoSoftDelete := False;
    lConfig.SoftDeleteTimestampNames := SplitCSV(
      GetEnvValue(lEnv, 'SOFT_DELETE_TIMESTAMP_NAME', 'deleted_at'));
    lConfig.SoftDeleteFlagNames := SplitCSV(
      GetEnvValue(lEnv, 'SOFT_DELETE_FLAG_NAME', 'is_deleted,deleted'));

    PrintKeyValue('Output file', lOutputFile);
    if lConfig.Schema <> '' then
      PrintKeyValue('Schema', lConfig.Schema);
    if Length(lConfig.Tables) > 0 then
      PrintKeyValue('Tables filter', string.Join(', ', lConfig.Tables));
    if Length(lConfig.ExcludeTables) > 0 then
      PrintKeyValue('Exclude filter', string.Join(', ', lConfig.ExcludeTables));
    MVCFramework.Console.WriteLine('');

    lConnection := TFDConnection.Create(nil);
    try
      lConnection.LoginPrompt := False;
      try
        ConfigureConnection(lConnection, lEnv, lConnectionOverride);
      except
        on E: Exception do
        begin
          MVCFramework.Console.WriteError('Config error: ' + E.Message);
          ExitCode := 1;
          Exit;
        end;
      end;

      if lUseColors then
        lSpinner := MVCFramework.Console.Spinner(
          '  Connecting to database...',
          TSpinnerStyle.ssDots,
          TConsoleColor.DarkCyan)
      else
      begin
        MVCFramework.Console.WriteInfo('Connecting to database...');
        lSpinner := nil;
      end;
      try
        try
          lConnection.Connected := True;
        finally
          if Assigned(lSpinner) then
            lSpinner.Hide;
          lSpinner := nil;
        end;
      except
        on E: Exception do
        begin
          MVCFramework.Console.WriteError('Connection failed: ' + E.Message);
          ExitCode := 1;
          Exit;
        end;
      end;
      MVCFramework.Console.WriteSuccess('Connected to database');
      MVCFramework.Console.WriteLine('');

      { Flush main-thread buffered writes before handing the console to the
        logger's async appender - without this, pipe-redirected stdout can
        interleave main-thread bytes with logger-thread bytes. }
      FlushStdout;

      { From here LoggerPro owns the console: no direct Console.Write calls
        until logger is released, or interleaving may occur. }
      lLogger := BuildLogger(lLogFile, lVerbose, lUseColors);
      lResult := Default(TEntGenResult);
      lGenerator := TMVCEntityGenerator.Create(lConnection);
      try
        lGenerator.Logger := lLogger;
        try
          lGenerator.GenerateToFile(lConfig, lOutputFile);
          lResult := lGenerator.LastResult;
          lGeneratedOk := True;
        except
          on E: Exception do
            lLogger.LogException(E, 'Generation failed', 'entgen');
        end;
      finally
        lGenerator.Free;
        { Release logger so appenders flush before we resume Console output. }
        lLogger := nil;
      end;

      if lGeneratedOk then
      begin
        PrintResultBanner(lResult, lOutputFile);
        FlushStdout;
      end;
    finally
      lConnection.Free;
    end;
  finally
    lConfig.TableClassMap.Free;
    lEnv.Free;
  end;

  if not lGeneratedOk then
    ExitCode := 1;
end;

end.
