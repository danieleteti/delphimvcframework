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
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.DApt, FireDAC.Comp.Client,
  EntGen.Core;

const
  APP_VERSION = '1.1.0';

  CONNECTION_PARAM_KEYS: array[0..7] of string = (
    'DRIVER_ID', 'SERVER', 'PORT', 'DATABASE',
    'USER_NAME', 'PASSWORD', 'CHARACTERSET', 'OSAUTHENTIC'
  );

type
  TEnvConfig = TDictionary<string, string>;

procedure PrintBanner;
begin
  WriteLn('DMVCFramework Entity Generator CLI v' + APP_VERSION);
  WriteLn('Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team');
  WriteLn('');
end;

procedure PrintUsage;
begin
  PrintBanner;
  WriteLn('Usage:');
  WriteLn('  mvcentgen --config <file.env> [--connection <name>] [--output <file.pas>]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --config <file>        Path to .env configuration file (required)');
  WriteLn('  --connection <name>    FireDAC connection definition name (overrides config)');
  WriteLn('  --output <file>        Output .pas file path (overrides config)');
  WriteLn('  --help                 Show this help message');
  WriteLn('');
  WriteLn('Config file format (.env):');
  WriteLn('  # Connection - use CONNECTION_DEF for a named FireDAC connection,');
  WriteLn('  # or specify parameters directly');
  WriteLn('  CONNECTION_DEF=MyConnection');
  WriteLn('  # DRIVER_ID=PG');
  WriteLn('  # SERVER=localhost');
  WriteLn('  # PORT=5432');
  WriteLn('  # DATABASE=mydb');
  WriteLn('  # USER_NAME=postgres');
  WriteLn('  # PASSWORD=secret');
  WriteLn('');
  WriteLn('  SCHEMA=public');
  WriteLn('  OUTPUT_FILE=EntitiesU.pas');
  WriteLn('');
  WriteLn('  # NAME_CASE: LowerCase, UpperCase, CamelCase, PascalCase, SnakeCase, AsIs');
  WriteLn('  NAME_CASE=LowerCase');
  WriteLn('  # FIELD_NAME_FORMAT: AsIs, PascalCase');
  WriteLn('  FIELD_NAME_FORMAT=AsIs');
  WriteLn('  GENERATE_MAPPING=true');
  WriteLn('  CLASS_AS_ABSTRACT=false');
  WriteLn('');
  WriteLn('  # Tables: exact name, wildcard (TBL_*), or regex (/^pattern$/)');
  WriteLn('  TABLES=');
  WriteLn('  EXCLUDE_TABLES=');
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
    // Strip surrounding quotes if present
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
  lConnDef, lKey, lValue: string;
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
    lValue := GetEnvValue(AEnv, 'DRIVER_ID');
    if lValue.IsEmpty then
      raise Exception.Create('Either CONNECTION_DEF or DRIVER_ID must be specified in config');
    AConnection.Params.Clear;
    for I := Low(CONNECTION_PARAM_KEYS) to High(CONNECTION_PARAM_KEYS) do
    begin
      lKey := CONNECTION_PARAM_KEYS[I];
      lValue := GetEnvValue(AEnv, lKey);
      if not lValue.IsEmpty then
        AConnection.Params.AddPair(lKey, lValue);
    end;
    { Pass any additional FireDAC params (FD_*) }
    for lPair in AEnv do
    begin
      if lPair.Key.StartsWith('FD_') and (not lPair.Value.IsEmpty) then
        AConnection.Params.AddPair(lPair.Key.Substring(3), lPair.Value);
    end;
  end;
  AConnection.Params.AddPair('ExtendedMetadata', 'True');
end;

procedure RunCLI;
var
  lConfigFile, lConnectionOverride, lOutputFile: string;
  lEnv: TEnvConfig;
  lConnection: TFDConnection;
  lGenerator: TMVCEntityGenerator;
  lConfig: TEntGenConfig;
begin
  if HasCmdSwitch('help') or HasCmdSwitch('h') or (ParamCount = 0) then
  begin
    PrintUsage;
    Exit;
  end;

  PrintBanner;

  if not GetCmdSwitch('config', lConfigFile) then
  begin
    WriteLn('ERROR: --config parameter is required');
    WriteLn('');
    PrintUsage;
    ExitCode := 1;
    Exit;
  end;

  { Resolve relative path based on current directory }
  if not TPath.IsPathRooted(lConfigFile) then
    lConfigFile := TPath.Combine(GetCurrentDir, lConfigFile);

  WriteLn('Loading config from: ' + lConfigFile);
  lEnv := ParseEnvFile(lConfigFile);
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

    { Load TABLE -> ClassName mapping (MAP_tablename=ClassName) }
    lConfig.TableClassMap := BuildTableClassMap(lEnv);

    WriteLn('Output file: ' + lOutputFile);
    if lConfig.Schema <> '' then
      WriteLn('Schema: ' + lConfig.Schema);
    if Length(lConfig.Tables) > 0 then
      WriteLn('Tables filter: ' + string.Join(', ', lConfig.Tables));

    lConnection := TFDConnection.Create(nil);
    try
      lConnection.LoginPrompt := False;
      ConfigureConnection(lConnection, lEnv, lConnectionOverride);

      WriteLn('Connecting to database...');
      lConnection.Connected := True;
      WriteLn('Connected successfully');
      WriteLn('');

      lGenerator := TMVCEntityGenerator.Create(lConnection);
      try
        lGenerator.OnLog :=
          procedure(const AMessage: string)
          begin
            WriteLn(AMessage);
          end;
        lGenerator.GenerateToFile(lConfig, lOutputFile);
      finally
        lGenerator.Free;
      end;

      WriteLn('');
      WriteLn('Done!');
    finally
      lConnection.Free;
    end;
  finally
    lConfig.TableClassMap.Free;
    lEnv.Free;
  end;
end;

end.
