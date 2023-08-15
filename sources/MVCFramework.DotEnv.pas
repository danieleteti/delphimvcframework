// *************************************************************************** }
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
// ***************************************************************************

unit MVCFramework.DotEnv;

interface

uses
  System.SysUtils, System.Generics.Collections, MVCFramework.DotEnv.Parser;

type
{$SCOPEDENUMS ON}
  TMVCDotEnvPriority = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);

  EMVCDotEnv = class(Exception)

  end;

  IMVCDotEnv = interface
    ['{5FD2C3CB-0895-4CCD-985F-27394798E4A8}']
    function Env(const Name: string): string; overload;
    function Env(const Name: string; const DefaultValue: String): string; overload;
    function Env(const Name: string; const DefaultValue: Integer): Integer; overload;
    function Env(const Name: string; const DefaultValue: Boolean): Boolean; overload;
    function SaveToFile(const FileName: String): IMVCDotEnv;
    function ToArray(): TArray<String>;
  end;

  IMVCDotEnvBuilder = interface
    ['{1A5EDD44-7226-40BC-A8EE-789E27522392}']
    function WithStrategy(const Strategy: TMVCDotEnvPriority = TMVCDotEnvPriority.EnvThenFile): IMVCDotEnvBuilder;
    function UseLogger( const Logger: TProc<String>): IMVCDotEnvBuilder;
    function UseProfile(const ProfileName: String): IMVCDotEnvBuilder;
    function ClearProfiles: IMVCDotEnvBuilder;
    function Build(const DotEnvPath: string = ''): IMVCDotEnv; overload;
  end;

function NewDotEnv: IMVCDotEnvBuilder;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  System.Classes;

var
  gDotEnv: IMVCDotEnvBuilder = nil;

{ TDotEnv }

type
{$SCOPEDENUMS ON}
  TdotEnvEngineState = (created, building, built);
  TMVCDotEnv = class(TInterfacedObject, IMVCDotEnv, IMVCDotEnvBuilder)
  strict private
    fState: TdotEnvEngineState;
    fPriority: TMVCDotEnvPriority;
    fEnvPath: string;
    fEnvDict: TMVCDotEnvDictionary;
    fLoggerProc: TProc<String>;
    fProfiles: TList<String>;
    procedure DoLog(const Value: String);
    procedure ReadEnvFile;
    function GetDotEnvVar(const key: string): string;
    function ExplodePlaceholders(const Value: string): string;
    procedure PopulateDictionary(const EnvDict: TDictionary<string, string>; const EnvFilePath: String);
    procedure CheckAlreadyBuilt;
    procedure ExplodeReferences;
  strict protected
    function WithStrategy(const Priority: TMVCDotEnvPriority = TMVCDotEnvPriority.EnvThenFile): IMVCDotEnvBuilder; overload;
    function UseProfile(const ProfileName: String): IMVCDotEnvBuilder;
    function UseLogger(const LoggerProc: TProc<String>): IMVCDotEnvBuilder;
    function ClearProfiles: IMVCDotEnvBuilder;
    function Build(const DotEnvDirectory: string = ''): IMVCDotEnv; overload;
    function Env(const Name: string): string; overload;
    function Env(const Name: string; const DefaultValue: String): string; overload;
    function Env(const Name: string; const DefaultValue: Integer): Integer; overload;
    function Env(const Name: string; const DefaultValue: Boolean): Boolean; overload;
    function SaveToFile(const FileName: String): IMVCDotEnv;
    function ToArray(): TArray<String>;
  public
    constructor Create;
    destructor Destroy; override;
  end;


function TMVCDotEnv.GetDotEnvVar(const key: string): string;
begin
  fEnvDict.TryGetValue(key, Result);
end;

function TMVCDotEnv.Env(const Name: string): string;
var
  lTmp: String;
begin
  if fState = TdotEnvEngineState.created then
  begin
    raise EMVCDotEnv.Create('dotEnv Engine not built');
  end;

  if fPriority in [TMVCDotEnvPriority.FileThenEnv, TMVCDotEnvPriority.OnlyFile] then
  begin
    Result := GetDotEnvVar(name);
    if Result.Contains('${' + Name + '}') then
    begin
      raise EMVCDotEnv.CreateFmt('Configuration loop detected with key "%s"', [Name]);
    end;

    if fPriority = TMVCDotEnvPriority.OnlyFile then
    begin
      // OnlyFile
      Exit;
    end;
    // FileThenEnv
    if Result.IsEmpty then
    begin
      Exit(ExplodePlaceholders(GetEnvironmentVariable(Name)));
    end;
  end
  else if fPriority in [TMVCDotEnvPriority.EnvThenFile, TMVCDotEnvPriority.OnlyEnv] then
  begin
    Result := ExplodePlaceholders(GetEnvironmentVariable(Name));
    if fPriority = TMVCDotEnvPriority.OnlyEnv then
    begin
      // OnlyEnv
      Exit;
    end;
    // EnvThenFile
    if Result.IsEmpty then
    begin
      lTmp := GetDotEnvVar(Name);
      if lTmp.Contains('${' + Name + '}') then
      begin
        raise EMVCDotEnv.CreateFmt('Configuration loop detected with key "%s"', [Name]);
      end;
      Exit(lTmp);
    end;
  end
  else
  begin
    raise EMVCDotEnv.CreateFmt('Unknown dotEnv Priority: %s', [GetEnumName(TypeInfo(TMVCDotEnvPriority), Ord(fPriority))]);
  end;
end;

function TMVCDotEnv.UseLogger(
  const LoggerProc: TProc<String>): IMVCDotEnvBuilder;
begin
  if Assigned(fLoggerProc) then
  begin
    raise EMVCDotEnv.Create('Logger already set');
  end;
  fLoggerProc := LoggerProc;
  Result := Self;
end;

function TMVCDotEnv.UseProfile(const ProfileName: String): IMVCDotEnvBuilder;
begin
  CheckAlreadyBuilt;
  fProfiles.Add(ProfileName);
  Result := Self;
end;

function TMVCDotEnv.WithStrategy(const Priority: TMVCDotEnvPriority): IMVCDotEnvBuilder;
begin
  CheckAlreadyBuilt;
  Result := Self;
  fPriority := Priority;
end;

function TMVCDotEnv.Build(const DotEnvDirectory: string): IMVCDotEnv;
var
  lAllProfiles: TArray<String>;
begin
  if fState <> TdotEnvEngineState.created then
  begin
    raise EMVCDotEnv.Create('dotEnv engine already built');
  end;
  fState := TdotEnvEngineState.building;
  Result := Self;
  fEnvPath := TDirectory.GetParent(GetModuleName(HInstance));
  if not DotEnvDirectory.IsEmpty then
  begin
    fEnvPath := TPath.Combine(fEnvPath, DotEnvDirectory);
  end;
  DoLog('Path = ' + fEnvPath);
  fEnvDict.Clear;
  lAllProfiles := ['default'] + fProfiles.ToArray();
  DoLog('Active profile/s priority = [' + String.Join(',', lAllProfiles) + ']');
  ReadEnvFile;
  ExplodeReferences;
  fState := TdotEnvEngineState.built;
end;

procedure TMVCDotEnv.CheckAlreadyBuilt;
begin
  if fState in [TdotEnvEngineState.built] then
  begin
    raise Exception.Create('DotEnv Engine Already Built');
  end;
end;

function TMVCDotEnv.ClearProfiles: IMVCDotEnvBuilder;
begin
  CheckAlreadyBuilt;
  fProfiles.Clear;
  Result := Self;
end;

constructor TMVCDotEnv.Create;
begin
  inherited;
  fState := TdotEnvEngineState.created;
  fProfiles := TList<String>.Create;
  fEnvDict := TMVCDotEnvDictionary.Create;
  fEnvPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  fPriority := TMVCDotEnvPriority.EnvThenFile;
end;

destructor TMVCDotEnv.Destroy;
begin
  FreeAndNil(fEnvDict);
  fProfiles.Free;
  inherited;
end;

procedure TMVCDotEnv.DoLog(const Value: String);
begin
  if Assigned(fLoggerProc) then
  begin
    fLoggerProc(Value);
  end;
end;

function TMVCDotEnv.Env(const Name, DefaultValue: String): string;
begin
  Result := Env(Name);
  if Result.IsEmpty then
  begin
    Result := DefaultValue;
  end;
end;

function TMVCDotEnv.Env(const Name: string;
  const DefaultValue: Integer): Integer;
var
  lTmp: string;
begin
  lTmp := Env(Name);
  if lTmp.IsEmpty then
  begin
    Result := DefaultValue;
  end
  else
  begin
    if not TryStrToInt(lTmp, Result) then
    begin
      raise EMVCDotEnv.CreateFmt('Env "%s" is not a valid integer', [Name]);
    end;
  end;
end;

function TMVCDotEnv.Env(const Name: string;
  const DefaultValue: Boolean): Boolean;
var
  lTmp: string;
begin
  lTmp := Env(Name);
  if lTmp.IsEmpty then
  begin
    Result := DefaultValue;
  end
  else
  begin
    if not TryStrToBool(lTmp, Result) then
    begin
      raise EMVCDotEnv.CreateFmt('Env "%s" is not a valid boolean', [Name]);
    end;
  end;
end;

function TMVCDotEnv.ExplodePlaceholders(const Value: string): string;
var
  lStartPos, lEndPos: Integer;
  lKey, lValue: string;
begin
  Result := Value;
  while Result.IndexOf('${') > -1 do
  begin
    lStartPos := Result.IndexOf('${');
    lEndPos := Result.IndexOf('}');
    if (lEndPos = -1) or (lEndPos < lStartPos) then
    begin
      raise EMVCDotEnv.Create('Unclosed expansion (${...}) at: ' + Value);
    end;
    lKey := Result.Substring(lStartPos + 2, lEndPos - (lStartPos + 2));
    lValue := Env(lKey);
    Result := StringReplace(Result, '${' + lKey + '}', lValue, [rfReplaceAll]);
  end;
end;

procedure TMVCDotEnv.ExplodeReferences;
var
  lKey: String;
begin
  for lKey in fEnvDict.Keys do
  begin
    fEnvDict.AddOrSetValue(lKey, ExplodePlaceholders(fEnvDict[lKey]));
  end;
end;

function TMVCDotEnv.SaveToFile(const FileName: String): IMVCDotEnv;
var
  lKeys: TArray<String>;
  lKey: String;
  lSL: TStringList;
begin
  lKeys := fEnvDict.Keys.ToArray;
  TArray.Sort<String>(lKeys);
  lSL := TStringList.Create;
  try
    for lKey in lKeys do
    begin
      lSL.Values[lKey] := GetDotEnvVar(lKey);
    end;
    lSL.SaveToFile(FileName);
  finally
    lSL.Free;
  end;
  Result := Self;
end;

function TMVCDotEnv.ToArray: TArray<String>;
var
  lKeys: TArray<String>;
  lKey: String;
  I: Integer;
begin
  lKeys := fEnvDict.Keys.ToArray;
  TArray.Sort<String>(lKeys);
  SetLength(Result, Length(lKeys));
  I := 0;
  for lKey in lKeys do
  begin
    Result[I] := lKey + '=' + GetDotEnvVar(lKey);
    Inc(I);
  end;
end;

procedure TMVCDotEnv.PopulateDictionary(const EnvDict: TDictionary<string, string>; const EnvFilePath: String);
var
  lDotEnvCode: string;
  lParser: TMVCDotEnvParser;
begin
  if not TFile.Exists(EnvFilePath) then
  begin
    DoLog('Missed dotEnv file ' + EnvFilePath);
    Exit;
  end;

  lDotEnvCode := TFile.ReadAllText(EnvFilePath);
  lParser := TMVCDotEnvParser.Create;
  try
    lParser.Parse(fEnvDict, lDotEnvCode);
    DoLog('Applied dotEnv file ' + EnvFilePath);
  finally
    lParser.Free;
  end;
end;

procedure TMVCDotEnv.ReadEnvFile;
var
  lProfileEnvPath: string;
  I: Integer;
begin
  PopulateDictionary(fEnvDict, IncludeTrailingPathDelimiter(fEnvPath) + '.env');
  for I := 0 to fProfiles.Count - 1 do
  begin
    lProfileEnvPath := TPath.Combine(fEnvPath, '.env') + '.' + fProfiles[I];
    PopulateDictionary(fEnvDict, lProfileEnvPath);
  end;
end;

function NewDotEnv: IMVCDotEnvBuilder;
begin
  Result := TMVCDotEnv.Create;
end;

end.
