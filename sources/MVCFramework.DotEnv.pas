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

uses System.SysUtils, System.Generics.Collections, MVCFramework.DotEnv.Parser;

type
{$SCOPEDENUMS ON}
  TMVCDotEnvPriority = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);

  EMVCDotEnv = class(Exception)

  end;

  IMVCDotEnv = interface
    ['{5FD2C3CB-0895-4CCD-985F-27394798E4A8}']
    function WithStrategy(const Strategy: TMVCDotEnvPriority = TMVCDotEnvPriority.EnvThenFile): IMVCDotEnv;
    function UseProfile(const ProfileName: String): IMVCDotEnv;
    function ClearProfiles: IMVCDotEnv;
    function Build(const DotEnvPath: string = ''): IMVCDotEnv; overload;
    function Env(const Name: string): string; overload;
    function SaveToFile(const FileName: String): IMVCDotEnv;
    function IsFrozen: Boolean;
  end;


function GlobalDotEnv: IMVCDotEnv;
function NewDotEnv: IMVCDotEnv;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  System.Classes;

var
  gDotEnv: IMVCDotEnv = nil;

{ TDotEnv }

type
  TMVCDotEnv = class(TInterfacedObject, IMVCDotEnv)
  strict private
    fFrozen: Boolean;
    fPriority: TMVCDotEnvPriority;
    fEnvPath: string;
    fEnvDict: TMVCDotEnvDictionary;
    fProfiles: TList<String>;
    procedure ReadEnvFile;
    function GetDotEnvVar(const key: string): string;
    function ExplodePlaceholders(const Value: string): string;
    procedure PopulateDictionary(const EnvDict: TDictionary<string, string>; const EnvFilePath: String);
    procedure CheckAlreadyBuilt;
    procedure CheckNotBuilt;
    procedure ExplodeReferences;
  strict protected
    function WithStrategy(const Priority: TMVCDotEnvPriority = TMVCDotEnvPriority.EnvThenFile): IMVCDotEnv; overload;
    function UseProfile(const ProfileName: String): IMVCDotEnv;
    function ClearProfiles: IMVCDotEnv;
    function Build(const DotEnvDirectory: string = ''): IMVCDotEnv; overload;
    function IsFrozen: Boolean;
    function Env(const Name: string): string; overload;
    function SaveToFile(const FileName: String): IMVCDotEnv;
  public
    constructor Create;
    destructor Destroy; override;
  end;


function TMVCDotEnv.GetDotEnvVar(const key: string): string;
begin
  fEnvDict.TryGetValue(key, Result);
end;

function TMVCDotEnv.Env(const Name: string): string;
begin
  CheckNotBuilt;
  if fPriority in [TMVCDotEnvPriority.FileThenEnv, TMVCDotEnvPriority.OnlyFile] then
  begin
    Result := GetDotEnvVar(name);
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
      Exit(GetDotEnvVar(Name));
    end;
  end
  else
  begin
    raise Exception.Create('Unknown Strategy');
  end;
end;

function TMVCDotEnv.UseProfile(const ProfileName: String): IMVCDotEnv;
begin
  CheckAlreadyBuilt;
  fProfiles.Add(ProfileName);
  Result := Self;
end;

function TMVCDotEnv.WithStrategy(const Priority: TMVCDotEnvPriority): IMVCDotEnv;
begin
  CheckAlreadyBuilt;
  Result := Self;
  fPriority := Priority;
end;

function TMVCDotEnv.Build(const DotEnvDirectory: string): IMVCDotEnv;
begin
  CheckNotBuilt;
  Result := Self;
  fEnvPath := TDirectory.GetParent(GetModuleName(HInstance));
  if not DotEnvDirectory.IsEmpty then
  begin
    fEnvPath := TPath.Combine(fEnvPath, DotEnvDirectory);
  end;
  fEnvDict.Clear;
  ReadEnvFile;
  ExplodeReferences;
  fFrozen := True;
end;

procedure TMVCDotEnv.CheckAlreadyBuilt;
begin
  if fFrozen then
  begin
    raise Exception.Create('DotEnv Engine Already Built');
  end;
end;

procedure TMVCDotEnv.CheckNotBuilt;
begin
  if fFrozen then
  begin
    raise EMVCDotEnv.Create('Build must be called before use the engine');
  end;
end;

function TMVCDotEnv.ClearProfiles: IMVCDotEnv;
begin
  CheckAlreadyBuilt;
  fProfiles.Clear;
  Result := Self;
end;

constructor TMVCDotEnv.Create;
begin
  inherited;
  fFrozen := False;
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

function TMVCDotEnv.IsFrozen: Boolean;
begin
  Result := fFrozen;
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

procedure TMVCDotEnv.PopulateDictionary(const EnvDict: TDictionary<string, string>; const EnvFilePath: String);
var
  lDotEnvCode: string;
  lParser: TMVCDotEnvParser;
begin
  if not TFile.Exists(EnvFilePath) then
  begin
    Exit;
  end;

  lDotEnvCode := TFile.ReadAllText(EnvFilePath);
  lParser := TMVCDotEnvParser.Create;
  try
    lParser.Parse(fEnvDict, lDotEnvCode);
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


function GlobalDotEnv: IMVCDotEnv;
begin
  Result := gDotEnv;
end;

function NewDotEnv: IMVCDotEnv;
begin
  Result := TMVCDotEnv.Create;
end;

initialization

gDotEnv := NewDotEnv;

end.
