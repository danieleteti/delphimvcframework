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

unit DMVC.Expert.AISkills;

{ The delphi-ai-skills (https://github.com/danieleteti/delphi-ai-skills) for a
  generated project: which skills fit it, the branch of the framework line the
  wizard was built with (dmvc-<major>.<minor>), and the install of that branch
  zip into <project>\.claude\skills. The generated update_ai_skills.bat does the
  same install with curl + tar, for later updates or when the wizard was offline. }

interface

uses
  System.SysUtils,
  JsonDataObjects,
  DMVC.Expert.SwaggerUI;

const
  AI_SKILLS_DEADLINE_MS = 30000;
  AI_SKILLS_FOLDER = '.claude\skills';

/// <summary>The framework version the wizard was built with.</summary>
function AISkillsDMVCVersion: string;
/// <summary>The framework line the wizard was built with, e.g. '3.5'.</summary>
function AISkillsLine: string;
/// <summary>The skills branch for that line, e.g. 'dmvc-3.5'.</summary>
function AISkillsRef: string;
function AISkillsZipURL: string;
/// <summary>The skills that fit the project described by AConfig.</summary>
function AISkillsFor(const AConfig: TJsonObject): TArray<string>;
/// <summary>One "- `path` - description" line per skill, for AGENTS.md.</summary>
function AISkillsMarkdownList(const ASkills: TArray<string>): string;
/// <summary>The framework checkout the IDE builds against ('' if not found):
/// the DMVC environment variable the generated project uses, else the folder
/// above the wizard package.</summary>
function AISkillsLocalDMVC: string;
/// <summary>$(BDS)\source of the running IDE with its CompilerVersion, or ''.</summary>
function AISkillsLocalDelphiSource: string;
/// <summary>Replaces <project>\.claude\skills\<skill> for each skill with the
/// branch zip's copy, plus VERSION. Returns '' or the problem.</summary>
function InstallAISkillsFromZip(const AZip: TBytes; const AProjectFolder: string;
  const ASkills: TArray<string>): string;
function InstallAISkills(const AProjectFolder: string; const ASkills: TArray<string>;
  const ACancelled: TSwaggerUICancelled): string; overload;
function InstallAISkills(const AProjectFolder: string; const ASkills: TArray<string>;
  const AURL: string; const ADeadlineMS: Cardinal; const ACancelled: TSwaggerUICancelled): string; overload;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Zip,
  System.StrUtils,
  DMVC.Expert.Commons;

{$I ..\sources\dmvcframeworkbuildconsts.inc}

type
  TAISkillInfo = record
    Name: string;
    Description: string;
  end;

const
  AI_SKILLS: array [0 .. 9] of TAISkillInfo = (
    (Name: 'delphi'; Description: 'the language and the RTL: version gating, lifetime, strings, generics, threading'),
    (Name: 'delphi-code-smells'; Description: 'code review: compiler warnings, static analysis, memory leaks'),
    (Name: 'dmvcframework'; Description: 'controllers, ActiveRecord, validation, DI, middleware, servers, dotEnv'),
    (Name: 'dmvcframework-minimal-api'; Description: 'lambda routes, route groups, filters'),
    (Name: 'dmvcframework-webapp'; Description: 'TemplatePro views, fragments, ViewData'),
    (Name: 'dmvcframework-ui'; Description: 'Bootstrap 5.3 layout, style.css, dark mode'),
    (Name: 'dmvcframework-security'; Description: 'REQUIRED for any endpoint taking client input'),
    (Name: 'dmvcframework-jsonrpc'; Description: 'JSON-RPC 2.0 services and client'),
    (Name: 'dmvcframework-testing'; Description: 'DUnitX integration tests'),
    (Name: 'htmx-skill'; Description: 'index of the official htmx.org docs'));

function AISkillsDMVCVersion: string;
begin
  Result := DMVCFRAMEWORK_VERSION;
end;

function AISkillsLine: string;
var
  lParts: TArray<string>;
begin
  lParts := DMVCFRAMEWORK_VERSION.Split(['.', '-']);
  Result := lParts[0] + '.' + lParts[1];
end;

function AISkillsRef: string;
begin
  Result := 'dmvc-' + AISkillsLine;
end;

function AISkillsZipURL: string;
begin
  Result := 'https://github.com/danieleteti/delphi-ai-skills/archive/refs/heads/' + AISkillsRef + '.zip';
end;

function AISkillsFor(const AConfig: TJsonObject): TArray<string>;
begin
  Result := ['delphi', 'delphi-code-smells', 'dmvcframework', 'dmvcframework-security', 'dmvcframework-testing'];
  if AConfig.B[TConfigKey.program_minimal_api] then
    Result := Result + ['dmvcframework-minimal-api'];
  // the web skills teach TemplatePro, not Mustache or WebStencils
  if AConfig.B[TConfigKey.program_ssv_templatepro] then
    Result := Result + ['dmvcframework-webapp', 'dmvcframework-ui'];
  if AConfig.B[TConfigKey.program_htmx] then
    Result := Result + ['htmx-skill'];
  if AConfig.B[TConfigKey.jsonrpc_generate] then
    Result := Result + ['dmvcframework-jsonrpc'];
end;

function AISkillsMarkdownList(const ASkills: TArray<string>): string;
var
  lSkill: string;
  I: Integer;
begin
  Result := '';
  for lSkill in ASkills do
    for I := Low(AI_SKILLS) to High(AI_SKILLS) do
      if AI_SKILLS[I].Name = lSkill then
        Result := Result + '- `' + AI_SKILLS_FOLDER.Replace('\', '/') + '/' + lSkill +
          '/SKILL.md` - ' + AI_SKILLS[I].Description + sLineBreak;
end;

function AISkillsLocalDMVC: string;

  function IsCheckout(const AFolder: string): Boolean;
  begin
    Result := (AFolder <> '') and
      TFile.Exists(TPath.Combine(AFolder, 'sources\dmvcframeworkbuildconsts.inc'));
  end;

begin
  Result := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('DMVC'));
  if IsCheckout(Result) then
    Exit;
  Result := TPath.GetDirectoryName(TPath.GetDirectoryName(ExtractFilePath(GetModuleName(HInstance))));
  if not IsCheckout(Result) then
    Result := '';
end;

function AISkillsLocalDelphiSource: string;
begin
  Result := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('BDS'));
  if (Result = '') or not TDirectory.Exists(TPath.Combine(Result, 'source')) then
    Exit('');
  Result := TPath.Combine(Result, 'source') +
    Format('   (CompilerVersion %.1f)', [CompilerVersion], TFormatSettings.Invariant);
end;

function InstallAISkillsFromZip(const AZip: TBytes; const AProjectFolder: string;
  const ASkills: TArray<string>): string;
var
  lZip: TZipFile;
  lStream: TBytesStream;
  lTarget, lEntry, lRel, lSkill, lDest: string;
  lData: TBytes;
  lFound: TArray<string>;
  lMissing: string;
  I, lSlash: Integer;
begin
  lTarget := TPath.Combine(AProjectFolder, AI_SKILLS_FOLDER);
  lStream := TBytesStream.Create(AZip);
  try
    lZip := TZipFile.Create;
    try
      try
        lZip.Open(lStream, zmRead);
      except
        on E: Exception do
          Exit('Not a valid zip: ' + E.Message);
      end;
      { the zip comes from the network: nothing may land outside the target,
        checked for every entry before anything already installed is touched }
      for I := 0 to lZip.FileCount - 1 do
        if lZip.FileName[I].Contains('..') or lZip.FileName[I].Contains(':') or
          lZip.FileName[I].Contains('\') or lZip.FileName[I].StartsWith('/') then
          Exit('Unsafe path in the zip: ' + lZip.FileName[I]);
      for lSkill in ASkills do
        if TDirectory.Exists(TPath.Combine(lTarget, lSkill)) then
          TDirectory.Delete(TPath.Combine(lTarget, lSkill), True); // an update drops renamed files too
      lFound := [];
      for I := 0 to lZip.FileCount - 1 do
      begin
        { <repo>-<ref>/skills/<skill>/...: drop the top folder, keep skills/ only }
        lEntry := lZip.FileName[I];
        lSlash := Pos('/', lEntry);
        if lSlash = 0 then
          Continue;
        lRel := Copy(lEntry, lSlash + 1, MaxInt);
        if not lRel.StartsWith('skills/') or lRel.EndsWith('/') then
          Continue;
        lRel := Copy(lRel, Length('skills/') + 1, MaxInt);
        lSkill := lRel.Split(['/'])[0];
        if (lRel <> 'VERSION') and not MatchStr(lSkill, ASkills) then
          Continue;
        if (lRel <> 'VERSION') and not MatchStr(lSkill, lFound) then
          lFound := lFound + [lSkill];
        lDest := TPath.Combine(lTarget, lRel.Replace('/', PathDelim));
        TDirectory.CreateDirectory(TPath.GetDirectoryName(lDest));
        lZip.Read(I, lData);
        TFile.WriteAllBytes(lDest, lData);
      end;
    finally
      lZip.Free;
    end;
  finally
    lStream.Free;
  end;
  lMissing := '';
  for lSkill in ASkills do
    if not MatchStr(lSkill, lFound) then
      lMissing := lMissing + ' ' + lSkill;
  if lMissing <> '' then
    Exit('Not in ' + AISkillsRef + ':' + lMissing);
  Result := '';
end;

function InstallAISkills(const AProjectFolder: string; const ASkills: TArray<string>;
  const ACancelled: TSwaggerUICancelled): string;
begin
  Result := InstallAISkills(AProjectFolder, ASkills, AISkillsZipURL, AI_SKILLS_DEADLINE_MS, ACancelled);
end;

function InstallAISkills(const AProjectFolder: string; const ASkills: TArray<string>;
  const AURL: string; const ADeadlineMS: Cardinal; const ACancelled: TSwaggerUICancelled): string;
begin
  try
    Result := InstallAISkillsFromZip(DownloadSwaggerUIZip(AURL, ADeadlineMS, ACancelled),
      AProjectFolder, ASkills);
  except
    on E: Exception do
      Result := 'Download of ' + AURL + ' failed: ' + E.Message;
  end;
end;

end.
