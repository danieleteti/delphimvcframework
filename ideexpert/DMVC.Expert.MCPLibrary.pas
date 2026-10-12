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

unit DMVC.Expert.MCPLibrary;

// Is mcp-server-delphi where a generated MCP project will find it? No ToolsAPI
// here: the caller passes the library search path of the IDE.

interface

const
  MCP_LIBRARY_URL = 'https://github.com/danieleteti/mcp-server-delphi';
  MCP_LIBRARY_MIN_VERSION = '0.9.0';

/// The folder of ASearchPath (';' separated, $(NAME) expanded from the process
/// environment) that holds the library, and the version its sources declare
/// ('' when it cannot be read). False when no folder has it.
function FindMCPLibrary(const ASearchPath: string; out AFolder, AVersion: string): Boolean;

/// True when AVersion ('0.9.0', '1.2.3-beta') is at least AMinimum
function MCPVersionAtLeast(const AVersion, AMinimum: string): Boolean;

/// '' when the library is there and recent enough, otherwise what to tell the user
function MCPLibraryWarning(const ASearchPath: string): string;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions;

function ExpandMacros(const APath: string): string;
var
  lMatches: TMatchCollection;
  lValue: string;
  I: Integer;
begin
  Result := APath;
  lMatches := TRegEx.Matches(APath, '\$\((\w+)\)');
  for I := lMatches.Count - 1 downto 0 do
  begin
    lValue := GetEnvironmentVariable(lMatches[I].Groups[1].Value);
    if lValue <> '' then // an unknown macro stays: the folder will simply not exist
      Result := Result.Remove(lMatches[I].Index - 1, lMatches[I].Length)
        .Insert(lMatches[I].Index - 1, lValue);
  end;
end;

function FindMCPLibrary(const ASearchPath: string; out AFolder, AVersion: string): Boolean;
var
  lEntry, lFolder, lTypes: string;
  lMatch: TMatch;
begin
  AFolder := '';
  AVersion := '';
  for lEntry in ASearchPath.Split([';'], TStringSplitOptions.ExcludeEmpty) do
  begin
    // quoted entries are common; TPath raises on a character a path cannot have
    lFolder := IncludeTrailingPathDelimiter(ExpandMacros(lEntry.Trim.DeQuotedString('"').Trim));
    try
      if not FileExists(lFolder + 'MVCFramework.MCP.Server.pas') then
        Continue;
      AFolder := ExcludeTrailingPathDelimiter(lFolder);
      lTypes := lFolder + 'MVCFramework.MCP.Types.pas';
      if FileExists(lTypes) then
      begin
        lMatch := TRegEx.Match(TFile.ReadAllText(lTypes),
          'MCP_SERVER_LIBRARY_VERSION\s*=\s*''([^'']*)''');
        if lMatch.Success then
          AVersion := lMatch.Groups[1].Value;
      end;
      Exit(True);
    except
      Continue; // an entry that cannot be read is an entry without the library
    end;
  end;
  Result := False;
end;

function MCPVersionAtLeast(const AVersion, AMinimum: string): Boolean;

  function Part(const AText: string; AIndex: Integer): Integer;
  var
    lParts: TArray<string>;
  begin
    // '0.9.0-beta' counts as 0.9.0
    lParts := AText.Split(['-', '+']);
    if Length(lParts) = 0 then
      Exit(0);
    lParts := lParts[0].Split(['.']);
    if (AIndex > High(lParts)) or not TryStrToInt(lParts[AIndex], Result) then
      Result := 0;
  end;

var
  I: Integer;
begin
  for I := 0 to 2 do
    if Part(AVersion, I) <> Part(AMinimum, I) then
      Exit(Part(AVersion, I) > Part(AMinimum, I));
  Result := True;
end;

function MCPLibraryWarning(const ASearchPath: string): string;
var
  lFolder, lVersion: string;
begin
  Result := '';
  if not FindMCPLibrary(ASearchPath, lFolder, lVersion) then
    Result := 'the MCP server needs mcp-server-delphi ' + MCP_LIBRARY_MIN_VERSION +
      ' or later, which is not in the library path: the project will not compile until its "sources" folder ' +
      'is added (Tools > Options > Language > Delphi > Library). Get it from ' + MCP_LIBRARY_URL
  else if (lVersion = '') or not MCPVersionAtLeast(lVersion, MCP_LIBRARY_MIN_VERSION) then
    Result := Format('the MCP server needs mcp-server-delphi %s or later; the library path has %s in %s. ' +
      'Update it from %s', [MCP_LIBRARY_MIN_VERSION, lVersion, lFolder, MCP_LIBRARY_URL]);
end;

end.
