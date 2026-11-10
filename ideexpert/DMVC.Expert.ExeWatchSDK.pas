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

unit DMVC.Expert.ExeWatchSDK;

// Will a generated project find the ExeWatch SDK, and is it the newest one? No
// ToolsAPI here: the caller passes the library search path of the IDE. The setup of
// ExeWatch for RAD Studio sets the environment variable EXEWATCH_SDK to the SDK
// folder; the folder may or may not be in the library path as well.
//
// The setup refuses to run while RAD Studio is open (it copies a package into the
// IDE), and the IDE reads EXEWATCH_SDK only when it starts: so the wizard, which runs
// inside the IDE, offers the download and does not run the setup itself.

interface

const
  EXEWATCH_URL = 'https://exewatch.com';
  EXEWATCH_SDK_ENV = 'EXEWATCH_SDK';
  EXEWATCH_SETUP_URL = EXEWATCH_URL + '/ui/sdk/delphi/ExeWatchRADStudioSetup.exe';
  // the newest SDK version and where its setup is (the API of ExeWatch itself)
  EXEWATCH_LATEST_URL = EXEWATCH_URL + '/api/v1/sdk/latest?type=delphi';

type
  TExeWatchSDKLocation = (
    eslNotFound,
    eslLibraryPath, // the compiler finds it with no help from the project
    eslEnvironment  // only $(EXEWATCH_SDK): the project needs that folder in its unit search path
  );

  TExeWatchSDK = record
    Location: TExeWatchSDKLocation;
    Folder: string;
    Version: string; // '' when the sources do not say
  end;

/// Where the SDK is, the folder and the version its sources declare ('' when
/// it cannot be read). The library path wins over EXEWATCH_SDK.
function FindExeWatchSDK(const ASearchPath: string; out AFolder, AVersion: string): TExeWatchSDKLocation;

function LocateExeWatchSDK(const ASearchPath: string): TExeWatchSDK;

/// '' when the SDK is not found, otherwise a line for the user: version and folder
function DescribeExeWatchSDK(const ASDK: TExeWatchSDK): string;
function ExeWatchSDKDescription(const ASearchPath: string): string;

/// What the project must add to its unit search path to compile: '$(EXEWATCH_SDK)'
/// when only the environment variable leads to the SDK, '' otherwise
function ExeWatchSDKProjectSearchPath(const ASearchPath: string): string;

/// True when AInstalled is lower than ALatest ('0.24.0' < '0.25.0'; a suffix such
/// as '-beta' is ignored). False when one of them is empty: no verdict.
function ExeWatchVersionIsOlder(const AInstalled, ALatest: string): Boolean;

/// The newest version and the absolute URL of its setup ('' when the server has
/// no setup to offer) from the answer of EXEWATCH_LATEST_URL. False when the text
/// is not that answer.
function ParseExeWatchLatest(const AJSON: string; out AVersion, ASetupURL: string): Boolean;

/// Asks ExeWatch for the newest SDK. Sends nothing about the user or the project;
/// False on any failure (offline, timeout, an answer that is not understood).
/// Blocks up to a few seconds: call it from a thread.
function FetchExeWatchLatest(out AVersion, ASetupURL: string): Boolean;

type
  /// The answer of FetchExeWatchLatest, asked in a thread of its own. Whoever holds the
  /// interface keeps it alive, the thread included: a form that closes while the request
  /// runs is safe, nothing in here touches it.
  IExeWatchLatestCheck = interface
    ['{5B0C7C31-6D34-4C58-9E38-0F6E1A8B2D47}']
    function IsDone: Boolean;
    function Version: string;   // '' when the check failed (offline, timeout...)
    function SetupURL: string;  // '' when the server has no setup to offer
  end;

/// Returns at once; poll IsDone from the thread of the form.
function StartExeWatchLatestCheck: IExeWatchLatestCheck;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  System.SyncObjs,
  System.Net.HttpClient,
  JsonDataObjects;

const
  SDK_UNIT = 'ExeWatchSDKv1.pas';
  VERSION_HEAD_BYTES = 16 * 1024;
  CONNECT_TIMEOUT_MS = 3000;
  RESPONSE_TIMEOUT_MS = 4000;

// $(NAME) from the process environment, as the IDE does for the library path
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

function HasSDK(const AEntry: string; out AFolder: string): Boolean;
begin
  AFolder := '';
  // quoted entries are common; TPath raises on a character a path cannot have
  try
    AFolder := ExcludeTrailingPathDelimiter(ExpandMacros(AEntry.Trim.DeQuotedString('"').Trim));
    Result := (AFolder <> '') and FileExists(IncludeTrailingPathDelimiter(AFolder) + SDK_UNIT);
  except
    Result := False; // an entry that cannot be read is an entry without the SDK
  end;
end;

function SDKVersion(const AFolder: string): string;
var
  lMatch: TMatch;
  lStream: TFileStream;
  lHead: TBytes;
begin
  Result := '';
  try
    // the constant is among the first lines of a unit of 260 KB, and the form asks at every click
    lStream := TFileStream.Create(IncludeTrailingPathDelimiter(AFolder) + SDK_UNIT, fmOpenRead or fmShareDenyNone);
    try
      SetLength(lHead, VERSION_HEAD_BYTES);
      SetLength(lHead, lStream.Read(lHead, Length(lHead)));
    finally
      lStream.Free;
    end;
    lMatch := TRegEx.Match(TEncoding.ANSI.GetString(lHead), 'EXEWATCH_SDK_VERSION\s*=\s*''([^'']*)''');
    if lMatch.Success then
      Result := lMatch.Groups[1].Value;
  except
    Result := '';
  end;
end;

function FindExeWatchSDK(const ASearchPath: string; out AFolder, AVersion: string): TExeWatchSDKLocation;
var
  lEntry: string;
begin
  AFolder := '';
  AVersion := '';
  Result := eslNotFound;
  for lEntry in ASearchPath.Split([';'], TStringSplitOptions.ExcludeEmpty) do
    if HasSDK(lEntry, AFolder) then
    begin
      Result := eslLibraryPath;
      Break;
    end;
  if (Result = eslNotFound) and HasSDK('$(' + EXEWATCH_SDK_ENV + ')', AFolder) then
    Result := eslEnvironment;
  if Result = eslNotFound then
    AFolder := '' // HasSDK leaves the last folder it looked at
  else
    AVersion := SDKVersion(AFolder);
end;

function LocateExeWatchSDK(const ASearchPath: string): TExeWatchSDK;
begin
  Result.Location := FindExeWatchSDK(ASearchPath, Result.Folder, Result.Version);
end;

function DescribeExeWatchSDK(const ASDK: TExeWatchSDK): string;
begin
  if ASDK.Location = eslNotFound then
    Exit('');
  if ASDK.Version <> '' then
    Result := 'ExeWatch SDK ' + ASDK.Version + ' in ' + ASDK.Folder
  else
    Result := 'ExeWatch SDK in ' + ASDK.Folder;
end;

function ExeWatchSDKDescription(const ASearchPath: string): string;
begin
  Result := DescribeExeWatchSDK(LocateExeWatchSDK(ASearchPath));
end;

function ExeWatchSDKProjectSearchPath(const ASearchPath: string): string;
begin
  if LocateExeWatchSDK(ASearchPath).Location = eslEnvironment then
    Result := '$(' + EXEWATCH_SDK_ENV + ')'
  else
    Result := '';
end;

function ExeWatchVersionIsOlder(const AInstalled, ALatest: string): Boolean;

  function Part(const AText: string; AIndex: Integer): Integer;
  var
    lParts: TArray<string>;
  begin
    // '0.25.0-beta' counts as 0.25.0
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
  if (AInstalled.Trim = '') or (ALatest.Trim = '') then
    Exit(False);
  for I := 0 to 2 do
    if Part(AInstalled, I) <> Part(ALatest, I) then
      Exit(Part(AInstalled, I) < Part(ALatest, I));
  Result := False;
end;

function ParseExeWatchLatest(const AJSON: string; out AVersion, ASetupURL: string): Boolean;
var
  lRoot: TJsonBaseObject;
  lURL: string;
begin
  AVersion := '';
  ASetupURL := '';
  Result := False;
  try
    lRoot := TJsonBaseObject.Parse(AJSON);
    try
      if not (lRoot is TJsonObject) then
        Exit;
      // a JSON null (download_url when the server has no setup to offer) is not a string: reading it as one raises
      if TJsonObject(lRoot).Types['latest'] = jdtString then
        AVersion := TJsonObject(lRoot).S['latest'];
      if TJsonObject(lRoot).Types['download_url'] = jdtString then
        lURL := TJsonObject(lRoot).S['download_url'];
      // only the setup of ExeWatch itself: the wizard opens this address in the browser
      if lURL.StartsWith('/') then
        ASetupURL := EXEWATCH_URL + lURL
      else if lURL.StartsWith(EXEWATCH_URL + '/') then
        ASetupURL := lURL;
      Result := AVersion <> '';
    finally
      lRoot.Free;
    end;
  except
    AVersion := '';
    ASetupURL := '';
    Result := False;
  end;
end;

function FetchExeWatchLatest(out AVersion, ASetupURL: string): Boolean;
var
  lClient: THTTPClient;
  lResponse: IHTTPResponse;
begin
  AVersion := '';
  ASetupURL := '';
  try
    lClient := THTTPClient.Create;
    try
      lClient.ConnectionTimeout := CONNECT_TIMEOUT_MS;
      lClient.ResponseTimeout := RESPONSE_TIMEOUT_MS;
      lResponse := lClient.Get(EXEWATCH_LATEST_URL);
      Result := (lResponse.StatusCode = 200) and ParseExeWatchLatest(lResponse.ContentAsString, AVersion, ASetupURL);
    finally
      lClient.Free;
    end;
  except
    AVersion := '';
    ASetupURL := '';
    Result := False;
  end;
end;

type
  TExeWatchLatestCheck = class(TInterfacedObject, IExeWatchLatestCheck)
  strict private
    fDone: Integer; // 1 when the strings below are final; written last, read first
    fVersion: string;
    fSetupURL: string;
  public
    procedure Run;
    function IsDone: Boolean;
    function Version: string;
    function SetupURL: string;
  end;

procedure TExeWatchLatestCheck.Run;
var
  lVersion, lSetupURL: string;
begin
  if FetchExeWatchLatest(lVersion, lSetupURL) then
  begin
    fVersion := lVersion;
    fSetupURL := lSetupURL;
  end;
  TInterlocked.Exchange(fDone, 1);
end;

function TExeWatchLatestCheck.IsDone: Boolean;
begin
  Result := TInterlocked.CompareExchange(fDone, 0, 0) = 1;
end;

function TExeWatchLatestCheck.Version: string;
begin
  Result := fVersion;
end;

function TExeWatchLatestCheck.SetupURL: string;
begin
  Result := fSetupURL;
end;

function StartExeWatchLatestCheck: IExeWatchLatestCheck;
var
  lCheck: TExeWatchLatestCheck;
  lHold: IExeWatchLatestCheck;
begin
  lCheck := TExeWatchLatestCheck.Create;
  lHold := lCheck; // the thread's own reference
  TThread.CreateAnonymousThread(
    procedure
    begin
      lCheck.Run;
      lHold := nil;
    end).Start;
  Result := lCheck;
end;

end.
