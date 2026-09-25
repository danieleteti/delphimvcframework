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

unit DMVC.Expert.SwaggerUI;

{ Puts Swagger UI into a generated project: downloads the official release
  archive of swagger-api/swagger-ui, checks its SHA-256, extracts the files of
  its "dist" folder the page needs and points swagger-initializer.js at the
  project's document. No IDE dependency, so the template tests can call it.

  It never raises and never waits more than SWAGGER_UI_DEADLINE_MS in total
  (connection, request and download together); the caller can also cancel it.
  When something goes wrong (offline, proxy, timeout, cancel, hash mismatch,
  unsafe archive) README-swagger-ui.txt explains how to add the files by hand,
  and the reason is returned to the caller: the README is served with the UI,
  so it carries neither the reason nor local paths. }

interface

uses
  System.SysUtils;

type
  TSwaggerUIRelease = record
    Version: string;
    SHA256: string; // of the GitHub archive of the tag, see SwaggerUIDownloadURL
  end;

const
  // Bump Version and SHA256 together (certutil -hashfile <zip> SHA256).
  SWAGGER_UI_RELEASE: TSwaggerUIRelease = (
    Version: '5.33.0';
    SHA256: 'c7817b8c74f8a63a7fac9ca9e86d5563096bc7130a078bc27328d88bb4844e6c');
  SWAGGER_UI_README = 'README-swagger-ui.txt';
  // Total time allowed to connect, send and download.
  SWAGGER_UI_DEADLINE_MS = 30000;

type
  /// <summary>Polled while downloading: return True to stop.</summary>
  TSwaggerUICancelled = reference to function: Boolean;

function SwaggerUIDownloadURL(const AVersion: string): string;

/// <summary>Downloads done by this process (the template tests download once).</summary>
function SwaggerUIDownloadCount: Integer;

/// <summary>Downloads AURL within ADeadlineMS; raises on failure, timeout or cancel.</summary>
function DownloadSwaggerUIZip(const AURL: string; const ADeadlineMS: Cardinal;
  const ACancelled: TSwaggerUICancelled): TBytes;

/// <summary>Downloads SWAGGER_UI_RELEASE into ATargetFolder. Returns '' on
/// success, otherwise the reason; in that case README-swagger-ui.txt is written.</summary>
function InstallSwaggerUI(const ATargetFolder, ADocumentURL: string;
  const ACancelled: TSwaggerUICancelled = nil): string; overload;
/// <summary>Same, from another URL and with another deadline (tests).</summary>
function InstallSwaggerUI(const ATargetFolder, ADocumentURL, ADownloadURL: string;
  const ADeadlineMS: Cardinal; const ACancelled: TSwaggerUICancelled): string; overload;

/// <summary>Same as InstallSwaggerUI, from an archive already in memory.</summary>
function InstallSwaggerUIFromZip(const AZip: TBytes; const ARelease: TSwaggerUIRelease;
  const ATargetFolder, ADocumentURL: string): string;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Hash,
  System.Zip,
  System.Generics.Collections,
  System.Diagnostics,
  System.SyncObjs,
  System.Types,
  System.Math,
  System.Net.HttpClient;

const
  CONNECT_TIMEOUT_MS = 10000;
  // after Cancel, how long to wait for the request thread to let go of the client
  CANCEL_GRACE_MS = 5000;

function SwaggerUIDownloadURL(const AVersion: string): string;
begin
  Result := 'https://github.com/swagger-api/swagger-ui/archive/refs/tags/v' + AVersion + '.zip';
end;

function InitializerJS(const ADocumentURL: string): string;
begin
  Result :=
    'window.onload = function() {' + sLineBreak +
    '  window.ui = SwaggerUIBundle({' + sLineBreak +
    '    url: "' + ADocumentURL + '",' + sLineBreak +
    '    dom_id: ''#swagger-ui'',' + sLineBreak +
    '    deepLinking: true,' + sLineBreak +
    '    validatorUrl: null,' + sLineBreak +
    '    presets: [' + sLineBreak +
    '      SwaggerUIBundle.presets.apis,' + sLineBreak +
    '      SwaggerUIStandalonePreset' + sLineBreak +
    '    ],' + sLineBreak +
    '    plugins: [' + sLineBreak +
    '      SwaggerUIBundle.plugins.DownloadUrl' + sLineBreak +
    '    ],' + sLineBreak +
    '    layout: "StandaloneLayout"' + sLineBreak +
    '  });' + sLineBreak +
    '};' + sLineBreak;
end;

procedure WriteReadme(const ATargetFolder, ADocumentURL: string;
  const ARelease: TSwaggerUIRelease);
begin
  try
    TDirectory.CreateDirectory(ATargetFolder);
    TFile.WriteAllText(TPath.Combine(ATargetFolder, SWAGGER_UI_README),
      'Swagger UI was not added to this folder when the project was created.' + sLineBreak +
      sLineBreak +
      'To add it by hand:' + sLineBreak +
      '1. Download ' + SwaggerUIDownloadURL(ARelease.Version) + sLineBreak +
      '   (expected SHA-256: ' + ARelease.SHA256 + ')' + sLineBreak +
      '2. Copy the files of the "dist" folder of the archive into this folder' + sLineBreak +
      '   (the *.map files are not needed).' + sLineBreak +
      '3. In swagger-initializer.js set the document URL:  url: "' + ADocumentURL + '",' + sLineBreak +
      '4. Start the server and open the path that serves this folder (/swagger/).' + sLineBreak,
      TEncoding.ASCII);
  except
    // the README is a courtesy: failing to write it must not fail the project
  end;
end;

{ Only the files the page loads, plus the license notices. The archive carries
  the whole source tree; the dist folder also has the npm-only bundles. }
function IsNeeded(const AFileName: string): Boolean;
begin
  Result := not (AFileName.EndsWith('.map', True) or
    SameText(AFileName, 'swagger-ui.js') or
    AFileName.StartsWith('swagger-ui-es-bundle', True));
end;

function InstallSwaggerUIFromZip(const AZip: TBytes; const ARelease: TSwaggerUIRelease;
  const ATargetFolder, ADocumentURL: string): string;
var
  lHash: THashSHA2;
  lZip: TZipFile;
  lStream: TBytesStream;
  lRoot: string;
  lName, lRel, lTarget: string;
  lSlash: Integer;
  I: Integer;
  lPlan: TList<TPair<Integer, string>>;
  lEntry: TPair<Integer, string>;
  lBytes: TBytes;
  lHasIndex, lHasBundle: Boolean;
  lWriting: Boolean;
begin
  Result := '';
  lWriting := False;
  try
    lHash := THashSHA2.Create;
    lHash.Update(AZip);
    if not SameText(lHash.HashAsString, ARelease.SHA256) then
      raise Exception.CreateFmt('SHA-256 mismatch: expected %s, got %s',
        [ARelease.SHA256, lHash.HashAsString]);

    lRoot := IncludeTrailingPathDelimiter(TPath.GetFullPath(ATargetFolder));
    lPlan := TList<TPair<Integer, string>>.Create;
    lStream := TBytesStream.Create(AZip);
    lZip := TZipFile.Create;
    try
      lZip.Open(lStream, zmRead);
      lHasIndex := False;
      lHasBundle := False;
      { Plan every entry first: one unsafe entry means nothing is written. }
      for I := 0 to lZip.FileCount - 1 do
      begin
        lName := lZip.FileNames[I].Replace('\', '/');
        lSlash := lName.IndexOf('/');
        if lSlash < 0 then
          Continue;
        lRel := lName.Substring(lSlash + 1); // drop "swagger-ui-<version>/"
        if lRel.StartsWith('dist/') then
          lRel := lRel.Substring(5)
        else if not (SameText(lRel, 'LICENSE') or SameText(lRel, 'NOTICE')) then
          Continue;
        if lRel.IsEmpty or lRel.EndsWith('/') or not IsNeeded(TPath.GetFileName(lRel)) then
          Continue;
        lTarget := TPath.GetFullPath(TPath.Combine(lRoot, lRel.Replace('/', PathDelim)));
        if not lTarget.StartsWith(lRoot, True) then
          raise Exception.CreateFmt('Unsafe archive entry rejected: "%s"', [lZip.FileNames[I]]);
        lHasIndex := lHasIndex or SameText(lRel, 'index.html');
        lHasBundle := lHasBundle or SameText(lRel, 'swagger-ui-bundle.js');
        lPlan.Add(TPair<Integer, string>.Create(I, lTarget));
      end;
      if not (lHasIndex and lHasBundle) then
        raise Exception.Create('The archive has no Swagger UI "dist" folder');

      lWriting := True;
      for lEntry in lPlan do
      begin
        lZip.Read(lEntry.Key, lBytes);
        TDirectory.CreateDirectory(TPath.GetDirectoryName(lEntry.Value));
        TFile.WriteAllBytes(lEntry.Value, lBytes);
      end;
    finally
      lZip.Free;
      lStream.Free;
      lPlan.Free;
    end;

    TFile.WriteAllBytes(TPath.Combine(lRoot, 'swagger-initializer.js'),
      TEncoding.UTF8.GetBytes(InitializerJS(ADocumentURL)));
  except
    on E: Exception do
    begin
      if lWriting then
        Result := 'Swagger UI installation incomplete: ' + E.Message
      else
        Result := E.Message;
      WriteReadme(ATargetFolder, ADocumentURL, ARelease);
      Exit;
    end;
  end;
  { A README left by an earlier failed attempt: the UI is there now. Not being
    able to delete it (read-only, locked) does not undo a good install. }
  try
    if TFile.Exists(TPath.Combine(ATargetFolder, SWAGGER_UI_README)) then
      TFile.Delete(TPath.Combine(ATargetFolder, SWAGGER_UI_README));
  except
    // non-fatal
  end;
end;

var
  gDownloads: Integer;

function SwaggerUIDownloadCount: Integer;
begin
  Result := gDownloads;
end;

function DownloadSwaggerUIZip(const AURL: string; const ADeadlineMS: Cardinal;
  const ACancelled: TSwaggerUICancelled): TBytes;
var
  lClient: THTTPClient;
  lResponse: IHTTPResponse;
  lBody: TBytesStream;
  lAsync: IAsyncResult;
  lWatch: TStopwatch;
  lStop: string;
  lLeaked: Boolean;
begin
  TInterlocked.Increment(gDownloads);
  lLeaked := False;
  lBody := TBytesStream.Create;
  try
    lClient := THTTPClient.Create;
    try
      { The per-phase timeouts alone do not bound the total (a server trickling
        bytes never trips ResponseTimeout): the request runs asynchronously and
        is cancelled when the deadline expires or the caller cancels. }
      lClient.ConnectionTimeout := Min(CONNECT_TIMEOUT_MS, Integer(ADeadlineMS));
{$IF CompilerVersion >= 34.0}
      lClient.SendTimeout := Integer(ADeadlineMS); // THTTPClient.SendTimeout: 10.4 Sydney and later
{$ENDIF}
      lClient.ResponseTimeout := Integer(ADeadlineMS);
      lWatch := TStopwatch.StartNew;
      lAsync := lClient.BeginGet(AURL, lBody);
      lStop := '';
      while not lAsync.IsCompleted do
      begin
        if Assigned(ACancelled) and ACancelled() then
          lStop := 'Download cancelled'
        else if lWatch.ElapsedMilliseconds >= ADeadlineMS then
          lStop := Format('Download stopped: no answer within %d s from %s', [ADeadlineMS div 1000, AURL]);
        if lStop <> '' then
        begin
          lAsync.Cancel;
          lWatch := TStopwatch.StartNew;
          while (not lAsync.IsCompleted) and (lWatch.ElapsedMilliseconds < CANCEL_GRACE_MS) do
            TThread.Sleep(20);
          if not lAsync.IsCompleted then
          begin
            lLeaked := True; // ponytail: still busy after the grace time, leaked rather than freed under its thread
            lClient := nil;
          end;
          raise Exception.Create(lStop);
        end;
        TThread.Sleep(50);
      end;
      lResponse := THTTPClient.EndAsyncHTTP(lAsync);
      if lResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('HTTP %d %s from %s',
          [lResponse.StatusCode, lResponse.StatusText, AURL]);
    finally
      lClient.Free;
    end;
    Result := Copy(lBody.Bytes, 0, lBody.Size);
  finally
    if not lLeaked then // a leaked request may still write into it
      lBody.Free;
  end;
end;

function InstallSwaggerUI(const ATargetFolder, ADocumentURL: string;
  const ACancelled: TSwaggerUICancelled): string;
begin
  Result := InstallSwaggerUI(ATargetFolder, ADocumentURL,
    SwaggerUIDownloadURL(SWAGGER_UI_RELEASE.Version), SWAGGER_UI_DEADLINE_MS, ACancelled);
end;

function InstallSwaggerUI(const ATargetFolder, ADocumentURL, ADownloadURL: string;
  const ADeadlineMS: Cardinal; const ACancelled: TSwaggerUICancelled): string;
var
  lURL: string;
begin
  lURL := ADownloadURL;
  try
    Result := InstallSwaggerUIFromZip(DownloadSwaggerUIZip(lURL, ADeadlineMS, ACancelled),
      SWAGGER_UI_RELEASE, ATargetFolder, ADocumentURL);
  except
    on E: Exception do
    begin
      Result := 'Download of ' + lURL + ' failed: ' + E.Message;
      WriteReadme(ATargetFolder, ADocumentURL, SWAGGER_UI_RELEASE);
    end;
  end;
end;

end.
