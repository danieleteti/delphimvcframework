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
// *************************************************************************** }

// ---------------------------------------------------------------------------
// Range Request Middleware for DMVCFramework
//
// Intercepts requests to a configurable URL path prefix and serves files
// with HTTP Range support (RFC 7233). This enables seeking in HTML5
// <audio> and <video> elements when the built-in static file middleware
// does not handle Range headers.
//
// Usage:
//   Engine.AddMiddleware(
//     UseRangeMediaMiddleware('/static/media', 'static/media')
//   );
// ---------------------------------------------------------------------------

unit MVCFramework.Middleware.RangeMedia;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.Generics.Collections;

type
  /// <summary>
  /// Serves files from a document root with HTTP Range request support
  /// (RFC 7233). Handles single-range byte requests returning 206 Partial
  /// Content, and falls back to full 200 responses when no Range header
  /// is present.
  /// </summary>
  TMVCRangeMediaMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FURLPath: string;
    FDocumentRoot: string;
    FMediaTypes: TDictionary<string, string>;

    /// <summary>
    /// Populates the MIME type dictionary with common media file extensions.
    /// </summary>
    procedure InitMediaTypes;

    /// <summary>
    /// Resolves a request path to a filesystem path and validates it against
    /// directory traversal attacks.
    /// </summary>
    /// <param name="APathInfo">The request URL path</param>
    /// <param name="AFileName">Resolved absolute file path (output)</param>
    /// <returns>True if the path is valid and within the document root</returns>
    function ResolveFilePath(const APathInfo: string;
      out AFileName: string): Boolean;

    /// <summary>
    /// Returns the MIME type for a file extension, defaulting to
    /// application/octet-stream for unknown types.
    /// </summary>
    function GetMediaType(const AFileName: string): string;

    /// <summary>
    /// Parses a single-range "bytes=START-END" Range header value.
    /// </summary>
    /// <param name="ARangeHeader">Raw Range header value</param>
    /// <param name="AFileSize">Total file size for resolving open-ended ranges</param>
    /// <param name="ARangeStart">Parsed start byte (output)</param>
    /// <param name="ARangeEnd">Parsed end byte inclusive (output)</param>
    /// <returns>True if the range is valid and satisfiable</returns>
    function ParseRangeHeader(const ARangeHeader: string;
      AFileSize: Int64; out ARangeStart, ARangeEnd: Int64): Boolean;

    /// <summary>
    /// Serves the file with Range support. Returns True if the request was
    /// handled (either 200, 206, or 416).
    /// </summary>
    function ServeFile(AContext: TWebContext;
      const AFileName: string): Boolean;
  public
    constructor Create(const AURLPath: string; const ADocumentRoot: string);
    destructor Destroy; override;

    { IMVCMiddleware }
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

/// <summary>
/// Factory function to create a Range-aware media file middleware.
/// </summary>
/// <param name="AURLPath">URL prefix to intercept (e.g. '/static/media')</param>
/// <param name="ADocumentRoot">Filesystem root relative to the application
/// directory (e.g. 'static/media')</param>
/// <returns>Configured middleware instance</returns>
function UseRangeMediaMiddleware(
  const AURLPath: string;
  const ADocumentRoot: string): TMVCRangeMediaMiddleware;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Math;

{ Factory function }

function UseRangeMediaMiddleware(
  const AURLPath: string;
  const ADocumentRoot: string): TMVCRangeMediaMiddleware;
begin
  Result := TMVCRangeMediaMiddleware.Create(AURLPath, ADocumentRoot);
end;

{ TMVCRangeMediaMiddleware }

constructor TMVCRangeMediaMiddleware.Create(
  const AURLPath: string; const ADocumentRoot: string);
begin
  inherited Create;
  FURLPath := AURLPath.TrimRight(['/']);
  FDocumentRoot := TPath.Combine(AppPath, ADocumentRoot);
  // Ensure trailing separator so StartsWith check is unambiguous:
  // '/app/media' must not match sibling directory '/app/media_evil'
  if not FDocumentRoot.EndsWith(TPath.DirectorySeparatorChar) then
    FDocumentRoot := FDocumentRoot + TPath.DirectorySeparatorChar;
  FMediaTypes := TDictionary<string, string>.Create;
  InitMediaTypes;
end;

destructor TMVCRangeMediaMiddleware.Destroy;
begin
  FMediaTypes.Free;
  inherited;
end;

procedure TMVCRangeMediaMiddleware.InitMediaTypes;
begin
  // Audio formats
  FMediaTypes.Add('.m4a', 'audio/mp4');
  FMediaTypes.Add('.mp3', 'audio/mpeg');
  FMediaTypes.Add('.ogg', 'audio/ogg');
  FMediaTypes.Add('.opus', 'audio/opus');
  FMediaTypes.Add('.wav', 'audio/wav');
  FMediaTypes.Add('.flac', 'audio/flac');
  FMediaTypes.Add('.aac', 'audio/aac');
  FMediaTypes.Add('.weba', 'audio/webm');
  // Video formats
  FMediaTypes.Add('.mp4', 'video/mp4');
  FMediaTypes.Add('.webm', 'video/webm');
  FMediaTypes.Add('.ogv', 'video/ogg');
  FMediaTypes.Add('.mkv', 'video/x-matroska');
  FMediaTypes.Add('.avi', 'video/x-msvideo');
  FMediaTypes.Add('.mov', 'video/quicktime');
end;

function TMVCRangeMediaMiddleware.ResolveFilePath(
  const APathInfo: string; out AFileName: string): Boolean;
var
  LRelativePath: string;
  LFullPath: string;
begin
  Result := False;
  AFileName := '';

  // Extract relative path after the URL prefix
  LRelativePath := APathInfo.Substring(FURLPath.Length);
  LRelativePath := LRelativePath.TrimLeft(['/']);

  if LRelativePath.IsEmpty then
    Exit;

  // Convert URL separators to OS path separators
  LRelativePath := LRelativePath.Replace('/', TPath.DirectorySeparatorChar);

  LFullPath := TPath.Combine(FDocumentRoot, LRelativePath);
  LFullPath := TPath.GetFullPath(LFullPath);

  // Directory traversal protection: FDocumentRoot has a trailing separator,
  // so this check is unambiguous (e.g. '/app/media/' does not match '/app/media_evil/...')
  if not LFullPath.StartsWith(FDocumentRoot, True) then
    Exit;

  if not TFile.Exists(LFullPath) then
    Exit;

  AFileName := LFullPath;
  Result := True;
end;

function TMVCRangeMediaMiddleware.GetMediaType(
  const AFileName: string): string;
var
  LExt: string;
begin
  LExt := LowerCase(TPath.GetExtension(AFileName));
  if not FMediaTypes.TryGetValue(LExt, Result) then
    Result := 'application/octet-stream';
end;

function TMVCRangeMediaMiddleware.ParseRangeHeader(
  const ARangeHeader: string; AFileSize: Int64;
  out ARangeStart, ARangeEnd: Int64): Boolean;
var
  LBytesSpec: string;
  LDashPos: Integer;
  LStartStr, LEndStr: string;
begin
  Result := False;
  ARangeStart := 0;
  ARangeEnd := AFileSize - 1;

  // Only support "bytes=" prefix
  if not ARangeHeader.StartsWith('bytes=', True) then
    Exit;

  LBytesSpec := ARangeHeader.Substring(6).Trim;

  // Only support single ranges (no multi-range)
  if LBytesSpec.Contains(',') then
    Exit;

  LDashPos := LBytesSpec.IndexOf('-');
  if LDashPos < 0 then
    Exit;

  LStartStr := LBytesSpec.Substring(0, LDashPos).Trim;
  LEndStr := LBytesSpec.Substring(LDashPos + 1).Trim;

  if LStartStr.IsEmpty and LEndStr.IsEmpty then
    Exit;

  if LStartStr.IsEmpty then
  begin
    // Suffix range: "bytes=-500" means last 500 bytes
    if not TryStrToInt64(LEndStr, ARangeEnd) then
      Exit;
    ARangeStart := Max(0, AFileSize - ARangeEnd);
    ARangeEnd := AFileSize - 1;
  end
  else
  begin
    if not TryStrToInt64(LStartStr, ARangeStart) then
      Exit;
    if not LEndStr.IsEmpty then
    begin
      if not TryStrToInt64(LEndStr, ARangeEnd) then
        Exit;
    end
    else
      ARangeEnd := AFileSize - 1;
  end;

  // Clamp end to file size
  ARangeEnd := Min(ARangeEnd, AFileSize - 1);

  // Validate range
  Result := (ARangeStart >= 0) and (ARangeStart <= ARangeEnd) and
    (ARangeEnd < AFileSize);
end;

function TMVCRangeMediaMiddleware.ServeFile(
  AContext: TWebContext; const AFileName: string): Boolean;
var
  LFileStream: TFileStream;
  LFileSize: Int64;
  LRangeHeader, LContentType: string;
  LRangeStart, LRangeEnd, LContentLength: Int64;
  LPartialStream: TMemoryStream;
begin
  Result := True;
  LContentType := GetMediaType(AFileName);

  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LFileSize := LFileStream.Size;
    LRangeHeader := AContext.Request.Headers['Range'];

    // Always advertise Range support
    AContext.Response.SetCustomHeader('Accept-Ranges', 'bytes');

    // Prevent compression middleware from encoding media responses,
    // which would break Content-Range semantics
    AContext.Response.SetCustomHeader('Content-Encoding', 'identity');

    // Chromium's media resource cache ignores Cache-Control: no-cache for
    // <video> and <audio> elements, serving stale content without
    // revalidating. Use no-store to prevent disk caching entirely.
    // Range requests still work within the same playback session
    // because the media element buffers data in memory.
    AContext.Response.SetCustomHeader('Cache-Control', 'no-store');

    if LRangeHeader.IsEmpty then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.OK;
      LFileStream.Position := 0;
      AContext.Response.SetContentStream(LFileStream, LContentType);
      LFileStream := nil; // SetContentStream takes ownership; prevent double-free in finally
      Exit;
    end;

    if not ParseRangeHeader(LRangeHeader, LFileSize, LRangeStart, LRangeEnd) then
    begin
      AContext.Response.StatusCode := HTTP_STATUS.RequestedRangeNotSatisfiable;
      AContext.Response.SetCustomHeader('Content-Range',
        'bytes */' + IntToStr(LFileSize));
      Exit;
    end;

    // Serve partial content
    LContentLength := LRangeEnd - LRangeStart + 1;
    LPartialStream := TMemoryStream.Create;
    LPartialStream.SetSize(LContentLength);
    LFileStream.Position := LRangeStart;
    LPartialStream.CopyFrom(LFileStream, LContentLength);
    LPartialStream.Position := 0;

    AContext.Response.StatusCode := HTTP_STATUS.PartialContent;
    AContext.Response.SetCustomHeader('Content-Range',
      Format('bytes %d-%d/%d', [LRangeStart, LRangeEnd, LFileSize]));
    AContext.Response.SetContentStream(LPartialStream, LContentType);
  finally
    LFileStream.Free;
  end;
end;

{ IMVCMiddleware }

procedure TMVCRangeMediaMiddleware.OnBeforeRouting(
  AContext: TWebContext; var AHandled: Boolean);
var
  LPathInfo, LFileName: string;
begin
  if AHandled then
    Exit;

  LPathInfo := AContext.Request.PathInfo;

  // Only intercept requests matching our URL prefix
  if not LPathInfo.StartsWith(FURLPath, True) then
    Exit;

  // Only handle GET and HEAD
  if not (AContext.Request.HTTPMethod in [httpGET, httpHEAD]) then
    Exit;

  // Only take ownership of the request if there is a filename component after
  // the prefix (e.g. '/media/foo.mp3'). A bare '/media' with no filename is
  // left to the router.
  if LPathInfo.Substring(FURLPath.Length).TrimLeft(['/']).IsEmpty then
    Exit;

  AHandled := True;
  if ResolveFilePath(LPathInfo, LFileName) then
    ServeFile(AContext, LFileName)
  else
    AContext.Response.StatusCode := HTTP_STATUS.NotFound;
end;

procedure TMVCRangeMediaMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; var AHandled: Boolean);
begin
  // Nothing to do
end;

procedure TMVCRangeMediaMiddleware.OnAfterControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; const AHandled: Boolean);
begin
  // Nothing to do
end;

procedure TMVCRangeMediaMiddleware.OnAfterRouting(
  AContext: TWebContext; const AHandled: Boolean);
begin
  // Nothing to do
end;

end.
