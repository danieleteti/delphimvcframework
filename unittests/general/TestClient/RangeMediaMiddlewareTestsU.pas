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

unit RangeMediaMiddlewareTestsU;

// Integration tests for TMVCRangeMediaMiddleware.
//
// Strategy
// --------
// An embedded HTTP server (TMVCListener on TEST_PORT) is started once for the
// whole fixture (SetupFixture / TeardownFixture). A temporary directory
// 'testmedia_tmp' is created next to the test executable, populated with
// deterministic binary files, and removed at the end.
//
// Requests are made with System.Net.HttpClient (THTTPClient) so we can set
// and inspect raw headers such as "Range" and "Content-Range" without any
// higher-level abstraction getting in the way.
//
// File layout
// -----------
//   testmedia_tmp/
//     test.bin   - 1024 bytes, values 0..255 repeated  (known content)
//     audio.mp3  - 256 bytes, all zeros
//     video.mp4  - 256 bytes, all zeros

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Net.HttpClient,
  MVCFramework.Server,
  MVCFramework.Server.Impl;

const
  TEST_PORT         = 8890;
  TEST_URL_BASE     = 'http://localhost:8890';
  TEST_DIR          = 'testmedia_tmp';
  TEST_FILE_BIN     = 'test.bin';      // 1024 bytes, values 0-255 x4
  TEST_FILE_MP3     = 'audio.mp3';     // 256 bytes
  TEST_FILE_MP4     = 'video.mp4';     // 256 bytes
  TEST_FILE_SIZE    = 1024;

type
  [TestFixture]
  TRangeMediaMiddlewareTests = class
  private
    FListener : IMVCListener;
    FTempDir  : string;
    FClient   : THTTPClient;

    function  Get(const AURL: string): IHTTPResponse;
    function  GetWithRange(const AURL, ARange: string): IHTTPResponse;
    function  PostWith(const AURL: string): IHTTPResponse;

    function  ContentRangeStart(const AResponse: IHTTPResponse): Int64;
    function  ContentRangeEnd(const AResponse: IHTTPResponse): Int64;
    function  ContentRangeTotal(const AResponse: IHTTPResponse): Int64;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TeardownFixture]
    procedure TeardownFixture;
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    // -----------------------------------------------------------------------
    // 200 OK (no Range header)
    // -----------------------------------------------------------------------
    [Test]
    procedure Test200_FullFile_NoRangeHeader;
    [Test]
    procedure Test200_HasAcceptRangesHeader;
    [Test]
    procedure Test200_ContentTypeMP3;
    [Test]
    procedure Test200_ContentTypeMP4;
    [Test]
    procedure Test200_ContentTypeUnknownExtension;

    // -----------------------------------------------------------------------
    // 206 Partial Content
    // -----------------------------------------------------------------------
    [Test]
    procedure Test206_ExplicitRange;
    [Test]
    procedure Test206_ExplicitRange_ContentLength;
    [Test]
    procedure Test206_ExplicitRange_ContentRangeHeader;
    [Test]
    procedure Test206_ExplicitRange_ActualBytes;
    [Test]
    procedure Test206_SuffixRange;
    [Test]
    procedure Test206_OpenEndedRange;
    [Test]
    procedure Test206_RangeClampedToFileSize;
    [Test]
    procedure Test206_SingleByteRange;
    [Test]
    procedure Test206_LastByteRange;
    [Test]
    procedure Test206_HasAcceptRangesHeader;

    // -----------------------------------------------------------------------
    // 416 Range Not Satisfiable
    // -----------------------------------------------------------------------
    [Test]
    procedure Test416_StartBeyondFileSize;
    [Test]
    procedure Test416_StartGreaterThanEnd;
    [Test]
    procedure Test416_MalformedHeader;
    [Test]
    procedure Test416_MultiRangeNotSupported;
    [Test]
    procedure Test416_HasContentRangeHeader;

    // -----------------------------------------------------------------------
    // 404 Not Found
    // -----------------------------------------------------------------------
    [Test]
    procedure Test404_FileNotFound;
    [Test]
    procedure Test404_DirectoryTraversal_DotDot;
    [Test]
    procedure Test404_DirectoryTraversal_Encoded;

    // -----------------------------------------------------------------------
    // Routing / method handling
    // -----------------------------------------------------------------------
    [Test]
    procedure TestPost_NotHandledByMiddleware;
    [Test]
    procedure TestBareUrlPrefix_NotHandledByMiddleware;
  end;

implementation

uses
  System.Net.URLClient,
  RangeMediaTestWebModuleU;

{ Helpers }

function TRangeMediaMiddlewareTests.Get(const AURL: string): IHTTPResponse;
begin
  Result := FClient.Get(AURL);
end;

function TRangeMediaMiddlewareTests.GetWithRange(const AURL, ARange: string): IHTTPResponse;
var
  LHeaders: TNetHeaders;
begin
  SetLength(LHeaders, 1);
  LHeaders[0] := TNameValuePair.Create('Range', ARange);
  Result := FClient.Get(AURL, nil, LHeaders);
end;

function TRangeMediaMiddlewareTests.PostWith(const AURL: string): IHTTPResponse;
var
  LBody: TStringStream;
begin
  LBody := TStringStream.Create('');
  try
    Result := FClient.Post(AURL, LBody, nil);
  finally
    LBody.Free;
  end;
end;

function TRangeMediaMiddlewareTests.ContentRangeStart(const AResponse: IHTTPResponse): Int64;
// Content-Range: bytes START-END/TOTAL
var
  LHeader, LBytesSpec, LRange: string;
  LDashPos: Integer;
begin
  LHeader := AResponse.HeaderValue['Content-Range'];
  // "bytes 0-99/1024"
  LBytesSpec := LHeader.Substring(6); // skip "bytes "
  LRange := LBytesSpec.Substring(0, LBytesSpec.IndexOf('/'));
  LDashPos := LRange.IndexOf('-');
  TryStrToInt64(LRange.Substring(0, LDashPos), Result);
end;

function TRangeMediaMiddlewareTests.ContentRangeEnd(const AResponse: IHTTPResponse): Int64;
var
  LHeader, LBytesSpec, LRange: string;
  LDashPos: Integer;
begin
  LHeader := AResponse.HeaderValue['Content-Range'];
  LBytesSpec := LHeader.Substring(6);
  LRange := LBytesSpec.Substring(0, LBytesSpec.IndexOf('/'));
  LDashPos := LRange.IndexOf('-');
  TryStrToInt64(LRange.Substring(LDashPos + 1), Result);
end;

function TRangeMediaMiddlewareTests.ContentRangeTotal(const AResponse: IHTTPResponse): Int64;
var
  LHeader, LBytesSpec: string;
begin
  LHeader := AResponse.HeaderValue['Content-Range'];
  LBytesSpec := LHeader.Substring(6);
  TryStrToInt64(LBytesSpec.Substring(LBytesSpec.IndexOf('/') + 1), Result);
end;

{ Fixture setup / teardown }

procedure TRangeMediaMiddlewareTests.SetupFixture;
var
  LBytes: TBytes;
  I: Integer;
begin
  // Create the temp directory used by the middleware document root
  FTempDir := TPath.Combine(ExtractFilePath(ParamStr(0)), TEST_DIR);
  TDirectory.CreateDirectory(FTempDir);

  // test.bin: 1024 bytes with values 0..255 repeated (predictable content)
  SetLength(LBytes, TEST_FILE_SIZE);
  for I := 0 to High(LBytes) do
    LBytes[I] := Byte(I mod 256);
  TFile.WriteAllBytes(TPath.Combine(FTempDir, TEST_FILE_BIN), LBytes);

  // audio.mp3 and video.mp4: 256 zero bytes (only used for MIME type checks)
  SetLength(LBytes, 256);
  FillChar(LBytes[0], Length(LBytes), 0);
  TFile.WriteAllBytes(TPath.Combine(FTempDir, TEST_FILE_MP3), LBytes);
  TFile.WriteAllBytes(TPath.Combine(FTempDir, TEST_FILE_MP4), LBytes);

  // Start the embedded HTTP server
  FListener := TMVCListener.Create(
    TMVCListenerProperties.New
      .SetName('RangeMediaTest')
      .SetPort(TEST_PORT)
      .SetMaxConnections(32)
      .SetWebModuleClass(RangeMediaTestWebModuleClass)
  );
  FListener.Start;
end;

procedure TRangeMediaMiddlewareTests.TeardownFixture;
begin
  FListener.Stop;
  FListener := nil;
  TDirectory.Delete(FTempDir, True);
end;

procedure TRangeMediaMiddlewareTests.Setup;
begin
  FClient := THTTPClient.Create;
  // Disable automatic redirect following so that 3xx responses are seen as-is
  // by the tests, preventing accidental misclassification of redirect codes
  // as success (200) or partial content (206).
  FClient.HandleRedirects := False;
end;

procedure TRangeMediaMiddlewareTests.Teardown;
begin
  FClient.Free;
end;

{ 200 OK tests }

procedure TRangeMediaMiddlewareTests.Test200_FullFile_NoRangeHeader;
var
  LResp: IHTTPResponse;
begin
  LResp := Get(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN);
  Assert.AreEqual(200, LResp.StatusCode, 'Expected 200 OK for request without Range header');
  Assert.AreEqual(TEST_FILE_SIZE, Integer(LResp.ContentLength), 'Content-Length must equal file size');
end;

procedure TRangeMediaMiddlewareTests.Test200_HasAcceptRangesHeader;
var
  LResp: IHTTPResponse;
begin
  LResp := Get(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN);
  Assert.AreEqual('bytes', LResp.HeaderValue['Accept-Ranges'],
    'Accept-Ranges: bytes must be present on all responses');
end;

procedure TRangeMediaMiddlewareTests.Test200_ContentTypeMP3;
var
  LResp: IHTTPResponse;
begin
  LResp := Get(TEST_URL_BASE + '/testmedia/' + TEST_FILE_MP3);
  Assert.AreEqual(200, LResp.StatusCode);
  Assert.IsTrue(LResp.MimeType.Contains('audio/mpeg'),
    'MIME type for .mp3 must be audio/mpeg, got: ' + LResp.MimeType);
end;

procedure TRangeMediaMiddlewareTests.Test200_ContentTypeMP4;
var
  LResp: IHTTPResponse;
begin
  LResp := Get(TEST_URL_BASE + '/testmedia/' + TEST_FILE_MP4);
  Assert.AreEqual(200, LResp.StatusCode);
  Assert.IsTrue(LResp.MimeType.Contains('video/mp4'),
    'MIME type for .mp4 must be video/mp4, got: ' + LResp.MimeType);
end;

procedure TRangeMediaMiddlewareTests.Test200_ContentTypeUnknownExtension;
const
  UNKNOWN_FILE = 'data.xyz';
var
  LResp: IHTTPResponse;
  LBytes: TBytes;
begin
  // Create a file with an unknown extension on the fly
  SetLength(LBytes, 16);
  TFile.WriteAllBytes(TPath.Combine(FTempDir, UNKNOWN_FILE), LBytes);
  try
    LResp := Get(TEST_URL_BASE + '/testmedia/' + UNKNOWN_FILE);
    Assert.AreEqual(200, LResp.StatusCode);
    Assert.IsTrue(LResp.MimeType.Contains('application/octet-stream'),
      'Unknown extension must fall back to application/octet-stream, got: ' + LResp.MimeType);
  finally
    TFile.Delete(TPath.Combine(FTempDir, UNKNOWN_FILE));
  end;
end;

{ 206 Partial Content tests }

procedure TRangeMediaMiddlewareTests.Test206_ExplicitRange;
var
  LResp: IHTTPResponse;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=0-99');
  Assert.AreEqual(206, LResp.StatusCode, 'Expected 206 Partial Content');
end;

procedure TRangeMediaMiddlewareTests.Test206_ExplicitRange_ContentLength;
var
  LResp: IHTTPResponse;
begin
  // bytes=0-99 → 100 bytes
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=0-99');
  Assert.AreEqual(206, LResp.StatusCode);
  Assert.AreEqual(100, Integer(LResp.ContentLength),
    'Content-Length must reflect the range size (100 bytes)');
end;

procedure TRangeMediaMiddlewareTests.Test206_ExplicitRange_ContentRangeHeader;
var
  LResp: IHTTPResponse;
  LHeader: string;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=0-99');
  Assert.AreEqual(206, LResp.StatusCode);
  LHeader := LResp.HeaderValue['Content-Range'];
  Assert.IsFalse(LHeader.IsEmpty, 'Content-Range header must be present on 206');
  Assert.AreEqual(0, ContentRangeStart(LResp), 'Range start must be 0');
  Assert.AreEqual(99, ContentRangeEnd(LResp), 'Range end must be 99');
  Assert.AreEqual(TEST_FILE_SIZE, ContentRangeTotal(LResp), 'Total must be file size');
end;

procedure TRangeMediaMiddlewareTests.Test206_ExplicitRange_ActualBytes;
var
  LResp: IHTTPResponse;
  LContent: TBytes;
  I: Integer;
begin
  // bytes=10-19 → 10 bytes, expected values 10,11,12,...,19
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=10-19');
  Assert.AreEqual(206, LResp.StatusCode);
  LContent := LResp.ContentAsBytes();
  Assert.AreEqual(10, Length(LContent), 'Must return exactly 10 bytes');
  for I := 0 to 9 do
    Assert.AreEqual(Byte(I + 10), LContent[I],
      Format('Byte[%d] should be %d, got %d', [I, I + 10, LContent[I]]));
end;

procedure TRangeMediaMiddlewareTests.Test206_SuffixRange;
var
  LResp: IHTTPResponse;
begin
  // bytes=-100 → last 100 bytes of a 1024-byte file → bytes 924-1023
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=-100');
  Assert.AreEqual(206, LResp.StatusCode, 'Suffix range must return 206');
  Assert.AreEqual(100, Integer(LResp.ContentLength), 'Suffix range must return 100 bytes');
  Assert.AreEqual(Int64(924), ContentRangeStart(LResp), 'Suffix range start must be 924');
  Assert.AreEqual(Int64(TEST_FILE_SIZE - 1), ContentRangeEnd(LResp), 'Suffix range end must be last byte');
end;

procedure TRangeMediaMiddlewareTests.Test206_OpenEndedRange;
var
  LResp: IHTTPResponse;
begin
  // bytes=900- → from byte 900 to end of 1024-byte file → 124 bytes
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=900-');
  Assert.AreEqual(206, LResp.StatusCode, 'Open-ended range must return 206');
  Assert.AreEqual(TEST_FILE_SIZE - 900, Integer(LResp.ContentLength),
    'Open-ended range must return bytes from start to EOF');
  Assert.AreEqual(Int64(900), ContentRangeStart(LResp));
  Assert.AreEqual(Int64(TEST_FILE_SIZE - 1), ContentRangeEnd(LResp));
end;

procedure TRangeMediaMiddlewareTests.Test206_RangeClampedToFileSize;
var
  LResp: IHTTPResponse;
begin
  // bytes=1000-9999 → end clamped to 1023 → 24 bytes
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=1000-9999');
  Assert.AreEqual(206, LResp.StatusCode, 'Range exceeding EOF must be clamped and return 206');
  Assert.AreEqual(Int64(TEST_FILE_SIZE - 1), ContentRangeEnd(LResp),
    'End must be clamped to last byte of file');
end;

procedure TRangeMediaMiddlewareTests.Test206_SingleByteRange;
var
  LResp: IHTTPResponse;
  LContent: TBytes;
begin
  // bytes=42-42 → exactly 1 byte, value = 42 mod 256 = 42
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=42-42');
  Assert.AreEqual(206, LResp.StatusCode);
  LContent := LResp.ContentAsBytes();
  Assert.AreEqual(1, Length(LContent), 'Single-byte range must return exactly 1 byte');
  Assert.AreEqual(Byte(42), LContent[0], 'Single-byte range must return the correct byte');
end;

procedure TRangeMediaMiddlewareTests.Test206_LastByteRange;
var
  LResp: IHTTPResponse;
  LContent: TBytes;
begin
  // bytes=1023-1023 → last byte of the 1024-byte file, value = 1023 mod 256 = 255
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=1023-1023');
  Assert.AreEqual(206, LResp.StatusCode);
  LContent := LResp.ContentAsBytes();
  Assert.AreEqual(1, Length(LContent));
  Assert.AreEqual(Byte(1023 mod 256), LContent[0]);
end;

procedure TRangeMediaMiddlewareTests.Test206_HasAcceptRangesHeader;
var
  LResp: IHTTPResponse;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=0-99');
  Assert.AreEqual('bytes', LResp.HeaderValue['Accept-Ranges'],
    'Accept-Ranges: bytes must be present on 206 responses too');
end;

{ 416 Range Not Satisfiable tests }

procedure TRangeMediaMiddlewareTests.Test416_StartBeyondFileSize;
var
  LResp: IHTTPResponse;
begin
  // Start byte = 2000 > file size 1024 → 416
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=2000-3000');
  Assert.AreEqual(416, LResp.StatusCode, 'Range start beyond EOF must return 416');
end;

procedure TRangeMediaMiddlewareTests.Test416_StartGreaterThanEnd;
var
  LResp: IHTTPResponse;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=500-100');
  Assert.AreEqual(416, LResp.StatusCode, 'Range where start > end must return 416');
end;

procedure TRangeMediaMiddlewareTests.Test416_MalformedHeader;
var
  LResp: IHTTPResponse;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=notanumber');
  Assert.AreEqual(416, LResp.StatusCode, 'Malformed Range header must return 416');
end;

procedure TRangeMediaMiddlewareTests.Test416_MultiRangeNotSupported;
var
  LResp: IHTTPResponse;
begin
  // Multi-range (bytes=0-10,20-30) is not supported → 416
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=0-10,20-30');
  Assert.AreEqual(416, LResp.StatusCode, 'Multi-range requests must return 416 (not supported)');
end;

procedure TRangeMediaMiddlewareTests.Test416_HasContentRangeHeader;
var
  LResp: IHTTPResponse;
  LHeader: string;
begin
  LResp := GetWithRange(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN, 'bytes=9999-99999');
  Assert.AreEqual(416, LResp.StatusCode);
  // RFC 7233: on 416, Content-Range must be "bytes */TOTAL"
  LHeader := LResp.HeaderValue['Content-Range'];
  Assert.IsTrue(LHeader.StartsWith('bytes */'),
    'On 416, Content-Range must be "bytes */SIZE", got: ' + LHeader);
  Assert.IsTrue(LHeader.Contains(IntToStr(TEST_FILE_SIZE)),
    'On 416, Content-Range must report correct total size');
end;

{ 404 Not Found tests }

procedure TRangeMediaMiddlewareTests.Test404_FileNotFound;
var
  LResp: IHTTPResponse;
begin
  LResp := Get(TEST_URL_BASE + '/testmedia/does_not_exist.mp3');
  Assert.AreEqual(404, LResp.StatusCode,
    'Request for non-existent file under prefix must return 404');
end;

procedure TRangeMediaMiddlewareTests.Test404_DirectoryTraversal_DotDot;
var
  LResp: IHTTPResponse;
begin
  // ../somefile would escape the document root → 404
  LResp := Get(TEST_URL_BASE + '/testmedia/../' + TEST_FILE_BIN);
  Assert.AreEqual(404, LResp.StatusCode,
    'Directory traversal attempt must return 404');
end;

procedure TRangeMediaMiddlewareTests.Test404_DirectoryTraversal_Encoded;
var
  LResp: IHTTPResponse;
begin
  // URL-encoded traversal: %2F%2E%2E%2F → /../
  LResp := Get(TEST_URL_BASE + '/testmedia/%2F%2E%2E%2F' + TEST_FILE_BIN);
  Assert.AreEqual(404, LResp.StatusCode,
    'URL-encoded traversal must return 404');
end;

{ Routing / method handling tests }

procedure TRangeMediaMiddlewareTests.TestPost_NotHandledByMiddleware;
var
  LResp: IHTTPResponse;
begin
  // POST is not in [httpGET, httpHEAD] → middleware must not intercept it
  LResp := PostWith(TEST_URL_BASE + '/testmedia/' + TEST_FILE_BIN);
  // POST is not handled by the middleware (GET/HEAD only); with no controller
  // registered for this path, DMVC returns 404.
  Assert.AreEqual(404, LResp.StatusCode,
    'POST to middleware path must return 404 (not handled by middleware or any controller)');
end;

procedure TRangeMediaMiddlewareTests.TestBareUrlPrefix_NotHandledByMiddleware;
var
  LResp: IHTTPResponse;
begin
  // GET /testmedia (no filename) must pass through to the router
  // With no controller registered, DMVC returns 404 from routing — that is fine.
  // What must NOT happen: the middleware returning 404 itself for a bare prefix.
  // We just verify it doesn't return 200 or 206.
  LResp := Get(TEST_URL_BASE + '/testmedia');
  Assert.AreNotEqual(200, LResp.StatusCode,
    'Bare URL prefix with no filename must not return 200');
  Assert.AreNotEqual(206, LResp.StatusCode,
    'Bare URL prefix with no filename must not return 206');
end;

initialization
  TDUnitX.RegisterTestFixture(TRangeMediaMiddlewareTests);

end.
