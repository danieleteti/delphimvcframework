// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// João Antônio Duarte (https://github.com/joaoduarte19)
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

unit MVCFramework.RESTClient.Commons;

{$I dmvcframework.inc}

interface

uses
  System.NetEncoding,
  System.SysUtils,
  System.Classes,
  MVCFramework.Commons,
  System.Net.HttpClient;

type

{$SCOPEDENUMS ON}
  TMVCRESTParamType = (Header, Path, Query, FormURLEncoded, Cookie);

  TMVCRESTParam = record
    /// <summary>Parameter type</summary>
    &Type: TMVCRESTParamType;
    /// <summary>Parameter name</summary>
    Name: string;
    /// <summary>Parameter value</summary>
    Value: string;
    /// <summary>Initializes a TMVCRESTParam</summary>
    constructor Create(const aType: TMVCRESTParamType; const aName, aValue: string);
  end;

  TMVCRESTClientHelper = class sealed
  private
    class function DecompressWithZlib(aContentStream, aOutStream: TStream; const aContentEncoding: string): Boolean;
    class function DecompressWithIndyZlib(aContentStream, aOutStream: TStream; const aContentEncoding: string): Boolean;
  public
    class function URIEncode(const aURI: string): string;
    /// <summary>
    /// Convert response content to byte array. If the response is compressed, it is decompressed in the process.
    /// </summary>
    class function GetResponseContentAsRawBytes(aContentStream: TStream; const aContentEncoding: string): TArray<Byte>;
    /// <summary>
    /// Get the response string, if it is of any type of text.
    /// </summary>
    class function GetResponseContentAsString(var aContentRawBytes: TArray<Byte>; const aContentType: string): string;
  end;

  EMVCRESTClientException = class(Exception);

  TMVCRESTClientConsts = record
  public const
    DEFAULT_ACCEPT_ENCODING = 'gzip,deflate';
    DEFAULT_ACCEPT = '*/*';
    DEFAULT_USER_AGENT = 'DelphiMVCFramework RESTClient/' + DMVCFRAMEWORK_VERSION;
    DEFAULT_FILE_NAME = 'file';
    AUTHORIZATION_HEADER = 'Authorization';
    BASIC_AUTH_PREFIX = 'Basic ';
    BEARER_AUTH_PREFIX = 'Bearer ';
    SERVER_HEADER = 'server';
    DEFAULT_MAX_REDIRECTS = 5;
{$IF defined(BERLINORBETTER)}
    REST_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='),
      Ord('>'), Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'),
      Ord('?'), Ord('#'), Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'), Ord(' '), Ord('*'),
      Ord('+')];
    PATH_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord('<'), Ord('>'), Ord('^'), Ord('`'), Ord('{'),
      Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'), Ord('+'), Ord('.')];
{$ENDIF}
  end;

implementation

uses
  IdCompressorZLib,
  System.ZLib,
  System.Net.Mime,
  System.Rtti;

{ TMVCRESTParam }

constructor TMVCRESTParam.Create(const aType: TMVCRESTParamType; const aName, aValue: string);
begin
  &Type := aType;
  Name := aName;
  Value := aValue;
end;

{ TMVCRESTClientHelper }

class function TMVCRESTClientHelper.DecompressWithIndyZlib(aContentStream, aOutStream: TStream;
  const aContentEncoding: string): Boolean;
var
  lDecompressor: TIdCompressorZLib;
begin
  try
    aContentStream.Position := 0;
    lDecompressor := TIdCompressorZLib.Create(nil);
    try
      if SameText(aContentEncoding, 'gzip') then
      begin
        lDecompressor.DecompressGZipStream(aContentStream, aOutStream);
      end
      else if SameText(aContentEncoding, 'deflate') then
      begin
        lDecompressor.DecompressHTTPDeflate(aContentStream, aOutStream);
      end;
      Result := True;
    finally
      FreeAndNil(lDecompressor);
    end;
  except
    Result := False;
  end;
end;

class function TMVCRESTClientHelper.DecompressWithZlib(aContentStream, aOutStream: TStream;
  const aContentEncoding: string): Boolean;
var
  lCompressionType: TMVCCompressionType;
  lDecompressor: TZDecompressionStream;
begin
  if aContentEncoding = 'deflate' then
  begin
    lCompressionType := TMVCCompressionType.ctDeflate;
  end
  else
  begin
    lCompressionType := TMVCCompressionType.ctGZIP;
  end;

  aContentStream.Position := 0;
  try
{$IF defined(BERLINORBETTER)}
    lDecompressor := TZDecompressionStream.Create(aContentStream,
      MVC_COMPRESSION_ZLIB_WINDOW_BITS[lCompressionType], False);
{$ELSE}
    lDecompressor := TZDecompressionStream.Create(aContentStream, MVC_COMPRESSION_ZLIB_WINDOW_BITS[lCompressionType]);
{$ENDIF}
    try
      aOutStream.CopyFrom(lDecompressor, 0);
      Result := True;
    finally
      FreeAndNil(lDecompressor);
    end;
  except
    Result := False;
  end;
end;

class function TMVCRESTClientHelper.GetResponseContentAsRawBytes(aContentStream: TStream;
  const aContentEncoding: string): TArray<Byte>;
var
  lDecompressed: TMemoryStream;
begin
  lDecompressed := TMemoryStream.Create;
  try
{$IF defined(MACOS) or defined(IOS)}
    lDecompressed.CopyFrom(aContentStream, 0); // MACOS automatically decompresses response body
{$ELSE}
    if SameText(aContentEncoding, 'gzip') or SameText(aContentEncoding, 'deflate') then
    begin
      /// Certain types of deflate compression cannot be decompressed by the standard Zlib,
      /// but are decompressed by Indy's Zlib.
      /// Examples:
      /// The deflate compression of the DMVC server is not decompressed by the Indy Zlib decompressor,
      /// only by the standard Zlib.
      /// The deflate compression of the server of the Embarcadero website (https://www.embarcadero.com/)
      /// is only decompressed with the Indy Zlib decompressor.
      /// Note: I think we can improve this later

      if not (DecompressWithZlib(aContentStream, lDecompressed, aContentEncoding) or
        DecompressWithIndyZlib(aContentStream, lDecompressed, aContentEncoding)) then
        raise EMVCRESTClientException.Create('Could not decompress response content');
    end
    else if aContentEncoding.IsEmpty or SameText(aContentEncoding, 'identity') then // No encoding
    begin
      lDecompressed.CopyFrom(aContentStream, 0);
    end
    else
    begin
      raise EMVCRESTClientException.CreateFmt('Content-Encoding not supported [%s]', [aContentEncoding]);
    end;
{$ENDIF}

    SetLength(Result, lDecompressed.Size);
    lDecompressed.Position := 0;
    lDecompressed.Read(Result, lDecompressed.Size);
  finally
    FreeAndNil(lDecompressed);
  end;
end;

class function TMVCRESTClientHelper.GetResponseContentAsString(var aContentRawBytes: TArray<Byte>;
  const aContentType: string): string;
var
  lContentIsString: Boolean;
  lEncoding: TEncoding;
  lContentType: string;
  lCharset: string;
{$IF defined(RIOORBETTER)}
  lExt: string;
  lMimeKind: TMimeTypes.TKind;
{$ENDIF}
  lReader: TStringStream;
begin
  Result := '';
  lContentIsString := False;
  SplitContentMediaTypeAndCharset(aContentType, lContentType, lCharset);

  if not lCharset.IsEmpty then
  begin
    lContentIsString := True
  end
  else
  begin
{$IF defined(RIOORBETTER)}
    TMimeTypes.Default.GetTypeInfo(lContentType.ToLower, lExt, lMimeKind);
    if lMimeKind = TMimeTypes.TKind.Text then
      lContentIsString := True;
{$ELSE}
    if not (lContentType.StartsWith('image', True) or
      lContentType.StartsWith('video', True) or
      lContentType.StartsWith('audio', True) or
      lContentType.ToLower.Equals('application/octet-stream') or
      lContentType.ToLower.Equals('application/pdf')) then
      lContentIsString := True;
{$ENDIF}
  end;

  if lContentIsString then
  begin
    if lCharset.isEmpty then
    begin
      lCharset := TMVCCharSet.UTF_8;
    end;
    lEncoding := TEncoding.GetEncoding(lCharset);

    lReader := TStringStream.Create('', lEncoding);
    try
      lReader.Write(aContentRawBytes, Length(aContentRawBytes));
      Result := lReader.DataString;
    finally
      FreeAndNil(lReader);
    end;
  end;
end;

class function TMVCRESTClientHelper.URIEncode(const aURI: string): string;
begin
  Result := TNetEncoding.URL.Encode(aURI
{$IF defined(BERLINORBETTER)}
    ,TMVCRESTClientConsts.REST_UNSAFE_CHARS, [TURLEncoding.TEncodeOption.EncodePercent]
{$ENDIF}
    );
end;

end.
