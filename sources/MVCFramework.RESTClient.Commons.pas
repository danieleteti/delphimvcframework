// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
  System.Classes;

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
  public
    class function URIEncode(const aURI: string): string;
    class function GetResponseContentAsRawBytes(aContentStream: TStream; const aContentEncoding: string): TArray<Byte>;
    class function GetResponseContentAsString(aContentRawBytes: TArray<Byte>; const aContentType: string): string;
  end;

  EMVCRESTClientException = class(Exception);

  TMVCRESTClientConsts = record
  public const
    DEFAULT_ACCEPT_ENCODING = 'gzip,deflate';
    DEFAULT_FILE_NAME = 'file';
    AUTHORIZATION_HEADER = 'Authorization';
    BASIC_AUTH_PREFIX = 'Basic ';
    BEARER_AUTH_PREFIX = 'Bearer ';
    HEADER_RESPONSE_COOKIES = 'Cookies';
    SERVER_HEADER = 'server';
    REST_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord('<'), Ord('>'), Ord('^'), Ord('`'), Ord('{'),
      Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'), Ord('+'), Ord('.')];
    QUERY_NAME_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='),
      Ord('>'), Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'),
      Ord('?'), Ord('#'), Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'), Ord(' '), Ord('*'),
      Ord('+')];
  end;


implementation

uses
  IdCompressorZLib,
  MVCFramework.Commons, System.Net.Mime;

{ TMVCRESTParam }

constructor TMVCRESTParam.Create(const aType: TMVCRESTParamType; const aName, aValue: string);
begin
  &Type := aType;
  Name := aName;
  Value := aValue;
end;

{ TMVCRESTClientHelper }

class function TMVCRESTClientHelper.GetResponseContentAsRawBytes(aContentStream: TStream;
  const aContentEncoding: string): TArray<Byte>;
var
  lDecompressed: TMemoryStream;
  lDecompressor: TIdCompressorZLib;
begin
  aContentStream.Position := 0;
  lDecompressed := TMemoryStream.Create;
  try
    lDecompressor := TIdCompressorZLib.Create(nil);
    try
      if SameText(aContentEncoding, 'gzip') then
      begin
        lDecompressor.DecompressGZipStream(aContentStream, lDecompressed);
      end
      else if SameText(aContentEncoding, 'deflate') then
      begin
        lDecompressor.DecompressHTTPDeflate(aContentStream, lDecompressed);
      end
      else
      begin
        // If it is not encoded, copy as is
        lDecompressed.CopyFrom(aContentStream, 0);
      end;
    finally
      FreeAndNil(lDecompressor);
    end;

    SetLength(Result, lDecompressed.Size);
    lDecompressed.Position := 0;
    lDecompressed.Read(Result, lDecompressed.Size);
  finally
    FreeAndNil(lDecompressed);
  end;
end;

class function TMVCRESTClientHelper.GetResponseContentAsString(aContentRawBytes: TArray<Byte>;
  const aContentType: string): string;
var
  lContentIsString: Boolean;
  lEncoding: TEncoding;
  lContentType: string;
  lCharset: string;
  lExt: string;
  lMimeKind: TMimeTypes.TKind;
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
    TMimeTypes.Default.GetTypeInfo(lContentType.ToLower, lExt, lMimeKind);
    if lMimeKind = TMimeTypes.TKind.Text then
      lContentIsString := True;
  end;

  if lContentIsString then
  begin
    if lCharset.isEmpty then
    begin
      lCharset := 'utf-8';
    end;
    lEncoding := TEncoding.GetEncoding(lCharset);

    lReader := TStringStream.Create('', lEncoding);
    try
      Result := lReader.DataString;
    finally
      FreeAndNil(lReader);
    end;
  end;
end;

class function TMVCRESTClientHelper.URIEncode(const aURI: string): string;
begin
  Result := TNetEncoding.URL.Encode(aURI, TMVCRESTClientConsts.REST_UNSAFE_CHARS,
    [TURLEncoding.TEncodeOption.EncodePercent]);
end;

end.
