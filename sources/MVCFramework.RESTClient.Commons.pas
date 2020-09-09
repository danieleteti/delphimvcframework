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
  System.NetEncoding;

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

  TMVCRESTClientConsts = record
  public const
    DEFAULT_ACCEPT_ENCODING = 'gzip,deflate';
    DEFAULT_FILE_NAME = 'file';
    AUTHORIZATION_HEADER = 'Authorization';
    BASIC_AUTH_PREFIX = 'Basic ';
    BEARER_AUTH_PREFIX = 'Bearer ';
    HEADER_RESPONSE_COOKIES = 'Cookies';
    REST_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord('<'), Ord('>'), Ord('^'), Ord('`'), Ord('{'),
      Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'), Ord('+'), Ord('.')];
    QUERY_NAME_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='),
      Ord('>'), Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'),
      Ord('?'), Ord('#'), Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'), Ord(' '), Ord('*'),
      Ord('+')];
  end;

function URIEncode(const aURI: string): string;

implementation

function URIEncode(const aURI: string): string;
begin
  Result := TNetEncoding.URL.Encode(aURI, TMVCRESTClientConsts.REST_UNSAFE_CHARS,
    [TURLEncoding.TEncodeOption.EncodePercent]);
end;

{ TMVCRESTParam }

constructor TMVCRESTParam.Create(const aType: TMVCRESTParamType; const aName, aValue: string);
begin
  &Type := aType;
  Name := aName;
  Value := aValue;
end;

end.
