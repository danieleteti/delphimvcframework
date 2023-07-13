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

unit MVCFramework.Filters.ETag;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  System.Classes;

type
  /// <summary>
  /// The <b>ETag</b> HTTP response header is an identifier for a specific version of a resource. It lets caches be
  /// more efficient and save bandwidth, as a web server does not need to resend a full response if the content has
  /// not changed. See more about the specification: <see href="https://tools.ietf.org/html/rfc7232#section-2.3">RFC
  /// 7232</see>
  /// </summary>
  TMVCETagProtocolFilter = class(TCustomProtocolFilter)
  protected
    procedure DoFilter(Context: TWebContext); override;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Commons, MVCFramework.Utils;

{ TMVCETagProtocolFilter }


procedure TMVCETagProtocolFilter.DoFilter(Context: TWebContext);
var
  lContentStream: TStream;
  lRequestETag: string;
  lETag: string;
begin
  DoNext(Context);
  lContentStream := Context.Response.RawWebResponse.ContentStream;
  if not Assigned(lContentStream) then
  begin
    Exit;
  end;

  if not Context.Response.RawWebResponse.GetCustomHeader('ETag').IsEmpty then
  begin
    //do not mess with ETag if someone else already set it
    Exit;
  end;

  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag#caching_of_unchanged_resources
  lRequestETag := Context.Request.Headers['If-None-Match'];
  lETag := GetSHA1HashFromStream(lContentStream);
  lContentStream.Position := 0;

  Context.Response.SetCustomHeader('ETag', lETag);

  if (lETag <> '') and (lRequestETag = lETag) then
  begin
    Context.Response.Content := '';
    if lContentStream is TFileStream then
    begin
      Context.Response.RawWebResponse.ContentStream := nil;
    end
    else
    begin
      lContentStream.Size := 0;
    end;
    Context.Response.StatusCode := HTTP_STATUS.NotModified;
    Context.Response.ReasonString := HTTP_STATUS.ReasonStringFor(HTTP_STATUS.NotModified);
    Context.Response.SetCustomHeader('Content-Type', '');
  end;
end;

end.
