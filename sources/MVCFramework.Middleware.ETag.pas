// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// Jo�o Ant�nio Duarte (https://github.com/joaoduarte19)
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

unit MVCFramework.Middleware.ETag;

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
  TMVCETagMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    function GetHashMD5FromStream(AStream: TStream): string;
  public
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
{$IF defined(SEATTLEORBETTER)}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
{$ENDIF}
  System.SysUtils,
  MVCFramework.Commons;

{ TMVCETagMiddleware }

function TMVCETagMiddleware.GetHashMD5FromStream(AStream: TStream): string;
{$IF not defined(SEATTLEORBETTER)}
var
  lMD5Hash: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IF defined(SEATTLEORBETTER)}
  Result := THashMD5.GetHashString(AStream);
{$ELSE}
  lMD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := lMD5Hash.HashStreamAsHex(AStream);
  finally
    lMD5Hash.Free;
  end;
{$ENDIF}
end;

procedure TMVCETagMiddleware.OnAfterControllerAction(AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCETagMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
var
  lContentStream: TStream;
  lRequestETag: string;
  lETag: string;
begin
  lContentStream := AContext.Response.RawWebResponse.ContentStream;
  if not Assigned(lContentStream) then
    Exit;

  lRequestETag := AContext.Request.Headers['If-None-Match'];
  lETag := GetHashMD5FromStream(lContentStream);

  AContext.Response.SetCustomHeader('ETag', lETag);

  if (lETag <> '') and (lRequestETag = lETag) then
  begin
    AContext.Response.Content := '';
    if lContentStream is TFileStream then
    begin
      AContext.Response.RawWebResponse.ContentStream := nil;
    end
    else
    begin
      lContentStream.Size := 0;
    end;
    AContext.Response.StatusCode := HTTP_STATUS.NotModified;
    AContext.Response.ReasonString := 'Not Modified'
  end;
end;

procedure TMVCETagMiddleware.OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCETagMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  // do nothing
end;

end.
