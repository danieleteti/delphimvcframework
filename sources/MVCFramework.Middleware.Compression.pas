// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.Compression;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger;

type
  TMVCCompressionMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCompressionThreshold: Integer;
  protected
    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string;
      var AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  public
    constructor Create(aCompressionThreshold: Integer = 1024); virtual;
  end;

implementation

uses
  System.SysUtils,
  System.ZLib,
  System.Classes,
  MVCFramework.Commons;

{ TMVCSalutationMiddleware }

constructor TMVCCompressionMiddleware.Create(aCompressionThreshold: Integer);
begin
  inherited Create;
  fCompressionThreshold := aCompressionThreshold;
end;

procedure TMVCCompressionMiddleware.OnAfterControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCCompressionMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
var
  lMemStream: TMemoryStream;
  lContentStream: TStream;
  lAcceptEncoding: string;
  lEncodings: TArray<string>;
  lItem: string;
  lRespCompressionType: TMVCCompressionType;
  lTmpItem: string;
{.$IF not Defined(ALEXANDRIAORBETTER)}
  lZStream: TZCompressionStream;
{.$ENDIF}
begin
  if IsLibrary then
  begin
    Exit;
  end;
  lContentStream := AContext.Response.RawWebResponse.ContentStream;
  if (lContentStream = nil) or (lContentStream.Size <= fCompressionThreshold)
  then
    Exit;

  lAcceptEncoding := AContext.Request.Headers['Accept-Encoding'];
  if lAcceptEncoding.IsEmpty then
  begin
    Exit;
  end;
  lAcceptEncoding := lAcceptEncoding.Trim.ToLower;

  lRespCompressionType := TMVCCompressionType.ctNone;
  lEncodings := lAcceptEncoding.Split([',']);
  for lItem in lEncodings do
  begin
    lTmpItem := lItem.Trim;
    if lTmpItem = 'gzip' then
    begin
{.$IF Defined(ALEXANDRIAORBETTER)}
      {
        There is a bug in 11 Alexandria with TZCompressionStream, so the gzip compression
        is not available until the fix.
        The issue has been created on https://quality.embarcadero.com/browse/RSP-35516
      }
{.$ELSE}
      lRespCompressionType := TMVCCompressionType.ctGZIP;
      break;
{.$ENDIF}
    end
    else if lTmpItem = 'deflate' then
    begin
      lRespCompressionType := TMVCCompressionType.ctDeflate;
      break;
    end;
  end;

  if lRespCompressionType = TMVCCompressionType.ctNone then
    Exit;

  { When it is a TFileStream copy it to a TMemoryStream, as TFileStream is read only }
  // if lContentStream is TFileStream then
  // begin
  lMemStream := TMemoryStream.Create;
  try
{.$IF Defined(ALEXANDRIAORBETTER)}
    //ZCompressStream(lContentStream, lMemStream);
    // use it only for deflate, ZCompressStream (Delphi 11.0) cannot
    // create gzip compliant streams
{.$ELSE}
    lZStream := TZCompressionStream.Create(lMemStream,
      TZCompressionLevel.zcMax, MVC_COMPRESSION_ZLIB_WINDOW_BITS
      [lRespCompressionType]);
    try
      lContentStream.Position := 0;
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
//    There is a bug in 11 Alexandria with TZCompressionStream, this bug
//    raises an exception here and the debugger catches it, but it its "normal" behavior.
//    The gzip stream is correctly created. Please, vote for its resolution.
//    https://quality.embarcadero.com/browse/RSP-35516
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
// WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
      lZStream.CopyFrom(lContentStream, 0);
    finally
      lZStream.Free;
    end;
{.$ENDIF}
  except
    lMemStream.Free;
    raise;
  end;
  lMemStream.Position := 0;
  AContext.Response.RawWebResponse.ContentStream := lMemStream;
{$IF Defined(SeattleOrBetter)}
  AContext.Response.RawWebResponse.ContentEncoding :=
    MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType];
{$ELSE}
  AContext.Response.RawWebResponse.ContentEncoding :=
    AnsiString(MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType]);
{$ENDIF}
  // end;
  //
  // lContentStream.Position := 0;
  // lMemStream := TMemoryStream.Create;
  // try
  // ZCompressStream(lContentStream, lMemStream, TZCompressionLevel.zcDefault);
  // except
  // lMemStream.Free;
  // raise;
  // end;
  //
  // { TODO -oDanieleT -cGeneral : Use directly lContentStreamHelper? }
  // // lZStream := TZCompressionStream.Create(lMemStream, TZCompressionLevel.zcMax,
  // // MVC_COMPRESSION_ZLIB_WINDOW_BITS[lRespCompressionType]);
  // // try
  // // lZStream.CopyFrom(lContentStreamHelper, 0);
  // // finally
  // // lZStream.Free;
  // // end;
  // lMemStream.Position := 0;
  //
  // // AContext.Response.Content := '';
  // // lContentStreamHelper.Size := 0;
  // // lContentStreamHelper.CopyFrom(lMemStream, 0);
  // AContext.Response.RawWebResponse.ContentStream := lMemStream;
  // {$IF Defined(SeattleOrBetter)}
  // AContext.Response.RawWebResponse.ContentEncoding :=
  // MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType];
  // {$ELSE}
  // AContext.Response.RawWebResponse.ContentEncoding :=
  // AnsiString(MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType]);
  // {$ENDIF}
  // // finally
  // // lMemStream.Free;
  // // end;
end;

procedure TMVCCompressionMiddleware.OnBeforeControllerAction
  (AContext: TWebContext; const AControllerQualifiedClassName,
  AActionNAme: string; var AHandled: Boolean);
begin
  // do nothing
end;

procedure TMVCCompressionMiddleware.OnBeforeRouting(AContext: TWebContext;
  var AHandled: Boolean);
begin
  // do nothing
end;

end.
