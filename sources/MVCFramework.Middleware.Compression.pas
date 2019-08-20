// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
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

procedure TMVCCompressionMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
var
  lMemStream: TMemoryStream;
  lContentStream: TStream;
  lAcceptEncoding: string;
  lEncodings: TArray<string>;
  lItem: string;
  lZStream: TZCompressionStream;
  lRespCompressionType: TMVCCompressionType;
  lTmpItem: string;
begin
  lContentStream := Context.Response.RawWebResponse.ContentStream;
  if (lContentStream = nil) or (lContentStream is TFileStream) or (lContentStream.Size <= fCompressionThreshold) then
    Exit;

  lAcceptEncoding := Context.Request.Headers['Accept-Encoding'];
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
      lRespCompressionType := TMVCCompressionType.ctGZIP;
      break;
    end
    else if lTmpItem = 'deflate' then
    begin
      lRespCompressionType := TMVCCompressionType.ctDeflate;
      break;
    end;
  end;

  if lRespCompressionType = TMVCCompressionType.ctNone then
    Exit;

  lContentStream.Position := 0;
  lMemStream := TMemoryStream.Create;
  try
    {TODO -oDanieleT -cGeneral : Use directly lContentStream?}
    lZStream := TZCompressionStream.Create(lMemStream, TZCompressionLevel.zcMax, MVC_COMPRESSION_ZLIB_WINDOW_BITS[lRespCompressionType]);
    try
      lZStream.CopyFrom(lContentStream, 0);
    finally
      lZStream.Free;
    end;
    lMemStream.Position := 0;

    Context.Response.Content := '';
    lContentStream.Size := 0;
    lContentStream.CopyFrom(lMemStream, 0);
{$IF Defined(SeattleOrBetter)}
    Context.Response.RawWebResponse.ContentEncoding := MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType];
{$ELSE}
    Context.Response.RawWebResponse.ContentEncoding := AnsiString(MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType]);
{$ENDIF}
  finally
    lMemStream.Free;
  end;	
end;

procedure TMVCCompressionMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCCompressionMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
