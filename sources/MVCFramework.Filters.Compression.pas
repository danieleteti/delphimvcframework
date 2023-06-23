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

unit MVCFramework.Filters.Compression;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger;

type
  TMVCCompressionProtocolFilter = class(TProtocolFilter)
  private
    fCompressionThreshold: Integer;
  protected
    procedure DoFilter(Context: TWebContext); override;
  public
    constructor Create(aCompressionThreshold: Integer = 1024); virtual;
  end;

implementation

uses
  System.SysUtils,
  System.ZLib,
  System.Classes,
  MVCFramework.Commons;

{ TMVCCompressionProtocolFilter }

constructor TMVCCompressionProtocolFilter.Create(aCompressionThreshold: Integer);
begin
  inherited Create;
  fCompressionThreshold := aCompressionThreshold;
end;

procedure TMVCCompressionProtocolFilter.DoFilter(Context: TWebContext);
var
  lMemStream: TMemoryStream;
  lContentStream: TStream;
  lAcceptEncoding: string;
  lEncodings: TArray<string>;
  lItem: string;
  lRespCompressionType: TMVCCompressionType;
  lTmpItem: string;
  lZStream: TZCompressionStream;
begin
  DoNext(Context);
  if IsLibrary then {disables compression i apache modules and ISAPIs}
  begin
    Exit;
  end;

  lContentStream := Context.Response.RawWebResponse.ContentStream;
  if (lContentStream = nil) or (lContentStream.Size <= fCompressionThreshold) then
  begin
    Exit;
  end;

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
      Break;
    end
    else if lTmpItem = 'deflate' then
    begin
      lRespCompressionType := TMVCCompressionType.ctDeflate;
      break;
    end;
  end;

  if lRespCompressionType = TMVCCompressionType.ctNone then
  begin
    Exit;
  end;

  { When it is a TFileStream copy it to a TMemoryStream, as TFileStream is read only }
  lMemStream := TMemoryStream.Create;
  try
    lZStream := TZCompressionStream.Create(lMemStream, TZCompressionLevel.zcMax,
      MVC_COMPRESSION_ZLIB_WINDOW_BITS[lRespCompressionType]);
    try
      lContentStream.Position := 0;
      lZStream.CopyFrom(lContentStream, 0);
    finally
      lZStream.Free;
    end;
  except
    lMemStream.Free;
    raise;
  end;
  lMemStream.Position := 0;
  Context.Response.RawWebResponse.ContentStream := lMemStream;
{$IF Defined(SeattleOrBetter)}
  Context.Response.RawWebResponse.ContentEncoding := MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType];
{$ELSE}
  Context.Response.RawWebResponse.ContentEncoding := AnsiString(MVC_COMPRESSION_TYPE_AS_STRING[lRespCompressionType]);
{$ENDIF}
end;

end.
