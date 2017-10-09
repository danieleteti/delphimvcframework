unit MVCFramework.Middleware.Compression;

interface

uses
  MVCFramework, MVCFramework.Logger;

type
  TCompressionMiddleware = class(TInterfacedObject, IMVCMiddleware)
  protected
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
  end;

implementation

uses
  System.SysUtils, System.ZLib, System.Classes;

{ TMVCSalutationMiddleware }

procedure TCompressionMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
var
  lMemStream: TMemoryStream;
  lContentStream: TStream;
begin
  { TODO -oDaniele -cGeneral : It doesn0t work! }
  if Context.Request.Headers['Accept-Encoding'].Trim.ToLower <> 'deflate' then
    Exit;

  lContentStream := Context.Response.RawWebResponse.ContentStream;
  if lContentStream = nil then
    Exit;

  lContentStream.Position := 0;
  lMemStream := TMemoryStream.Create;
  try
    ZCompressStream(Context.Response.RawWebResponse.ContentStream, lMemStream);
    lMemStream.Position := 0;
  except
    lMemStream.Free;
    raise;
  end;

  Context.Response.Content := '';
  Context.Response.RawWebResponse.ContentStream := lMemStream;
  Context.Response.RawWebResponse.ContentEncoding := 'deflate';
end;

procedure TCompressionMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin
  // do nothing
end;

procedure TCompressionMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
