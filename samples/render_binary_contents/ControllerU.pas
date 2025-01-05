unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections,
  System.Classes;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/files/($FileName)')]
    [MVCHTTPMethod([httpGET])]
    function GetFileByName(const FileName: String): TStream;

    [MVCPath('/files2/($FileName)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetStreamByFileName(const FileName: String);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  MVCFramework.Logger,
  System.StrUtils;

{ TMyController }

{
  This action show how to return binary contents using a functional action (the action is a function)
}
function TMyController.GetFileByName(const FileName: String): TStream;
var
  lPath: String;
  lExt: String;
begin
  lPath := TPath.Combine(TPath.Combine(TDirectory.GetParent(AppPath), 'files_repository'), FileName);
  lPath := TPath.GetFullPath(lPath);
  if not lPath.StartsWith(AppPath) then // directory traversal check
  begin
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid path');
  end;

  LogD(Context.QualifiedClassName + ': ' + lPath);

  if not TFile.Exists(lPath) then
  begin
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'File not found');
  end;

  lExt := TPath.GetExtension(lPath).ToLower;

  if lExt = '.jpg' then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
  end
  else if lExt = '.txt' then
  begin
    Context.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  end
  else if lExt = '.pdf' then
  begin
    Context.Response.ContentType := TMVCMediaType.APPLICATION_PDF;
  end
  else if lExt = '.png' then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_PNG;
  end
  else if lExt = '.html' then
  begin
    Context.Response.ContentType := TMVCMediaType.TEXT_HTML;
  end
  else
  begin
    Context.Response.ContentType := TMVCMediaType.APPLICATION_OCTET_STREAM;
  end;
  StatusCode := HTTP_STATUS.OK;
  Result := TFileStream.Create(lPath, fmOpenRead, fmShareDenyNone)
end;


{
  This action show how to return binary contents using a classic action (the action is a procedure)
}
procedure TMyController.GetStreamByFileName(const FileName: String);
var
  lPath: String;
  lExt: String;
begin
  lPath := TPath.Combine(TPath.Combine(TDirectory.GetParent(AppPath), 'files_repository'), FileName);
  lPath := TPath.GetFullPath(lPath);
  if not lPath.StartsWith(AppPath) then // directory traversal check
  begin
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Invalid path');
  end;

  LogD(Context.QualifiedClassName + ': ' + lPath);

  if not TFile.Exists(lPath) then
  begin
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'File not found');
  end;

  lExt := TPath.GetExtension(lPath).ToLower;

  if lExt = '.jpg' then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
  end
  else if lExt = '.txt' then
  begin
    Context.Response.ContentType := TMVCMediaType.TEXT_PLAIN;
  end
  else if lExt = '.pdf' then
  begin
    Context.Response.ContentType := TMVCMediaType.APPLICATION_PDF;
  end
  else if lExt = '.png' then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_PNG;
  end
  else if lExt = '.html' then
  begin
    Context.Response.ContentType := TMVCMediaType.TEXT_HTML;
  end
  else
  begin
    Context.Response.ContentType := TMVCMediaType.APPLICATION_OCTET_STREAM;
  end;
  StatusCode := HTTP_STATUS.OK;
  SendFile(lPath);
end;

end.

