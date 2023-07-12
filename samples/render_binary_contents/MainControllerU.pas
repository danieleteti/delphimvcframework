unit MainControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/files/($FileName)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetFileByName(const FileName: String);

    [MVCPath('/streams/($FileName)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetStreamByFileName(const FileName: String);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  MVCFramework.Logger,
  System.StrUtils, System.Classes;

{ TMyController }

procedure TMyController.GetFileByName(const FileName: String);
var
  lPath: String;
begin
  lPath := TPath.Combine(AppPath, FileName);
  if TPath.GetExtension(lPath).ToLower.Equals('.jpg') and
     TPath.GetFullPath(lPath).StartsWith(AppPath) and
     TFile.Exists(lPath)
  then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
    RenderFile(lPath);
  end
  else
    raise EMVCException.Create(HTTP_STATUS.NotFound, '');
end;

procedure TMyController.GetStreamByFileName(const FileName: String);
var
  lPath: String;
begin
  lPath := TPath.Combine(AppPath, FileName);
  if TPath.GetExtension(lPath).ToLower.Equals('.jpg') and
     TPath.GetFullPath(lPath).StartsWith(AppPath) and
     TFile.Exists(lPath)
  then
  begin
    Context.Response.ContentType := TMVCMediaType.IMAGE_JPEG;
    RenderStream(
      TFileStream.Create(lPath, fmOpenRead, fmShareDenyNone)
    );
  end
  else
    raise EMVCException.Create(HTTP_STATUS.NotFound, '');
end;

end.
