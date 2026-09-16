unit UploadControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons,
  UploadDTOU;

type
  [MVCPath('/api')]
  TUploadController = class(TMVCController)
  public
    [MVCPath('/uploads')]
    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_JSON)]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function Upload([MVCFromBody] const AUpload: TUploadDTO): TUploadResultDTO;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Classes;

function TUploadController.Upload([MVCFromBody] const AUpload: TUploadDTO): TUploadResultDTO;
var
  lUploadsDir: string;
  lSafeName: string;
  lTargetPath: string;
  lStream: TBytesStream;
begin
  if (AUpload = nil) or AUpload.FileDataURL.IsEmpty then
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Empty upload');

  lUploadsDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'uploaded');
  TDirectory.CreateDirectory(lUploadsDir);

  lSafeName := TPath.GetFileName(AUpload.Name);
  if lSafeName = '' then
    lSafeName := 'upload.bin';

  lTargetPath := TPath.Combine(lUploadsDir,
    FormatDateTime('yyyymmdd_hhnnsszzz_', Now) + lSafeName);

  lStream := TBytesStream.Create(AUpload.FileDataURL.Data);
  try
    lStream.SaveToFile(lTargetPath);
  finally
    lStream.Free;
  end;

  Result := TUploadResultDTO.Create;
  Result.Name := AUpload.Name;
  Result.MimeType := AUpload.FileDataURL.MimeType;
  Result.Size := AUpload.FileDataURL.Size;
  Result.SavedAs := lTargetPath;
end;

end.
