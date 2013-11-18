unit FileUploadControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Logger;

type

  [MVCPath('/file')]
  TFileUploadController = class(TMVCController)
  public
    [MVCPath('')]
    [MVCProduce('text/plain')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveFile(CTX: TWebContext);
  end;

implementation

uses
  system.ioutils,
  system.Classes,
  system.SysUtils,
  system.Types;

{ TFileUploadController }

procedure TFileUploadController.SaveFile(CTX: TWebContext);

var
  s, fname     : string;
  I            : Integer;
  fs           : TFileStream;
  UploadedFiles: TStringDynArray;
const
  UPLOAD_FOLDER = 'uploadedfiles';
begin
  TDirectory.CreateDirectory(UPLOAD_FOLDER);
  for I := 0 to CTX.Request.RawWebRequest.Files.Count - 1 do
  begin
    Log('Uploading ' + fname);
    fname := CTX.Request.Files[I].FileName;
    fname := fname.Replace('"', '');
    fs := TFile.Create(TPath.Combine(UPLOAD_FOLDER, fname));
    try
      fs.CopyFrom(CTX.Request.Files[I].Stream, 0);
    finally
      fs.free;
    end;
  end;

  ResponseStream.AppendLine('**Upload Folder Content**');
  UploadedFiles := TDirectory.GetFiles(UPLOAD_FOLDER);

  for fname in UploadedFiles do
  begin
    ResponseStream.AppendLine(ExtractFileName(fname));
  end;
  Render;
end;

end.
