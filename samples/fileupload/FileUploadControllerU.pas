unit FileUploadControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger;

type

  [MVCPath('/file')]
  TFileUploadController = class(TMVCController)
  private const
    UPLOAD_FOLDER = 'uploadedfiles';
  public
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveFile(CTX: TWebContext);
    [MVCPath('/list')]
    [MVCProduces('text/html')]
    [MVCHTTPMethod([httpGET])]
    procedure FileList(CTX: TWebContext);
  end;

implementation

uses
  system.ioutils,
  system.Classes,
  system.SysUtils,
  system.Types;

{ TFileUploadController }

procedure TFileUploadController.FileList(CTX: TWebContext);
var
  UploadedFiles: TStringDynArray;
  fname: string;
begin
  ResponseStream.AppendLine('<!doctype html><html><body>');
  ResponseStream.AppendLine('<h2>**Upload Folder Content**</h2>');
  UploadedFiles := TDirectory.GetFiles(UPLOAD_FOLDER);
  ResponseStream.AppendLine('<ul>');
  for fname in UploadedFiles do
  begin
    ResponseStream.AppendLine('<li>' + ExtractFileName(fname) + '</li>');
  end;
  ResponseStream.AppendLine('</ul>')
    .AppendLine('<p><a href="/fileupload.html">back to upload form</a></p>')
    .AppendLine('</body></html>');
  Render;
end;

procedure TFileUploadController.SaveFile(CTX: TWebContext);
var
  fname: string;
  I: Integer;
  fs: TFileStream;
begin
  TDirectory.CreateDirectory(UPLOAD_FOLDER);
  for I := 0 to CTX.Request.RawWebRequest.Files.Count - 1 do
  begin
    fname := String(CTX.Request.Files[I].FileName);
    fname := TPath.GetFileName(fname.Trim(['"']));
    if not TPath.HasValidFileNameChars(fname, false) then
      raise EMVCException.Create
        (fname + ' is not a valid filename for the hosting OS');
    Log('Uploading ' + fname);
    fs := TFile.Create(TPath.Combine(UPLOAD_FOLDER, fname));
    try
      fs.CopyFrom(CTX.Request.Files[I].Stream, 0);
    finally
      fs.free;
    end;
  end;

  Redirect('/file/list');
end;

end.
