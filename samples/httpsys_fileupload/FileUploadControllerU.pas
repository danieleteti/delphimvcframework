unit FileUploadControllerU;

interface

uses
  ReqMulti, // this unit is required to enable file uploading
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger;

type

  [MVCPath]
  TFileUploadController = class(TMVCController)
  private const
    UPLOAD_FOLDER = 'uploadedfiles';
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/file/upload')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveFile(CTX: TWebContext);

    [MVCPath('/file/list')]
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
  lUploadedFiles: TStringDynArray;
  lFName: string;
begin
  LoadView(['header']);
  // ResponseStream.AppendLine('<!doctype html><html><body>');
  ResponseStream.AppendLine('<h2>**Upload Folder Content**</h2>');
  lUploadedFiles := TDirectory.GetFiles(UPLOAD_FOLDER);
  ResponseStream.AppendLine('<ul>');
  for lFName in lUploadedFiles do
  begin
    ResponseStream.AppendLine('<li>' + ExtractFileName(lFName) + '</li>');
  end;
  ResponseStream.AppendLine('</ul>')
    .AppendLine('<p><a href="/">&lt;&lt; BACK TO HOME</a></p>');
  LoadView(['footer']);
  RenderResponseStream;
end;

procedure TFileUploadController.Index;
begin
  LoadView(['header', 'fileupload', 'footer']);
  RenderResponseStream();
end;

procedure TFileUploadController.SaveFile(CTX: TWebContext);
var
  lFName: string;
  I: Integer;
  lFile: TFileStream;
begin
  TDirectory.CreateDirectory(UPLOAD_FOLDER);
  for I := 0 to CTX.Request.RawWebRequest.Files.Count - 1 do
  begin
    lFName := String(CTX.Request.Files[I].FileName);
    lFName := TPath.GetFileName(lFName.Trim(['"']));
    if not TPath.HasValidFileNameChars(lFName, false) then
      raise EMVCException.Create
        (lFName + ' is not a valid filename for the hosting OS');
    Log('Uploading ' + lFName);
    lFile := TFile.Create(TPath.Combine(UPLOAD_FOLDER, lFName));
    try
      lFile.CopyFrom(CTX.Request.Files[I].Stream, 0);
    finally
      lFile.free;
    end;
  end;

  Redirect('/file/list');
end;

end.
