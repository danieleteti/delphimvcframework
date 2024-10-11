unit FileUploadControllerU;

interface

uses
  ReqMulti, // this unit is required to enable file uploading
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger;

type

  [MVCPath]
  TFileUploadController = class(TMVCController)
  public const
    UPLOAD_FOLDER = 'uploadedfiles';
  private
    function GetFiles: TList<String>;

  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    function Index: string;

    [MVCPath('/file/upload')]
    [MVCHTTPMethod([httpPOST])]
    function SaveFile: IMVCResponse;
  end;

implementation

uses
  system.ioutils,
  system.Classes,
  system.SysUtils,
  system.Types,
  JsonDataObjects;

{ TFileUploadController }

//function TFileUploadController.FileList: String;
//var
//  lFileNames: TList<String>;
//begin
//  lFileNames := GetFiles;
//  try
//    ViewData['files'] := lFileNames;
//    Result := PageFragment(['filelist']);
//  finally
//    lFileNames.free;
//  end;
//end;

function TFileUploadController.GetFiles: TList<String>;
var
  lUploadedFiles: TArray<string>;
  lFName: string;
begin
  lUploadedFiles := TDirectory.GetFiles(UPLOAD_FOLDER);
  Result := TList<String>.Create;
  try
    for lFName in lUploadedFiles do
    begin
      Result.Add(ExtractFileName(lFName));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TFileUploadController.Index: String;
var
  lFileNames: TList<String>;
begin
  lFileNames := GetFiles;
  try
    ViewData['files'] := lFileNames;
    ViewData['files_count'] := lFileNames.Count;
    Result := Page(['fileupload','filelist']);
  finally
    lFileNames.free;
  end;
end;

procedure TFileUploadController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  SetPagesCommonHeaders(['header']);
  SetPagesCommonFooters(['footer']);
end;

function TFileUploadController.SaveFile: IMVCResponse;
var
  lFName: string;
  lFile: TFileStream;
begin
  if Context.Request.Files.Count <> 1 then
  begin
    Exit(RedirectResponse('/'));
  end;

  lFName := String(Context.Request.Files[0].FileName);
  lFName := TPath.GetFileName(lFName.Trim(['"']));
  if not TPath.HasValidFileNameChars(lFName, false) then
  begin
    raise EMVCException.Create
      (HTTP_STATUS.BadRequest, lFName + ' is not a valid filename for the hosting OS');
  end;
  if TFile.Exists(TPath.Combine(UPLOAD_FOLDER, lFName)) then
  begin
    raise EMVCException.Create
      (HTTP_STATUS.BadRequest, lFName + ' already present, cannot override');
  end;
  Log('Uploading ' + lFName);
  lFile := TFile.Create(TPath.Combine(UPLOAD_FOLDER, lFName));
  try
    lFile.CopyFrom(Context.Request.Files[0].Stream, 0);
  finally
    lFile.free;
  end;
  Result := RedirectResponse('/');
end;

end.
