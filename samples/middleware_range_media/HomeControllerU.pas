unit HomeControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons;

type
  [MVCPath('/')]
  THomeController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    function Index: string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections;

function THomeController.Index: string;
var
  LMediaPath: string;
  LFiles: TArray<string>;
  LFileList: TList<string>;
  LFile: string;
  LExt: string;
begin
  LMediaPath := TPath.Combine(AppPath, 'media');
  LFileList := TList<string>.Create;
  try
    if TDirectory.Exists(LMediaPath) then
    begin
      LFiles := TDirectory.GetFiles(LMediaPath);
      for LFile in LFiles do
      begin
        LExt := LowerCase(TPath.GetExtension(LFile));
        if LExt.StartsWith('.mp') or LExt.StartsWith('.m4') or
           LExt.StartsWith('.ogg') or LExt.StartsWith('.wav') or
           LExt.StartsWith('.web') or LExt.StartsWith('.flac') or
           LExt.StartsWith('.aac') or LExt.StartsWith('.opus') or
           LExt.StartsWith('.mkv') or LExt.StartsWith('.avi') or
           LExt.StartsWith('.mov') or LExt.StartsWith('.ogv') then
          LFileList.Add(TPath.GetFileName(LFile));
      end;
    end;
    ViewData['files'] := String.Join(',', LFileList.ToArray);
    ViewData['file_count'] := LFileList.Count;
  finally
    LFileList.Free;
  end;
  Result := RenderView('index');
end;

end.
