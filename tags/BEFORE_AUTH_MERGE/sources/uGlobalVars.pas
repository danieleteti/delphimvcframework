unit uGlobalVars;

interface

uses
  Classes, SysUtils;

var
  gAppName, gAppPath, gAppExe: string;

implementation

uses
  ioutils;

initialization

gAppExe := ExtractFileName(GetModuleName(HInstance) { ParamStr(0) } );
gAppName := ChangeFileExt(gAppExe, '');
// if not IsConsole then
// gAppPath := IncludeTrailingPathDelimiter(TPath.GetPublicPath)
// else
gAppPath := IncludeTrailingPathDelimiter
  (ExtractFilePath(GetModuleName(HInstance) { ParamStr(0) } ));

finalization

end.
