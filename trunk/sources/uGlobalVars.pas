unit uGlobalVars;

interface

uses
  Classes, SysUtils;

var
  gAppName, gAppPath, gAppExe: string;

implementation

initialization

  gAppExe := ExtractFileName(GetModuleName(HInstance) {ParamStr(0)});
  gAppName := ChangeFileExt(gAppExe, '');
  gAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance) {ParamStr(0)}));

finalization

end.
