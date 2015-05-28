unit uGlobalVars;

interface

uses
  Classes, SysUtils;

var
  gAppName, gAppPath, gAppExe: string;

implementation

initialization
  gAppExe := ExtractFileName(ParamStr(0));
  gAppName := ChangeFileExt(gAppExe, '');
  gAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

finalization

end.
