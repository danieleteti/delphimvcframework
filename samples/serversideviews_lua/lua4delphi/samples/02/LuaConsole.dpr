program LuaConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  LuaBind,
  System.SysUtils,
  LuaBind.Intf in '..\..\LuaBind.Intf.pas';

var
  LuaEngine: TLuaEngine;
  ScriptDir, PackagePathToAppend: string;

begin
  try
    LuaEngine := TLuaEngine.Create;
    try
      ScriptDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(1)));
      PackagePathToAppend := ScriptDir + '?.lua';
      LuaEngine.ExecuteScript('package.path = package.path .. ";' +
        PackagePathToAppend.Replace('\', '\\') + '"' + sLineBreak +
        '__SCRIPTFILE__ = "' + ParamStr(1).Replace('\', '\\') + '"' +
        sLineBreak +
        '__SCRIPTDIR__ = "' + ScriptDir.Replace('\', '\\') + '"' +
        sLineBreak);
      LuaEngine.LoadFromFile(ParamStr(1));
      LuaEngine.Execute;
    finally
      LuaEngine.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  if DebugHook <> 0 then
    readln;

end.
