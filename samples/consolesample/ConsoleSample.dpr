program ConsoleSample;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.TypInfo,
  MVCFramework.Console;

var
  F, B: Integer;
  lFGColorName, lBGColorName: String;
  lSize: TMVCConsoleSize;

begin
  Write('Press ANY key');
  var c: Char := GetCh;
  try
    for F := 0 to 15 do
    begin
      TextBackground(Black);
      TextColor(TConsoleColor(F));
      lFGColorName := ColorName(TConsoleColor(F));
      WriteLn(''.PadLeft(GetConsoleSize.Columns));
      WriteLn(('** TEST FOREGROUND COLOR: ' + lFGColorName + ' **').PadRight(GetConsoleSize.Columns));
      WriteLn(StringOfChar('_', GetConsoleSize.Columns));
      WriteLn(''.PadLeft(GetConsoleSize.Columns));
      for B := 0 to 15 do
      begin
        lBGColorName := ColorName(TConsoleColor(B));
        TextBackground(TConsoleColor(B));
        WriteLn((lFGColorName + ' on ' + lBGColorName).PadLeft(GetConsoleSize.Columns));
      end;
      GetCh;
    end;
    ReadLn;
    ResetConsole;
    ClrScr;
    lSize := GetConsoleSize;
    WriteLn(Format('Console Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
    lSize := GetConsoleBufferSize;
    WriteLn(Format('Console Buffer Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
    readln;
    GotoXY(0, 0);
    TextColor(TConsoleColor.Red);
    Write('X');
    GotoXY(lSize.Columns - 1, lSize.Rows - 2);
    Write('X');
    ResetConsole;
    GotoXY(0,0);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  if DebugHook <> 0 then
  begin
    ResetConsole;
    TextColor(TConsoleColor.Red);
    Write('Press ANY key to exit...');
    ResetConsole;
    GetCh;
    ClrScr;
  end;
end.
