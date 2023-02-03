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
  var c: Char := GetCh;
  WriteLn('Hai premuto ', c);
  readln;
  try
    for F := 0 to 15 do
    begin
      TextBackground(Black);
      TextColor(TConsoleColor(F));
      lFGColorName := ColorName(TConsoleColor(F));
      WriteLn(''.PadLeft(40));
      WriteLn(('** TEST FOREGROUND COLOR: ' + lFGColorName + ' **').PadLeft(40));
      WriteLn(''.PadLeft(40));
      for B := 0 to 15 do
      begin
        lBGColorName := ColorName(TConsoleColor(B));
        TextBackground(TConsoleColor(B));
        WriteLn((lFGColorName + ' on ' + lBGColorName).PadLeft(40));
      end;
    end;
    readln;
    ResetConsole;
    ClrScr;
    lSize := GetConsoleSize;
    WriteLn(Format('Console Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
    lSize := GetConsoleBufferSize;
    WriteLn(Format('Console Buffer Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
    readln;
    GotoXY(0, 0);
    Write('X');
    GotoXY(lSize.Columns - 1, lSize.Rows - 2);
    Write('X');

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  readln;

end.
