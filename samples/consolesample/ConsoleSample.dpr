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
  ShowCursor;
  try
    ClrScr;
    GotoXY(0,0);
    Write('Press ANY key (cursor is visible)');
    GetCh;
    GotoXY(0,0);
    HideCursor;
    Write('Press ANY key (cursor is now hidden)');
    GetCh;
    try
      for F := 0 to 15 do
      begin
        TextBackground(Black);
        ClrScr;
        TextBackground(Black);
        TextColor(TConsoleColor(F));
        lFGColorName := ColorName(TConsoleColor(F));
        WriteLn(''.PadLeft(GetConsoleSize.Columns));
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
        GotoXY(0,0);
        TextBackground(White);
        TextColor(Red);
        Write(' Press any key to continue ');
        GetCh;
      end;
      ResetConsole;
      ClrScr;
      lSize := GetConsoleSize;
      WriteLn(Format('Console Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
      lSize := GetConsoleBufferSize;
      WriteLn(Format('Console Buffer Size: %d columns x %d rows', [lSize.Columns, lSize.Rows]));
      GetCh;

      // limits
      ClrScr;
      TextColor(TConsoleColor.Red);
      GotoXY(0, 0);
      Write('X');
      GotoXY(lSize.Columns - 1, 0);
      Write('X');
      GotoXY(lSize.Columns - 1, lSize.Rows - 1);
      Write('X');
      GotoXY(0, lSize.Rows - 1);
      Write('X');
      CenterInScreen('CONSOLE LIMITS');
      GetCh;
      ResetConsole;
      ClrScr;
      GotoXY(0,0);
    except
      on E: Exception do
        WriteLn(E.ClassName, ': ', E.Message);
    end;
  finally
    ShowCursor;
  end;

  ResetConsole;
  TextColor(TConsoleColor.Red);
  Write('Press ANY key to exit...');
  ResetConsole;
  GetCh;
  ClrScr;
end.
