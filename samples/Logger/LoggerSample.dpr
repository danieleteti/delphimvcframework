program LoggerSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, MVCFramework.Logger;

begin
  try
    Log.Info('This is an info log', 'log1');
    Log.Warn('This is a warn log', 'log1');
    Log.Debug('This is a debug log', 'log2');
    Log.Error('This is an error log', 'log2');
    Log.Fatal('This is a fatal log', 'log3');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
