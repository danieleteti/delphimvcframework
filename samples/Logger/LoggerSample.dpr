program LoggerSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Rtti,
  MVCFramework.Logger,
  LoggerPro;

begin
  try
    WriteLn('=== Basic Logging ===');
    Log.Info('This is an info log', 'log1');
    Log.Warn('This is a warn log', 'log1');
    Log.Debug('This is a debug log', 'log2');
    Log.Error('This is an error log', 'log2');
    Log.Fatal('This is a fatal log', 'log3');

    WriteLn;
    WriteLn('=== Context Logging (LoggerPro 2.0) ===');
    // Context logging with LogParam - structured key-value pairs
    LogI('User login successful', [
      LogParam.S('username', 'john.doe'),
      LogParam.S('ip', '192.168.1.100'),
      LogParam.I('session_id', 12345)
    ]);

    LogW('Slow query detected', [
      LogParam.S('query', 'SELECT * FROM users'),
      LogParam.I('duration_ms', 1500),
      LogParam.S('database', 'main_db')
    ]);

    WriteLn;
    WriteLn('=== Exception Logging ===');
    try
      raise Exception.Create('Something went wrong!');
    except
      on E: Exception do
        LogException(E, 'Error during processing');
    end;

    WriteLn;
    WriteLn('Check the "logs" folder for log files.');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
