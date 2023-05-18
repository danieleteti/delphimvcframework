program SimpleConsole_appender;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.Threading,
  LoggerPro,
  LoggerPro.SimpleConsoleAppender;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

procedure LogInfo;
begin
  Log.Info('This is a info message with TAG1', 'TAG1');
  Log.Info('This is a info message with TAG2', 'TAG2');
end;

procedure LogDebug;
begin
  Log.Debug('This is a debug message with TAG1', 'TAG1');
  Log.Debug('This is a debug message with TAG2', 'TAG2');
end;

procedure LogWarning;
begin
  Log.Warn('This is a warning message with TAG1', 'TAG1');
  Log.Warn('This is a warning message with TAG2', 'TAG2');
end;

procedure LogError;
begin
  Log.Error('This is an error message with TAG1', 'TAG1');
  Log.Error('This is an error message with TAG2', 'TAG2');
end;

procedure SetupLogger;
begin
  _Log := BuildLogWriter([TLoggerProSimpleConsoleAppender.Create]);
end;

const
  MAX_TASK = 5;
var
  tasks: array of ITask;
begin
  SetupLogger;

  Setlength (tasks, MAX_TASK);
  for var i := 0 to MAX_TASK - 1 do begin
    tasks[i] := TTask.Create(procedure
    var
      I: Integer;
      lThreadID: string;
    begin
      lThreadID := TTask.CurrentTask.Id.ToString;
      for I := 1 to 200 do
      begin
        Log.Debug('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        Log.Info('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        Log.Warn('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        Log.Error('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
      end;
    end);
    tasks[i].Start;
  end;

  TTask.WaitForAll(tasks);
end.
