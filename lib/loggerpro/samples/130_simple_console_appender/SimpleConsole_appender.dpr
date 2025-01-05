program SimpleConsole_appender;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.Threading,
  LoggerPro,
  LoggerPro.SimpleConsoleAppender;

const
  MAX_TASK = 5;
var
  lTasks: array of ITask;
  lLog: ILogWriter;
begin
  lLog := BuildLogWriter([TLoggerProSimpleConsoleAppender.Create]);

  Setlength (lTasks, MAX_TASK);
  for var i := 0 to MAX_TASK - 1 do begin
    lTasks[i] := TTask.Create(procedure
    var
      I: Integer;
      lThreadID: string;
    begin
      lThreadID := TTask.CurrentTask.Id.ToString;
      for I := 1 to 200 do
      begin
        lLog.Debug('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        lLog.Info('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        lLog.Warn('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        lLog.Error('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
        lLog.Fatal('log message ' + TimeToStr(now) + ' ThreadID: ' + lThreadID, 'MULTITHREADING');
      end;
    end);
    lTasks[i].Start;
  end;

  TTask.WaitForAll(lTasks);
end.
