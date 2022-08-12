{ <@abstract(The unit to include if you want to use @link(TLoggerProConsoleAppender))
  @author(Daniele Teti)
  @author(Fulgan - https://github.com/Fulgan) }

unit LoggerPro.ConsoleAppender;

interface

uses
  Classes,
  SysUtils,
  LoggerPro,
  SyncObjs;

type
  /// <summary>TLoggerProConsoleAppender
  /// This class creates a new console (if needed) when setup.
  /// Warning: If the application is of type GUI and it is started from a console,
  /// this will NOT log to the calling console but create a new one.
  /// This is because the way cmd.exe works: if the application is of type GUI,
  /// then is immediately detaches from cmd.exe which doesn't wait. This means that
  /// there is never a console to attach to except if a new console is created
  /// elsewhere in the app.
  /// In case this class is used from a console, then there is no guarantee that
  /// the messages will be
  /// displayed in chronological order.
  /// </summary>
  TLoggerProConsoleAppender = class(TLoggerProAppenderBase)
  strict private
    class var FLock: TCriticalSection; // used to prevent syncroneous operations to run at the same time
    class var FConsoleAllocated: Int64; // used to ensure one and only one console is created
    class constructor Create; // allocate global vars
    class destructor Destroy;
  protected
    procedure SetColor(const Color: Integer);
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

  // for some reason, AttachConsole has been left out of Winapi.windows.pas
function AttachConsole(PID: Cardinal): LongBool; stdcall;

implementation

{ TLoggerProConsoleAppender }

uses
  Winapi.Windows,
  Winapi.Messages;

// for some reason, AttachConsole has been left out of Winapi.windows.pas
const
  ATTACH_PARENT_PROCESS = Cardinal(-1);
function AttachConsole; external kernel32 name 'AllocConsole';

const
  { FOREGROUND COLORS - CAN BE COMBINED }
  FOREGROUND_BLUE = 1; { text color blue. }
  FOREGROUND_GREEN = 2; { text color green }
  FOREGROUND_RED = 4; { text color red }
  FOREGROUND_INTENSITY = 8; { text color is intensified }
  { BACKGROUND COLORS - CAN BE COMBINED }
  BACKGROUND_BLUE = $10; { background color blue }
  BACKGROUND_GREEN = $20; { background color green }
  BACKGROUND_RED = $40; { background color red. }
  BACKGROUND_INTENSITY = $80; { background color is intensified }

procedure TLoggerProConsoleAppender.SetColor(const Color: Integer);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), Color);
end;

procedure TLoggerProConsoleAppender.Setup;
begin
  inherited;
  if TInterlocked.read(TLoggerProConsoleAppender.FConsoleAllocated) < 2 then
  begin
    TLoggerProConsoleAppender.FLock.Enter;
    try
      if TInterlocked.Increment(TLoggerProConsoleAppender.FConsoleAllocated) = 1 then
      begin
        // Attempt to attach to the parent (if there is already a console allocated)
        if not IsConsole then
        begin
          if not AttachConsole(ATTACH_PARENT_PROCESS) then
            AllocConsole; // No console allocated, create a new one
        end;
        TInterlocked.Increment(TLoggerProConsoleAppender.FConsoleAllocated);
      end;
    finally
      TLoggerProConsoleAppender.FLock.Leave;
    end;
  end;
end;

procedure TLoggerProConsoleAppender.TearDown;
begin

end;

procedure TLoggerProConsoleAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
  lColor: Integer;
begin
  lColor := FOREGROUND_GREEN; // Avoid W1030
  case aLogItem.LogType of
    TLogType.Debug:
      lColor := FOREGROUND_GREEN;
    TLogType.Info:
      lColor := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
    TLogType.Warning:
      lColor := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    TLogType.Error:
      lColor := FOREGROUND_RED or FOREGROUND_INTENSITY;
  end;

  lText := FormatLog(aLogItem);

  TLoggerProConsoleAppender.FLock.Enter;
  try
    SetColor(lColor);
    Writeln(lText);
  finally
    TLoggerProConsoleAppender.FLock.Leave;
  end;
end;

class constructor TLoggerProConsoleAppender.Create;
begin
  TLoggerProConsoleAppender.FLock := TCriticalSection.Create;
  TLoggerProConsoleAppender.FConsoleAllocated := 0;
end;

class destructor TLoggerProConsoleAppender.Destroy;
begin
  // make sure all code
  try
    TLoggerProConsoleAppender.FLock.Enter;
    FreeAndNil(TLoggerProConsoleAppender.FLock);
  except
    // No exception checking here or the app might blow up with a RTE 217
  end;
end;

end.
