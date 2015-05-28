unit Iocp.Logger;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, uGlobalVars, Iocp.Utils;

type
  TLogType = (ltNormal, ltWarning, ltError, ltException);
  TLogTypeSets = set of TLogType;

const
  LogTypeStr: array [TLogType] of string = ('', 'WAR', 'ERR', 'EXP');

type
  TCacheFileStream = class(TThread)
  private
    FFileStream: TFileStream;
    FFileTime: TDateTime;
    FLocker: TCriticalSection;
    FCacheBuffer, FCacheBufferA, FCacheBufferB: TMemoryStream;
    FFlushInterval: DWORD;
  protected
    procedure Lock;
    procedure Unlock;

    procedure Execute; override;
  public
    constructor Create(const AFileName: string); reintroduce;
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint;
    procedure AppendStr(const S: RawByteString); overload;
    procedure AppendStr(const S: UTF8String); overload;
    procedure AppendStr(const S: UnicodeString); overload;
    procedure Flush;

    property FileTime: TDateTime read FFileTime;
    property FlushInterval: DWORD read FFlushInterval write FFlushInterval;
  end;

  TIocpLogger = class
  private
    FRefCount: Integer;
    FFileWriters: array [TLogType] of TCacheFileStream;
    FFileLocker: array [TLogType] of TCriticalSection;
    FLogColor: array [TLogType] of Integer;
    FConsoleHandle: THandle;
    FShowConsole: Boolean;
    FConsoleLocker: TCriticalSection;
    FFilters: TLogTypeSets;
    procedure SetShowConsole(const Value: Boolean);
    procedure SetFilters(const Value: TLogTypeSets);
  protected
    function GetLogFileName(LogType: TLogType; Date: TDateTime): string;
    procedure AppendStrToLogFile(const S: UnicodeString; LogType: TLogType);

    function AddRef: Integer;
    function Release: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AppendLog(const Log: UnicodeString; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
    procedure AppendLog(const Log: UnicodeString; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
    procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
    procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;

    property ShowConsole: Boolean read FShowConsole write SetShowConsole;
    property Filters: TLogTypeSets read FFilters write SetFilters;
  end;

procedure ShowConsoleLog(OnOff: Boolean);
procedure AppendLog(const Log: UnicodeString; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
procedure AppendLog(const Log: UnicodeString; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;
procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; LogType: TLogType = ltNormal; CRLF: string = ';'); overload;

function gIocpLogger: TIocpLogger;

implementation

constructor TIocpLogger.Create;
var
  i: TLogType;
begin
  FRefCount := 1;
  FFilters := [ltNormal, ltWarning, ltError, ltException];

  for i := Low(TLogType) to High(TLogType) do
  begin
    FFileLocker[i] := TCriticalSection.Create;
  end;

  FShowConsole := False;
  FConsoleHandle := INVALID_HANDLE_VALUE;
  FConsoleLocker := TCriticalSection.Create;

  {控制台颜色：

    FOREGROUND_INTENSITY - 前景色加亮
    FOREGROUND_BLUE      - 前景色包含蓝色
    FOREGROUND_GREEN     - 前景色包含绿色
    FOREGROUND_RED       - 前景色包含红色

    BACKGROUND_INTENSITY - 背景色加亮
    BACKGROUND_BLUE      - 背景色包含蓝色
    BACKGROUND_GREEN     - 背景色包含绿色
    BACKGROUND_RED       - 背景色包含红色

   颜色由三原色组合而成
  }
  FLogColor[ltNormal]    := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  FLogColor[ltWarning]   := FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_GREEN;
  FLogColor[ltError]     := FOREGROUND_INTENSITY or FOREGROUND_RED;
  FLogColor[ltException] := FOREGROUND_INTENSITY or FOREGROUND_RED or FOREGROUND_BLUE;
end;

destructor TIocpLogger.Destroy;
var
  i: TLogType;
begin
  for i := Low(TLogType) to High(TLogType) do
  begin
    if Assigned(FFileWriters[i]) then
    begin
      FFileWriters[i].Flush;
      FFileWriters[i].Terminate;
      FFileWriters[i].Free;
      FFileWriters[i] := nil;
    end;
    FFileLocker[i].Free;
  end;
  FConsoleLocker.Free;

  inherited Destroy;
end;

function TIocpLogger.GetLogFileName(LogType: TLogType; Date: TDateTime): string;
begin
  Result := LogTypeStr[LogType];
  if (Result <> '') then
    Result := Result + '-';
  Result := Result + ThreadFormatDateTime('YYYY-MM-DD', Date) + '.log';
end;

function TIocpLogger.Release: Boolean;
begin
  Result := (InterlockedDecrement(FRefCount) = 0);

  if Result then
    Free;
end;

procedure TIocpLogger.SetFilters(const Value: TLogTypeSets);
begin
  FFilters := Value;
end;

procedure TIocpLogger.SetShowConsole(const Value: Boolean);
var
  ConSize: TCoord;
  ConRec: TSmallRect;
begin
  if (FShowConsole = Value) then Exit;
  
  FShowConsole := Value;

  if FShowConsole then
  begin
    if (FConsoleHandle = INVALID_HANDLE_VALUE) then
    begin
      AllocConsole;
      FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);

      // 设置控制台屏幕缓冲区大小，X=列(字节数), Y=行
      ConSize.X := 80;
      ConSize.Y := 8192;
      SetConsoleScreenBufferSize(FConsoleHandle, ConSize);

      // 设置控制台位置大小(以行列为单位)
      ConRec.Left := 0;
      ConRec.Top := 0;
      ConRec.Right := 80 - 1;
      ConRec.Bottom := 25 - 1;
      SetConsoleWindowInfo(FConsoleHandle, True, ConRec);
    end;
  end else
  begin
    if (FConsoleHandle <> INVALID_HANDLE_VALUE) then
    begin
      FreeConsole;
      FConsoleHandle := INVALID_HANDLE_VALUE;
    end;
  end;
end;

procedure TIocpLogger.AppendLog(const Log: UnicodeString; const TimeFormat: string; LogType: TLogType; CRLF: string);
var
  LogText: UnicodeString;
begin
  if (AddRef = 1) then Exit;
  if not (LogType in FFilters) then Exit;

  try
    if (CRLF <> '') then
      LogText := StringReplace(StringReplace(Log, #13#10, CRLF, [rfReplaceAll]),  #10, CRLF, [rfReplaceAll])
    else
      LogText := Log;
    LogText := ThreadFormatDateTime(TimeFormat, Now) + ' ' + LogText + #13#10;

    if FShowConsole then
    try
      FConsoleLocker.Enter;
      SetConsoleTextAttribute(FConsoleHandle, FLogColor[LogType]);
      System.Write(LogText);
      SetConsoleTextAttribute(FConsoleHandle, FLogColor[ltNormal]);
    finally
      FConsoleLocker.Leave;
    end;

    AppendStrToLogFile(LogText, LogType);
  finally
    Release;
  end;
end;

procedure TIocpLogger.AppendLog(const Log: UnicodeString; LogType: TLogType; CRLF: string);
begin
  if (AddRef = 1) then Exit;
  try
    AppendLog(Log, 'HH:NN:SS:ZZZ', LogType, CRLF);
  finally
    Release;
  end;
end;

procedure TIocpLogger.AppendLog(const Fmt: UnicodeString; const Args: array of const; const TimeFormat: string; LogType: TLogType; CRLF: string);
begin
  if (AddRef = 1) then Exit;
  try
    AppendLog(ThreadFormat(Fmt, Args), TimeFormat, LogType, CRLF);
  finally
    Release;
  end;
end;

function TIocpLogger.AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

procedure TIocpLogger.AppendLog(const Fmt: UnicodeString; const Args: array of const; LogType: TLogType; CRLF: string);
begin
  if (AddRef = 1) then Exit;
  try
    AppendLog(ThreadFormat(Fmt, Args), LogType, CRLF);
  finally
    Release;
  end;
end;

procedure TIocpLogger.AppendStrToLogFile(const S: UnicodeString; LogType: TLogType);
var
  LogDir, LogFile: string;
begin
  if (AddRef = 1) then Exit;
  try
    FFileLocker[LogType].Enter;
    try
      if not Assigned(FFileWriters[LogType]) or (Trunc(FFileWriters[LogType].FileTime) <> Trunc(Now)) then
      begin
        if Assigned(FFileWriters[LogType]) then
        begin
          FFileWriters[LogType].Flush;
          FFileWriters[LogType].Terminate;
        end;

        LogDir := gAppPath + gAppName + '.Log\';
        LogFile := LogDir + GetLogFileName(LogType, Now);
        ForceDirectories(LogDir);
        FFileWriters[LogType] := TCacheFileStream.Create(LogFile);
      end;
      FFileWriters[LogType].AppendStr(S);
    finally
      FFileLocker[LogType].Leave;
    end;
  finally
    Release;
  end;
end;

{ TCacheFileStream }

procedure TCacheFileStream.AppendStr(const S: RawByteString);
begin
  Write(S[1], Length(S));
end;

procedure TCacheFileStream.AppendStr(const S: UTF8String);
begin
  AppendStr(RawByteString(S));
end;

procedure TCacheFileStream.AppendStr(const S: UnicodeString);
begin
  AppendStr(UTF8Encode(S));
end;

constructor TCacheFileStream.Create(const AFileName: string);
var
  UTF8Header: RawByteString;
begin
  // 在线程创建之后触发打开文件失败的异常会造成程序崩溃
  // 所以把创建线程(inherited Create)放在打开日志文件之后
  if FileExists(AFileName) then
  begin
    FFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
    UTF8Header := '';
  end else
  begin
    FFileStream := TFileStream.Create(AFileName, fmCreate);
    FFileStream.Free;
    FFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
    // 写UTF8文件头
    UTF8Header := RawByteString(#$EF#$BB#$BF);
  end;

  inherited Create(True);

//  FreeOnTerminate := True;

  FLocker := TCriticalSection.Create;
  FCacheBufferA := TMemoryStream.Create;
  FCacheBufferB := TMemoryStream.Create;
  FCacheBuffer := FCacheBufferA;
  FFileTime := Now;
  FFlushInterval := 1000;

  if (UTF8Header <> '') then
    AppendStr(UTF8Header);

  Suspended := False;
end;

destructor TCacheFileStream.Destroy;
begin
  Lock;
  try
    FCacheBuffer := nil;

    if Assigned(FCacheBufferA) then
      FreeAndNil(FCacheBufferA);
    if Assigned(FCacheBufferB) then
      FreeAndNil(FCacheBufferB);
    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);
  finally
    Unlock;
  end;
  if Assigned(FLocker) then
    FreeAndNil(FLocker);

  inherited Destroy;
end;

procedure TCacheFileStream.Execute;
var
  t: DWORD;
begin
  t := GetTickCount;

  while not Terminated do
  begin
    if (CalcTickDiff(t, GetTickCount) >= FFlushInterval) then
    begin
      Flush;
      t := GetTickCount;
    end else
      SleepEx(100, True);
  end;

  Flush;
end;

procedure TCacheFileStream.Flush;
var
  Buffer: TMemoryStream;
begin
  // 如果当前缓存中没有数据，则不用写文件
  if (FCacheBuffer = nil) or (FCacheBuffer.Position <= 0) then Exit;

  // 双缓冲策略：
  // 读取当前缓存数据，并将另一缓存置为当前缓存
  // 这个过程需要加锁，TCacheFileStream.Write中也需要加同一个锁
  Lock;
  Buffer := FCacheBuffer;
  if (FCacheBuffer = FCacheBufferA) then
    FCacheBuffer := FCacheBufferB
  else
    FCacheBuffer := FCacheBufferA;
  FCacheBuffer.Position := 0;
  Unlock;

  // 将缓存中的数据追加到文件尾
  FFileStream.Seek(0, soEnd);
  FFileStream.Write(Buffer.Memory^, Buffer.Position);
  FlushFileBuffers(FFileStream.Handle);
  Buffer.Position := 0;
end;

procedure TCacheFileStream.Lock;
begin
  FLocker.Enter;
end;

procedure TCacheFileStream.Unlock;
begin
  FLocker.Leave;
end;

function TCacheFileStream.Write(const Buffer; Count: Integer): Longint;
begin
  Lock;
  try
    Result := FCacheBuffer.Write(Buffer, Count);
  finally
    Unlock;
  end;
end;

procedure ShowConsoleLog(OnOff: Boolean);
begin
  gIocpLogger.ShowConsole := OnOff;
end;

procedure AppendLog(const Log: UnicodeString; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';');
begin
  gIocpLogger.AppendLog(Log, TimeFormat, LogType, CRLF);
end;

procedure AppendLog(const Log: UnicodeString; LogType: TLogType = ltNormal; CRLF: string = ';');
begin
  gIocpLogger.AppendLog(Log, LogType, CRLF);
end;

procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; const TimeFormat: string; LogType: TLogType = ltNormal; CRLF: string = ';');
begin
  gIocpLogger.AppendLog(Fmt, Args, TimeFormat, LogType, CRLF);
end;

procedure AppendLog(const Fmt: UnicodeString; const Args: array of const; LogType: TLogType = ltNormal; CRLF: string = ';');
begin
  gIocpLogger.AppendLog(Fmt, Args, LogType, CRLF);
end;

var
  _gIocpLogger: TIocpLogger;

function gIocpLogger: TIocpLogger;
begin
{  if (TInterlocked.CompareExchange<TIocpLogger>(_gIocpLogger, nil, nil) <> nil) then Exit(_gIocpLogger);

  Result := TIocpLogger.Create;
  TInterlocked.Exchange<TIocpLogger>(_gIocpLogger, Result);}

  Result := _gIocpLogger;
end;

initialization
  _gIocpLogger := TIocpLogger.Create;

finalization
  if Assigned(_gIocpLogger) then
    _gIocpLogger.Release;

end.

