unit VaniConfig;

interface

uses
  Windows, Classes, SysUtils, SyncObjs, DateUtils, System.Generics.Collections,
  FileSystemWatcher, IniFiles, Iocp.ReadWriteLocker;

type
  // 如果要在多线程中读取配置信息，需要调用Lock/Unlock加锁/解锁
  TVaniConfig = class
  private
    FFileWatcher: TFileSystemWatcher;
    FFileChangeTime: TDictionary<string, TDateTime>;
    FLog: Boolean;
    FLocker: TIocpReadWriteLocker;
    FMinChangeSec: Integer;

    procedure DoOnFileChange(FileOperation: TFileOperation; const FileName1, FileName2: string);
  protected
    FIniFile: TIniFile;
    FIniFileName: string;

    function IsConfigFile(const FileName: string): Boolean; virtual;
    procedure DoReadConfig; virtual;
    procedure InitDataStruct; virtual;
  public
    constructor Create(IniFile: string = ''; Log: Boolean = False; MinChangeSec: Integer = 0); virtual;
    destructor Destroy; override;

    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;

    procedure ReadConfig; virtual; abstract;
    function CheckConfig: Boolean; virtual;
    function BR(const s: WideString): WideString;
    function RB(const s: WideString): WideString;

    property IniFile: TIniFile read FIniFile;
    property Log: Boolean read FLog write FLog;
    property MinChangeSec: Integer read FMinChangeSec write FMinChangeSec;
  end;

implementation

uses
  uGlobalVars, Iocp.Logger, Iocp.Utils;

{ TVaniConfig }

function WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString;
begin
  {$if COMPILERVERSION >= 20}
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
  {$else}
  Result := Tnt_WideStringReplace(S, OldPattern, NewPattern, Flags);
  {$ifend}
end;

function TVaniConfig.BR(const s: WideString): WideString;
begin
  Result := WideStringReplace(s, '<br>', #13#10, [rfReplaceAll, rfIgnoreCase]);
end;

function TVaniConfig.CheckConfig: Boolean;
begin
  Result := True;
end;

constructor TVaniConfig.Create(IniFile: string; Log: Boolean; MinChangeSec: Integer);
begin
  if (IniFile = '') then
    IniFile := gAppPath + gAppName + '.ini';
  FIniFileName := ExpandFullFileName(IniFile);
  FIniFile := TIniFile.Create(FIniFileName);
  FLog := Log;
  FMinChangeSec := MinChangeSec;

  FFileChangeTime := TDictionary<string, TDateTime>.Create;
  FLocker := TIocpReadWriteLocker.Create;

  FFileWatcher := TFileSystemWatcher.Create(nil);
  FFileWatcher.WatchedDir := ExtractFilePath(FIniFileName);
  FFileWatcher.WatchSubTree := True;
  FFileWatcher.NotifyFilters := [nfWriteChange];
  FFileWatcher.OnChange := DoOnFileChange;

  FFileWatcher.Actived := True;
  InitDataStruct;
  DoReadConfig;
end;

destructor TVaniConfig.Destroy;
begin
  FIniFile.Free;
  FFileChangeTime.Free;
  FLocker.Free;
  inherited Destroy;
end;

procedure TVaniConfig.DoOnFileChange(FileOperation: TFileOperation;
  const FileName1, FileName2: string);
var
  Time: TDateTime;
begin
  if IsConfigFile(FileName1) then
  begin
    if FFileChangeTime.TryGetValue(FileName1, Time) then
    begin
      // 忽略掉设定时间以内的文件变化
      if (FMinChangeSec > 0) and (IncSecond(Time, FMinChangeSec) > Now) then Exit;
    end;
    FFileChangeTime.AddOrSetValue(FileName1, Now);
    
    DoReadConfig;
  end;
end;

procedure TVaniConfig.DoReadConfig;
begin
  try
    WriteLock;
    try
      ReadConfig;
    finally
      WriteUnlock;
    end;

    if FLog then
      AppendLog('加载: ' + FIniFileName);
  except
    on e: Exception do
      AppendLog(Format('TVaniConfig.DoReadConfig:%s:%s', [e.ClassName, e.Message]));
  end;
end;

procedure TVaniConfig.InitDataStruct;
begin
end;

function TVaniConfig.IsConfigFile(const FileName: string): Boolean;
begin
  Result := SameText(FileName, FIniFileName);
end;

function TVaniConfig.RB(const s: WideString): WideString;
begin
  Result := WideStringReplace(s, #13#10, '<br>', [rfReplaceAll, rfIgnoreCase]);
  Result := WideStringReplace(Result, #13, '<br>', [rfReplaceAll, rfIgnoreCase]);
  Result := WideStringReplace(Result, #10, '<br>', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TVaniConfig.ReadLock;
begin
  FLocker.ReadLock;
end;

procedure TVaniConfig.ReadUnlock;
begin
  FLocker.ReadUnlock;
end;

procedure TVaniConfig.WriteLock;
begin
  FLocker.WriteLock;
end;

procedure TVaniConfig.WriteUnlock;
begin
  FLocker.WriteUnlock;
end;

end.
