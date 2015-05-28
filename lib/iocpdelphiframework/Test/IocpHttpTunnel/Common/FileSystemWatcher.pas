{*******************************************************}
{                                                       }
{       FileSystemWatcher                               }
{                                                       }
{       ∞Ê»®À˘”– (C) 2007 solokey                       }
{                                                       }
{       http://blog.csdn.net/solokey                    }
{                                                       }
{*******************************************************}

unit FileSystemWatcher;

interface
uses
  Windows, Classes, SysUtils, Iocp.ApiFix;

type
  TFileOperation = (foAdded, foRemoved, foModified, foRenamed);
  TFileDealMethod = procedure(FileOperation: TFileOperation; const FileName1, FileName2: string) of object;

  TNotifyFilter = (nfFileNameChange, nfDirNameChange, nfAttributeChange,
    nfSizeChange, nfWriteChange, nfAccessChange, nfCreationDateChange, nfSecurityChange);
  TNotifyFilters = set of TNotifyFilter;

  TNotificationBuffer = array[0..4095] of Byte;

  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;

  TShellChangeThread = class(TThread)
  private
    FActived: Boolean;
    FDirectoryHandle: Cardinal;
    FCS: TRTLCriticalSection;
    FChangeEvent: TFileDealMethod;
    FDirectory: string;
    FWatchSubTree: Boolean;
    FCompletionPort: Cardinal;
    FOverlapped: TOverlapped;
    FNotifyOptionFlags: DWORD;
    FBytesWritten: DWORD;
    FNotificationBuffer: TNotificationBuffer;
  protected
    procedure Execute; override;
    procedure DoIOCompletionEvent;
    function ResetReadDirctory: Boolean;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(ChangeEvent: TFileDealMethod); virtual;
    destructor Destroy; override;
    procedure SetDirectoryOptions(Directory: string; Actived: Boolean; WatchSubTree: Boolean;
      NotifyOptionFlags: DWORD);
    property ChangeEvent: TFileDealMethod read FChangeEvent write FChangeEvent;
  end;

  TFileSystemWatcher = class(TComponent)
  private
    FActived: Boolean;
    FWatchedDir: string;
    FThread: TShellChangeThread;
    FOnChange: TFileDealMethod;
    FWatchSubTree: Boolean;
    FFilters: TNotifyFilters;
    procedure SetWatchedDir(const Value: string);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure SetOnChange(const Value: TFileDealMethod);
    procedure SetFilters(const Value: TNotifyFilters);
    function NotifyOptionFlags: DWORD;
    procedure SetActived(const Value: Boolean);
  protected
    procedure Change;
    procedure Start;
    procedure Stop;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Actived: Boolean read FActived write SetActived;
    property WatchedDir: string read FWatchedDir write SetWatchedDir;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property NotifyFilters: TNotifyFilters read FFilters write SetFilters;
    property OnChange: TFileDealMethod read FOnChange write SetOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TFileSystemWatcher]);
end;

{ TShellChangeThread }

constructor TShellChangeThread.Create(ChangeEvent: TFileDealMethod);
begin
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  InitializeCriticalSection(FCS);
  FDirectoryHandle := 0;
  FCompletionPort := 0;
  inherited Create(True);
end;

destructor TShellChangeThread.Destroy;
begin
  CloseHandle(FDirectoryHandle);
  CloseHandle(FCompletionPort);
  DeleteCriticalSection(FCS);
  inherited Destroy;
end;

procedure TShellChangeThread.DoIOCompletionEvent;
var
  TempBuffer: TNotificationBuffer;
  FileOpNotification: PFileNotifyInformation;
  Offset: LongInt;
  FileName1, FileName2: string;
  FileOperation: TFileOperation;
  procedure DoDirChangeEvent;
  begin
    if Assigned(ChangeEvent) and FActived then
      ChangeEvent(FileOperation, FileName1, FileName2);
  end;
  function CompleteFileName(const FileName: string): string;
  begin
    Result := '';
    if Trim(FileName) <> '' then
      Result := FDirectory + FileName;
  end;
begin
  Lock;
  TempBuffer := FNotificationBuffer;
  FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
  Unlock;
  Pointer(FileOpNotification) := @TempBuffer[0];
  repeat
    with FileOpNotification^ do
    begin
      Offset := NextEntryOffset;
      FileName2 := '';
      case Action of
        FILE_ACTION_ADDED..FILE_ACTION_MODIFIED:
          begin
            FileName1 := CompleteFileName(WideCharLenToString(@FileName, FileNameLength div SizeOf(WideChar)));
            FileOperation := TFileOperation(Action - 1);
            DoDirChangeEvent;
          end;
        FILE_ACTION_RENAMED_OLD_NAME:
          begin
            FileName1 := CompleteFileName(WideCharLenToString(@FileName, FileNameLength div SizeOf(WideChar)));
            FileOperation := TFileOperation(Action - 1);
          end;
        FILE_ACTION_RENAMED_NEW_NAME:
          begin
            if FileOperation = foRenamed then
            begin
              FileName2 := CompleteFileName(WideCharLenToString(@FileName, FileNameLength div SizeOf(WideChar)));
              DoDirChangeEvent;
            end;
          end;
      end;
    end;
    Pointer(FileOpNotification) := Pointer(PChar(FileOpNotification) + OffSet);
  until Offset = 0;
end;

procedure TShellChangeThread.Execute;
var
  numBytes: DWORD;
  CompletionKey: ULONG_PTR;
  PFOverlapped: POverlapped;
  TempDirectoryHandle: Cardinal;
  TempCompletionPort: Cardinal;
begin
  Lock;
  TempDirectoryHandle := FDirectoryHandle;
  TempCompletionPort := FCompletionPort;
  Unlock;

  while not Terminated do
  begin
    if TempDirectoryHandle > 0 then
    begin
      PFOverlapped := @FOverlapped;
      Iocp.ApiFix.GetQueuedCompletionStatus(TempCompletionPort, numBytes, CompletionKey, PFOverlapped, INFINITE);
      if CompletionKey = Handle then
      begin
        Synchronize(DoIOCompletionEvent);
        FBytesWritten := 0;
        FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
        ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyOptionFlags, @FBytesWritten, @FOverlapped, nil);
      end;
    end;
  end;
  PostQueuedCompletionStatus(TempCompletionPort, 0, 0, nil);
end;

procedure TShellChangeThread.Lock;
begin
  EnterCriticalSection(FCS);
end;

function TShellChangeThread.ResetReadDirctory: Boolean;
var
  TempHandle: Cardinal;
  TempCompletionPort: Cardinal;
begin
  Result := False;
  CloseHandle(FDirectoryHandle);
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  CloseHandle(FCompletionPort);

  TempHandle := CreateFile(PChar(FDirectory), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
  Lock;
  FDirectoryHandle := TempHandle;
  Unlock;

  if (GetLastError = ERROR_FILE_NOT_FOUND) or (GetLastError = ERROR_PATH_NOT_FOUND) then
  begin
    Lock;
    FDirectoryHandle := 0;
    FCompletionPort := 0;
    Unlock;
    Exit;
  end;

  TempCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, Handle, 0);

  Lock;
  FCompletionPort := TempCompletionPort;
  Unlock;

  FBytesWritten := 0;
  FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
  Result := ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyOptionFlags, @FBytesWritten, @FOverlapped, nil);
end;

procedure TShellChangeThread.SetDirectoryOptions(Directory: string; Actived: Boolean;
  WatchSubTree: Boolean; NotifyOptionFlags: DWORD);
begin
  FWatchSubTree := WatchSubTree;
  FNotifyOptionFlags := NotifyOptionFlags;
  FDirectory := IncludeTrailingBackslash(Directory);
  FActived := Actived;
  ResetReadDirctory;
end;

procedure TShellChangeThread.Unlock;
begin
  LeaveCriticalSection(FCS);
end;

{ TFileSystemWatcher }

procedure TFileSystemWatcher.Change;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FThread) then
  begin
    FThread.SetDirectoryOptions(FWatchedDir, FActived, LongBool(FWatchSubTree), NotifyOptionFlags);
  end;
end;

constructor TFileSystemWatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActived := False;
  FWatchedDir := 'C:\';
  FFilters := [nfFilenameChange, nfDirNameChange];
  FWatchSubTree := True;
  FOnChange := nil;
end;

destructor TFileSystemWatcher.Destroy;
begin
  if Assigned(FThread) then
    FThread.Terminate;
  inherited Destroy;
end;

function TFileSystemWatcher.NotifyOptionFlags: DWORD;
begin
  Result := 0;
  if nfFileNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if nfDirNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if nfSizeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if nfAttributeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if nfWriteChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if nfAccessChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if nfCreationDateChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
  if nfSecurityChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
end;

procedure TFileSystemWatcher.SetActived(const Value: Boolean);
begin
  if FActived <> Value then
  begin
    FActived := Value;
    Change;
    if FActived then
      Start
    else
      Stop;
  end;
end;

procedure TFileSystemWatcher.SetFilters(const Value: TNotifyFilters);
begin
  if FFilters <> Value then
  begin
    FFilters := Value;
    Change;
  end;
end;

procedure TFileSystemWatcher.SetOnChange(const Value: TFileDealMethod);
begin
  FOnChange := Value;
  if Assigned(FOnChange) and FActived then
    Start
  else
    Stop;
  Change;
end;

procedure TFileSystemWatcher.SetWatchedDir(const Value: string);
begin
  if not SameText(FWatchedDir, Value) then
  begin
    FWatchedDir := Value;
    Change;
  end;
end;

procedure TFileSystemWatcher.SetWatchSubTree(const Value: Boolean);
begin
  if FWatchSubTree <> Value then
  begin
    FWatchSubTree := Value;
    Change;
  end;
end;

procedure TFileSystemWatcher.Start;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FOnChange) then
  begin
    FThread := TShellChangeThread.Create(FOnChange);
    FThread.SetDirectoryOptions(FWatchedDir, FActived, LongBool(FWatchSubTree), NotifyOptionFlags);
    FThread.Suspended := False;
  end;
end;

procedure TFileSystemWatcher.Stop;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

end.

