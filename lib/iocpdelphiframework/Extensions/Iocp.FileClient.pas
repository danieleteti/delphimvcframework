unit Iocp.FileClient;

interface

uses
  Windows, Messages, SysUtils, Classes, IoUtils, Math,
  Iocp.VariantPacket, Iocp.VariantSocket, Iocp.FileConst, Iocp.Utils, Iocp.Logger;

const
  BLOCK_SIZE = 16 * 1024;
  
type
  EFileException = class(Exception);

  TTransportMode = (tmAbort, tmOverwrite, tmResume);

  TClientGetFileRealInfoEvent = procedure(Sender: TObject; const LocalFile, RemoteFile: string; var LocalRealSize: Int64; var LocalRealTime: TDateTime) of object;
  TClientTransportBeginEvent = procedure(Sender: TObject; const LocalFile, RemoteFile: string;
    const LocalSize, RemoteSize, RealSize: Int64; const LocalTime, RemoteTime, RealTime: TDateTime; var Mode: TTransportMode) of object;
  TClientTransporttingEvent = procedure(Sender: TObject; const LocalFile, RemoteFile: string;
    const MaxSize, NowSize: Int64) of object;
  TClientTransportCompleteEvent = procedure(Sender: TObject; const LocalFile, RemoteFile: string) of object;

  TIocpFileClient = class(TIocpVariantClient)
  private
    FOnGetFileBegin: TClientTransportBeginEvent;
    FOnGetFileComplete: TClientTransportCompleteEvent;
    FOnGetFileTransport: TClientTransporttingEvent;
    FOnPutFileBegin: TClientTransportBeginEvent;
    FOnPutFileComplete: TClientTransportCompleteEvent;
    FOnPutFileTransport: TClientTransporttingEvent;
    FOnGetFileRealInfo: TClientGetFileRealInfoEvent;
    FFileRetry: Integer;

    function ProcessRetry(var ErrCount: Integer): Boolean;
  protected
    procedure TriggerGetFileComplete(const LocalFile, RemoteFile: string); virtual;
    procedure TriggerPutFileComplete(const LocalFile, RemoteFile: string; LocalTime: TDateTime); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetFile(const RemoteFile, LocalFile: string): Int64; overload;
    function GetFile(const RemoteFile: string; const Stream: TStream): Int64; overload;

    function PutFile(const LocalFile, RemoteFile: string): Int64; overload;
    function PutFile(const Stream: TStream; const RemoteFile: string): Int64; overload;
  published
    property TimeOut default 60000;

    // <0 无限重试、=0 不重试、>0 重试次数
    property FileRetry: Integer read FFileRetry write FFileRetry default -1;

    property OnGetFileRealInfo: TClientGetFileRealInfoEvent read FOnGetFileRealInfo write FOnGetFileRealInfo;
    property OnGetFileBegin: TClientTransportBeginEvent read FOnGetFileBegin write FOnGetFileBegin;
    property OnGetFileTransport: TClientTransporttingEvent read FOnGetFileTransport write FOnGetFileTransport;
    property OnGetFileComplete: TClientTransportCompleteEvent read FOnGetFileComplete write FOnGetFileComplete;

    property OnPutFileBegin: TClientTransportBeginEvent read FOnPutFileBegin write FOnPutFileBegin;
    property OnPutFileTransport: TClientTransporttingEvent read FOnPutFileTransport write FOnPutFileTransport;
    property OnPutFileComplete: TClientTransportCompleteEvent read FOnPutFileComplete write FOnPutFileComplete;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpFileClient]);
end;

{ TIocpFileClient }

constructor TIocpFileClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited TimeOut := 60000;
  FileRetry := -1;
end;

function TIocpFileClient.GetFile(const RemoteFile: string;
  const Stream: TStream): Int64;
var
  Mode: TTransportMode;
  LocalSize, RemoteSize, RemoteRealSize: Int64;
  LocalTime, RemoteTime, RemoteRealTime: TDateTime;
  Req, Res: TIocpVariantPacket;
  p, n: Int64;
  ErrCount: Integer;
begin
  Req := TIocpVariantPacket.Create('get_file_begin');
  Res := TIocpVariantPacket.Create;
  try
    try
      Req.Params['file_name'] := RemoteFile;
      if not SyncRequest(Req, Res) then
      begin
        Result := VFE_DISCONNECT;
        Exit;
      end;

      LocalSize := Stream.Size;
      LocalTime := 0;

      if Assigned(FOnGetFileRealInfo) then
        FOnGetFileRealInfo(Self, '', RemoteFile, LocalSize, LocalTime);

      RemoteSize := Res.Params['file_size'];
      RemoteTime := Res.Params['file_time'];
      if (RemoteSize < 0) then
      begin
        Result := VFE_NOT_EXISTS;
        Exit;
      end;

      RemoteRealSize := Res.Params['file_real_size'];
      RemoteRealTime := Res.Params['file_real_time'];
      if (RemoteSize > RemoteRealSize) then
      begin
        Result := VFE_CHECK_ERROR;
        Exit;
      end;
      if (RemoteSize < RemoteRealSize) then
      begin
        Result := VFE_INCOMPLETE_FILE;
        Exit;
      end;

      if (LocalSize = RemoteSize) then
        Mode := tmAbort
      else if (LocalSize < RemoteSize) and (LocalSize > 0) then
        Mode := tmResume
      else
        Mode := tmOverwrite;

      if Assigned(FOnPutFileBegin) then
        FOnGetFileBegin(Self, '', RemoteFile, LocalSize, RemoteSize, RemoteRealSize, LocalTime, RemoteTime, RemoteRealTime, Mode);

      if (Mode = tmAbort) then
      begin
        Result := RemoteSize;
        TriggerGetFileComplete('', RemoteFile);
        Exit;
      end
      else if (Mode = tmResume) then
        p := Max(LocalSize, 0)
      else // tmOverwrite
        p := 0;

      Req.Params.Clear;
      Req.Data.Clear;
      Req.Cmd := 'get_file';
      Req.Params['file_name'] := RemoteFile;

      Stream.Position := p;
      repeat
        Req.Params['file_pos'] := p;
        Req.Params['file_block_size'] := BLOCK_SIZE;
        if not SyncRequest(Req, Res) then
        begin
          Result := VFE_DISCONNECT;
          if ProcessRetry(ErrCount) then Exit
          else Continue;
        end;

        if not Res.Params['success'] then
        begin
          Result := VFE_SYSTEM_ERROR;
          if ProcessRetry(ErrCount) then Exit
          else Continue;
        end;

        Stream.Position := p;
        n := Stream.Write(Res.Data.Memory^, Res.Data.Size);
        if (n = 0) then
        begin
          Result := VFE_SYSTEM_ERROR;
          if ProcessRetry(ErrCount) then Exit
          else Continue;
        end;

        Inc(p, n);

        if Assigned(FOnGetFileTransport) then
          FOnGetFileTransport(Self, '', RemoteFile, RemoteSize, p);
      until (p >= RemoteSize);

      TriggerGetFileComplete('', RemoteFile);
      Result := p;
    except
      on e: Exception do
      begin
        Result := -1;
        AppendLog('%s.GetFile: %s=%s', [ClassName, e.ClassName, e.Message]);
      end;
    end;
  finally
    Req.Free;
    Res.Free;
  end;
end;

function TIocpFileClient.GetFile(const RemoteFile, LocalFile: string): Int64;
var
  Mode: TTransportMode;
  Stream: TFileStream;
  LocalSize, RemoteSize, RemoteRealSize: Int64;
  LocalTime, RemoteTime, RemoteRealTime: TDateTime;
  Req, Res: TIocpVariantPacket;
  p, n: Int64;
  ErrCount: Integer;
begin
  Req := TIocpVariantPacket.Create('get_file_begin');
  Res := TIocpVariantPacket.Create;
  try
    try
      Req.Params['file_name'] := RemoteFile;
      if not SyncRequest(Req, Res) then
      begin
        AppendLog('get_file_begin failed');
        Result := VFE_DISCONNECT;
        Exit;
      end;

      if FileExists(LocalFile) then
      begin
        try
          LocalTime := TFile.GetLastWriteTime(LocalFile);
          Stream := TFileStream.Create(LocalFile, fmOpenRead);
          LocalSize := Stream.Size;
          Stream.Free;
        except
          LocalSize := -1;
          LocalTime := 0;
        end;
      end else
      begin
        LocalSize := -1;
        LocalTime := 0;
      end;

      if Assigned(FOnGetFileRealInfo) then
        FOnGetFileRealInfo(Self, LocalFile, RemoteFile, LocalSize, LocalTime);

      RemoteSize := Res.Params['file_size'];
      RemoteTime := Res.Params['file_time'];
      if (RemoteSize < 0) then
      begin
        Result := VFE_NOT_EXISTS;
        Exit;
      end;

      RemoteRealSize := Res.Params['file_real_size'];
      RemoteRealTime := Res.Params['file_real_time'];
      if (RemoteSize > RemoteRealSize) then
      begin
        Result := VFE_CHECK_ERROR;
        Exit;
      end;
      if (RemoteSize < RemoteRealSize) then
      begin
        Result := VFE_INCOMPLETE_FILE;
        Exit;
      end;

      if (LocalSize = RemoteSize) and (LocalTime = RemoteTime) then
        Mode := tmAbort
      else if (LocalSize < RemoteSize) and (LocalSize > 0) and (LocalTime >= RemoteRealTime) then
        Mode := tmResume
      else
        Mode := tmOverwrite;

      if Assigned(FOnPutFileBegin) then
        FOnGetFileBegin(Self, LocalFile, RemoteFile, LocalSize, RemoteSize, RemoteRealSize, LocalTime, RemoteTime, RemoteRealTime, Mode);

      if (Mode = tmAbort) then
      begin
        Result := RemoteSize;
        TriggerGetFileComplete(LocalFile, RemoteFile);
        Exit;
      end
      else if (Mode = tmResume) then
        p := Max(LocalSize, 0)
      else // tmOverwrite
      begin
        p := 0;
        if FileExists(LocalFile) then
          DeleteFile(LocalFile)
      end;

      if FileExists(LocalFile) then
        Stream := TFileStream.Create(LocalFile, fmOpenWrite or fmShareExclusive)
      else
      begin
        ForceDirectories(ExtractFilePath(ExpandFileName(LocalFile)));
        Stream := TFileStream.Create(LocalFile, fmCreate);
      end;

      try
        Req.Params.Clear;
        Req.Data.Clear;
        Req.Cmd := 'get_file';
        Req.Params['file_name'] := RemoteFile;

        Stream.Position := p;
        repeat
          Req.Params['file_pos'] := p;
          Req.Params['file_block_size'] := Min(BLOCK_SIZE, RemoteSize - p);
//          Req.Params['file_block_size'] := Min(RandRange(4096, BLOCK_SIZE), RemoteSize - p);

          if not SyncRequest(Req, Res) then
          begin
            Result := VFE_DISCONNECT;
            if ProcessRetry(ErrCount) then Exit
            else Continue;
          end;

          if not Res.Params['success'] then
          begin
            Result := VFE_SYSTEM_ERROR;
            if ProcessRetry(ErrCount) then Exit
            else Continue;
          end;

          Stream.Position := p;
          n := Stream.Write(Res.Data.Memory^, Res.Data.Size);
          if (n = 0) then
          begin
            Result := VFE_SYSTEM_ERROR;
            if ProcessRetry(ErrCount) then Exit
            else Continue;
          end;
          Inc(p, n);

          if Assigned(FOnGetFileTransport) then
            FOnGetFileTransport(Self, LocalFile, RemoteFile, RemoteSize, p);
        until (p >= RemoteSize);

        TriggerGetFileComplete(LocalFile, RemoteFile);
        Result := p;
      finally
        Stream.Free;
      end;

      if (Result >= 0) then
        SetFileDate(LocalFile, RemoteTime);
    except
      on e: Exception do
      begin
        Result := -1;
        AppendLog('%s.GetFile err: %s=%s', [ClassName, e.ClassName, e.Message]);
      end;
    end;
  finally
    Req.Free;
    Res.Free;
  end;
end;

function TIocpFileClient.ProcessRetry(var ErrCount: Integer): Boolean;
begin
  Inc(ErrCount);
  if (FFileRetry > 0) then
    Result := (ErrCount > FFileRetry)
  else if (FFileRetry < 0) then
    Result := False
  else
    Result := True;

  if not Result then
    Sleep(RetryDelay);
end;


function TIocpFileClient.PutFile(const Stream: TStream;
  const RemoteFile: string): Int64;
var
  Mode: TTransportMode;
  LocalSize, RemoteSize, RemoteRealSize: Int64;
  LocalTime, RemoteTime, RemoteRealTime: TDateTime;
  Req, Res: TIocpVariantPacket;
  p, n: Int64;
  ErrCount: Integer;
begin
  Req := TIocpVariantPacket.Create('put_file_begin');
  Res := TIocpVariantPacket.Create;
  try
    try
      LocalSize := Stream.Size;
      LocalTime := 0;

      if Assigned(FOnGetFileRealInfo) then
        FOnGetFileRealInfo(Self, '', RemoteFile, LocalSize, LocalTime);

      Req.Params['file_name'] := RemoteFile;
      Req.Params['file_size'] := LocalSize;
      Req.Params['file_time'] := LocalTime;
      if not SyncRequest(Req, Res) then
      begin
        Result := VFE_DISCONNECT;
        Exit;
      end;

      RemoteSize := Res.Params['file_size'];
      RemoteTime := Res.Params['file_time'];
      RemoteRealSize := Res.Params['file_real_size'];
      RemoteRealTime := Res.Params['file_real_time'];

      if (LocalSize = RemoteSize) and (LocalTime = RemoteTime) then
        Mode := tmAbort
      else if (LocalSize = RemoteRealSize) and (LocalTime = RemoteRealTime) then
        Mode := tmResume
      else
        Mode := tmOverwrite;

      if Assigned(FOnPutFileBegin) then
        FOnPutFileBegin(Self, '', RemoteFile, LocalSize, RemoteSize, RemoteRealSize, LocalTime, RemoteTime, RemoteRealTime, Mode);

      if (Mode = tmAbort) then
      begin
        Result := LocalSize;
        TriggerPutFileComplete('', RemoteFile, LocalTime);
        Exit;
      end
      else if (Mode = tmResume) then
        p := Max(RemoteSize, 0)
      else // tmOverwrite
        p := 0;

      Req.Params.Clear;
      Req.Data.Clear;
      Req.Cmd := 'put_file';
      Req.Params['file_name'] := RemoteFile;

      Stream.Position := p;
      repeat
        Req.Data.SetSize(BLOCK_SIZE);
        n := Stream.Read(Req.Data.Memory^, BLOCK_SIZE);
        Req.Data.SetSize(n);
        Req.Params['file_pos'] := p;
        Req.Params['file_block_size'] := n;
        if not SyncRequest(Req, Res) then
        begin
          Result := VFE_DISCONNECT;
          if ProcessRetry(ErrCount) then Exit
          else Continue;
        end;

        if not Res.Params['success'] then
        begin
          Result := VFE_SYSTEM_ERROR;
          if ProcessRetry(ErrCount) then Exit
          else Continue;
        end;

        Inc(p, n);

        if Assigned(FOnPutFileTransport) then
          FOnPutFileTransport(Self, '', RemoteFile, LocalSize, p);
      until (p >= LocalSize);

      TriggerPutFileComplete('', RemoteFile, LocalTime);
      Result := p;
    except
      on e: Exception do
      begin
        Result := -1;
        AppendLog('%s.PutFile: %s=%s', [ClassName, e.ClassName, e.Message]);
      end;
    end;
  finally
    Req.Free;
    Res.Free;
  end;
end;

procedure TIocpFileClient.TriggerGetFileComplete(const LocalFile,
  RemoteFile: string);
var
  Req: TIocpVariantPacket;
begin
  Req := TIocpVariantPacket.Create('get_file_complete');
  try
    Req.Params['file_name'] := RemoteFile;
    AsyncRequest(Req);

    if Assigned(FOnGetFileComplete) then
      FOnGetFileComplete(Self, LocalFile, RemoteFile);
  finally
    Req.Free;
  end;
end;

procedure TIocpFileClient.TriggerPutFileComplete(const LocalFile,
  RemoteFile: string; LocalTime: TDateTime);
var
  Req: TIocpVariantPacket;
begin
  Req := TIocpVariantPacket.Create('put_file_complete');
  try
    Req.Params['file_name'] := RemoteFile;
    Req.Params['file_time'] := LocalTime;
    AsyncRequest(Req);

    if Assigned(FOnPutFileComplete) then
      FOnPutFileComplete(Self, LocalFile, RemoteFile);
  finally
    Req.Free;
  end;
end;

function TIocpFileClient.PutFile(const LocalFile, RemoteFile: string): Int64;
var
  Mode: TTransportMode;
  Stream: TFileStream;
  LocalSize, RemoteSize, RemoteRealSize: Int64;
  LocalTime, RemoteTime, RemoteRealTime: TDateTime;
  Req, Res: TIocpVariantPacket;
  p, n: Int64;
  ErrCount: Integer;
begin
  Req := TIocpVariantPacket.Create('put_file_begin');
  Res := TIocpVariantPacket.Create;
  try
    if not FileExists(LocalFile) then
    begin
      Result := VFE_NOT_EXISTS;
      Exit;
    end;

    try
      LocalTime := TFile.GetLastWriteTime(LocalFile);
      Stream := TFileStream.Create(LocalFile, fmOpenRead);
      LocalSize := Stream.Size;
      Stream.Free;
    except
      Result := VFE_NOT_EXISTS;
      Exit;
    end;

    if Assigned(FOnGetFileRealInfo) then
      FOnGetFileRealInfo(Self, LocalFile, RemoteFile, LocalSize, LocalTime);

    Req.Params['file_name'] := RemoteFile;
    Req.Params['file_size'] := LocalSize;
    Req.Params['file_time'] := LocalTime;
    if not SyncRequest(Req, Res) then
    begin
      Result := VFE_DISCONNECT;
      Exit;
    end;

    RemoteSize := Res.Params['file_size'];
    RemoteTime := Res.Params['file_time'];
    RemoteRealSize := Res.Params['file_real_size'];
    RemoteRealTime := Res.Params['file_real_time'];

    if (LocalSize = RemoteSize) and (LocalTime = RemoteTime) then
      Mode := tmAbort
    else if (LocalSize = RemoteRealSize) and (LocalTime = RemoteRealTime) then
      Mode := tmResume
    else
      Mode := tmOverwrite;

    if Assigned(FOnPutFileBegin) then
      FOnPutFileBegin(Self, '', RemoteFile, LocalSize, RemoteSize, RemoteRealSize, LocalTime, RemoteTime, RemoteRealTime, Mode);

    if (Mode = tmAbort) then
    begin
      Result := LocalSize;
      TriggerPutFileComplete(LocalFile, RemoteFile, LocalTime);
      Exit;
    end
    else if (Mode = tmResume) then
      p := Max(RemoteSize, 0)
    else // tmOverwrite
      p := 0;

    Stream := TFileStream.Create(LocalFile, fmOpenRead or fmShareDenyWrite);
    try
      try
        Req.Params.Clear;
        Req.Data.Clear;
        Req.Cmd := 'put_file';
        Req.Params['file_name'] := RemoteFile;

        Stream.Position := p;
        repeat
          Req.Data.SetSize(BLOCK_SIZE);
          n := Stream.Read(Req.Data.Memory^, BLOCK_SIZE);
          {if (n <= 0) then
          begin
            AppendLog('put_file error: ' + SysErrorMessage(GetLastError));
          end;}
          Req.Data.SetSize(n);
          Req.Params['file_pos'] := p;
          Req.Params['file_block_size'] := n;
          if not SyncRequest(Req, Res) then
          begin
            Result := VFE_DISCONNECT;
            if ProcessRetry(ErrCount) then Exit
            else Continue;
          end;

          if not Res.Params['success'] then
          begin
            Result := VFE_SYSTEM_ERROR;
            if ProcessRetry(ErrCount) then Exit
            else Continue;
          end;

          Inc(p, n);

          if Assigned(FOnPutFileTransport) then
            FOnPutFileTransport(Self, LocalFile, RemoteFile, LocalSize, p);
        until (p >= LocalSize);

        TriggerPutFileComplete(LocalFile, RemoteFile, LocalTime);
        Result := p;
      except
        on e: Exception do
        begin
          Result := -1;
          AppendLog('%s.PutFile: %s=%s', [ClassName, e.ClassName, e.Message]);
        end;
      end;
    finally
      Stream.Free;
    end;
  finally
    Req.Free;
    Res.Free;
  end;
end;

end.
