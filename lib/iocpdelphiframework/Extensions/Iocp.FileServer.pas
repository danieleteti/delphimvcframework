unit Iocp.FileServer;

//{$define __DBG_VANI_FILE__}

interface

uses
  Windows, Messages, SysUtils, Classes, IoUtils,
  Iocp.VariantPacket, Iocp.VariantSocket, Iocp.FileConst, Iocp.Logger, Iocp.Utils;

type
  TServerTransorttingEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; const FileName: string; const FilePos, BlockSize: Int64; Stream: TMemoryStream) of object;
  TServerTransportFileMapEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; var FileName: string) of object;
  TServerGetFileRealInfoEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; const FileName: string; var RealSize: Int64; var RealTime: TDateTime) of object;
  TServerTransportBeginEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; const FileName: string) of object;
  TServerTransportCompleteEvent = procedure(Sender: TObject; Client: TIocpVariantServerConnection; const FileName: string) of object;

  TIocpFileServer = class(TIocpVariantServer)
  private
    FOnPutFileTransport: TServerTransorttingEvent;
    FOnGetFileTransport: TServerTransorttingEvent;
    FOnFileMap: TServerTransportFileMapEvent;
    FOnGetFileRealInfo: TServerGetFileRealInfoEvent;
    FOnPutFileBegin: TServerTransportBeginEvent;
    FOnPutFileComplete: TServerTransportCompleteEvent;
    FOnGetFileBegin: TServerTransportBeginEvent;
    FOnGetFileComplete: TServerTransportCompleteEvent;
  protected
    procedure DoOnRequest(Client: TIocpVariantServerConnection; Request, Response: TIocpVariantPacket); override;
  published
    // 开始传输文件时用于确认文件名
    property OnFileMap: TServerTransportFileMapEvent read FOnFileMap write FOnFileMap;

    // 填充文件的尺寸及时间信息，在get和put开始的时候会被触发，可以在这个事件里保存每个文件的信息
    property OnGetFileRealInfo: TServerGetFileRealInfoEvent read FOnGetFileRealInfo write FOnGetFileRealInfo;

    // 传输数据事件，可以不提供（不提供该事件则由服务器自动完成数据的读写）
    property OnGetFileBegin: TServerTransportBeginEvent read FOnGetFileBegin write FOnGetFileBegin;
    property OnGetFileTransport: TServerTransorttingEvent read FOnGetFileTransport write FOnGetFileTransport;
    property OnGetFileComplete: TServerTransportCompleteEvent read FOnGetFileComplete write FOnGetFileComplete;

    property OnPutFileBegin: TServerTransportBeginEvent read FOnPutFileBegin write FOnPutFileBegin;
    property OnPutFileTransport: TServerTransorttingEvent read FOnPutFileTransport write FOnPutFileTransport;
    property OnPutFileComplete: TServerTransportCompleteEvent read FOnPutFileComplete write FOnPutFileComplete;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpFileServer]);
end;

{ TIocpFileServer }

procedure TIocpFileServer.DoOnRequest(Client: TIocpVariantServerConnection; Request, Response: TIocpVariantPacket);
var
  FileName: string;
  FileSize, RealSize: Int64;
  FileTime, RealTime: TDateTime;
  FilePos, BlockSize: Int64;
  Stream: TFileStream;
begin
  try
    if (Request.Cmd = 'get_file_begin') then
    begin
      FileName := Request.Params['file_name'];

      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      try
        FileTime := TFile.GetLastWriteTime(FileName);
        Stream := TFileStream.Create(FileName, fmOpenRead);
        FileSize := Stream.Size;
        Stream.Free;
      except
        FileTime := 0;
        FileSize := -1;
      end;

      {$ifdef __DBG_VANI_FILE__}
      AppendLog(Format('get_file_begin[%s], filesize=%d, file_time=%s',
        [FileName, FileSize, FormatDateTime('YYYY-MM-DD HH:NN:SS', FileTime)]));
      {$endif}
        
      RealSize := FileSize;
      RealTime := FileTime;
      if Assigned(FOnGetFileRealInfo) then
        FOnGetFileRealInfo(Self, Client, FileName, RealSize, RealTime);

      Response.Params['file_size'] := FileSize;
      Response.Params['file_time'] := FileTime;
      Response.Params['file_real_size'] := RealSize;
      Response.Params['file_real_time'] := RealTime;

      if Assigned(FOnGetFileBegin) then
        FOnGetFileBegin(Self, Client, FileName);
    end else
    if (Request.Cmd = 'get_file_complete') then
    begin
      FileName := Request.Params['file_name'];

      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      if Assigned(FOnGetFileComplete) then
        FOnGetFileComplete(Self, Client, FileName);
    end else
    if (Request.Cmd = 'get_file') then
    begin
      FileName := Request.Params['file_name'];
      FilePos := Request.Params['file_pos'];
      BlockSize := Request.Params['file_block_size'];

      {$ifdef __DBG_VANI_FILE__}
      AppendLog(Format('start get block[%s], filepos=%d, blocksize=%d',
        [FileName, FilePos, BlockSize]));
      {$endif}

      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      if Assigned(FOnGetFileTransport) then
        FOnGetFileTransport(Self, Client, FileName, FilePos, BlockSize, Response.Data)
      else
      begin
        try
          Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
          try
            Stream.Position := FilePos;
            Response.Data.SetSize(BlockSize);
            BlockSize := Stream.Read(Response.Data.Memory^, BlockSize);
            Response.Data.SetSize(BlockSize);
            Response.Params['success'] := True;
            {$ifdef __DBG_VANI_FILE__}
            AppendLog(Format('end get block[%s], filepos=%d, blocksize=%d',
              [FileName, FilePos, BlockSize]));
            {$endif}
          finally
            Stream.Free;
          end;
        except
          on e: Exception do
          begin
            Response.Params['success'] := False;
            Response.Params['error'] := e.ClassName + ': ' + e.Message;
          end;
        end;
      end;
    end else
    if (Request.Cmd = 'put_file_begin') then
    begin
      FileName := Request.Params['file_name'];

      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      try
        FileTime := TFile.GetLastWriteTime(FileName);
        Stream := TFileStream.Create(FileName, fmOpenRead);
        FileSize := Stream.Size;
        Stream.Free;
      except
        FileTime := 0;
        FileSize := -1;
      end;
      RealSize := FileSize;
      RealTime := FileTime;

      if Assigned(FOnGetFileRealInfo) then
        FOnGetFileRealInfo(Self, Client, FileName, RealSize, RealTime);

      Response.Params['file_size'] := FileSize;
      Response.Params['file_time'] := FileTime;
      Response.Params['file_real_size'] := RealSize;
      Response.Params['file_real_time'] := RealTime;

      if Assigned(FOnPutFileBegin) then
        FOnPutFileBegin(Self, Client, FileName);
    end else
    if (Request.Cmd = 'put_file_complete') then
    begin
      FileName := Request.Params['file_name'];
      FileTime := Request.Params['file_time'];

      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      if (FileTime > 0) then
        SetFileDate(FileName, FileTime);

      if Assigned(FOnPutFileComplete) then
        FOnPutFileComplete(Self, Client, FileName);
    end else
    if (Request.Cmd = 'put_file') then
    begin
      FileName := Request.Params['file_name'];
      FilePos := Request.Params['file_pos'];
      BlockSize := Request.Params['file_block_size'];

      {$ifdef __DBG_VANI_FILE__}
      AppendLog(Format('start put block[%s], filepos=%d, blocksize=%d',
        [FileName, FilePos, BlockSize]));
      {$endif}
      if Assigned(FOnFileMap) then
        FOnFileMap(Self, Client, FileName);

      FileName := ExpandFullFileName(FileName);

      if Assigned(FOnPutFileTransport) then
        FOnPutFileTransport(Self, Client, FileName, FilePos, BlockSize, Request.Data)
      else
      begin
        try
          {$ifdef __DBG_VANI_FILE__}
          AppendLog(Format('put opening file %s', [FileName]));
          {$endif}
          if (FilePos = 0) then
            DeleteFile(FileName);
          if FileExists(FileName) then
            Stream := TFileStream.Create(FileName, fmOpenWrite or fmShareExclusive)
          else
          begin
            ForceDirectories(ExtractFilePath(FileName));
            Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
          end;
          {$ifdef __DBG_VANI_FILE__}
          AppendLog(Format('put opened file %s', [FileName]));
          {$endif}
          try
            Stream.Position := FilePos;
            Stream.Write(Request.Data.Memory^, Request.Data.Size);
            Response.Params['success'] := True;
            {$ifdef __DBG_VANI_FILE__}
            AppendLog(Format('end put block[%s], filepos=%d, blocksize=%d',
              [FileName, FilePos, BlockSize]));
            {$endif}
          finally
            Stream.Free;
          end;
        except
          on e: Exception do
          begin
            Response.Params['success'] := False;
            Response.Params['error'] := e.ClassName + ': ' + e.Message;
          end;
        end;
      end;
    end else
      inherited DoOnRequest(Client, Request, Response);
  except
    on e: Exception do
      AppendLog('%s.DoOnRequest: %s:%s:%s', [ClassName, Request.Cmd, e.ClassName, e.Message]);
  end;
end;

end.
