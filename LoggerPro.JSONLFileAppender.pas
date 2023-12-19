unit LoggerPro.JSONLFileAppender;

{ <@abstract(The unit to include if you want to use @link(TLoggerProJSONLFileAppender))
  @author(Daniele Teti) }

{$DEFINE USE_JDO}


interface

uses
  LoggerPro.FileAppender,
  System.Classes,
  LoggerPro,
  System.SysUtils;

type
  { @abstract(JSONL file appender with multiple tags)
    This file appender writes all TLogItems into a single log file as JSONL
  }
  TLoggerProJSONLFileAppender = class(TLoggerProSimpleFileAppender)
  protected
    function GetLogFileName(const aTag: string; const aFileNumber: Integer): string; override;
    procedure EmitStartRotateLogItem(aWriter: TStreamWriter); override;
    procedure EmitEndRotateLogItem(aWriter: TStreamWriter); override;
  public
    function FormatLog(const ALogItem: TLogItem): string; override;
    constructor Create(aMaxBackupFileCount: Integer = TLoggerProFileAppender.DEFAULT_MAX_BACKUP_FILE_COUNT;
      aMaxFileSizeInKiloByte: Integer = TLoggerProFileAppender.DEFAULT_MAX_FILE_SIZE_KB; aLogsFolder: string = ''; aFileAppenderOptions: TFileAppenderOptions = [];
      aLogFileNameFormat: string = TLoggerProSimpleFileAppender.DEFAULT_FILENAME_FORMAT; aEncoding: TEncoding = nil);
      reintroduce;
  end;

implementation


uses
  System.IOUtils,
{$IF Defined(USE_JDO)}
  JsonDataObjects
{$ELSE}
  System.JSON
{$ENDIF}
;


{ TLoggerProJSONLFileAppender }

constructor TLoggerProJSONLFileAppender.Create(
  aMaxBackupFileCount, aMaxFileSizeInKiloByte: Integer;
  aLogsFolder: string;
  aFileAppenderOptions: TFileAppenderOptions;
  aLogFileNameFormat: string;
  aEncoding: TEncoding);
begin
  inherited Create(aMaxBackupFileCount, aMaxFileSizeInKiloByte,
    aLogsFolder, aFileAppenderOptions, aLogFileNameFormat,
    DEFAULT_LOG_FORMAT, aEncoding);
end;

procedure TLoggerProJSONLFileAppender.EmitEndRotateLogItem(aWriter: TStreamWriter);
begin
  // do nothing
end;

procedure TLoggerProJSONLFileAppender.EmitStartRotateLogItem(aWriter: TStreamWriter);
begin
  // do nothing
end;

function TLoggerProJSONLFileAppender.FormatLog(const ALogItem: TLogItem): string;
var
  lJSON: TJsonObject;
begin
  lJSON := TJSONObject.Create;
  try
    {$IF Defined(USE_JDO)}
    lJSON.S['type'] := ALogItem.LogTypeAsString;
    lJSON.S['message'] := ALogItem.LogMessage;
    lJSON.S['tag'] := ALogItem.LogTag;
    lJSON.S['ts'] := DateTimeToStr(ALogItem.TimeStamp, Self.FormatSettings).TrimRight;
    lJSON.I['tid'] := ALogItem.ThreadID;
    {$ELSE}
    lJSON.AddPair('type', ALogItem.LogTypeAsString);
    lJSON.AddPair('message', ALogItem.LogMessage);
    lJSON.AddPair('tag', ALogItem.LogTag);
    lJSON.AddPair('ts', DateTimeToStr(ALogItem.TimeStamp, Self.FormatSettings).TrimRight);
    lJSON.AddPair('tid', ALogItem.ThreadID);
    {$ENDIF}
    Result := lJSON.ToJSON;
    if Assigned(OnLogRow) then
      OnLogRow(ALogItem, Result)
  finally
    lJSON.Free;
  end;
end;

function TLoggerProJSONLFileAppender.GetLogFileName(const aTag: string; const aFileNumber: Integer): string;
var
  lOrigFName, lOrigExt: string;
begin
  lOrigFName := inherited;
  lOrigExt := TPath.GetExtension(lOrigFName);
  if lOrigExt.IsEmpty then
  begin
    lOrigExt := '.log';
  end;
  Result := TPath.ChangeExtension(lOrigFName, '.jsonl' + lOrigExt);
end;



end.
