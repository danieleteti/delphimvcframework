unit LoggerPro.FileAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProFileAppender))
  @author(Daniele Teti) }

{$IF Defined(Android) or Defined(iOS)}
{$DEFINE MOBILE}
{$ENDIF}

interface

uses
  LoggerPro,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

type
  {
    @abstract(Logs to file using one different file for each different TAG used.)
    @author(Daniele Teti - d.teti@bittime.it)
    Implements log rotations.
    This appender is the default appender when no configuration is done on the @link(TLogger) class.

    Without any configuration LoggerPro uses the @link(TLoggerProFileAppender) with the default configuration.

    So the following two blocks of code are equivalent:

    @longcode(#
    ...
    TLogger.Initialize; //=> uses the TLoggerProFileAppender because no other configuration is provided
    ...

    ...
    TLogger.AddAppender(TLoggerProFileAppender.Create);
    TLogger.Initialize //=> uses the TLoggerProFileAppender as configured
    ...
    #)

  }

  TFileAppenderOption = (IncludePID);
  TFileAppenderOptions = set of TFileAppenderOption;
  TFileAppenderLogRow = reference to procedure(const LogItem: TLogItem; out LogRow: string);

  { @abstract(The default file appender)
    To learn how to use this appender, check the sample @code(file_appender.dproj)
  }
  TLoggerProFileAppender = class(TLoggerProAppenderBase)
  private
    fFormatSettings: TFormatSettings;
    fWritersDictionary: TObjectDictionary<string, TStreamWriter>;
    fMaxBackupFileCount: Integer;
    fMaxFileSizeInKiloByte: Integer;
    fLogFormat: string;
    fLogFileNameFormat: string;
    fFileAppenderOptions: TFileAppenderOptions;
    fLogsFolder: string;
    fEncoding: TEncoding;
    fOnLogRow: TFileAppenderLogRow;
    function CreateWriter(const aFileName: string): TStreamWriter;
    procedure AddWriter(const aLogItem: TLogItem; var lWriter: TStreamWriter; var lLogFileName: string);
    procedure RotateLog(const aLogItem: TLogItem; lWriter: TStreamWriter);
    procedure RetryMove(const aFileSrc, aFileDest: string);
  protected
    function GetLogFileName(const aTag: string; const aFileNumber: Integer): string;
    procedure InternalWriteLog(const aStreamWriter: TStreamWriter; const aValue: string); inline;
  public const
    { @abstract(Defines the default format string used by the @link(TLoggerProFileAppender).)
      The positional parameters are the followings:
      @orderedList(
      @itemSetNumber 0
      @item TimeStamp
      @item ThreadID
      @item LogType
      @item LogMessage
      @item LogTag
      )
    }
    DEFAULT_LOG_FORMAT = '%0:s [TID %1:10u][%2:-8s] %3:s [%4:s]';
    { @abstract(Defines the default format string used by the @link(TLoggerProFileAppender).)
      The positional parameters are the followings:
      @orderedList(
      @item SetNumber 0
      @item ModuleName
      @item LogNum
      @item LogTag
      )
    }
    DEFAULT_FILENAME_FORMAT = '%s.%2.2d.%s.log';
    { @abstract(Defines number of log file set to mantain during logs rotation) }
    DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
    { @abstract(Defines the max size of each log file)
      The actual meaning is: "If the file size is > than @link(DEFAULT_MAX_FILE_SIZE_KB) then rotate logs. }
    DEFAULT_MAX_FILE_SIZE_KB = 1000;
    { @abstract(Milliseconds to wait between the RETRY_COUNT times. }
    RETRY_DELAY = 200;
    { @abstract(How much times we have to retry if the file is locked?. }
    RETRY_COUNT = 5;
    constructor Create(aMaxBackupFileCount: Integer = DEFAULT_MAX_BACKUP_FILE_COUNT;
      aMaxFileSizeInKiloByte: Integer = DEFAULT_MAX_FILE_SIZE_KB; aLogsFolder: string = ''; aFileAppenderOptions: TFileAppenderOptions = [];
      aLogFileNameFormat: string = DEFAULT_FILENAME_FORMAT; aLogFormat: string = DEFAULT_LOG_FORMAT; aEncoding: TEncoding = nil);
      reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); overload; override;
    property OnLogRow: TFileAppenderLogRow read fOnLogRow write fOnLogRow;
  end;

implementation

uses
  System.IOUtils,
  idGlobal
{$IF Defined(Android)}
    ,Androidapi.Helpers
    ,Androidapi.JNI.GraphicsContentViewText
    ,Androidapi.JNI.JavaTypes
{$ENDIF}
    ;

{ TLoggerProFileAppender }

function TLoggerProFileAppender.GetLogFileName(const aTag: string; const aFileNumber: Integer): string;
var
  lExt: string;
  lModuleName: string;
  lPath: string;
  lFormat: string;
begin
{$IF Defined(Android)}
  lModuleName := TAndroidHelper.ApplicationTitle.Replace(' ', '_', [rfReplaceAll]);
{$ENDIF}
{$IF not Defined(Mobile)}
  lModuleName := TPath.GetFileNameWithoutExtension(GetModuleName(HInstance));
{$ENDIF}
{$IF Defined(IOS)}
  raise Exception.Create('Platform not supported');
{$ENDIF}
  lFormat := fLogFileNameFormat;

  if TFileAppenderOption.IncludePID in fFileAppenderOptions then
    lModuleName := lModuleName + '_pid_' + IntToStr(CurrentProcessId).PadLeft(6, '0');

  lPath := fLogsFolder;
  lExt := Format(lFormat, [lModuleName, aFileNumber, aTag]);
  Result := TPath.Combine(lPath, lExt);
end;

procedure TLoggerProFileAppender.Setup;
begin
  if fLogsFolder = '' then
  begin
{$IF (Defined(MSWINDOWS) or Defined(POSIX)) and (not Defined(MOBILE))}
    fLogsFolder := TPath.GetDirectoryName(GetModuleName(HInstance));
{$ENDIF}
{$IF Defined(Android) or Defined(IOS)}
    fLogsFolder := TPath.GetSharedDocumentsPath();
{$ENDIF}
  end;
  if not TDirectory.Exists(fLogsFolder) then
    TDirectory.CreateDirectory(fLogsFolder);

  fFormatSettings := LoggerPro.GetDefaultFormatSettings;
  fWritersDictionary := TObjectDictionary<string, TStreamWriter>.Create([doOwnsValues]);
end;

procedure TLoggerProFileAppender.TearDown;
begin
  fWritersDictionary.Free;
end;

procedure TLoggerProFileAppender.InternalWriteLog(const aStreamWriter: TStreamWriter; const aValue: string);
begin
  aStreamWriter.WriteLine(aValue);
  aStreamWriter.Flush;
end;

procedure TLoggerProFileAppender.WriteLog(const aLogItem: TLogItem);
var
  lWriter: TStreamWriter;
  lLogFileName: string;
  lLogRow: string;
begin
  if not fWritersDictionary.TryGetValue(aLogItem.LogTag, lWriter) then
  begin
    AddWriter(aLogItem, lWriter, lLogFileName);
  end;

  if not Assigned(fOnLogRow) then
  begin
    InternalWriteLog(lWriter, Format(fLogFormat, [datetimetostr(aLogItem.TimeStamp, fFormatSettings), aLogItem.ThreadID,
      aLogItem.LogTypeAsString, aLogItem.LogMessage, aLogItem.LogTag]));
  end
  else
  begin
    fOnLogRow(aLogItem, lLogRow);
    InternalWriteLog(lWriter, lLogRow);
  end;

  if lWriter.BaseStream.Size > fMaxFileSizeInKiloByte * 1024 then
  begin
    RotateLog(aLogItem, lWriter);
  end;
end;

procedure TLoggerProFileAppender.RetryMove(const aFileSrc, aFileDest: string);
var
  lRetries: Integer;
const
  MAX_RETRIES = 5;
begin
  lRetries := 0;
  repeat
    try
      Sleep(50);
      // the incidence of "Locked file goes to nearly zero..."
      TFile.Move(aFileSrc, aFileDest);
      Break;
    except
      on E: EInOutError do
      begin
        Inc(lRetries);
        Sleep(50);
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
  until lRetries = MAX_RETRIES;

  if lRetries = MAX_RETRIES then
    raise ELoggerPro.CreateFmt('Cannot rename %s to %s', [aFileSrc, aFileDest]);
end;

procedure TLoggerProFileAppender.RotateLog(const aLogItem: TLogItem; lWriter: TStreamWriter);
var
  lLogFileName: string;
  lRenamedFile: string;
  I: Integer;
  lCurrentFileName: string;
begin
  InternalWriteLog(lWriter, '#[ROTATE LOG ' + datetimetostr(Now, fFormatSettings) + ']');
  fWritersDictionary.Remove(aLogItem.LogTag);
  lLogFileName := GetLogFileName(aLogItem.LogTag, 0);
  // remove the last file of backup set
  lRenamedFile := GetLogFileName(aLogItem.LogTag, fMaxBackupFileCount);
  if TFile.Exists(lRenamedFile) then
    TFile.Delete(lRenamedFile);
  // shift the files names
  for I := fMaxBackupFileCount - 1 downto 1 do
  begin
    lCurrentFileName := GetLogFileName(aLogItem.LogTag, I);
    lRenamedFile := GetLogFileName(aLogItem.LogTag, I + 1);
    if TFile.Exists(lCurrentFileName) then
      RetryMove(lCurrentFileName, lRenamedFile);

  end;
  lRenamedFile := GetLogFileName(aLogItem.LogTag, 1);
  RetryMove(lLogFileName, lRenamedFile);
  // read the writer
  AddWriter(aLogItem, lWriter, lLogFileName);
  InternalWriteLog(lWriter, '#[START LOG ' + datetimetostr(Now, fFormatSettings) + ']');
end;

procedure TLoggerProFileAppender.AddWriter(const aLogItem: TLogItem; var lWriter: TStreamWriter; var lLogFileName: string);
begin
  lLogFileName := GetLogFileName(aLogItem.LogTag, 0);
  lWriter := CreateWriter(lLogFileName);
  fWritersDictionary.Add(aLogItem.LogTag, lWriter);
end;

constructor TLoggerProFileAppender.Create(aMaxBackupFileCount: Integer; aMaxFileSizeInKiloByte: Integer; aLogsFolder: string;
  aFileAppenderOptions: TFileAppenderOptions; aLogFileNameFormat: string; aLogFormat: string; aEncoding: TEncoding);
begin
  inherited Create;
  fLogsFolder := aLogsFolder;
  fMaxBackupFileCount := aMaxBackupFileCount;
  fMaxFileSizeInKiloByte := aMaxFileSizeInKiloByte;
  fLogFormat := aLogFormat;
  fLogFileNameFormat := aLogFileNameFormat;
  fFileAppenderOptions := aFileAppenderOptions;
  if Assigned(aEncoding) then
    fEncoding := aEncoding
  else
    fEncoding := TEncoding.DEFAULT;
end;

function TLoggerProFileAppender.CreateWriter(const aFileName: string): TStreamWriter;
var
  lFileStream: TFileStream;
  lFileAccessMode: Word;
  lRetries: Integer;
begin
  lFileAccessMode := fmOpenWrite or fmShareDenyNone;
  if not TFile.Exists(aFileName) then
    lFileAccessMode := lFileAccessMode or fmCreate;

  // If the file si still blocked by a precedent execution or
  // for some other reasons, we try to access the file for 5 times.
  // If after 5 times (with a bit of delay in between) the file is still
  // locked, then the exception is raised.
  lRetries := 0;
  while true do
  begin
    try
      lFileStream := TFileStream.Create(aFileName, lFileAccessMode);
      try
        lFileStream.Seek(0, TSeekOrigin.soEnd);
        Result := TStreamWriter.Create(lFileStream, fEncoding, 32);
        Result.AutoFlush := true;
        Result.OwnStream;
        Break;
      except
        lFileStream.Free;
        raise;
      end;
    except
      if lRetries = RETRY_COUNT then
      begin
        raise;
      end
      else
      begin
        Inc(lRetries);
        Sleep(RETRY_DELAY); // just wait a little bit
      end;
    end;
  end;
end;

end.

