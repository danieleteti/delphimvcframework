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

  { @abstract(The base class for different file appenders)
    Do not use this class directly, but one of TLoggerProFileAppender or TLoggerProSimpleFileAppender.
    Check the sample @code(file_appender.dproj)
  }
  TLoggerProFileAppenderBase = class(TLoggerProAppenderBase)
  private
    fMaxBackupFileCount: Integer;
    fMaxFileSizeInKiloByte: Integer;
    fLogFileNameFormat: string;
    fFileAppenderOptions: TFileAppenderOptions;
    fLogsFolder: string;
    fEncoding: TEncoding;
    function CreateWriter(const aFileName: string): TStreamWriter;
    procedure RetryMove(const aFileSrc, aFileDest: string);
  protected
    function GetLogFileName(const aTag: string; const aFileNumber: Integer): string;
    procedure WriteToStream(const aStreamWriter: TStreamWriter; const aValue: string); inline;
    procedure RotateFile(const aLogTag: string; out aNewFileName: string); virtual;
    procedure InternalWriteLog(const aStreamWriter: TStreamWriter; const aLogItem: TLogItem);
  public const
    { @abstract(Defines the default format string used by the @link(TLoggerProFileAppender).)
      The positional parameters are the following:
      @orderedList(
      @item SetNumber 0
      @item ModuleName
      @item LogNum
      @item LogTag
      )
    }
    DEFAULT_FILENAME_FORMAT = '%0:s.%1:2.2d.%2:s.log';
    { @abstract(Defines number of log file set to maintain during logs rotation) }
    DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
    { @abstract(Defines the max size of each log file)
      The actual meaning is: "If the file size is > than @link(DEFAULT_MAX_FILE_SIZE_KB) then rotate logs. }
    DEFAULT_MAX_FILE_SIZE_KB = 1000;
    { @abstract(Milliseconds to wait between the RETRY_COUNT times. }
    RETRY_DELAY = 200;
    { @abstract(How many times do we have to retry if the file is locked?. }
    RETRY_COUNT = 5;
    constructor Create(aMaxBackupFileCount: Integer = DEFAULT_MAX_BACKUP_FILE_COUNT;
      aMaxFileSizeInKiloByte: Integer = DEFAULT_MAX_FILE_SIZE_KB; aLogsFolder: string = ''; aFileAppenderOptions: TFileAppenderOptions = [];
      aLogFileNameFormat: string = DEFAULT_FILENAME_FORMAT; aLogFormat: string = DEFAULT_LOG_FORMAT; aEncoding: TEncoding = nil);
      reintroduce;
    procedure Setup; override;
  end;

  { @abstract(The default file appender)
    This file appender separates TLogItems with different tags into a log file for each tag.
    To learn how to use this appender, check the sample @code(file_appender.dproj)
  }
  TLoggerProFileAppender = class(TLoggerProFileAppenderBase)
  private
    fWritersDictionary: TObjectDictionary<string, TStreamWriter>;
    procedure AddWriter(const aLogTag: string; var aWriter: TStreamWriter; var aLogFileName: string);
    procedure RotateLog(const aLogTag: string; aWriter: TStreamWriter);
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); overload; override;
  end;

  { @abstract(A simple file appender)
    This file appender writes all TLogItems into a single log file.
    Combined with a @code(TLoggerProAppenderFilterImpl) you can filter out any log tags you like.
    If you want to run several TLoggerProSimpleFileAppender in parallel you have to provide a different
    LogFileFormat for each of them in the constructor in order to prevent name collisions.
    To learn how to use this appender, check the sample @code(file_appender.dproj)
  }
  TLoggerProSimpleFileAppender = class(TLoggerProFileAppenderBase)
  private const
    cDefaultLogTag = '---';
  private
    fFileWriter: TStreamWriter;
    procedure RotateLog;
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); overload; override;
  end;

implementation

uses
  System.IOUtils,
  System.StrUtils,
  System.Math,
  idGlobal
{$IF Defined(Android)}
    ,Androidapi.Helpers
    ,Androidapi.JNI.GraphicsContentViewText
    ,Androidapi.JNI.JavaTypes
{$ENDIF}
    ;

{ TLoggerProFileAppenderBase }

function TLoggerProFileAppenderBase.GetLogFileName(const aTag: string; const aFileNumber: Integer): string;
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

procedure TLoggerProFileAppenderBase.Setup;
begin
  inherited;

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
end;

procedure TLoggerProFileAppenderBase.WriteToStream(const aStreamWriter: TStreamWriter; const aValue: string);
begin
  aStreamWriter.WriteLine(aValue);
  aStreamWriter.Flush;
end;

procedure TLoggerProFileAppenderBase.InternalWriteLog(const aStreamWriter: TStreamWriter; const aLogItem: TLogItem);
begin
  WriteToStream(aStreamWriter, FormatLog(aLogItem));
end;

procedure TLoggerProFileAppenderBase.RetryMove(const aFileSrc, aFileDest: string);
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

procedure TLoggerProFileAppenderBase.RotateFile(const aLogTag: string; out aNewFileName: string);
var
  lRenamedFile: string;
  I: Integer;
  lCurrentFileName: string;
begin
  aNewFileName := GetLogFileName(aLogTag, 0);
  // remove the last file of backup set
  lRenamedFile := GetLogFileName(aLogTag, fMaxBackupFileCount);
  if TFile.Exists(lRenamedFile) then
    TFile.Delete(lRenamedFile);
  // shift the files names
  for I := fMaxBackupFileCount - 1 downto 1 do
  begin
    lCurrentFileName := GetLogFileName(aLogTag, I);
    lRenamedFile := GetLogFileName(aLogTag, I + 1);
    if TFile.Exists(lCurrentFileName) then
      RetryMove(lCurrentFileName, lRenamedFile);

  end;
  lRenamedFile := GetLogFileName(aLogTag, 1);
  RetryMove(aNewFileName, lRenamedFile);
end;

constructor TLoggerProFileAppenderBase.Create(aMaxBackupFileCount: Integer; aMaxFileSizeInKiloByte: Integer; aLogsFolder: string;
  aFileAppenderOptions: TFileAppenderOptions; aLogFileNameFormat: string; aLogFormat: string; aEncoding: TEncoding);
begin
  inherited Create(ALogFormat);
  fLogsFolder := aLogsFolder;  
  fMaxBackupFileCount:= Min(1, aMaxBackupFileCount);
  fMaxFileSizeInKiloByte := aMaxFileSizeInKiloByte;
  fLogFileNameFormat := aLogFileNameFormat;
  fFileAppenderOptions := aFileAppenderOptions;
  if Assigned(aEncoding) then
    fEncoding := aEncoding
  else
    fEncoding := TEncoding.DEFAULT;
end;

function TLoggerProFileAppenderBase.CreateWriter(const aFileName: string): TStreamWriter;
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

{ TLoggerProFileAppender }

procedure TLoggerProFileAppender.AddWriter(const aLogTag:string; var aWriter: TStreamWriter; var aLogFileName: string);
begin
  aLogFileName := GetLogFileName(aLogTag, 0);
  aWriter := CreateWriter(aLogFileName);
  fWritersDictionary.Add(aLogTag, aWriter);
end;

procedure TLoggerProFileAppender.RotateLog(const aLogTag: string; aWriter: TStreamWriter);
var
  lLogFileName: string;
begin
  WriteToStream(aWriter, '#[ROTATE LOG ' + datetimetostr(Now, FormatSettings) + ']');
  // remove the writer during rename
  fWritersDictionary.Remove(aLogTag);
  RotateFile(aLogTag, lLogFileName);
  // re-create the writer
  AddWriter(aLogTag, aWriter, lLogFileName);
  WriteToStream(aWriter, '#[START LOG ' + datetimetostr(Now, FormatSettings) + ']');
end;

procedure TLoggerProFileAppender.Setup;
begin
  inherited;
  fWritersDictionary := TObjectDictionary<string, TStreamWriter>.Create([doOwnsValues]);
end;

procedure TLoggerProFileAppender.TearDown;
begin
  fWritersDictionary.Free;
  inherited;
end;

procedure TLoggerProFileAppender.WriteLog(const aLogItem: TLogItem);
var
  lWriter: TStreamWriter;
  lLogFileName:string;
begin
  if not fWritersDictionary.TryGetValue(aLogItem.LogTag, lWriter) then
  begin
    AddWriter(aLogItem.LogTag, lWriter, lLogFileName);
  end;

  InternalWriteLog(lWriter, aLogItem);

  if lWriter.BaseStream.Size > fMaxFileSizeInKiloByte * 1024 then
  begin
    RotateLog(aLogItem.LogTag, lWriter);
  end;
end;

{ TLoggerProSimpleFileAppender }

procedure TLoggerProSimpleFileAppender.RotateLog;
var
  lLogFileName: string;
begin
  WriteToStream(fFileWriter, '#[ROTATE LOG ' + datetimetostr(Now, FormatSettings) + ']');
  // remove the writer during rename
  fFileWriter.Free;
  RotateFile(cDefaultLogTag, lLogFileName);
  // re-create the writer
  fFileWriter := CreateWriter(GetLogFileName(cDefaultLogTag, 0));
  WriteToStream(fFileWriter, '#[START LOG ' + datetimetostr(Now, FormatSettings) + ']');
end;

procedure TLoggerProSimpleFileAppender.Setup;
begin
  inherited;
  fFileWriter := CreateWriter(GetLogFileName(cDefaultLogTag, 0));
end;

procedure TLoggerProSimpleFileAppender.TearDown;
begin
  fFileWriter.Free;
  inherited;
end;

procedure TLoggerProSimpleFileAppender.WriteLog(const aLogItem: TLogItem);
begin
  InternalWriteLog(fFileWriter, aLogItem);

  if fFileWriter.BaseStream.Size > fMaxFileSizeInKiloByte * 1024 then
  begin
    RotateLog;
  end;
end;

end.

