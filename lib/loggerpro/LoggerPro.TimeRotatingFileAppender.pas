// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit LoggerPro.TimeRotatingFileAppender;

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.Generics.Collections;

type
  TTimeRotationInterval = (
    Hourly,     // Rotate every hour: app.2025120312.log
    Daily,      // Rotate every day: app.20251203.log
    Weekly,     // Rotate every week: app.2025W49.log
    Monthly     // Rotate every month: app.202512.log
  );

  { @abstract(File appender that rotates based on time intervals instead of file size)
    Creates a new log file when the time interval changes.
    File naming includes timestamp to allow easy identification and sorting.
  }
  TLoggerProTimeRotatingFileAppender = class(TLoggerProAppenderBase)
  private
    FLogsFolder: string;
    FFileBaseName: string;
    FInterval: TTimeRotationInterval;
    FMaxBackupFiles: Integer;
    FCurrentWriter: TStreamWriter;
    FCurrentTimestamp: string;
    FEncoding: TEncoding;
    function GetTimestampForInterval(aDateTime: TDateTime): string;
    function GetLogFileName(const aTimestamp: string): string;
    procedure RotateIfNeeded(aCurrentDateTime: TDateTime);
    procedure CreateNewWriter(const aTimestamp: string);
    procedure CleanupOldFiles;
    procedure WriteToStream(const aValue: string);
  public
    const DEFAULT_MAX_BACKUP_FILES = 30;

    { Creates a time-rotating file appender.
      @param aInterval The rotation interval (Hourly, Daily, Weekly, Monthly)
      @param aMaxBackupFiles Maximum number of old log files to keep (0 = unlimited)
      @param aLogsFolder Folder for log files (empty = application folder)
      @param aFileBaseName Base name for log files (empty = module name)
      @param aLogItemRenderer Optional custom renderer
      @param aEncoding File encoding (default UTF-8) }
    constructor Create(
      aInterval: TTimeRotationInterval = TTimeRotationInterval.Daily;
      aMaxBackupFiles: Integer = DEFAULT_MAX_BACKUP_FILES;
      aLogsFolder: string = '';
      aFileBaseName: string = '';
      aLogItemRenderer: ILogItemRenderer = nil;
      aEncoding: TEncoding = nil); reintroduce;

    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;

    property Interval: TTimeRotationInterval read FInterval;
    property MaxBackupFiles: Integer read FMaxBackupFiles;
    property LogsFolder: string read FLogsFolder;
  end;

implementation

{$IF Defined(Android)}
uses
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes;
{$ENDIF}

{ TLoggerProTimeRotatingFileAppender }

constructor TLoggerProTimeRotatingFileAppender.Create(
  aInterval: TTimeRotationInterval;
  aMaxBackupFiles: Integer;
  aLogsFolder: string;
  aFileBaseName: string;
  aLogItemRenderer: ILogItemRenderer;
  aEncoding: TEncoding);
begin
  inherited Create(aLogItemRenderer);
  if aMaxBackupFiles < 0 then
    raise ELoggerPro.CreateFmt('MaxBackupFiles must be >= 0, got %d', [aMaxBackupFiles]);
  FInterval := aInterval;
  FMaxBackupFiles := aMaxBackupFiles;
  FLogsFolder := aLogsFolder;
  FFileBaseName := aFileBaseName;
  if Assigned(aEncoding) then
    FEncoding := aEncoding
  else
    FEncoding := TEncoding.UTF8;
end;

procedure TLoggerProTimeRotatingFileAppender.Setup;
var
  lModuleName: string;
begin
  inherited;

  // Set default logs folder
  if FLogsFolder.IsEmpty then
  begin
    {$IF Defined(MSWINDOWS) or Defined(POSIX)}
      {$IF not Defined(MOBILE)}
    FLogsFolder := TPath.GetDirectoryName(GetModuleName(HInstance));
      {$ENDIF}
    {$ENDIF}
    {$IF Defined(Android) or Defined(IOS)}
    FLogsFolder := TPath.GetSharedDocumentsPath;
    {$ENDIF}
  end;

  if not TDirectory.Exists(FLogsFolder) then
    TDirectory.CreateDirectory(FLogsFolder);

  // Set default file base name
  if FFileBaseName.IsEmpty then
  begin
    {$IF Defined(Android)}
    lModuleName := TAndroidHelper.ApplicationTitle.Replace(' ', '_', [rfReplaceAll]);
    {$ELSEIF not Defined(MOBILE)}
    lModuleName := TPath.GetFileNameWithoutExtension(GetModuleName(HInstance));
    {$ELSE}
    lModuleName := 'app';
    {$ENDIF}
    FFileBaseName := lModuleName;
  end;

  // Create initial writer
  FCurrentTimestamp := GetTimestampForInterval(Now);
  CreateNewWriter(FCurrentTimestamp);

  // Cleanup old files on startup
  CleanupOldFiles;
end;

procedure TLoggerProTimeRotatingFileAppender.TearDown;
begin
  if Assigned(FCurrentWriter) then
  begin
    FCurrentWriter.Flush;
    FCurrentWriter.Free;
    FCurrentWriter := nil;
  end;
  inherited;
end;

function TLoggerProTimeRotatingFileAppender.GetTimestampForInterval(aDateTime: TDateTime): string;
var
  lYear, lMonth, lDay, lHour, lWeek: Word;
begin
  DecodeDate(aDateTime, lYear, lMonth, lDay);
  lHour := HourOf(aDateTime);

  case FInterval of
    TTimeRotationInterval.Hourly:
      Result := Format('%.4d%.2d%.2d%.2d', [lYear, lMonth, lDay, lHour]);

    TTimeRotationInterval.Daily:
      Result := Format('%.4d%.2d%.2d', [lYear, lMonth, lDay]);

    TTimeRotationInterval.Weekly:
      begin
        lWeek := WeekOfTheYear(aDateTime);
        Result := Format('%.4dW%.2d', [lYear, lWeek]);
      end;

    TTimeRotationInterval.Monthly:
      Result := Format('%.4d%.2d', [lYear, lMonth]);
  else
    Result := Format('%.4d%.2d%.2d', [lYear, lMonth, lDay]);
  end;
end;

function TLoggerProTimeRotatingFileAppender.GetLogFileName(const aTimestamp: string): string;
begin
  Result := TPath.Combine(FLogsFolder, FFileBaseName + '.' + aTimestamp + '.log');
end;

procedure TLoggerProTimeRotatingFileAppender.CreateNewWriter(const aTimestamp: string);
var
  lFileName: string;
  lFileStream: TFileStream;
  lFileAccessMode: Word;
begin
  lFileName := GetLogFileName(aTimestamp);

  lFileAccessMode := fmOpenWrite or fmShareDenyNone;
  if not TFile.Exists(lFileName) then
    lFileAccessMode := lFileAccessMode or fmCreate;

  lFileStream := TFileStream.Create(lFileName, lFileAccessMode);
  try
    lFileStream.Seek(0, TSeekOrigin.soEnd);
    FCurrentWriter := TStreamWriter.Create(lFileStream, FEncoding, 32);
    FCurrentWriter.AutoFlush := True;
    FCurrentWriter.OwnStream;
  except
    lFileStream.Free;
    raise;
  end;
end;

procedure TLoggerProTimeRotatingFileAppender.RotateIfNeeded(aCurrentDateTime: TDateTime);
var
  lNewTimestamp: string;
begin
  lNewTimestamp := GetTimestampForInterval(aCurrentDateTime);

  if lNewTimestamp <> FCurrentTimestamp then
  begin
    // Close current writer
    if Assigned(FCurrentWriter) then
    begin
      FCurrentWriter.Flush;
      FCurrentWriter.Free;
      FCurrentWriter := nil;
    end;

    // Update timestamp and create new writer
    FCurrentTimestamp := lNewTimestamp;
    CreateNewWriter(FCurrentTimestamp);

    // Cleanup old files after rotation
    CleanupOldFiles;
  end;
end;

procedure TLoggerProTimeRotatingFileAppender.CleanupOldFiles;
var
  lFiles: TArray<string>;
  lPattern: string;
  I: Integer;
begin
  if FMaxBackupFiles <= 0 then
    Exit; // No cleanup needed

  lPattern := FFileBaseName + '.*.log';
  lFiles := TDirectory.GetFiles(FLogsFolder, lPattern);

  // Sort by name (timestamps ensure chronological order)
  TArray.Sort<string>(lFiles);

  // Delete oldest files if we exceed the limit
  for I := 0 to Length(lFiles) - FMaxBackupFiles - 1 do
  begin
    try
      TFile.Delete(lFiles[I]);
    except
      // Ignore deletion errors (file might be locked)
    end;
  end;
end;

procedure TLoggerProTimeRotatingFileAppender.WriteToStream(const aValue: string);
begin
  FCurrentWriter.WriteLine(aValue);
  FCurrentWriter.Flush;
end;

procedure TLoggerProTimeRotatingFileAppender.WriteLog(const aLogItem: TLogItem);
begin
  RotateIfNeeded(aLogItem.TimeStamp);
  WriteToStream(FormatLog(aLogItem));
end;

end.
