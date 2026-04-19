// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
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

unit LoggerPro.FileBySourceAppender;

{ File appender that organizes logs into per-source folders.

  The "source" is extracted from the log item's context using a configurable
  key (default: 'source'). Each source gets its own subfolder. Within each
  folder, files are named with source, tag, date, and sequence number:

    logs/
      ClientA/
        ClientA.ORDERS.20260326.00.log
        ClientA.ORDERS.20260326.01.log   (size rotation)
        ClientA.PAYMENTS.20260326.00.log
      ClientB/
        ClientB.API.20260326.00.log

  Rotation:
    - New file on day change
    - New file (incremented sequence) when MaxFileSizeInKB is exceeded

  Retention:
    - Files older than RetainDays are deleted on startup and on each day change
    - Empty source folders are removed after cleanup }

interface

uses
  LoggerPro,
  System.Generics.Collections,
  System.Classes,
  System.SysUtils;

type
  TLoggerProFileBySourceAppender = class(TLoggerProAppenderBase)
  public const
    DEFAULT_MAX_FILE_SIZE_KB = 1000;
    DEFAULT_RETAIN_DAYS = 30;
    DEFAULT_SOURCE = 'default';
    SOURCE_CONTEXT_KEY = 'source';
  private const
    RETRY_COUNT = 5;
    RETRY_DELAY_MS = 200;
  private
    FLogsFolder: string;
    FMaxFileSizeInKB: Integer;
    FRetainDays: Integer;
    FDefaultSource: string;
    FEncoding: TEncoding;
    FCurrentDateStr: string;
    // Key = Source + '|' + Tag + '|' + DateStr
    FWriters: TObjectDictionary<string, TStreamWriter>;
    // Key = WriterKey, Value = current sequence number
    FSequences: TDictionary<string, Integer>;

    function ExtractSource(const aLogItem: TLogItem): string;
    function GetDateStr(aDateTime: TDateTime): string;
    function GetWriterKey(const aSource, aTag, aDateStr: string): string;
    function GetSourceFolder(const aSource: string): string;
    function GetLogFileName(const aSource, aTag, aDateStr: string;
      aSequence: Integer): string;
    function FindCurrentSequence(const aSource, aTag, aDateStr: string): Integer;
    function GetOrCreateWriter(const aSource, aTag, aDateStr: string): TStreamWriter;
    function InternalCreateWriter(const aFileName: string): TStreamWriter;
    procedure RotateBySize(const aSource, aTag, aDateStr: string);
    procedure CloseAllWriters;
    procedure CloseWritersForDate(const aDateStr: string);
    procedure CleanupOldFiles;
    procedure CleanupSourceFolder(const aSourceFolder: string);
    function ParseDateFromFileName(const aFileName: string): string;
  public
    constructor Create(
      const aLogsFolder: string = '';
      aMaxFileSizeInKB: Integer = DEFAULT_MAX_FILE_SIZE_KB;
      aRetainDays: Integer = DEFAULT_RETAIN_DAYS;
      const aDefaultSource: string = DEFAULT_SOURCE;
      const aLogItemRenderer: ILogItemRenderer = nil;
      const aEncoding: TEncoding = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  LoggerPro.Renderers;

{ TLoggerProFileBySourceAppender }

constructor TLoggerProFileBySourceAppender.Create(
  const aLogsFolder: string;
  aMaxFileSizeInKB: Integer;
  aRetainDays: Integer;
  const aDefaultSource: string;
  const aLogItemRenderer: ILogItemRenderer;
  const aEncoding: TEncoding);
begin
  inherited Create(aLogItemRenderer);
  FLogsFolder := aLogsFolder;
  FMaxFileSizeInKB := aMaxFileSizeInKB;
  FRetainDays := aRetainDays;
  FDefaultSource := aDefaultSource;
  if Assigned(aEncoding) then
    FEncoding := aEncoding
  else
    FEncoding := TEncoding.UTF8;
end;

procedure TLoggerProFileBySourceAppender.Setup;
begin
  inherited;
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

  FWriters := TObjectDictionary<string, TStreamWriter>.Create([doOwnsValues]);
  FSequences := TDictionary<string, Integer>.Create;
  FCurrentDateStr := GetDateStr(Now);

  CleanupOldFiles;
end;

procedure TLoggerProFileBySourceAppender.TearDown;
begin
  CloseAllWriters;
  FreeAndNil(FSequences);
  FreeAndNil(FWriters);
  inherited;
end;

procedure TLoggerProFileBySourceAppender.WriteLog(const aLogItem: TLogItem);
var
  lSource, lTag, lDateStr: string;
  lWriter: TStreamWriter;
begin
  lSource := ExtractSource(aLogItem);
  lTag := aLogItem.LogTag;
  lDateStr := GetDateStr(aLogItem.TimeStamp);

  // Day changed: close old writers, run cleanup
  if lDateStr <> FCurrentDateStr then
  begin
    CloseWritersForDate(FCurrentDateStr);
    FCurrentDateStr := lDateStr;
    CleanupOldFiles;
  end;

  lWriter := GetOrCreateWriter(lSource, lTag, lDateStr);
  lWriter.WriteLine(FormatLog(aLogItem));
  lWriter.Flush;

  // Size-based rotation
  if (FMaxFileSizeInKB > 0) and
     (lWriter.BaseStream.Size > Int64(FMaxFileSizeInKB) * 1024) then
    RotateBySize(lSource, lTag, lDateStr);
end;

function TLoggerProFileBySourceAppender.ExtractSource(const aLogItem: TLogItem): string;
var
  lParam: LogParam;
begin
  // Scan all context params; take the LAST 'source' value.
  // This ensures inline LogParam.S('source', ...) overrides
  // any value set via WithDefaultContext (which is prepended).
  Result := FDefaultSource;
  for lParam in aLogItem.Context do
  begin
    if SameText(lParam.Key, SOURCE_CONTEXT_KEY) then
      Result := lParam.Value.AsString;
  end;
end;

function TLoggerProFileBySourceAppender.GetDateStr(aDateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyymmdd', aDateTime);
end;

function TLoggerProFileBySourceAppender.GetWriterKey(
  const aSource, aTag, aDateStr: string): string;
begin
  Result := aSource + '|' + aTag + '|' + aDateStr;
end;

function TLoggerProFileBySourceAppender.GetSourceFolder(const aSource: string): string;
begin
  Result := TPath.Combine(FLogsFolder, aSource);
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

function TLoggerProFileBySourceAppender.GetLogFileName(
  const aSource, aTag, aDateStr: string; aSequence: Integer): string;
begin
  Result := TPath.Combine(
    GetSourceFolder(aSource),
    Format('%s.%s.%s.%.2d.log', [aSource, aTag, aDateStr, aSequence]));
end;

function TLoggerProFileBySourceAppender.FindCurrentSequence(
  const aSource, aTag, aDateStr: string): Integer;
var
  lFolder, lPattern: string;
  lFiles: TArray<string>;
  lFileName, lSeqStr: string;
  lSeq: Integer;
  lParts: TArray<string>;
begin
  Result := 0;
  lFolder := GetSourceFolder(aSource);
  lPattern := Format('%s.%s.%s.*.log', [aSource, aTag, aDateStr]);

  if not TDirectory.Exists(lFolder) then
    Exit;

  lFiles := TDirectory.GetFiles(lFolder, lPattern);
  for lFileName in lFiles do
  begin
    // Parse sequence from: Source.Tag.Date.NN.log
    lParts := TPath.GetFileNameWithoutExtension(
      TPath.GetFileName(lFileName)).Split(['.']);
    if Length(lParts) >= 4 then
    begin
      lSeqStr := lParts[Length(lParts) - 1];
      if TryStrToInt(lSeqStr, lSeq) and (lSeq > Result) then
        Result := lSeq;
    end;
  end;
end;

function TLoggerProFileBySourceAppender.GetOrCreateWriter(
  const aSource, aTag, aDateStr: string): TStreamWriter;
var
  lKey: string;
  lSeq: Integer;
  lFileName: string;
begin
  lKey := GetWriterKey(aSource, aTag, aDateStr);

  if FWriters.TryGetValue(lKey, Result) then
    Exit;

  // Find the highest existing sequence on disk
  lSeq := FindCurrentSequence(aSource, aTag, aDateStr);
  lFileName := GetLogFileName(aSource, aTag, aDateStr, lSeq);

  // If that file already exceeds the size limit, start a new one
  if TFile.Exists(lFileName) and (FMaxFileSizeInKB > 0) and
     (TFile.GetSize(lFileName) > Int64(FMaxFileSizeInKB) * 1024) then
  begin
    Inc(lSeq);
    lFileName := GetLogFileName(aSource, aTag, aDateStr, lSeq);
  end;

  Result := InternalCreateWriter(lFileName);
  FWriters.Add(lKey, Result);
  FSequences.AddOrSetValue(lKey, lSeq);
end;

function TLoggerProFileBySourceAppender.InternalCreateWriter(
  const aFileName: string): TStreamWriter;
var
  lFileStream: TFileStream;
  lFileAccessMode: Word;
  lRetry: Integer;
begin
  lRetry := 0;
  while True do
  begin
    try
      lFileAccessMode := fmOpenWrite or fmShareDenyNone;
      if not TFile.Exists(aFileName) then
        lFileAccessMode := lFileAccessMode or fmCreate;

      lFileStream := TFileStream.Create(aFileName, lFileAccessMode);
      try
        lFileStream.Seek(0, TSeekOrigin.soEnd);
        Result := TStreamWriter.Create(lFileStream, FEncoding, 32);
        Result.AutoFlush := True;
        Result.OwnStream;
      except
        lFileStream.Free;
        raise;
      end;
      Exit;
    except
      Inc(lRetry);
      if lRetry >= RETRY_COUNT then
        raise;
      Sleep(RETRY_DELAY_MS);
    end;
  end;
end;

procedure TLoggerProFileBySourceAppender.RotateBySize(
  const aSource, aTag, aDateStr: string);
var
  lKey: string;
  lSeq: Integer;
  lFileName: string;
  lWriter: TStreamWriter;
begin
  lKey := GetWriterKey(aSource, aTag, aDateStr);

  // Close current writer
  FWriters.ExtractPair(lKey);

  // Get next sequence
  if FSequences.TryGetValue(lKey, lSeq) then
    Inc(lSeq)
  else
    lSeq := FindCurrentSequence(aSource, aTag, aDateStr) + 1;

  lFileName := GetLogFileName(aSource, aTag, aDateStr, lSeq);
  lWriter := InternalCreateWriter(lFileName);
  FWriters.Add(lKey, lWriter);
  FSequences.AddOrSetValue(lKey, lSeq);
end;

procedure TLoggerProFileBySourceAppender.CloseAllWriters;
begin
  if Assigned(FWriters) then
    FWriters.Clear;
  if Assigned(FSequences) then
    FSequences.Clear;
end;

procedure TLoggerProFileBySourceAppender.CloseWritersForDate(const aDateStr: string);
var
  lKeysToRemove: TArray<string>;
  lKey: string;
  lCount: Integer;
begin
  SetLength(lKeysToRemove, FWriters.Count);
  lCount := 0;
  for lKey in FWriters.Keys do
  begin
    if lKey.EndsWith('|' + aDateStr) then
    begin
      lKeysToRemove[lCount] := lKey;
      Inc(lCount);
    end;
  end;

  for var I := 0 to lCount - 1 do
  begin
    FWriters.Remove(lKeysToRemove[I]);
    FSequences.Remove(lKeysToRemove[I]);
  end;
end;

procedure TLoggerProFileBySourceAppender.CleanupOldFiles;
var
  lDirs: TArray<string>;
  lDir: string;
begin
  if FRetainDays <= 0 then
    Exit;

  if not TDirectory.Exists(FLogsFolder) then
    Exit;

  lDirs := TDirectory.GetDirectories(FLogsFolder);
  for lDir in lDirs do
  begin
    try
      CleanupSourceFolder(lDir);
    except
      // Ignore cleanup errors
    end;
  end;
end;

procedure TLoggerProFileBySourceAppender.CleanupSourceFolder(
  const aSourceFolder: string);
var
  lFiles: TArray<string>;
  lFile, lDateStr: string;
  lFileDate: TDateTime;
  lCutoffDate: TDateTime;
  lYear, lMonth, lDay: Integer;
begin
  lCutoffDate := Date - FRetainDays;

  lFiles := TDirectory.GetFiles(aSourceFolder, '*.log');
  for lFile in lFiles do
  begin
    lDateStr := ParseDateFromFileName(lFile);
    if lDateStr.IsEmpty then
      Continue;

    // Parse yyyymmdd
    if (Length(lDateStr) = 8) and
       TryStrToInt(Copy(lDateStr, 1, 4), lYear) and
       TryStrToInt(Copy(lDateStr, 5, 2), lMonth) and
       TryStrToInt(Copy(lDateStr, 7, 2), lDay) then
    begin
      try
        lFileDate := EncodeDate(lYear, lMonth, lDay);
        if lFileDate < lCutoffDate then
          TFile.Delete(lFile);
      except
        // Ignore parse/delete errors
      end;
    end;
  end;

  // Remove empty folder
  try
    if Length(TDirectory.GetFiles(aSourceFolder)) = 0 then
      TDirectory.Delete(aSourceFolder);
  except
    // Ignore
  end;
end;

function TLoggerProFileBySourceAppender.ParseDateFromFileName(
  const aFileName: string): string;
var
  lName: string;
  lParts: TArray<string>;
begin
  // Expected format: Source.Tag.YYYYMMDD.NN.log
  // After removing extension: Source.Tag.YYYYMMDD.NN
  // The date is the third-to-last part (index Length-2)
  Result := '';
  lName := TPath.GetFileNameWithoutExtension(TPath.GetFileName(aFileName));
  lParts := lName.Split(['.']);
  if Length(lParts) >= 4 then
    Result := lParts[Length(lParts) - 2]; // YYYYMMDD is before the sequence number
end;

end.
