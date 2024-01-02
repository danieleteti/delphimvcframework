unit LoggerProConfig;

interface

uses
  LoggerPro,
  LoggerPro.Proxy;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender, System.IOUtils {TPath};

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization

// The TLoggerProFileAppender has its defaults defined as follows:
//   TLogLayout = record
//     const LOG_LAYOUT_0 = '{timestamp} [TID {threadid}][{loglevel}] {message} [{tag}]';  // << Default
//     const LOG_LAYOUT_1 = '{timestamp} [TID {threadid}][{loglevel}] {message}';
//     const LOG_LAYOUT_2 = '{timestamp} [{loglevel}] {message} [{tag}]';
//     const LOG_LAYOUT_3 = '{timestamp} [{loglevel}] {message}';
//   end;
//
//   DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
//   DEFAULT_MAX_FILE_SIZE_KB = 1000;
//
//  You can override these dafaults passing parameters to the constructor.
//  Here's some configuration examples:
//
//  Creates log with default settings
//  _Log := BuildLogWriter([TLoggerProFileAppender.Create]);
//
//  Create logs in the exe' same folder. Backupset = 10, max size for single file 5k
//  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);
//
//  Creates log in the AppData/Roaming with PID in the filename
//  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
//     TPath.GetHomePath,
//     TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT_WITH_PID
//   )]);
//
//  Creates log in the same folder with PID in the filename
//  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5, '',
//     TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT_WITH_PID)]);
//
//  Creates logs in the ..\..\ folder using the default filename
//  The FilteringFileAppender selects the 'TAG1' and 'TAG2' log messages into a separate file
//   _Log := BuildLogWriter([
//     TLoggerProFileAppender.Create(10, 5, '..\..',
//       TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT,
//       TLogLayout.LOG_LAYOUT_0),
//     TLoggerProFilter.Build(
//       TLoggerProSimpleFileAppender.Create(10, 5, '..\..'),
//       function(ALogItem: TLogItem): boolean
//       begin
//         Result := (ALogItem.LogTag = 'TAG1') or (ALogItem.LogTag = 'TAG2');
//       end)
//     ]);
//
end.
