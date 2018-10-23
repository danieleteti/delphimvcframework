unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender,
  System.SysUtils;

var
  _Log: ILogWriter;
  _FileAppender: ILogAppender;
  _FormatSettings: TFormatSettings;
  _CustomLogFormat: string;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization

_FormatSettings.DateSeparator := '-';
_FormatSettings.TimeSeparator := ':';
_FormatSettings.ShortDateFormat := 'YYY-MM-DD HH:NN:SS:ZZZ';
_FormatSettings.ShortTimeFormat := 'HH:NN:SS';

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
_CustomLogFormat := '%0:s %s';

// Creates log in the ..\..\ folder without PID in the filename
_FileAppender := TLoggerProFileAppender.Create(10, 1000, 'logs');
TLoggerProFileAppender(_FileAppender).OnLogRow := procedure(const aLogItem: TLogItem; out aLogRow: string)
  begin
    aLogRow := Format(_CustomLogFormat, [datetimetostr(aLogItem.TimeStamp, _FormatSettings), aLogItem.LogMessage]);
  end;
_Log := BuildLogWriter([_FileAppender]);
// Create logs in the exe' same folder
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

// Creates log in the AppData/Roaming with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

// Creates log in the same folder with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

end.
