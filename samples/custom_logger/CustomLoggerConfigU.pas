unit CustomLoggerConfigU;

interface

uses
  LoggerPro;

function GetLogger: ILogWriter;

implementation

uses
  System.IOUtils,
  LoggerPro.Builder;

function GetLogger: ILogWriter;
begin
  { LoggerPro 2.0 - Builder Pattern API }
  Result := LoggerProBuilder
    .WriteToFile
      .WithLogsFolder(TPath.Combine('MyFolder', 'MyLogs'))
      .WithMaxBackupFiles(10)
      .WithMaxFileSizeInKB(1000)
      .Done
    .WriteToSimpleConsole.Done
    {$IFDEF MSWINDOWS}
    .WriteToOutputDebugString.Done
    {$ENDIF}
    .Build;
end;

end.
