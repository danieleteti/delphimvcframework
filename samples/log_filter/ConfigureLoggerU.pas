unit ConfigureLoggerU;

interface

procedure ConfigureLogger;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  LoggerPro,
  LoggerPro.FileAppender,
  LoggerPro.SimpleConsoleAppender,
  LoggerPro.Proxy,
  LoggerPro.Renderers;

procedure ConfigureLogger;
begin
  SetDefaultLogger(
      BuildLogWriter(
          [
              TLoggerProSimpleConsoleAppender.Create,
              TLoggerProFilter.Build(
                  TLoggerProFileAppender.Create(
                      10,
                      5,
                      '..\..\logs',
                      TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT,
                      TLogItemRendererNoTag.Create
                  ),
                  function(ALogItem: TLogItem): boolean
                  begin
                    Result := not ALogItem.LogMessage.StartsWith('GET:/api/notlogged');
                  end
              )
          ]
      )
  );

end;

end.
