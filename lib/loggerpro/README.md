# LoggerPro for Delphi

An modern and pluggable logging framework for Delphi

## Compatibility

LoggerPro is compatibile with
- Delphi 11 Alexandria
- Delphi 10.4 Sydney
- Delphi 10.3 Rio
- Delphi 10.2 Tokyo (Added Linux compatibility)
- Delphi 10.1 Berlin
- Delphi 10 Seattle
- Delphi XE8
- Delphi XE7
- Delphi XE6
- Delphi XE5
- Delphi XE4
- Delphi XE3
- Delphi XE2


## What's new in 1.4.0

- Improved VCL and FMX visual appenders
- Appenders can be added and removed programmatically
- Added packages for latest versions of Delphi
- Improved algorithm used to handle slow consumers
- Added [DMSContainer's EventStreams](http://dmscontainer.bittimeprofessionals.com/) Appender
- Added very basic console appender that assumes is running from console in order to provide simple Linux console logging
- FIX [Issue 50](https://github.com/danieleteti/loggerpro/issues/50)
- New filtering log file appender that allows to pick individual tags to be written into a file.
- FIX [Issue 57](https://github.com/danieleteti/loggerpro/issues/57)
- FIX [Issue 60](https://github.com/danieleteti/loggerpro/issues/60)

## What's new in 1.3.2

- Added support for Android API level 26 in mobile demo
- Added packages for Delphi 10.3 Rio, Delphi 10.2 Tokyo, Delphi 10.1 Berlin and Delphi 10.0 Seattle.
- Added packages for Delphi XE7 and Delphi XE8 (these packages do not contain appenders which uses `System.Net.HttpClient`)
- Added support for Linux in `TLoggerProFileAppender` (Thank you [charoit](https://github.com/charoit))

## What's new in 1.3.0
- Replace `TThreadedList<T>` with a custom implementation (`TThreadSafeQueue<T>`) because of a [bug](https://forums.embarcadero.com/thread.jspa?messageID=941762) and [this](https://quality.embarcadero.com/browse/RSP-19993) in `TMonitor`.
  - `TThreadSafeQueue<T>` is not a drop-in replacement for the `TThreadedQueue<T>` but can be used in other projects if you are fighting with the same bug.
- `TVCLMemoLogAppender.Create` gots new parameter: `aClearOnStartup` which optionally clear the memo at the startup.
- Improvement to the `TLoggerProConsoleAppender` (Thanks to [Fulgan](https://github.com/Fulgan))
- Improvement to the `TLoggerProFileAppender`; now there is a `OnLogRow` callback that can be used to customize log row format.
- New overloaded `Log` methods. The `*Fmt` versions are deprecated and will be removed in a future version [ISSUE #17](https://github.com/danieleteti/loggerpro/issues/17)
- New [NSQ](https://nsq.io) appender (Thanks to [Fulgan](https://github.com/Fulgan))
- New logger filter decorator (Thanks to [Fulgan](https://github.com/Fulgan))
- New REST appender with support for extended information (samples for Windows and Android)
  - Extended information are supported in Windows (fully) and Android (partially)  
  - In the sample folder is provided also the `RESTLogCollector`
- New [Elastic Search](https://www.elastic.co/products/elasticsearch) Log appender (Thanks to Salvatore Sparacino)


## Getting started
```delphi
program getting_started_console;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  LoggerPro.GlobalLogger; //this is the global logger, it is perfect to understand the basic operation of LoggerPro.

begin
  try
    //the global logger uses a TLoggerProFileAppender, so your logs will be written on a 
    //set of files with automatic rolling/rotating
    
    Log.Debug('Debug message', 'main'); //TLoggerProFileAppender uses the "tag" to select a different log file	
    Log.Info('Info message', 'main');
    Log.Warn('Warning message', 'main');
    Log.Error('Error message', 'errors');
    WriteLn('Check "getting_started_console.00.main.log" and "getting_started_console.00.errors.log" to see your logs');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
```

The most flexible/correct approach is not much complicated than the global logger one. Check how is simple to create a custom instance of logwriter

```delphi
program getting_started_console_appenders;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  LoggerPro, //LoggerPro core
  LoggerPro.FileAppender, //File appender
  LoggerPro.OutputDebugStringAppender; //OutputDebugString appender

var
  Log: ILogWriter;

begin
  Log := BuildLogWriter([TLoggerProFileAppender.Create,
    TLoggerProOutputDebugStringAppender.Create]);

  try
    Log.Debug('Debug message', 'main');
    Log.Info('Info message', 'main');
    Log.Warn('Warning message', 'main');
    Log.Error('Error message', 'errors');
    WriteLn('Check ');
    WriteLn('  "getting_started_console.00.main.log"');
    WriteLn('  "getting_started_console.00.errors.log"');

    if DebugHook <> 0 then //inform the user where his/her logs are
    begin
      WriteLn('also, you logs have been sent to the current debugger, check the Delphi''s EventLog window to see them.');
    end
    else
    begin
      WriteLn('..seems that no debugger is present. The logs can be seen using DebugView.');
      WriteLn('Download it from here https://technet.microsoft.com/en-us/sysinternals/debugview.aspx');
      WriteLn('Learn how to use http://tedgustaf.com/blog/2011/5/use-debugview-to-view-debug-output-from-asp-net-web-application/');
    end;
    ReadLn;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
```

## Built-in log appenders
The framework contains the following built-in log appenders
- File appender (`TLoggerProFileAppender`) (v1.0.0+)
- Console appender (`TLoggerProConsoleAppender`) (v1.0.0+)
- OutputDebugString appender (`TLoggerProOutputDebugStringAppender`) (v1.0.0+)
- VCL Memo appender (`TVCLMemoLogAppender`) (v1.0.0+)
- VCL ListView appender (`TVCLMemoLogAppender`) -- thanks to [https://github.com/he3p94uu](https://github.com/he3p94uu) (v1.3.0+)
- Redis Appender with LogsViewer(to aggregate logs from different instances on a single Redis instance) (v1.2.0+)
- Email appender (to send email as log, very useful for fatal errors) (v1.2.0+)
- SysLog appender [RFC 5424](https://tools.ietf.org/html/rfc5424) compliant -- thanks to [https://github.com/nurettin](https://github.com/nurettin) (v1.3.0+)
- [NSQ](https://nsq.io) appender (Thanks to [Fulgan](https://github.com/Fulgan)) (v1.3.0+)
- Decorator appender (Thanks to [Fulgan](https://github.com/Fulgan)) (v1.3.0+)

Next appenders in the development pipeline
- RESTful Appender (to send logs to a rest endpoint using a specific request format, so that you can implement log server in DelphiMVCFramework, PHP, Java, Python, Node etc)
- Twitter Appender (to send logs to a Twitter Account)
- Database appender (to send logs to a database table using FireDAC components -- Thank You Omar Bossoni)

The log writers and all the appenders are asycnhronous.

**Check the samples to see how to use each appender or even combine different appenders.**

## Documentation

Documentation is available in the `docs` folder as HTML.

## Other
You can install [Delphinus package manager](https://github.com/Memnarch/Delphinus/wiki/Installing-Delphinus) and install LoggerPro as a package there. (Delphinus-Support)
