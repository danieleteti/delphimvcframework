# LoggerPro for Delphi

<p align="center">
  <img src="loggerpro_logo.png" alt="LoggerPro Logo" width="300"/>
</p>

<h3 align="center">The Modern, Async, Pluggable Logging Framework for Delphi</h3>

<p align="center">
  <a href="https://github.com/danieleteti/loggerpro/releases/tag/v2.1.0"><img src="https://img.shields.io/badge/version-2.1.0-brightgreen.svg" alt="Version"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="License"></a>
  <a href="https://www.embarcadero.com/products/delphi"><img src="https://img.shields.io/badge/Delphi-10.3%20to%2013-orange.svg" alt="Delphi"></a>
  <a href="https://www.danieleteti.it/loggerpro/"><img src="https://img.shields.io/badge/docs-danieleteti.it%2Floggerpro-green.svg" alt="Documentation"></a>
  <a href="https://github.com/HashLoad/boss"><img src="https://img.shields.io/badge/boss-install-purple.svg" alt="BOSS"></a>
</p>

---

<h2 align="center">
  📖 Full documentation, tutorials, and guides live on the blog:<br><br>
  <a href="https://www.danieleteti.it/loggerpro/">https://www.danieleteti.it/loggerpro/</a>
</h2>

---

## What is LoggerPro

Async, pluggable, production-proven logging framework for Delphi. Used in
thousands of applications worldwide since 2010.

- **Async by design** - non-blocking, zero impact on your app's hot path
- **20+ built-in appenders** - File, Console, HTTP, Syslog, ElasticSearch, Database, Email, Windows Event Log, ...
- **Fluent Builder API** - Serilog-style configuration
- **Structured logging** - first-class `LogParam` context
- **Cross-platform** - Windows, Linux, macOS, Android, iOS
- **Thread-safe**, **DLL-safe**, Apache 2.0

## What's New in 2.1

- 🆕 **JSON configuration** - reshape your logger at deploy time without rebuilding
- 🆕 **HTML live log viewer** - self-contained `.html` with filters, search, export
- 🆕 **Pluggable appenders** - just `uses LoggerPro.ExeWatchAppender` to enable the type in JSON
- 🆕 **LogFmt renderer** - spec-compliant `key=value` output for Loki, humanlog, ripgrep
- 🆕 **FileBySource appender** - per-tenant subfolders with day+size rotation
- 🆕 **Runtime log level** - change the global gate on the fly via `ILogWriter.MinimumLevel`
- 🆕 **UTF-8 console output** - correct Unicode in Docker and Windows consoles
- 🆕 **DLL-safe init** - fixes the Windows Loader Lock deadlock
- 🆕 **ElasticSearch auth** - Basic / API Key / Bearer Token
- 🆕 **UDP Syslog local time** option
- 🆕 **`GetCurrentLogFileName` API** on file appenders

**[Read the full 2.1 guide →](https://www.danieleteti.it/loggerpro/)**

## Quick Taste

```delphi
uses
  LoggerPro, LoggerPro.Builder;

var
  Log: ILogWriter;
begin
  Log := LoggerProBuilder
    .WithDefaultTag('MYAPP')
    .WriteToFile.WithLogsFolder('logs').Done
    .WriteToConsole.Done
    .Build;

  Log.Info('Application started');
  Log.Info('Order placed', 'ORDERS', [
    LogParam.I('order_id', 42),
    LogParam.F('amount', 99.95)
  ]);
end.
```

For the full API, all appenders, LogFmt querying on Windows, live tailing,
Docker/DLL guidance, Windows Service integration, and the FAQ:
👉 **[www.danieleteti.it/loggerpro](https://www.danieleteti.it/loggerpro/)**

## Install

### Manual

Add the LoggerPro source folder to your Delphi Library Path.

### BOSS

```bash
boss install github.com/danieleteti/loggerpro
```

### Delphinus

Search for "LoggerPro" in the Delphinus package manager.

## Requirements

**Minimum Delphi version:** 10.3 Rio. **Tested on:** 10.3, 10.4, 11, 12, 13.
**Platforms:** Windows, Linux, macOS, Android, iOS.

## Sample Projects

25+ working examples in the `samples/` folder - one per appender and use case.

## License

Apache License 2.0 - free for personal and commercial use.

## Author

**Daniele Teti**

- Blog & docs: [danieleteti.it](https://www.danieleteti.it) · [danieleteti.it/loggerpro](https://www.danieleteti.it/loggerpro/)
- Twitter/X: [@danieleteti](https://twitter.com/danieleteti)

---

<p align="center">
  <b>LoggerPro - Professional Logging for Professional Delphi Developers</b><br>
  <a href="https://www.danieleteti.it/loggerpro/">📖 Full documentation →</a>
</p>
