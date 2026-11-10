# ExeWatch in a DelphiMVCFramework project

[ExeWatch](https://exewatch.com) collects logs, errors, timings and metrics from a running application. In
DMVCFramework it is **a LoggerPro appender**, configured like the other ones. The framework has no ExeWatch API of its
own: what is specific to ExeWatch is the ExeWatch SDK, called where you need it.

The appender (`LoggerPro.ExeWatchAppender`, in `lib/loggerpro`, outside the LoggerPro package) needs the ExeWatch SDK for
Delphi, which is not part of DMVCFramework. The setup of ExeWatch for RAD Studio installs it and sets `EXEWATCH_SDK`.

## With the project wizard

The "ExeWatch" box (Logging page, off in every preset) adds the appender to the generated logger:

```delphi
if dotEnv.Env('exewatch.apikey', '').StartsWith('ew_') then
  lBuilder := WithExeWatch(lBuilder)
    .WithAPIKey(dotEnv.Env('exewatch.apikey', ''))
    .WithCustomerId(dotEnv.Env('exewatch.customerid', 'MyProject'))
    .WithAppVersion(dotEnv.Env('exewatch.appversion', '1.0.0'))
    .Done;
```

- **The key goes in `bin\.env`** (`exewatch.apikey`). Without a valid key (it starts with `ew_`) ExeWatch stays off, the
  server runs as usual and the log says so. Before, the placeholder key made `InitializeExeWatch` raise and the server
  stopped at its first start.
- **`exewatch.customerid` is the name of the program** and `exewatch.appversion` is `1.0.0`. The customer id must not be
  empty: with an empty one the SDK keeps every event in memory and sends nothing.
- **JSON logging profile.** The appender is not a block of `loggerpro.json`: the code adds it after the file is read, and
  the key stays in `.env`. The reason is that the generated `.gitignore` excludes `.env` and not `loggerpro.json`: a key
  in the JSON file would be committed. (An ExeWatch block written by hand in the file still works: the unit registers the
  `ExeWatch` appender type.)
- **The SDK.** The form says whether it was found (library path or `EXEWATCH_SDK`). If only `EXEWATCH_SDK` leads to it,
  the new project gets `$(EXEWATCH_SDK)` in its unit search path. When the SDK is missing, or older than the one
  ExeWatch announces (`GET /api/v1/sdk/latest?type=delphi`, asked in the background only after the box is ticked,
  nothing about the user or the project is sent), the group links the download of the setup. The wizard does not run
  it: the setup stops when RAD Studio is open (it copies a package into the IDE), and the IDE reads `EXEWATCH_SDK` only
  when it starts. Close the IDE, run the setup, start the wizard again.

## What the appender sends

The log lines: text, level, tag, with the time and the thread of the line itself.

**An error carries its stack.** LoggerPro writes from a thread of its own, so the appender installs a capture hook
(`ILoggerProBuilder.WithCapture`, see `TLogItemCapture`) that runs on the thread that logs:

- an error logged while an exception is being handled (inside an `except` block, or with `LogException`) carries the
  stack **of the raise**: this is the case of an action that raises, which the engine logs;
- any other error carries the stack of the place that logged it, without the frames of LoggerPro.

One failure is one event: the engine's log line, with the stack as the structured `stack_trace` ExeWatch reads. Nothing
to add in your code. `WithExeWatch(...)` installs the hook; with `NewExeWatchAppender` add
`.WithCapture(ExeWatchCapture)` to the builder.

Its limits: the event has the text of the log line, not the class of the exception as a field of its own, and the
breadcrumbs of the request are not attached.

## What is specific to ExeWatch: the SDK, in your code

Add `ExeWatchSDKv1` to the unit and call `EW.xxx` where it is needed (they do nothing before the SDK is started: check
`ExeWatchIsInitialized`):

```delphi
EW.AddBreadcrumb('Search: ' + lQuery, 'people');
EW.StartTiming('people.search', 'database');
...
EW.EndTiming('people.search');
EW.IncrementCounter('orders.created');
```

`EW.ErrorWithException` is still there for an exception you want to send yourself with its own tag and message; an
exception you also log arrives twice.

## Things to know

- **Every event counts on your ExeWatch quota.** On a server with a lot of traffic set `TExeWatchConfig.SampleRate`
  (errors are always sent).
- **Readable stack traces** need the map file linked into the executable (`ExeWatchMapCLI`, or the ExeWatch menu of the
  IDE). Without it a frame reads `MyApp.exe!__dbk_fcall_wrapper+0x...`.
- **Never put secrets in what you send**: not in a message, a breadcrumb, a tag.
- **`SetDefaultLogger` is ignored when a default logger already exists** (`dotEnv` creates one the first time it logs).
  The generated project configures dotEnv without a logger and then the logger, in that order; do the same by hand.
- Tests: `ideexpert/tests` (`indydirect_exewatch`, `indydirect_exewatch_json`, `isapi_exewatch`, and the checks on the SDK
  with `TestTemplateGenerator.exe --exewatch-sdk-only`).
