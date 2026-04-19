// ***************************************************************************
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
// ***************************************************************************

unit LoggerPro.HTMLFileAppender;

{ Self-contained HTML log file appender with rotation.

  Inherits from TLoggerProSimpleFileAppender, so size/time-based rotation,
  numbered backups and retention are shared with the rest of LoggerPro's
  file appenders (File, JSONLFile, TimeRotatingFile). The only HTML-specific
  behavior is:
    - UTF-8 encoding (forced).
    - .html filename format.
    - A custom renderer that emits <tr> rows.
    - EmitStart / EmitEnd overrides that write the HTML head/foot + sentinel.

  The sentinel (<span id="filecompleted"></span>) is written on clean
  close / rotation. Its presence tells the browser-side script that the
  file is finalized; its absence means the logger is still writing, in
  which case the page enables a LIVE badge and periodic auto-reload. }

interface

uses
  LoggerPro,
  LoggerPro.FileAppender,
  System.Classes,
  System.SysUtils;

type
  TLoggerProHTMLFileAppender = class(TLoggerProSimpleFileAppender)
  private
    FTitle: string;
    FSetupDone: Boolean;
  protected
    procedure EmitStartRotateLogItem(aWriter: TStreamWriter); override;
    procedure EmitEndRotateLogItem(aWriter: TStreamWriter); override;
  public
    const DEFAULT_FILENAME_FORMAT = '{module}.{number}.html';

    constructor Create(
      const aTitle: string = 'LoggerPro';
      aMaxBackupFileCount: Integer = TLoggerProFileAppenderBase.DEFAULT_MAX_BACKUP_FILE_COUNT;
      aMaxFileSizeInKiloByte: Integer = TLoggerProFileAppenderBase.DEFAULT_MAX_FILE_SIZE_KB;
      const aLogsFolder: string = '';
      const aLogFileNameFormat: string = DEFAULT_FILENAME_FORMAT;
      aRotationInterval: TTimeRotationInterval = TTimeRotationInterval.None;
      aMaxRetainedFiles: Integer = 0); reintroduce;

    procedure Setup; override;
    procedure TearDown; override;

    /// <summary>Page title shown in the browser tab and the H1 header.
    /// Can be changed at any time; takes effect on the next file that
    /// is opened (current run / next rotation).</summary>
    property Title: string read FTitle write FTitle;
  end;

{ HTML-escape a string. Handles &, <, >, ", '. Public so custom renderers
  can reuse it. }
function HtmlEscape(const s: string): string;

implementation

uses
  System.Rtti,
  System.TypInfo,
  System.IOUtils,
  System.DateUtils;

const
  // Polling interval for live reload (milliseconds). Chosen so the
  // page feels responsive without hammering the disk.
  LIVE_RELOAD_MS = 3000;

  HTML_HEAD_TEMPLATE =
    '<!DOCTYPE html>'#10 +
    '<html lang="en">'#10 +
    '<head>'#10 +
    '<meta charset="UTF-8">'#10 +
    // No <meta http-equiv="refresh"> here on purpose. Browsers schedule
    // the meta-refresh timer at parse time and removing the element from
    // the DOM does NOT cancel that pending navigation (verified on
    // Chrome). That means once the file becomes FINALIZED there is no
    // way to stop the loop from JS. Instead the defer'd script below
    // schedules its own setTimeout, which is trivially cancellable -
    // and which we simply skip altogether when the FINALIZED sentinel
    // is already present in the DOM.
    '<title>$TITLE$</title>'#10 +
    '<style>'#10 +
    '  :root { color-scheme: dark; }'#10 +
    '  * { box-sizing: border-box; }'#10 +
    '  body { margin: 0; font-family: -apple-system, Segoe UI, Roboto, Ubuntu, sans-serif; background: #1a1a1a; color: #ddd; font-size: 13px; }'#10 +
    '  header { background: #0d1117; color: #fff; padding: 14px 20px; }'#10 +
    '  header h1 { margin: 0 0 8px 0; font-size: 18px; font-weight: 600; }'#10 +
    '  header .meta { font-size: 11px; color: #8b949e; }'#10 +
    '  header .meta a { color: #58a6ff; text-decoration: none; }'#10 +
    '  header .meta a:hover { text-decoration: underline; }'#10 +
    // Wrap header + filter bar in ONE sticky container: the entire block
    // (title, meta link, filter bar) stays pinned to the top while rows
    // scroll under it. Single sticky element avoids the "cascading
    // sticky" layout bugs (hardcoded top offsets drift when fonts,
    // zoom or content heights change).
    '  .app-bar { position: sticky; top: 0; z-index: 10; background: #0d1117; border-bottom: 2px solid #30363d; }'#10 +
    '  #filters { background: #21262d; padding: 10px 20px; border-top: 1px solid #30363d; display: flex; gap: 16px; align-items: center; flex-wrap: wrap; }'#10 +
    '  #filters label { display: inline-flex; align-items: center; gap: 4px; cursor: pointer; user-select: none; }'#10 +
    '  #filters input[type="text"] { flex: 1; min-width: 200px; background: #0d1117; color: #ddd; border: 1px solid #30363d; border-radius: 4px; padding: 6px 10px; font-family: inherit; font-size: 12px; }'#10 +
    '  #filters .count { margin-left: auto; color: #8b949e; font-size: 11px; }'#10 +
    '  table { width: 100%; border-collapse: collapse; font-family: SFMono-Regular, Consolas, Liberation Mono, Menlo, monospace; font-size: 12px; table-layout: fixed; }'#10 +
    '  col.c-ts  { width: 180px; }'#10 +
    '  col.c-lvl { width:  80px; }'#10 +
    '  col.c-tid { width:  70px; }'#10 +
    '  col.c-tag { width: 120px; }'#10 +
    '  thead th { background: #2a2f37; color: #c9d1d9; text-align: left; padding: 6px 10px; border-bottom: 1px solid #30363d; font-weight: 600; }'#10 +
    '  tbody tr { border-bottom: 1px solid #262b33; }'#10 +
    '  tbody tr:hover { background: #1f242c; }'#10 +
    '  tbody td { padding: 5px 10px; vertical-align: top; }'#10 +
    '  td.msg { white-space: pre-wrap; overflow-wrap: break-word; }'#10 +
    '  td.ts { color: #8b949e; white-space: nowrap; }'#10 +
    '  td.tid { color: #8b949e; text-align: right; white-space: nowrap; }'#10 +
    '  td.tag { color: #d2a8ff; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }'#10 +
    '  td.lvl { font-weight: 600; text-transform: uppercase; letter-spacing: 0.3px; white-space: nowrap; }'#10 +
    '  tr.L-DEBUG   td.lvl { color: #8b949e; }'#10 +
    '  tr.L-INFO    td.lvl { color: #7ee787; }'#10 +
    '  tr.L-WARNING td.lvl { color: #d29922; }'#10 +
    '  tr.L-ERROR   td.lvl { color: #f85149; }'#10 +
    '  tr.L-FATAL   td.lvl { color: #ff9ae0; }'#10 +
    '  tr.L-WARNING:not(.ctx-row) { background: rgba(210, 153, 34, 0.08); }'#10 +
    '  tr.L-ERROR:not(.ctx-row)   { background: rgba(248, 81, 73, 0.10); }'#10 +
    '  tr.L-FATAL:not(.ctx-row)   { background: rgba(255, 154, 224, 0.10); }'#10 +
    '  tr.ctx-row > td { border-top: none; padding: 0 10px 6px 28px; font-size: 11px; color: #58a6ff; white-space: pre-wrap; overflow-wrap: break-word; }'#10 +
    '  tr.ctx-row > td b { color: #7ee787; font-weight: normal; }'#10 +
    '  tr.ctx-row > td::before { content: "\21B3"; margin-right: 6px; color: #484f58; }'#10 +
    '  tr.hidden { display: none; }'#10 +
    '  .btn { background: #30363d; color: #c9d1d9; border: 1px solid #484f58; border-radius: 4px; padding: 5px 10px; font: inherit; font-size: 11px; cursor: pointer; }'#10 +
    '  .btn:hover { background: #3e464f; }'#10 +
    '  .live-badge { display: inline-block; margin-left: 8px; padding: 2px 8px; border-radius: 10px; font-size: 10px; font-weight: 600; letter-spacing: 0.5px; vertical-align: middle; }'#10 +
    '  .live-badge.live { background: #d29922; color: #000; animation: lp-pulse 2s infinite; }'#10 +
    '  .live-badge.finalized { background: #30363d; color: #8b949e; border: 1px solid #484f58; }'#10 +
    '  @keyframes lp-pulse { 0%,100% { opacity: 1; } 50% { opacity: 0.55; } }'#10 +
    '</style>'#10 +
    '<script defer>'#10 +
    '(() => {'#10 +
    // Disable browser scroll restoration IMMEDIATELY (synchronously, before
    // any layout or paint). Browsers restore the previous scroll position
    // *after* DOMContentLoaded, which would undo our scroll-to-bottom.
    // Setting this flag here wins the race.
    '  if ("scrollRestoration" in history) history.scrollRestoration = "manual";'#10 +
    // <script defer> guarantees the document is fully parsed before this
    // runs, so the FINALIZED sentinel (if present) is already in the DOM.
    // Read it ONCE here and use the same value for every downstream
    // decision (reload scheduling, badge, scroll-to-bottom).
    '  const isClosed = !!document.getElementById("filecompleted");'#10 +
    // LIVE: schedule a JS-driven reload. We use location.replace(href)
    // rather than location.reload() because Chrome blocks reload() on
    // file:// origins ("Unsafe attempt to load URL ... unique security
    // origin"); a same-URL navigation via replace() is not subject to
    // that policy. FINALIZED: simply never schedule the timer - so the
    // loop stops the moment the writer closes the file cleanly.
    '  if (!isClosed) setTimeout(() => location.replace(location.href), $RELOAD_MS$);'#10 +
    '  const onReady = () => {'#10 +
    '    const q = document.getElementById("q");'#10 +
    '    const rows = document.getElementById("rows");'#10 +
    '    const count = document.getElementById("count");'#10 +
    '    const lvls = document.querySelectorAll(".f-lvl");'#10 +
    '    const badge = document.getElementById("live-badge");'#10 +
    // LIVE: always scroll to the bottom on every (re)load - this is a
    // tail-following view, the latest log line is what matters. We defer
    // the scroll to window.load (+ rAF) so all rows, fonts and the sticky
    // app-bar have their final dimensions; document.body.scrollHeight at
    // DOMContentLoaded is often stale. documentElement.scrollHeight is
    // more reliable than body.scrollHeight when sticky elements are in play.
    // FINALIZED: leave the natural top position; the user is reading
    // historical content and choosing where to look themselves.
    '    if (!isClosed) {'#10 +
    '      const scrollToBottom = () => window.scrollTo(0, document.documentElement.scrollHeight);'#10 +
    '      if (document.readyState === "complete") requestAnimationFrame(scrollToBottom);'#10 +
    '      else window.addEventListener("load", () => requestAnimationFrame(scrollToBottom), { once: true });'#10 +
    '    }'#10 +
    '    if (badge) {'#10 +
    '      if (isClosed) { badge.textContent = "FINALIZED"; badge.className = "live-badge finalized"; }'#10 +
    '      else { badge.textContent = "LIVE"; badge.className = "live-badge live"; }'#10 +
    '    }'#10 +
    '    const apply = () => {'#10 +
    '      const active = Array.from(lvls).filter(c => c.checked).map(c => c.value);'#10 +
    '      const needle = (q.value || "").toLowerCase();'#10 +
    '      let shown = 0, total = 0;'#10 +
    '      rows.querySelectorAll("tr:not(.ctx-row)").forEach(tr => {'#10 +
    '        total++;'#10 +
    '        const lvl = tr.dataset.lvl || "";'#10 +
    '        const ctx = tr.nextElementSibling;'#10 +
    '        let ok = active.includes(lvl);'#10 +
    '        if (ok && needle) {'#10 +
    '          let txt = tr.textContent;'#10 +
    '          if (ctx && ctx.classList.contains("ctx-row")) txt += " " + ctx.textContent;'#10 +
    '          ok = txt.toLowerCase().includes(needle);'#10 +
    '        }'#10 +
    '        tr.classList.toggle("hidden", !ok);'#10 +
    '        if (ctx && ctx.classList.contains("ctx-row")) ctx.classList.toggle("hidden", !ok);'#10 +
    '        if (ok) shown++;'#10 +
    '      });'#10 +
    '      count.textContent = shown + " / " + total;'#10 +
    '    };'#10 +
    '    q.addEventListener("input", apply);'#10 +
    '    lvls.forEach(c => c.addEventListener("change", apply));'#10 +
    '    const collectVisible = () => {'#10 +
    '      const out = [];'#10 +
    '      rows.querySelectorAll("tr:not(.ctx-row):not(.hidden)").forEach(tr => {'#10 +
    '        const cells = tr.querySelectorAll("td");'#10 +
    '        const next = tr.nextElementSibling;'#10 +
    '        const ctxTxt = (next && next.classList.contains("ctx-row")) ? next.textContent.trim() : "";'#10 +
    '        out.push({ timestamp: cells[0].textContent, level: cells[1].textContent, thread: cells[2].textContent, tag: cells[3].textContent, message: cells[4].textContent, context: ctxTxt });'#10 +
    '      });'#10 +
    '      return out;'#10 +
    '    };'#10 +
    '    const download = (data, mime, ext) => {'#10 +
    '      const blob = new Blob([data], {type: mime});'#10 +
    '      const url = URL.createObjectURL(blob);'#10 +
    '      const a = document.createElement("a");'#10 +
    '      const stamp = new Date().toISOString().replace(/[:.]/g, "-");'#10 +
    '      a.href = url; a.download = "loggerpro-" + stamp + "." + ext;'#10 +
    '      document.body.appendChild(a); a.click();'#10 +
    '      setTimeout(() => { URL.revokeObjectURL(url); a.remove(); }, 100);'#10 +
    '    };'#10 +
    '    const csvEscape = s => { s = String(s == null ? "" : s); return /[",\r\n]/.test(s) ? ''"'' + s.replace(/"/g,''""'') + ''"'' : s; };'#10 +
    '    const jBtn = document.getElementById("exp-json");'#10 +
    '    if (jBtn) jBtn.addEventListener("click", () => { download(JSON.stringify(collectVisible(), null, 2), "application/json", "json"); });'#10 +
    '    const cBtn = document.getElementById("exp-csv");'#10 +
    '    if (cBtn) cBtn.addEventListener("click", () => {'#10 +
    '      const data = collectVisible();'#10 +
    '      const header = ["timestamp","level","thread","tag","message","context"];'#10 +
    '      const lines = [header.join(",")];'#10 +
    '      data.forEach(r => lines.push(header.map(h => csvEscape(r[h])).join(",")));'#10 +
    '      download(lines.join("\r\n"), "text/csv;charset=utf-8", "csv");'#10 +
    '    });'#10 +
    '    apply();'#10 +
    '  };'#10 +
    '  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", onReady); else onReady();'#10 +
    '})();'#10 +
    '</script>'#10 +
    '</head>'#10 +
    '<body>'#10 +
    '<div class="app-bar">'#10 +
    '<header>'#10 +
    '  <h1>$TITLE$<span id="live-badge" class="live-badge"></span></h1>'#10 +
    '  <div class="meta">Generated by <a href="https://www.danieleteti.it/loggerpro/" target="_blank" rel="noopener">LoggerPro</a> &middot; Opened $GENERATED$</div>'#10 +
    '</header>'#10 +
    '<div id="filters">'#10 +
    '  <label><input type="checkbox" class="f-lvl" value="DEBUG"   checked> Debug</label>'#10 +
    '  <label><input type="checkbox" class="f-lvl" value="INFO"    checked> Info</label>'#10 +
    '  <label><input type="checkbox" class="f-lvl" value="WARNING" checked> Warning</label>'#10 +
    '  <label><input type="checkbox" class="f-lvl" value="ERROR"   checked> Error</label>'#10 +
    '  <label><input type="checkbox" class="f-lvl" value="FATAL"   checked> Fatal</label>'#10 +
    '  <input type="text" id="q" placeholder="Search text or tag...">'#10 +
    '  <button class="btn" id="exp-csv" title="Download visible rows as CSV">CSV</button>'#10 +
    '  <button class="btn" id="exp-json" title="Download visible rows as JSON">JSON</button>'#10 +
    '  <span class="count" id="count">0 / 0</span>'#10 +
    '</div>'#10 +
    '</div>'#10 +  // closes .app-bar
    '<table>'#10 +
    '<colgroup><col class="c-ts"><col class="c-lvl"><col class="c-tid"><col class="c-tag"><col></colgroup>'#10 +
    '<thead><tr><th>Timestamp</th><th>Level</th><th>TID</th><th>Tag</th><th>Message</th></tr></thead>'#10 +
    '<tbody id="rows">'#10;

  HTML_FOOT =
    '</tbody>'#10 +
    '</table>'#10 +
    // Sentinel marking "file is finalized" - defer'd script in <head>
    // checks for this element to decide LIVE vs FINALIZED mode.
    '<span id="filecompleted"></span>'#10 +
    '</body>'#10 +
    '</html>'#10;

function HtmlEscape(const s: string): string;
var
  i: Integer;
  c: Char;
  sb: TStringBuilder;
begin
  if s = '' then
    Exit('');
  sb := TStringBuilder.Create(Length(s) + 16);
  try
    for i := 1 to Length(s) do
    begin
      c := s[i];
      case c of
        '&': sb.Append('&amp;');
        '<': sb.Append('&lt;');
        '>': sb.Append('&gt;');
        '"': sb.Append('&quot;');
        '''': sb.Append('&#39;');
      else
        sb.Append(c);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

type
  TLogItemRendererHTML = class(TLogItemRenderer)
  protected
    function RenderLogItem(const aLogItem: TLogItem): String; override;
    function RenderContextHtml(const aLogItem: TLogItem): string;
  end;

{ TLogItemRendererHTML }

function TLogItemRendererHTML.RenderContextHtml(const aLogItem: TLogItem): string;
var
  i: Integer;
  lParam: LogParam;
  lValue: string;
  sb: TStringBuilder;
begin
  if (not aLogItem.HasContext) and (aLogItem.PreRenderedContext = '') then
    Exit('');
  if aLogItem.PreRenderedContext <> '' then
    Exit(HtmlEscape(aLogItem.PreRenderedContext));

  sb := TStringBuilder.Create;
  try
    for i := 0 to High(aLogItem.Context) do
    begin
      lParam := aLogItem.Context[i];
      case lParam.Value.Kind of
        tkInteger, tkInt64:
          lValue := lParam.Value.AsInt64.ToString;
        tkFloat:
          if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
            lValue := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
          else
            lValue := FloatToStr(lParam.Value.AsExtended);
        tkEnumeration:
          if lParam.Value.TypeInfo = TypeInfo(Boolean) then
            lValue := BoolToStr(lParam.Value.AsBoolean, True).ToLower
          else
            lValue := lParam.Value.ToString;
      else
        lValue := lParam.Value.ToString;
      end;
      if i > 0 then
        sb.Append(' ');
      sb.Append('<b>').Append(HtmlEscape(lParam.Key)).Append('</b>=').Append(HtmlEscape(lValue));
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TLogItemRendererHTML.RenderLogItem(const aLogItem: TLogItem): String;
var
  lLevel, lTs, lTid, lTag, lMsg, lCtx: string;
begin
  lLevel := aLogItem.LogTypeAsString;
  lTs    := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', aLogItem.TimeStamp);
  lTid   := IntToStr(aLogItem.ThreadID);
  lTag   := HtmlEscape(aLogItem.LogTag);
  lMsg   := HtmlEscape(aLogItem.LogMessage);
  lCtx   := RenderContextHtml(aLogItem);

  Result :=
    '<tr class="L-' + lLevel + '" data-lvl="' + lLevel + '">' +
    '<td class="ts">' + lTs + '</td>' +
    '<td class="lvl">' + lLevel + '</td>' +
    '<td class="tid">' + lTid + '</td>' +
    '<td class="tag">' + lTag + '</td>' +
    '<td class="msg">' + lMsg + '</td>' +
    '</tr>';
  // When context is present, emit a second row below so it wraps
  // naturally with full page width.
  if lCtx <> '' then
    Result := Result + #10 +
      '<tr class="ctx-row L-' + lLevel + '" data-lvl="' + lLevel + '">' +
      '<td colspan="5">' + lCtx + '</td>' +
      '</tr>';
end;

{ TLoggerProHTMLFileAppender }

constructor TLoggerProHTMLFileAppender.Create(const aTitle: string;
  aMaxBackupFileCount, aMaxFileSizeInKiloByte: Integer;
  const aLogsFolder, aLogFileNameFormat: string;
  aRotationInterval: TTimeRotationInterval;
  aMaxRetainedFiles: Integer);
begin
  inherited Create(
    aMaxBackupFileCount,
    aMaxFileSizeInKiloByte,
    aLogsFolder,
    aLogFileNameFormat,
    TLogItemRendererHTML.Create,
    TEncoding.UTF8,
    aRotationInterval,
    aMaxRetainedFiles);
  if aTitle = '' then
    FTitle := 'LoggerPro'
  else
    FTitle := aTitle;
  FSetupDone := False;
end;

procedure TLoggerProHTMLFileAppender.Setup;
var
  lCurrentName, lNewFileName: string;
  lPreexisting: Boolean;
begin
  // Detect pre-existing file BEFORE parent's Setup, which would open
  // (and maybe create) it. An HTML document is a single unit:
  // concatenating multiple runs in one file would stack
  // <html>...</html> sections whose sticky bars and script blocks
  // overlap. If the current file already exists with content from a
  // prior run, rotate it to a numbered backup so this run starts fresh.
  lCurrentName := GetLogFileName('', 0);
  lPreexisting := TFile.Exists(lCurrentName) and (TFile.GetSize(lCurrentName) > 0);

  inherited; // parent opens fFileWriter on the current file

  if lPreexisting then
  begin
    FreeAndNil(fFileWriter);
    RotateFile('', lNewFileName);
    fFileWriter := CreateWriter(GetLogFileName('', 0));
  end;

  EmitStartRotateLogItem(fFileWriter);
  FSetupDone := True;
end;

procedure TLoggerProHTMLFileAppender.TearDown;
begin
  // Parent's TearDown flushes + frees the writer WITHOUT writing any
  // end-of-file marker. For HTML we MUST write the sentinel so the
  // browser detects the file as finalized (otherwise it would stay in
  // LIVE mode forever, periodically reloading).
  if FSetupDone and (fFileWriter <> nil) then
    EmitEndRotateLogItem(fFileWriter);
  inherited;
end;

procedure TLoggerProHTMLFileAppender.EmitStartRotateLogItem(aWriter: TStreamWriter);
var
  lHeader: string;
begin
  lHeader := HTML_HEAD_TEMPLATE;
  lHeader := StringReplace(lHeader, '$TITLE$', HtmlEscape(FTitle), [rfReplaceAll]);
  lHeader := StringReplace(lHeader, '$GENERATED$',
    HtmlEscape(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)), [rfReplaceAll]);
  // setTimeout takes milliseconds.
  lHeader := StringReplace(lHeader, '$RELOAD_MS$',
    IntToStr(LIVE_RELOAD_MS), [rfReplaceAll]);
  aWriter.Write(lHeader);
  aWriter.Flush;
end;

procedure TLoggerProHTMLFileAppender.EmitEndRotateLogItem(aWriter: TStreamWriter);
begin
  aWriter.Write(HTML_FOOT);
  aWriter.Flush;
end;

end.
