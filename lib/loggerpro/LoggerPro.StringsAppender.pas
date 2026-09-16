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

unit LoggerPro.StringsAppender;

{ Appends formatted log items to any TStrings instance (TStringList, TMemo.Lines, etc.)
  The TStrings instance is NOT owned by this appender. The caller is
  responsible for its lifetime and must ensure it outlives the ILogWriter. }

interface

uses
  LoggerPro,
  System.Classes;

type
  { @abstract(Appends formatted @link(TLogItem) to a TStrings instance.
    Cross-platform: works with TStringList, TMemo.Lines, TRichEdit.Lines, etc.
    Default: MaxLogLines = 100, ClearOnStartup = False.) }
  TStringsLogAppender = class(TLoggerProAppenderBase)
  private
    FStrings: TStrings;
    FMaxLogLines: Word;
    FClearOnStartup: Boolean;
  public
    { Default maximum number of log lines retained before oldest is deleted. }
    const DEFAULT_MAX_LOG_LINES: Word = 100;
    { aMaxLogLines must be > 0. The TStrings instance is not owned here. }
    constructor Create(aStrings: TStrings;
      aMaxLogLines: Word = 100;
      aClearOnStartup: Boolean = False;
      aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    { Nils the TStrings reference so any TThread.Queue closures still pending
      on the main-thread queue exit safely. Does NOT free the TStrings. }
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils;

{ TStringsLogAppender }

constructor TStringsLogAppender.Create(aStrings: TStrings; aMaxLogLines: Word;
  aClearOnStartup: Boolean; aLogItemRenderer: ILogItemRenderer);
begin
  if aMaxLogLines = 0 then
    raise EArgumentOutOfRangeException.Create('aMaxLogLines must be greater than zero');
  inherited Create(aLogItemRenderer);
  FStrings := aStrings;
  FMaxLogLines := aMaxLogLines;
  FClearOnStartup := aClearOnStartup;
end;

procedure TStringsLogAppender.Setup;
begin
  inherited;
  if FClearOnStartup then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        FStrings.Clear;
      end);
  end;
end;

procedure TStringsLogAppender.TearDown;
begin
  // Nil the reference so any TThread.Queue closures still pending on the
  // main-thread message queue will safely exit without accessing freed memory.
  // The TStrings instance is not owned here and must not be freed.
  FStrings := nil;
end;

procedure TStringsLogAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
  lStrings: TStrings;
begin
  // Snapshot the reference: TearDown may nil FStrings concurrently
  lStrings := FStrings;
  if not Assigned(lStrings) then
    Exit;
  lText := FormatLog(aLogItem);
  TThread.Queue(nil,
    procedure
    begin
      // Re-check: the TStrings may have been freed between WriteLog and execution
      if not Assigned(lStrings) then
        Exit;
      lStrings.BeginUpdate;
      try
        // Use >= to handle pre-populated lists that already exceed MaxLogLines
        while lStrings.Count >= FMaxLogLines do
          lStrings.Delete(0);
        lStrings.Add(lText);
      finally
        lStrings.EndUpdate;
      end;
    end);
end;

end.
