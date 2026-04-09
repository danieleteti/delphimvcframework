// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit MVCFramework.Logger.ColorConsoleRenderer;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  LoggerPro;

type
  /// <summary>
  /// Gin-style color console renderer for DMVC logging.
  /// Produces two formats:
  ///   HTTP:  TIME | TID | STATUS | DURATION | IP | METHOD "PATH"
  ///   App:   TIME | TID | LEVEL  | MESSAGE
  /// When ANSI is not available, produces plain text (fallback for Win less than 10).
  /// </summary>
  TDMVCColorConsoleRenderer = class(TLogItemRenderer)
  private
    fFormatSettings: TFormatSettings;
    function GetContextValue(const aLogItem: TLogItem; const aKey: string): string;
    function IsHTTPLog(const aLogItem: TLogItem): Boolean;
    function StatusColor(const aStatusCode: Integer): string;
    function MethodColor(const aMethod: string): string;
    function LevelColor(const aLogType: TLogType): string;
    function LevelLabel(const aLogType: TLogType): string;
    function RenderHTTPANSI(const aLogItem: TLogItem): string;
    function RenderAppANSI(const aLogItem: TLogItem): string;
    function RenderPlain(const aLogItem: TLogItem): string;
  protected
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): string; override;
  end;

implementation

uses
  System.Rtti,
  MVCFramework.Console;

const
  DIM_SEP = Fore.DarkGray + ' | ' + Style.ResetAll;

function PadR(const S: string; Len: Integer): string;
begin
  Result := S;
  if Length(Result) < Len then
    Result := Result + StringOfChar(' ', Len - Length(Result));
end;

{ TDMVCColorConsoleRenderer }

procedure TDMVCColorConsoleRenderer.Setup;
begin
  EnableANSIColorConsole;
  fFormatSettings := TFormatSettings.Create;
end;

procedure TDMVCColorConsoleRenderer.TearDown;
begin
  // nothing
end;

function TDMVCColorConsoleRenderer.GetContextValue(const aLogItem: TLogItem; const aKey: string): string;
var
  I: Integer;
begin
  Result := '';
  if not aLogItem.HasContext then
    Exit;
  for I := 0 to High(aLogItem.Context) do
  begin
    if aLogItem.Context[I].Key = aKey then
    begin
      Result := aLogItem.Context[I].Value.ToString;
      Exit;
    end;
  end;
end;

function TDMVCColorConsoleRenderer.IsHTTPLog(const aLogItem: TLogItem): Boolean;
begin
  Result := GetContextValue(aLogItem, 'status') <> '';
end;

function TDMVCColorConsoleRenderer.StatusColor(const aStatusCode: Integer): string;
begin
  case aStatusCode div 100 of
    2: Result := Fore.White + Back.DarkGreen;     // Gin: 97;42
    3: Result := Fore.DarkGray + Back.Gray;       // Gin: 90;47
    4: Result := Fore.White + Back.DarkYellow;       // Gin: 90;43 (White for readability)
    5: Result := Fore.White + Back.DarkRed;       // Gin: 97;41
  else
    Result := Fore.White + Back.DarkGray;
  end;
end;

function TDMVCColorConsoleRenderer.MethodColor(const aMethod: string): string;
begin
  if aMethod = 'GET' then
    Result := Fore.White + Back.DarkBlue          // Gin: 97;44
  else if aMethod = 'POST' then
    Result := Fore.White + Back.DarkCyan          // Gin: 97;46
  else if aMethod = 'PUT' then
    Result := Fore.White + Back.DarkYellow            // Gin: 90;43 (White for readability)
  else if aMethod = 'DELETE' then
    Result := Fore.White + Back.DarkRed           // Gin: 97;41
  else if aMethod = 'PATCH' then
    Result := Fore.White + Back.DarkGreen         // Gin: 97;42
  else if aMethod = 'HEAD' then
    Result := Fore.White + Back.DarkMagenta       // Gin: 97;45
  else if aMethod = 'OPTIONS' then
    Result := Fore.DarkGray + Back.Gray           // Gin: 90;47
  else
    Result := Fore.White + Back.DarkGray;
end;

function TDMVCColorConsoleRenderer.LevelColor(const aLogType: TLogType): string;
begin
  // Option 3: Distinct colors that don't overlap with HTTP badges
  case aLogType of
    TLogType.Debug:   Result := Fore.DarkGray;
    TLogType.Info:    Result := Fore.Blue;
    TLogType.Warning: Result := Fore.Yellow;
    TLogType.Error:   Result := Style.Bright + Fore.Red;
    TLogType.Fatal:   Result := Fore.White + Back.DarkRed;
  else
    Result := Fore.White;
  end;
end;

function TDMVCColorConsoleRenderer.LevelLabel(const aLogType: TLogType): string;
begin
  case aLogType of
    TLogType.Debug:   Result := 'DEBUG';
    TLogType.Info:    Result := 'INFO ';
    TLogType.Warning: Result := 'WARN ';
    TLogType.Error:   Result := 'ERROR';
    TLogType.Fatal:   Result := 'FATAL';
  else
    Result := '?????';
  end;
end;

function TDMVCColorConsoleRenderer.RenderHTTPANSI(const aLogItem: TLogItem): string;
var
  lTime, lStatus, lMethod, lPath, lDuration, lIp, lExtra: string;
  lStatusCode: Integer;
begin
  lTime := FormatDateTime('hh:nn:ss', aLogItem.TimeStamp);
  lStatus := GetContextValue(aLogItem, 'status');
  lStatusCode := StrToIntDef(lStatus, 0);
  lMethod := GetContextValue(aLogItem, 'method');
  lPath := GetContextValue(aLogItem, 'path');
  lDuration := GetContextValue(aLogItem, 'duration');
  lIp := GetContextValue(aLogItem, 'ip');
  if lIp = '0:0:0:0:0:0:0:1' then
    lIp := '::1';
  lExtra := '';
  if aLogItem.LogMessage <> '' then
    lExtra := ' ' + Fore.DarkGray + aLogItem.LogMessage + Style.ResetAll;

  // Gin format: [GIN] %v |%s %3d %s|%s %8v %s| %15s |%s %-7s %s %#v
  Result :=
    Fore.DarkGray + lTime + Style.ResetAll +
    DIM_SEP +
    Fore.DarkGray + Format('%5d', [aLogItem.ThreadID]) + Style.ResetAll +
    ' ' + Fore.DarkGray + '|' + Style.ResetAll +
    StatusColor(lStatusCode) + Format(' %d ', [lStatusCode]) + Style.ResetAll +
    Fore.DarkGray + '|' + Style.ResetAll +
    Fore.DarkGray + Format('%8s', [lDuration]) + Style.ResetAll +
    Fore.DarkGray + ' |' + Style.ResetAll +
    ' ' + Fore.DarkGray + PadR(lIp, 15) + Style.ResetAll +
    ' ' + Fore.DarkGray + '|' + Style.ResetAll +
    MethodColor(lMethod) + ' ' + PadR(lMethod, 7) + ' ' + Style.ResetAll +
    ' ' + Fore.White + '"' + lPath + '"' + Style.ResetAll +
    lExtra;
end;

function TDMVCColorConsoleRenderer.RenderAppANSI(const aLogItem: TLogItem): string;
var
  lTime: string;
begin
  lTime := FormatDateTime('hh:nn:ss', aLogItem.TimeStamp);

  Result :=
    Fore.DarkGray + lTime + Style.ResetAll +
    DIM_SEP +
    Fore.DarkGray + Format('%5d', [aLogItem.ThreadID]) + Style.ResetAll +
    ' ' + Fore.DarkGray + '|' + Style.ResetAll +
    LevelColor(aLogItem.LogType) + LevelLabel(aLogItem.LogType) + Style.ResetAll +
    Fore.DarkGray + '|' + Style.ResetAll +
    ' ' + aLogItem.LogMessage;
end;

function TDMVCColorConsoleRenderer.RenderPlain(const aLogItem: TLogItem): string;
var
  lTime: string;
begin
  // Fallback: same format as TLogItemRendererNoTag for backward compatibility
  lTime := DateTimeToStr(aLogItem.TimeStamp, fFormatSettings);
  Result := Format('%s[TID %d][%s] %s', [
    lTime,
    aLogItem.ThreadID,
    aLogItem.LogTypeAsString,
    aLogItem.LogMessage
  ]);
end;

function TDMVCColorConsoleRenderer.RenderLogItem(const aLogItem: TLogItem): string;
begin
  if IsANSIColorConsoleEnabled then
  begin
    if IsHTTPLog(aLogItem) then
      Result := RenderHTTPANSI(aLogItem)
    else
      Result := RenderAppANSI(aLogItem);
  end
  else
    Result := RenderPlain(aLogItem);
end;

end.
