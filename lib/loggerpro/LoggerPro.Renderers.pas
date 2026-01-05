// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
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

unit LoggerPro.Renderers;

interface

uses
  LoggerPro, System.SysUtils;

type
  TLogItemRendererDefault = class(TLogItemRenderer)
  private
    fInternalLogFormat: string;
    fInternalFormatSettings: TFormatSettings;
    fInitialized: Boolean;
  protected
    property InternalLogFormat: String read fInternalLogFormat;
    property InternalFormatSettings: TFormatSettings read fInternalFormatSettings;
    function GetLogLayoutWithPlaceHolders: String; virtual;
    function RenderContext(const aLogItem: TLogItem): String; virtual;
    // ILogLayoutRenderer
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoTag = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoThreadID = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoTagNoThreadID = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererLogFmt = class(TLogItemRenderer)
  private
    fFormatSettings: TFormatSettings;
  protected
    // ILogLayoutRenderer
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;


function GetDefaultLogItemRenderer: ILogItemRenderer;

var
  gDefaultLogItemRenderer: TLogItemRendererClass = TLogItemRendererDefault;

implementation

uses
  System.DateUtils,
  System.Rtti,
  System.TypInfo;

function GetDefaultLogItemRenderer: ILogItemRenderer;
begin
  Result := gDefaultLogItemRenderer.Create;
end;

{ TLogItemRendererDefault }

procedure TLogItemRendererDefault.TearDown;
begin
  // do nothing
end;

procedure TLogItemRendererDefault.Setup;
begin
  if not fInitialized then
  begin
    fInternalFormatSettings := GetDefaultFormatSettings;
    fInternalLogFormat := LogLayoutByPlaceHoldersToLogLayoutByIndexes(GetLogLayoutWithPlaceHolders, True);
    fInitialized := True;
  end;
end;

function TLogItemRendererDefault.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[TID {threadid}][{loglevel}] {message} [{tag}]';
end;

function TLogItemRendererDefault.RenderContext(const aLogItem: TLogItem): String;
var
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
begin
  // Use pre-rendered context if available (optimization for fixed context)
  if aLogItem.PreRenderedContext <> '' then
  begin
    Result := aLogItem.PreRenderedContext;
    Exit;
  end;

  Result := '';
  if not aLogItem.HasContext then
    Exit;

  for I := 0 to High(aLogItem.Context) do
  begin
    lParam := aLogItem.Context[I];
    case lParam.Value.Kind of
      tkInteger, tkInt64:
        lValueStr := lParam.Value.AsInt64.ToString;
      tkFloat:
        if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
          lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
        else
          lValueStr := FloatToStr(lParam.Value.AsExtended, InternalFormatSettings);
      tkEnumeration:
        if lParam.Value.TypeInfo = TypeInfo(Boolean) then
          lValueStr := BoolToStr(lParam.Value.AsBoolean, True).ToLower
        else
          lValueStr := lParam.Value.ToString;
    else
      lValueStr := lParam.Value.ToString.QuotedString('"');
    end;
    Result := Result + ' ' + lParam.Key + '=' + lValueStr;
  end;
end;

function TLogItemRendererDefault.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.ThreadID.ToString,
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage,
    ALogItem.LogTag
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoTag }

function TLogItemRendererNoTag.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[TID {threadid}][{loglevel}] {message}';
end;

function TLogItemRendererNoTag.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.ThreadID.ToString,
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoThreadID }

function TLogItemRendererNoThreadID.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[{loglevel}] {message} [{tag}]';
end;

function TLogItemRendererNoThreadID.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage,
    aLogItem.LogTag
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoTagNoThreadID }

function TLogItemRendererNoTagNoThreadID.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[{loglevel}] {message}';
end;

function TLogItemRendererNoTagNoThreadID.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage
  ]) + RenderContext(aLogItem);
end;



{ TLogItemRendererLogFmt }

function TLogItemRendererLogFmt.RenderLogItem(const aLogItem: TLogItem): String;
var
  lContextStr: string;
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
begin
  Result :=
    Format('time=%s threadid=%d type=%s msg=%s tag=%s',
      [
        DateToISO8601(ALogItem.TimeStamp, False),
        ALogItem.ThreadID,
        ALogItem.LogTypeAsString,
        ALogItem.LogMessage.QuotedString('"'),
        ALogItem.LogTag
        ]);

  // Use pre-rendered context if available (optimization for fixed context)
  if ALogItem.PreRenderedContext <> '' then
  begin
    Result := Result + ALogItem.PreRenderedContext;
    Exit;
  end;

  if ALogItem.HasContext then
  begin
    lContextStr := '';
    for I := 0 to High(ALogItem.Context) do
    begin
      lParam := ALogItem.Context[I];
      case lParam.Value.Kind of
        tkInteger, tkInt64:
          lValueStr := lParam.Value.AsInt64.ToString;
        tkFloat:
          if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
            lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
          else
            lValueStr := FloatToStr(lParam.Value.AsExtended, fFormatSettings);
        tkEnumeration:
          if lParam.Value.TypeInfo = TypeInfo(Boolean) then
            lValueStr := BoolToStr(lParam.Value.AsBoolean, True).ToLower
          else
            lValueStr := lParam.Value.ToString;
      else
        lValueStr := lParam.Value.ToString.QuotedString('"');
      end;
      lContextStr := lContextStr + ' ' + lParam.Key + '=' + lValueStr;
    end;
    Result := Result + lContextStr;
  end;
end;

procedure TLogItemRendererLogFmt.Setup;
begin
  inherited;
  fFormatSettings := GetDefaultFormatSettings;
end;

procedure TLogItemRendererLogFmt.TearDown;
begin
  inherited;

end;


end.
