// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
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


function GetDefaultLogItemRenderer: ILogItemRenderer;

var
  gDefaultLogItemRenderer: TLogItemRendererClass = TLogItemRendererDefault;

implementation

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

function TLogItemRendererDefault.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.ThreadID.ToString,
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage,
    ALogItem.LogTag
  ]);
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
  ]);
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
  ]);
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
  ]);
end;




end.
