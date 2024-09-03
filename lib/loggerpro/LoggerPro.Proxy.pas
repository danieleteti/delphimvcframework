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

unit LoggerPro.Proxy;

interface

uses Classes, System.SysUtils, LoggerPro;

type
  ILogAppenderProxy=interface
    ['{34816F83-9FBF-461E-8913-F10F9460D712}']
    function GetInternalAppender: ILogAppender;
    property InternalAppender: ILogAppender read GetInternalAppender;
  end;

  TLoggerProFilter = class abstract
    class function Build(Appender: ILogAppender; Filter: TFunc<TLogItem, boolean>): ILogAppender;
  end;

  TLogWriterPredicate = reference to function (const aType: TLogType; const aMessage, aTag: string): Boolean;

  TLogWriterDecorator = class(TInterfacedObject, ILogWriter)
  private
    fDecoratedLogWriter: ILogWriter;
    fFilter: TLogWriterPredicate;
  protected
    { ILogWriter }
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;

    { ICustomLogWriter}
    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const aIndex: Integer): ILogAppender;
    property Appenders[const aIndex: Integer]: ILogAppender read GetAppenders;
    procedure AddAppender(const aAppender: ILogAppender);
    procedure DelAppender(const aAppender: ILogAppender);
    function AppendersCount(): Integer;


    ///
    constructor Create(LogWriter: ILogWriter; Filter: TLogWriterPredicate);
  public
    class function Build(LogWriter: ILogWriter; Filter: TLogWriterPredicate): ILogWriter;
  end;


implementation

type
  TLoggerProAppenderFilterImpl = class(TLoggerProAppenderBase, ILogAppender, ILogAppenderProxy)
  private
    FAppender: ILogAppender;
    FFilter: TFunc<TLogItem, boolean>;
    function GetInternalAppender: ILogAppender;
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    property InternalAppender: ILogAppender read GetInternalAppender;
    constructor Create(Appender: ILogAppender; Filter: TFunc<TLogItem, boolean>);  reintroduce;
  end;

{ TLoggerProAppenderFilterImpl }

constructor TLoggerProAppenderFilterImpl.Create(Appender: ILogAppender;
  Filter: TFunc<TLogItem, boolean>);
begin
  inherited Create;
  self.FFilter := Filter;
  self.FAppender := Appender;
end;

function TLoggerProAppenderFilterImpl.GetInternalAppender: ILogAppender;
begin
  result := FAppender;
end;

procedure TLoggerProAppenderFilterImpl.Setup;
begin
  FAppender.Setup;
end;

procedure TLoggerProAppenderFilterImpl.TearDown;
begin
  FAppender.TearDown;
end;

procedure TLoggerProAppenderFilterImpl.WriteLog(const aLogItem: TLogItem);
begin
  if FFilter(aLogItem) then
    FAppender.WriteLog(aLogItem);
end;


class function TLoggerProFilter.Build(Appender: ILogAppender;
  Filter: TFunc<TLogItem, boolean>): ILogAppender;
begin
  Result := TLoggerProAppenderFilterImpl.Create(Appender, Filter);
end;


{ TLogWriterDecorator }

class function TLogWriterDecorator.Build(LogWriter: ILogWriter;
  Filter: TLogWriterPredicate): ILogWriter;
begin
  Result := TLogWriterDecorator.Create(LogWriter, Filter);
end;

constructor TLogWriterDecorator.Create(LogWriter: ILogWriter; Filter: TLogWriterPredicate);
begin
  inherited Create;
  fDecoratedLogWriter := LogWriter;
  fFilter := Filter;
end;

procedure TLogWriterDecorator.AddAppender(const aAppender: ILogAppender);
begin
  fDecoratedLogWriter.AddAppender(aAppender);
end;

function TLogWriterDecorator.AppendersCount: Integer;
begin
  Result := fDecoratedLogWriter.AppendersCount;
end;

function TLogWriterDecorator.GetAppenders(const aIndex: Integer): ILogAppender;
begin
  Result := fDecoratedLogWriter.GetAppenders(aIndex);
end;

function TLogWriterDecorator.GetAppendersClassNames: TArray<string>;
begin
  Result := fDecoratedLogWriter.GetAppendersClassNames;
end;

// ILogWriter

procedure TLogWriterDecorator.Debug(const aMessage, aTag: string);
begin
  Log(TLogType.Debug, aMessage, aTag);
end;

procedure TLogWriterDecorator.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Debug, aMessage, aParams, aTag);
end;

procedure TLogWriterDecorator.DelAppender(const aAppender: ILogAppender);
begin
  fDecoratedLogWriter.DelAppender(aAppender);
end;

procedure TLogWriterDecorator.Error(const aMessage, aTag: string);
begin
  Log(TLogType.Error, aMessage, aTag);
end;

procedure TLogWriterDecorator.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Error, aMessage, aParams, aTag);
end;

procedure TLogWriterDecorator.Fatal(const aMessage, aTag: string);
begin
  Log(TLogType.Fatal, aMessage, aTag);
end;

procedure TLogWriterDecorator.Fatal(const aMessage: string;
  const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Fatal, aMessage, aParams, aTag);
end;

procedure TLogWriterDecorator.Info(const aMessage, aTag: string);
begin
  Log(TLogType.Info, aMessage, aTag);
end;

procedure TLogWriterDecorator.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Info, aMessage, aParams, aTag);
end;

procedure TLogWriterDecorator.Log(const aType: TLogType; const aMessage, aTag: string);
begin
  if fFilter(aType, aMessage, aTag) then
  begin
    fDecoratedLogWriter.Log(aType, aMessage, aTag);
  end;
end;

procedure TLogWriterDecorator.Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  Log(aType, Format(aMessage, aParams), aTag);
end;

procedure TLogWriterDecorator.Warn(const aMessage, aTag: string);
begin
  Log(TLogType.Warning, aMessage, aTag);
end;

procedure TLogWriterDecorator.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Warning, aMessage, aParams, aTag);
end;


end.
