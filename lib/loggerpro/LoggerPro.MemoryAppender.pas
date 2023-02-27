// ***************************************************************************
//
// LoggerPro
//
// Copyright (c) 2015-2017 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
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

unit LoggerPro.MemoryAppender;

interface

uses
  LoggerPro, System.Generics.Collections, System.SyncObjs, System.SysUtils,
  ThreadSafeQueueU;

type
  TMREWLogItemList = class(TMREWObjectList<TLogItem>)
  end;

  TLoggerProMemoryAppender = class(TLoggerProAppenderBase)
  private
    fMREWLogList: TMREWLogItemList;
    fTag: string;
    fMaxSize: Int32;
  public
    procedure Setup; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    procedure TearDown; override;
    constructor Create(aLogList: TMREWLogItemList; aTag: string; aMaxSize: Int32); reintroduce; virtual;
  end;

implementation

var
  Glock: TObject = nil;

  { TLoggerProMemoryAppender }

constructor TLoggerProMemoryAppender.Create(aLogList: TMREWLogItemList; aTag: string; aMaxSize: Int32);
begin
  inherited Create;
  fMREWLogList := aLogList;
  fTag := aTag;
  fMaxSize := aMaxSize
end;

procedure TLoggerProMemoryAppender.Setup;
begin
  inherited;
end;

procedure TLoggerProMemoryAppender.TearDown;
begin
  inherited;
end;

procedure TLoggerProMemoryAppender.WriteLog(const aLogItem: TLogItem);
var
  lList: TObjectList<TLogItem>;
begin
  if aLogItem.LogTag <> fTag then
    Exit;

  lList := fMREWLogList.BeginWrite;
  try
    if lList.Count >= fMaxSize then
    begin
      while lList.Count > (fMaxSize * 0.9) do // remove 10% of the list HEAD
        lList.Delete(0);
    end;
    lList.Add(aLogItem.Clone);
  finally
    fMREWLogList.EndWrite;
  end;
end;

initialization

Glock := TObject.Create;

finalization

Glock.Free;

end.
