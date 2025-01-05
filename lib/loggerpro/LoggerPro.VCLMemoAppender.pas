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

unit LoggerPro.VCLMemoAppender;

interface

uses
  LoggerPro,
  System.Classes,
  Vcl.StdCtrls;

type
  { @abstract(Appends formatted @link(TLogItem) to a TMemo in a VCL application) }
  TVCLMemoLogAppender = class(TLoggerProAppenderBase)
  private
    FMemo: TMemo;
    FMaxLogLines: Word;
    FClearOnStartup: Boolean;
  public
    constructor Create(aMemo: TMemo; aMaxLogLines: Word = 100; aClearOnStartup: Boolean = False; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages;

{ TVCLMemoLogAppender }

constructor TVCLMemoLogAppender.Create(aMemo: TMemo; aMaxLogLines: Word; aClearOnStartup: Boolean; aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  FMemo := aMemo;
  FMaxLogLines := aMaxLogLines;
  FClearOnStartup := aClearOnStartup;
end;

procedure TVCLMemoLogAppender.Setup;
begin
  inherited;
  if FClearOnStartup then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        FMemo.Clear;
      end);
  end;
end;

procedure TVCLMemoLogAppender.TearDown;
begin
  // do nothing
end;

procedure TVCLMemoLogAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
begin
  if Assigned(FMemo) then
  begin
    if FMemo.owner = nil then exit;
  end;

  lText := FormatLog(aLogItem);
  TThread.Queue(nil,
    procedure
    begin
      FMemo.Lines.BeginUpdate;
      try
        if FMemo.Lines.Count = FMaxLogLines then
          FMemo.Lines.Delete(0);
        FMemo.Lines.Add(lText)
      finally
        FMemo.Lines.EndUpdate;
      end;
      SendMessage(FMemo.Handle, EM_SCROLLCARET, 0, 0);
    end);
end;

end.
