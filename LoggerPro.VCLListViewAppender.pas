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

unit LoggerPro.VCLListViewAppender;

interface

uses
  LoggerPro,
  System.Classes,
  Vcl.ComCtrls;

type
  { @abstract(Appends formatted @link(TLogItem) to a TMemo in a VCL application) }
  TVCLListViewAppender = class(TLoggerProAppenderBase)
  private
    FLV: TListView;
    FMaxLogLines: Word;
  public
    constructor Create(aLV: TListView; aMaxLogLines: Word = 500; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
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

constructor TVCLListViewAppender.Create(aLV: TListView; aMaxLogLines: Word; aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create(aLogItemRenderer);
  FLV := aLV;
  FMaxLogLines := aMaxLogLines;
end;

procedure TVCLListViewAppender.Setup;
begin
  inherited;
  TThread.Synchronize(nil,
    procedure
    begin
      FLV.Clear;
    end);
end;

procedure TVCLListViewAppender.TearDown;
begin
  // do nothing
end;

procedure TVCLListViewAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
begin
  lText := FormatLog(aLogItem);

  TThread.Queue(nil,
    procedure
    var
      Lines: integer;
    begin
      FLV.Items.BeginUpdate;
      try
        Lines := FLV.Items.Count;
        if Lines > FMaxLogLines then
        begin
          FLV.Items.Delete(0);
        end;
        FLV.AddItem(lText, nil)
      finally
        FLV.Items.EndUpdate;
      end;
      FLV.Scroll(0, FLV.Items.Count);
      SendMessage(FLV.Handle, EM_SCROLLCARET, 0, 0);
    end);
end;

end.
