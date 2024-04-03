// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// Contributors for this file: 
//    David Cornelius
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

unit LoggerPro.VCLListBoxAppender;

interface

uses
  LoggerPro,
  System.Classes,
  Vcl.StdCtrls;

type
  { @abstract(Appends formatted @link(TLogItem) to a TListBox in a VCL application) }
  TVCLListBoxAppender = class(TLoggerProAppenderBase)
  private
    FLB: TListBox;
    FMaxLogLines: Word;
  public
    constructor Create(aLB: TListBox; aMaxLogLines: Word = 500; aLogItemRenderer: ILogItemRenderer = nil); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils;

{ TVCLListBoxAppender }

constructor TVCLListBoxAppender.Create(aLB: TListBox; aMaxLogLines: Word = 500; aLogItemRenderer: ILogItemRenderer = nil);
begin
  inherited Create(aLogItemRenderer);
  FLB := aLB;
  FMaxLogLines := aMaxLogLines;
end;

procedure TVCLListBoxAppender.Setup;
begin
  inherited;
  TThread.Synchronize(nil,
    procedure
    begin
      FLB.Clear;
    end);
end;

procedure TVCLListBoxAppender.TearDown;
begin
  // do nothing
end;

procedure TVCLListBoxAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
begin
  lText := FormatLog(aLogItem);
  TThread.Queue(nil,
    procedure
    var
      Lines: integer;
    begin
      FLB.Items.BeginUpdate;
      try
        Lines := FLB.Items.Count;
        if Lines > FMaxLogLines then
          FLB.Items.Delete(0);
        FLB.AddItem(lText, nil);
        FLB.ItemIndex := FLB.Items.Count - 1;
      finally
        FLB.Items.EndUpdate;
      end;
    end);
end;

end.
