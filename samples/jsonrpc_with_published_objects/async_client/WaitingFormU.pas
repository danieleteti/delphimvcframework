// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit WaitingFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TWaitingForm = class(TForm)
    lblMessage: TLabel;
    Shape1: TShape;
    lblRunningRequests: TLabel;
    TimerWaiting: TTimer;
    procedure TimerWaitingTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWaitingCount: Integer;
    fPoints: Integer;
    procedure SetWaitingCount(const Value: Integer);
    { Private declarations }
  public
    property WaitingCount: Integer read FWaitingCount write SetWaitingCount;
    procedure IncreaseWaitingCount;
    procedure DecreaseWaitingCount;

  end;

implementation

uses
  System.Math, System.StrUtils;

{$R *.dfm}
{ TWaitingForm }

procedure TWaitingForm.DecreaseWaitingCount;
begin
  WaitingCount := WaitingCount - 1;
end;

procedure TWaitingForm.FormDestroy(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TWaitingForm.IncreaseWaitingCount;
begin
  WaitingCount := WaitingCount + 1;
end;

procedure TWaitingForm.SetWaitingCount(const Value: Integer);
begin
  FWaitingCount := Max(0, Value);
  if FWaitingCount = 0 then
  begin
    TimerWaiting.Enabled := False;
    Hide;
    Screen.Cursor := crDefault;
  end
  else
  begin
    if not Visible then
    begin
      Screen.Cursor := crHourGlass;
      fPoints := 0;
      TimerWaiting.Enabled := True;
      Show;
    end;
    lblRunningRequests.Caption := FWaitingCount.ToString + ' running request' + ifthen(FWaitingCount > 1, 's');
    lblRunningRequests.Update;
  end;
end;

procedure TWaitingForm.TimerWaitingTimer(Sender: TObject);
begin
  if fPoints = 3 then
  begin
    fPoints := 0;
  end
  else
  begin
    Inc(fPoints);
  end;
  lblMessage.Caption := 'Please wait' + StringOfChar('.', fPoints);
end;

end.
