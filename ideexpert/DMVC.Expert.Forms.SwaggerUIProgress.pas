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

unit DMVC.Expert.Forms.SwaggerUIProgress;

{ Runs InstallSwaggerUI in a background task behind a small modal dialog with a
  Cancel button, so the IDE keeps pumping messages while Swagger UI downloads.
  The download itself stops at SWAGGER_UI_DEADLINE_MS; Cancel (or closing the
  dialog) stops it earlier. VCL only, no ToolsAPI. }

interface

/// <summary>Same result as InstallSwaggerUI: '' or the reason. ADownloadURL
/// overrides the release URL (tests).</summary>
function InstallSwaggerUIWithProgress(const ATargetFolder, ADocumentURL: string;
  const ADownloadURL: string = ''): string;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Diagnostics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  DMVC.Expert.SwaggerUI;

type
  TSwaggerUIProgressForm = class(TForm)
  private
    fLabel: TLabel;
    fCancelButton: TButton;
    fTimer: TTimer;
    fTask: ITask;
    fWatch: TStopwatch;
    fCancel: TProc;
    procedure CancelClick(Sender: TObject);
    procedure TimerTick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function TaskDone: Boolean;
  public
    constructor CreateFor(const ATask: ITask; const ACancel: TProc);
  end;

constructor TSwaggerUIProgressForm.CreateFor(const ATask: ITask; const ACancel: TProc);
begin
  inherited CreateNew(nil);
  fTask := ATask;
  fCancel := ACancel;
  Caption := 'DelphiMVCFramework Wizard';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 380;
  ClientHeight := 96;
  OnCloseQuery := FormCloseQuery;

  fLabel := TLabel.Create(Self);
  fLabel.Parent := Self;
  fLabel.SetBounds(16, 16, 348, 32);
  fLabel.AutoSize := False;
  fLabel.WordWrap := True;
  fLabel.Caption := Format('Downloading Swagger UI %s...', [SWAGGER_UI_RELEASE.Version]);

  fCancelButton := TButton.Create(Self);
  fCancelButton.Parent := Self;
  fCancelButton.SetBounds(ClientWidth - 96, 56, 80, 25);
  fCancelButton.Caption := 'Cancel';
  fCancelButton.Cancel := True;
  fCancelButton.OnClick := CancelClick;

  fTimer := TTimer.Create(Self);
  fTimer.Interval := 100;
  fTimer.OnTimer := TimerTick;
  fWatch := TStopwatch.StartNew;
end;

function TSwaggerUIProgressForm.TaskDone: Boolean;
begin
  Result := fTask.Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception];
end;

procedure TSwaggerUIProgressForm.CancelClick(Sender: TObject);
begin
  fCancel();
  fCancelButton.Enabled := False;
  fLabel.Caption := 'Cancelling...';
end;

procedure TSwaggerUIProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { closing the dialog means cancel; it goes away when the task has stopped }
  CanClose := TaskDone;
  if not CanClose then
    CancelClick(nil);
end;

procedure TSwaggerUIProgressForm.TimerTick(Sender: TObject);
begin
  if TaskDone then
  begin
    fTimer.Enabled := False;
    ModalResult := mrOk;
  end
  else if fCancelButton.Enabled then
    fLabel.Caption := Format('Downloading Swagger UI %s... %d s (at most %d s)',
      [SWAGGER_UI_RELEASE.Version, fWatch.ElapsedMilliseconds div 1000, SWAGGER_UI_DEADLINE_MS div 1000]);
end;

function InstallSwaggerUIWithProgress(const ATargetFolder, ADocumentURL: string;
  const ADownloadURL: string): string;
var
  lCancelled: Boolean;
  lResult: string;
  lTask: ITask;
  lForm: TSwaggerUIProgressForm;
  lCursor: TCursor;
begin
  lCancelled := False;
  lTask := TTask.Run(
    procedure
    var
      lIsCancelled: TSwaggerUICancelled;
    begin
      lIsCancelled :=
        function: Boolean
        begin
          Result := lCancelled;
        end;
      if ADownloadURL = '' then
        lResult := InstallSwaggerUI(ATargetFolder, ADocumentURL, lIsCancelled)
      else
        lResult := InstallSwaggerUI(ATargetFolder, ADocumentURL, ADownloadURL,
          SWAGGER_UI_DEADLINE_MS, lIsCancelled);
    end);

  lCursor := Screen.Cursor;
  Screen.Cursor := crDefault; // the dialog is interactive: no hourglass over it
  lForm := TSwaggerUIProgressForm.CreateFor(lTask,
    procedure
    begin
      lCancelled := True;
    end);
  try
    lForm.ShowModal;
  finally
    lForm.Free;
    Screen.Cursor := lCursor;
  end;
  lTask.Wait; // already finished: the dialog closes only when the task has
  Result := lResult;
end;

end.
