program GlobalDemo;

{$MODE Delphi}

uses
  Forms, LResources, Interfaces,
  MainForm in 'MainForm.pas' {frmMain},
  StompClient in '../../../StompClient.pas',
  StompTypes in '../../../StompTypes.pas';

  {$IFDEF WINDOWS}{$R GlobalDemo.rc}{$ENDIF}
begin
  {$I GlobalDemo.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
