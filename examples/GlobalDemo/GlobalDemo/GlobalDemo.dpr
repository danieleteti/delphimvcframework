program GlobalDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  StompClient in '..\..\..\StompClient.pas',
  StompTypes in '..\..\..\StompTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
