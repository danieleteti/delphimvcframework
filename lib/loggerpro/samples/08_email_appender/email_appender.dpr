program email_appender;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  LoggerProConfig in 'LoggerProConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
