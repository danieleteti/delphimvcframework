program console_appender;

uses
  Vcl.Forms,
  LoggerProConfig in 'LoggerProConfig.pas',
  MainFormU in '..\common\MainFormU.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
