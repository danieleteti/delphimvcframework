program REDISAppenderSample;

uses
  Vcl.Forms,
  RedisAppenderFormU in 'RedisAppenderFormU.pas' {MainForm},
  LoggerPro.RedisAppender in '..\..\LoggerPro.RedisAppender.pas',
  LoggerProConfig in 'LoggerProConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
