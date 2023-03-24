program ElasticSearchAppenderSample;

uses
  Vcl.Forms,
  ESAppenderFormU in 'ESAppenderFormU.pas' {MainForm},
  LoggerProConfig in 'LoggerProConfig.pas',
  LoggerPro.RESTAppender in '..\..\LoggerPro.RESTAppender.pas',
  LoggerPro.ElasticSearchAppender in '..\..\LoggerPro.ElasticSearchAppender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
