program DMSContainerESAppenderSample;

uses
  Vcl.Forms,
  EventStreamsAppenderFormU in 'EventStreamsAppenderFormU.pas' {MainForm},
  LoggerProConfig in 'LoggerProConfig.pas',
  LoggerPro.DMSEventStreamsAppender in '..\..\LoggerPro.DMSEventStreamsAppender.pas',
  EventStreamsRPCProxy in 'EventStreamsRPCProxy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
