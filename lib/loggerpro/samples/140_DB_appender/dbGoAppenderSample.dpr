program dbGoAppenderSample;

uses
  Vcl.Forms,
  ADOAppenderFormU in 'ADOAppenderFormU.pas' {MainForm},
  LoggerProConfig in 'LoggerProConfig.pas';
  //LoggerPro.RESTAppender in '..\..\LoggerPro.RESTAppender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
