program FireDACAppenderSample;

uses
  Vcl.Forms,
  FireDACAppenderFormU in 'FireDACAppenderFormU.pas' {MainForm},
  LoggerProConfig in 'LoggerProConfig.pas',
  FDConnectionConfigU in 'FDConnectionConfigU.pas';

//LoggerPro.RESTAppender in '..\..\LoggerPro.RESTAppender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  CreatePostgresqlPrivateConnDef(False);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
