program file_appender;

uses
  Vcl.Forms,
  LoggerProConfig in 'LoggerProConfig.pas',
  MainFormU in '..\common\MainFormU.pas' {MainForm},
  LoggerPro.Renderers in '..\..\LoggerPro.Renderers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
