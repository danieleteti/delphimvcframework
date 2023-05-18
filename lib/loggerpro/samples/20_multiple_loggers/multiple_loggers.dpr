program multiple_loggers;

uses
  Vcl.Forms,
  MultipleLoggersU in 'MultipleLoggersU.pas' {MultipleLoggersForm},
  LoggerProConfig in 'LoggerProConfig.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMultipleLoggersForm, MultipleLoggersForm);
  Application.Run;
end.
