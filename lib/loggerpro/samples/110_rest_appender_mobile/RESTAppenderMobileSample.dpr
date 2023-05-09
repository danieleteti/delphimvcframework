program RESTAppenderMobileSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFormU in 'MainFormU.pas' {Form2},
  LoggerProConfig in '..\110_rest_appender\LoggerProConfig.pas',
  LoggerPro.FileAppender in '..\..\LoggerPro.FileAppender.pas',
  LoggerPro in '..\..\LoggerPro.pas',
  LoggerPro.RESTAppender in '..\..\LoggerPro.RESTAppender.pas',
  ThreadSafeQueueU in '..\..\ThreadSafeQueueU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
