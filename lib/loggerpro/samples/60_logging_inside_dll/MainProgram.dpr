program MainProgram;

uses
  Vcl.Forms,
  LoggerPro.GlobalLogger,
  MainFormU in 'MainFormU.pas' {Form5};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
  ReleaseGlobalLogger;
end.
