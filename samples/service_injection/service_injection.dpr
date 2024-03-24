program service_injection;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {Form13},
  MVCFramework.Injector in '..\..\sources\MVCFramework.Injector.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
