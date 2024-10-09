program jsonrpcclient_async;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {MainForm},
  RandomUtilsU in '..\..\commons\RandomUtilsU.pas',
  CommonTypesU in '..\CommonTypesU.pas',
  WaitingFormU in 'WaitingFormU.pas' {WaitingForm},
  BusinessObjectsU in '..\..\commons\BusinessObjectsU.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
