program jsonrpcclientwithobjects_sync;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {MainForm},
  RandomUtilsU in '..\..\commons\RandomUtilsU.pas',
  BusinessObjectsU in '..\..\commons\BusinessObjectsU.pas',
  CommonTypesU in '..\CommonTypesU.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
