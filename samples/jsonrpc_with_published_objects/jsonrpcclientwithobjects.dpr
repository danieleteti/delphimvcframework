program jsonrpcclientwithobjects;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {MainForm},
  BusinessObjectsU in '..\commons\BusinessObjectsU.pas',
  MVCFramework.JSONRPC.Client in '..\..\sources\MVCFramework.JSONRPC.Client.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
