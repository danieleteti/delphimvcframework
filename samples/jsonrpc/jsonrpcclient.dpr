program jsonrpcclient;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {Form10},
  BusinessObjectsU in '..\commons\BusinessObjectsU.pas',
  MVCFramework.JSONRPC.Client in '..\..\sources\MVCFramework.JSONRPC.Client.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
