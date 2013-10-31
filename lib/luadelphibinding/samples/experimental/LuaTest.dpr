program LuaTest;

uses
  Vcl.Forms,
  Unit11 in 'Unit11.pas' {Form11},
  LuaWrapper in '..\src\LuaWrapper.pas',
  LuaWrapper.CustomType.DataSet in '..\src\LuaWrapper.CustomType.DataSet.pas',
  LuaIntf in '..\src\LuaIntf.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;

end.
