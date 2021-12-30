program LuaBindingUnitTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  DUnitTestRunner,
  TestLuaWrapper in 'TestLuaWrapper.pas',
  TestObjects in 'TestObjects.pas',
  TestLuaEmbeddedTextFilter in 'TestLuaEmbeddedTextFilter.pas',
  LuaBind.Filters.Text in '..\LuaBind.Filters.Text.pas',
  LuaBind.Intf in '..\LuaBind.Intf.pas',
  LuaBind.CustomType.DataSet in '..\LuaBind.CustomType.DataSet.pas',
  LuaBind.CustomType.PODO in '..\LuaBind.CustomType.PODO.pas',
  LuaBind.CustomType.UserType in '..\LuaBind.CustomType.UserType.pas',
  LuaBind in '..\LuaBind.pas',
  LuaBind.DelphiObjects in '..\LuaBind.DelphiObjects.pas';

{$R *.RES}


begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;
end.
