program PeopleManagerTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  TestPeopleModuleU in 'TestPeopleModuleU.pas',
  TestCommonsU in 'TestCommonsU.pas',
  CommonsU in '..\CommonsU.pas',
  PersonBO in '..\BusinessObjects\PersonBO.pas',
  PeopleModuleU in '..\Modules\PeopleModuleU.pas' {PeopleModule: TDataModule} ,
  DUnitTestRunner;

{$R *.RES}


begin
  DUnitTestRunner.RunRegisteredTests;

end.
