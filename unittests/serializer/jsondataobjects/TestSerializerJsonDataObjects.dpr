// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

program TestSerializerJsonDataObjects;
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
  DUnitTestRunner,
  MVCFramework.Tests.Serializer.JsonDataObjects in 'MVCFramework.Tests.Serializer.JsonDataObjects.pas',
  MVCFramework.Serializer.JsonDataObjects in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.pas',
  MVCFramework.Tests.Serializer.Entities in '..\..\common\MVCFramework.Tests.Serializer.Entities.pas',
  MVCFramework.Serializer.JsonDataObjects.CustomTypes in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.CustomTypes.pas',
  MVCFramework.Tests.Serializer.Intf in '..\..\common\MVCFramework.Tests.Serializer.Intf.pas',
  MVCFramework.Tests.Serializer.EntitiesModule in '..\..\common\MVCFramework.Tests.Serializer.EntitiesModule.pas' {EntitiesModule: TDataModule};

{$R *.RES}

begin

  ReportMemoryLeaksOnShutdown := True;

  DUnitTestRunner.RunRegisteredTests;

end.
