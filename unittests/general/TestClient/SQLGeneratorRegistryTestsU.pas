// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit SQLGeneratorRegistryTestsU;

interface

uses
  DUnitX.TestFramework;

type
  // The error a project gets when it forgets the SQL generator unit must name
  // that unit as the file is spelled.
  [TestFixture]
  TTestSQLGeneratorRegistryHint = class
  public
    [Test] procedure TestMissingGeneratorNamesTheRealUnit;
    [Test] procedure TestBackendWithoutShippedGenerator;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.ActiveRecord,
  MVCFramework.RQL.Parser,
  MVCFramework.SQLGenerators.Sqlite;

function MessageOf(const ABackend: string): string;
begin
  Result := '';
  try
    TMVCSQLGeneratorRegistry.Instance.GetSQLGenerator(ABackend);
  except
    on E: ERQLCompilerNotFound do
      Result := E.Message;
  end;
end;

procedure TTestSQLGeneratorRegistryHint.TestMissingGeneratorNamesTheRealUnit;
var
  lMessage: string;
begin
  TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('sqlite');
  try
    lMessage := MessageOf('sqlite');
  finally
    TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('sqlite', TMVCSQLGeneratorSQLite);
  end;
  Assert.Contains(lMessage, 'MVCFramework.SQLGenerators.Sqlite ', False);
  Assert.DoesNotContain(lMessage, 'SQLGenerators.sqlite', False);
end;

procedure TTestSQLGeneratorRegistryHint.TestBackendWithoutShippedGenerator;
var
  lMessage: string;
begin
  lMessage := MessageOf('db2');
  Assert.Contains(lMessage, 'ships no SQL generator', False);
  Assert.DoesNotContain(lMessage, 'MVCFramework.SQLGenerators.', False);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSQLGeneratorRegistryHint);

end.
