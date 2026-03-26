// *************************************************************************** }
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

unit MVCFramework.SQLGenerators;

// Include this unit in your project to register SQL generators and RQL compilers
// for all supported databases. Each generator auto-registers in its initialization
// section - no code needed, just add this unit to the uses clause of your .dpr.
//
// Supported: PostgreSQL, Firebird, Interbase, MSSQL, MySQL, Oracle, SQLite

interface

// nothing to declare - this unit exists only for its side effects (initialization sections)

implementation

uses
  MVCFramework.SQLGenerators.PostgreSQL,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.SQLGenerators.Interbase,
  MVCFramework.SQLGenerators.MSSQL,
  MVCFramework.SQLGenerators.MySQL,
  MVCFramework.SQLGenerators.Oracle,
  MVCFramework.SQLGenerators.Sqlite;

end.
