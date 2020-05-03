// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RQL.AST2InterbaseSQL;

interface

uses
  MVCFramework.RQL.Parser,
  MVCFramework.RQL.AST2FirebirdSQL;

type
  TRQLInterbaseCompiler = class(TRQLFirebirdCompiler)
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.ActiveRecord;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('interbase', TRQLInterbaseCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('interbase');

end.
