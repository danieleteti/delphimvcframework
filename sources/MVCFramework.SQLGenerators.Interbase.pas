// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.SQLGenerators.Interbase;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  FireDAC.Phys.IB,
  FireDAC.Phys.IBDef,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.RQL.Parser;

type
  TMVCSQLGeneratorInterbase = class(TMVCSQLGeneratorFirebird)
  end;

implementation

initialization

TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('interbase',
  TMVCSQLGeneratorInterbase);

finalization

TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('interbase');

end.
