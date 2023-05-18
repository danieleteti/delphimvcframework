// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: João Antônio Duarte (joao.antonioduarte@hotmail.com)
//
// TNullabletypes requires spring4d framework
//
// https://bitbucket.org/sglienke/spring4d.git
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
// *************************************************************************** }

unit MVCFramework.NullableTypes;

interface

uses
  Spring;

type
  /// <summary>Alias for nullables types of Spring</summary>
  TNullableInteger = Spring.TNullableInteger;
  TNullableInt64 = Spring.TNullableInt64;
  TNullableCurrency = Spring.TNullableCurrency;
  TNullableString = Spring.TNullableString;
  TNullableDateTime = Spring.TNullableDateTime;
  TNullableDate = Spring.Nullable<TDate>;
  TNullableTime = Spring.Nullable<TTime>;
  TNullableGuid = Spring.TNullableGuid;
  TNullableDouble = Spring.TNullableDouble;

implementation

end.
