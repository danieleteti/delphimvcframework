// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
//
// ***************************************************************************
//
// Sempare Template Engine
//
// Copyright (c) 2019-2023 Conrad Vermeulen and Sempare Limited
//
// https://github.com/sempare/sempare-delphi-template-engine
//
// NOTE: The Sempare Template Engine is available under GPL or commercial license.
//
// Free as in speech, NOT Free as in beer.
//
// ***************************************************************************
//
// This adaptor is licensed under the Apache License.
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

unit CustomSempareHelpersU;

interface

// For more information on custom functions in the Sempare Template Engine,
// see https://github.com/sempare/sempare-delphi-template-engine/blob/main/docs/custom-functions.md

type
  TMySempareHelpers = class sealed
  public
    class function MyHelper1(const Value: string): string; static;
    class function MyHelper2(const Value: string): string; static;
  end;

implementation

uses
  System.SysUtils;

{ TMySempareHelpers }

class function TMySempareHelpers.MyHelper1(const Value: string): string;
begin
  Result := Value + ' (I''m The MyHelper1)';
end;

class function TMySempareHelpers.MyHelper2(const Value: string): string;
begin
  Result := Value + ' (I''m The MyHelper2)';
end;

end.
