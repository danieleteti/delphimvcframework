// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

unit CustomMustacheHelpersU;

interface

uses
  mormot.core.mustache;

type
  TMyMustacheHelpers = class sealed
  public
    class procedure MyHelper1(const Value: variant; out Result: variant);
    class procedure MyHelper2(const Value: variant; out Result: variant);
  end;

implementation

uses
  MVCFramework.View.Renderers.Mustache, System.SysUtils;

{ TMyMustacheHelpers }

class procedure TMyMustacheHelpers.MyHelper1(const Value: variant;
  out Result: variant);
begin
  Result := Value +  ' (I''m The MyHelper1)';
end;

class procedure TMyMustacheHelpers.MyHelper2(const Value: variant; out Result: variant);
begin
  Result := Value +  ' (I''m The MyHelper2)';
end;


end.
