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

unit CustomTemplateProFiltersU;

interface

uses
  System.Rtti;

procedure TemplateProContextConfigure;

implementation

uses
  TemplatePro, System.SysUtils;

function MyHelper1(const aValue: TValue; const aParameters: TArray<string>): TValue;
begin
  Result := aValue.ToString +  ' (I''m The MyHelper1)';
end;

function MyHelper2(const aValue: TValue; const aParameters: TArray<string>): TValue;
begin
  Result := aValue.ToString +  ' (I''m The MyHelper2)';
end;


procedure TemplateProContextConfigure;
begin
  TTProConfiguration.OnContextConfiguration := procedure(const CompiledTemplate: ITProCompiledTemplate)
  begin
    // These filters will be available to the TemplatePro views as if they were the standard ones
    CompiledTemplate.AddFilter('MyHelper1', MyHelper1);
    CompiledTemplate.AddFilter('MyHelper2', MyHelper2);

    CompiledTemplate.OnGetValue :=
      procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean)
      begin
        if SameText(DataSource, 'ext1') then
        begin
          if Members.IsEmpty then
          begin
            Value := 'External Value Ext1'
          end
          else
          begin
            Value := 'Reading ext1.' + Members;
          end;
          Handled := True;
        end;
      end
  end;
end;


end.
