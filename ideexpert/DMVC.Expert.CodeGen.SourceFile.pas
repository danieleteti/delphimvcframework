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

unit DMVC.Expert.CodeGen.SourceFile;

interface

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects,
  ToolsAPI;

type
  TSourceFile = class(TInterfacedObject, IOTAFile)
  private
    fTemplateName: string;
    fConfigRef: TJsonObject; // Reference, not owned
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const ATemplateName: string; const AConfigRef: TJsonObject);
  end;

implementation

uses
  DMVC.Expert.CodeGen.TemplateEngine;

{ TSourceFile }

constructor TSourceFile.Create(const ATemplateName: string; const AConfigRef: TJsonObject);
begin
  inherited Create;
  fTemplateName := ATemplateName;
  fConfigRef := AConfigRef; // Keep reference, don't clone - values are set after creation
end;

function TSourceFile.GetAge: TDateTime;
begin
  Result := Now;
end;

function TSourceFile.GetSource: string;
begin
  Result := TDMVCTemplateEngine.Render(fTemplateName, fConfigRef);
end;

end.
