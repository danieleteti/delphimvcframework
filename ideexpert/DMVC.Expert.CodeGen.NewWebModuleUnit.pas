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
//
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************

unit DMVC.Expert.CodeGen.NewWebModuleUnit;

interface

uses
  ToolsApi,
  DMVC.Expert.CodeGen.NewUnit, JsonDataObjects;

type
  TNewWebModuleUnitEx = class(TNewUnit)
  private
    FUnitIdent, FFormName, FFileName : String;
  protected
    function GetCreatorType: string; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  public
    constructor Create(
      const ConfigModelRef: TJSONObject;
      const aPersonality : String);reintroduce;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  VCL.Dialogs,
  DMVC.Expert.CodeGen.SourceFile,
  DMVC.Expert.CodeGen.Executor,
  DMVC.Expert.Commands.Templates,
  DMVC.Expert.Commons;

constructor TNewWebModuleUnitEx.Create(
      const ConfigModelRef: TJSONObject;
      const aPersonality : String);
begin
  inherited Create(ConfigModelRef);
  Personality := APersonality;
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(
    '', FUnitIdent, FFormName, FFileName);
end;

function TNewWebModuleUnitEx.GetCreatorType: string;
begin
  Result := sForm;
end;

function TNewWebModuleUnitEx.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TSourceFile.Create(
    procedure (Gen: TMVCCodeGenerator)
    begin
      FillWebModuleDFMTemplates(Gen);
    end,
    fConfigModelRef);
end;

function TNewWebModuleUnitEx.NewImplSource(const ModuleIdent, FormIdent,  AncestorIdent: string): IOTAFile;
begin
  //ModuleIdent is blank for some reason.
  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api

  fConfigModelRef.S[TConfigKey.webmodule_unit_name] := FUnitIdent;
  Result := TSourceFile.Create(
    procedure (Gen: TMVCCodeGenerator)
    begin
      FillWebModuleTemplates(Gen);
    end,
    fConfigModelRef);
end;



end.
