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
// This IDE expert is based off of the one included with the DUnitX }
// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti. }
//
// The DUnitX project is run by Vincent Parrett and can be found at: }
//
// https://github.com/VSoftTechnologies/DUnitX }
// ***************************************************************************

unit DMVC.Expert.CodeGen.NewControllerUnit;

interface

uses
  ToolsApi,
  System.IOUtils,
  DMVC.Expert.CodeGen.NewUnit,
  JsonDataObjects, DMVC.Expert.CodeGen.Executor;

type
  TNewControllerUnitEx = class(TNewUnit)
  protected
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile; override;
  public
    constructor Create(
      const ConfigModelRef: TJSONObject;
      const APersonality: string = ''); reintroduce;
  end;

  TTemplateLoadProcedure = procedure(Gen: TMVCCodeGenerator);

  TNewGenericUnitFromTemplate = class(TNewUnit)
  private
    fTemplateLoadProcedure: TTemplateLoadProcedure;
    fUnitIdentKeyName: string;
  protected
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile; override;
  public
    constructor Create(
      const ConfigModelRef: TJSONObject;
      const TemplateLoadProcedure: TTemplateLoadProcedure;
      const UnitIdentKeyName: String;
      const APersonality: string = '');reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DMVC.Expert.CodeGen.SourceFile,
  DMVC.Expert.Commands.Templates,
  DMVC.Expert.Commons;

constructor TNewControllerUnitEx.Create(
  const ConfigModelRef: TJSONObject;
  const APersonality: string = '');
begin
  inherited Create(ConfigModelRef);
  Personality := APersonality;
end;

function TNewControllerUnitEx.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  lUnitIdent: string;
  lFormName: string;
  lFileName: string;
begin
  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
  // So using method mentioned by Marco Cantu.
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('',
    lUnitIdent, lFormName, lFileName);


  fConfigModelRef.S[TConfigKey.controller_unit_name] := lUnitIdent;

  Result := TSourceFile.Create(
    procedure (Gen: TMVCCodeGenerator)
    begin
      FillControllerTemplates(Gen);
    end,
    fConfigModelRef);
end;

{ TNewJSONRPCUnitEx }

constructor TNewGenericUnitFromTemplate.Create(
  const ConfigModelRef: TJSONObject;
  const TemplateLoadProcedure: TTemplateLoadProcedure;
  const UnitIdentKeyName: String;
  const APersonality: string);
begin
  inherited Create(ConfigModelRef);
  fTemplateLoadProcedure := TemplateLoadProcedure;
  fUnitIdentKeyName := UnitIdentKeyName;
  Personality := aPersonality;
end;

function TNewGenericUnitFromTemplate.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  lUnitIdent: string;
  lFileName: string;
  lDummy: String;
begin
  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
  // So using method mentioned by Marco Cantu.
  lFileName := '';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('',
    lUnitIdent, lDummy, lFileName);
  fConfigModelRef.S[fUnitIdentKeyName] := lUnitIdent;
  Result := TSourceFile.Create(
    procedure (Gen: TMVCCodeGenerator)
    begin
      //FillJSONRPCTemplates(Gen);
      fTemplateLoadProcedure(Gen);
    end,
    fConfigModelRef);
end;

end.
