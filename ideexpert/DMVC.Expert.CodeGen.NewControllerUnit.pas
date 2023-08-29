// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
// project.  Original source by Robert Love.  Adapted by Nick Hodges. }
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
  DMVC.Expert.CodeGen.NewUnit;

type
  TNewControllerUnitEx = class(TNewUnit)
  protected
    FCreateIndexMethod: Boolean;
    FCreateCRUDMethods: Boolean;
    FCreateActionFiltersMethods: Boolean;
    FControllerClassName: string;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile; override;
  public
    constructor Create(
      const aCreateIndexMethod, aCreateCRUDMethods, aCreateActionFiltersMethods: Boolean;
      const AControllerClassName: string;
      const APersonality: string = '');
  end;

  TNewJSONRPCUnitEx = class(TNewUnit)
  protected
    fJSONRPCClassName: String;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
      : IOTAFile; override;
  public
    constructor Create(const aJSONRPCClassName: String;
      const APersonality: string = '');
  end;

implementation

uses
  System.SysUtils,
  VCL.Dialogs,
  DMVC.Expert.CodeGen.Templates,
  DMVC.Expert.CodeGen.SourceFile;

constructor TNewControllerUnitEx.Create(
  const aCreateIndexMethod, aCreateCRUDMethods,
  aCreateActionFiltersMethods: Boolean;
  const AControllerClassName: string;
  const APersonality: string = '');
begin
  Assert(Length(AControllerClassName) > 0);
  FAncestorName := '';
  FFormName := '';
  FImplFileName := '';
  FIntfFileName := '';
  FControllerClassName := AControllerClassName;
  FCreateIndexMethod := aCreateIndexMethod;
  FCreateCRUDMethods := aCreateCRUDMethods;
  FCreateActionFiltersMethods := aCreateActionFiltersMethods;
  Personality := APersonality;
end;

function TNewControllerUnitEx.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  lUnitIdent: string;
  lFormName: string;
  lFileName: string;
  lIndexMethodIntf: string;
  lIndexMethodImpl: string;
  lControllerUnit: string;
  lActionFiltersMethodsIntf: string;
  lActionFiltersMethodsImpl: string;
  lCRUDMethodsIntf: string;
  lCRUDMethodsImpl: string;
  lBOClassesIntf: string;
  lBOClassesImpl: string;
begin
  lControllerUnit := sControllerUnit;

  lIndexMethodIntf := sIndexMethodIntf;
  lIndexMethodImpl := Format(sIndexMethodImpl, [FControllerClassName]);

  lCRUDMethodsIntf := sCRUDMethodsIntf;
  lCRUDMethodsImpl := Format(sCRUDMethodsImpl, [FControllerClassName]);
  lBOClassesIntf := sBOClassesIntf;
  lBOClassesImpl := Format(sBOClassesImpl, ['TPerson']);


  if not FCreateIndexMethod then
  begin
    lIndexMethodIntf := '';
    lIndexMethodImpl := '';
  end;

  if not FCreateCRUDMethods then
  begin
    lCRUDMethodsIntf := '';
    lCRUDMethodsImpl := '';
    lBOClassesIntf := '';
    lBOClassesImpl := '';
  end;

  lActionFiltersMethodsIntf := sActionFiltersIntf;
  lActionFiltersMethodsImpl := Format(sActionFiltersImpl,
    [FControllerClassName]);

  if not FCreateActionFiltersMethods then
  begin
    lActionFiltersMethodsIntf := '';
    lActionFiltersMethodsImpl := '';
  end;

  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
  // So using method mentioned by Marco Cantu.
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('',
    lUnitIdent, lFormName, lFileName);
  Result := TSourceFile.Create(sControllerUnit,
    [
      lUnitIdent,
      FControllerClassName,
      lIndexMethodIntf,
      lIndexMethodImpl,
      lActionFiltersMethodsIntf,
      lActionFiltersMethodsImpl,
      lCRUDMethodsIntf,
      lCRUDMethodsImpl,
      lBOClassesIntf,
      lBOClassesImpl
      ]);
end;

{ TNewJSONRPCUnitEx }

constructor TNewJSONRPCUnitEx.Create(const aJSONRPCClassName,
  APersonality: string);
begin
  inherited Create;
  fJSONRPCClassName := aJSONRPCClassName;
  Personality := aPersonality;
end;

function TNewJSONRPCUnitEx.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  lUnitIdent: string;
  lFileName: string;
  lDummy: String;
begin
  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
  // So using method mentioned by Marco Cantu.
  lFileName := ''; //fJSONRPCClassName + 'U';
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('',
    lUnitIdent, lDummy, lFileName);
  Result := TSourceFile.Create(sJSONRPCUnit,
    [
      lUnitIdent,
      fJSONRPCClassName
    ]);
end;

end.
