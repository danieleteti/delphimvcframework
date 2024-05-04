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

unit DMVC.Expert.CodeGen.SourceFile;

interface

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects,
  ToolsAPI,
  DMVC.Expert.CodeGen.Executor;

type
  TSourceFile = class(TInterfacedObject, IOTAFile)
  private
    fGeneratorCallback: TProc<TMVCCodeGenerator>;
    fJSON: TJsonObject;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const GeneratorCallback: TProc<TMVCCodeGenerator>; const Args: TJsonObject);
    destructor Destroy; override;
  end;

implementation

{ TSourceFile }

constructor TSourceFile.Create(const GeneratorCallback: TProc<TMVCCodeGenerator>; const Args: TJsonObject);
begin
  inherited Create;
  fGeneratorCallback := GeneratorCallback;
  fJSON := Args.Clone as TJsonObject;
end;

destructor TSourceFile.Destroy;
begin
  fJSON.Free;
  inherited;
end;

function TSourceFile.GetAge: TDateTime;
begin
  Result := Now;
end;

function TSourceFile.GetSource: string;
begin
  Result := TMVCCodeGenerator.GenerateSource(fJSON,
                        procedure (Gen: TMVCCodeGenerator)
                        begin
                          fGeneratorCallback(Gen)
                        end);
end;

end.

