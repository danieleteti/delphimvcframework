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
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************

unit DMVC.Expert.CodeGen.NewUnit;
// This is done to Warnings that I can't control, as Embarcadero has
// deprecated the functions, but due to design you are still required to
// to implement.
{$WARN SYMBOL_DEPRECATED OFF}
interface

uses
  ToolsAPI;

type

  TNewUnit = class(TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FPersonality : string;
  protected
    //Specific to class
    FFormName: string;
    FImplFileName: string;
    FIntfFileName: string;
    FAncestorName: string;
    procedure SetAncestorName(const Value: string); virtual;
    procedure SetFormName(const Value: string); virtual;
    procedure SetImplFileName(const Value: string); virtual;
    procedure SetIntfFileName(const Value: string); virtual;
    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;
  public
    property FormName:  string read GetFormName write SetFormName;
    property ImplFileName: string read GetImplFileName write SetImplFileName;
    property IntfFileName: string read GetIntfFileName write SetIntfFileName;
    property AncestorName:  string read GetAncestorName write SetAncestorName;
    property Personality: string read FPersonality write FPersonality;
  end;

implementation

{ TUnitCreator }

procedure TNewUnit.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TNewUnit.GetAncestorName: string;
begin
  Result := FAncestorName;
end;

function TNewUnit.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TNewUnit.GetExisting: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetFileSystem: string;
begin
  Result := '';
end;

function TNewUnit.GetFormName: string;
begin
  Result := FFormName;
end;

function TNewUnit.GetImplFileName: string;
begin
  Result := FImplFileName;
end;

function TNewUnit.GetIntfFileName: string;
begin
  Result := FIntfFileName;
end;

function TNewUnit.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TNewUnit.GetShowForm: Boolean;
begin
  Result := False;
end;

function TNewUnit.GetShowSource: Boolean;
begin
  Result := True;
end;

function TNewUnit.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TNewUnit.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNewUnit.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNewUnit.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TNewUnit.SetAncestorName(const Value: string);
begin
  FAncestorName  := Value;
end;

procedure TNewUnit.SetFormName(const Value: string);
begin
  FFormName := Value;
end;

procedure TNewUnit.SetImplFileName(const Value: string);
begin
  FImplFileName := Value;
end;

procedure TNewUnit.SetIntfFileName(const Value: string);
begin
  FIntfFileName := Value;
end;




end.
