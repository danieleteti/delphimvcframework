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

unit DMVC.Expert.Commands.Templates;

interface

uses
  System.Generics.Collections, DMVC.Expert.CodeGen.Executor;

procedure FillProgramTemplates(Gen: TMVCCodeGenerator);
procedure FillControllerTemplates(Gen: TMVCCodeGenerator);
procedure FillWebModuleTemplates(Gen: TMVCCodeGenerator);
procedure FillWebModuleDFMTemplates(Gen: TMVCCodeGenerator);
procedure FillJSONRPCTemplates(Gen: TMVCCodeGenerator);
procedure FillTemplateProTemplates(Gen: TMVCCodeGenerator);
procedure FillWebStencilsTemplates(Gen: TMVCCodeGenerator);
procedure FillMustacheTemplates(Gen: TMVCCodeGenerator);
procedure FillEntitiesTemplates(Gen: TMVCCodeGenerator);
procedure FillServicesTemplates(Gen: TMVCCodeGenerator);

implementation

uses
  DMVC.Expert.Commons,
  DMVC.Expert.CodeGen.Commands;

procedure FillProgramTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitProgramCommand.Create,
    TUnitRunServerProcBody.Create,
    TUnitMainBeginEndCommand.Create
  ]);
end;

procedure FillControllerTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitControllerCommand.Create,
    TUnitControllerControllerDeclarationCommand.Create,
    TUnitFooterCommand.Create
    ]);
end;

procedure FillWebModuleTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitWebModuleDeclarationCommand.Create
    ]);
end;

procedure FillJSONRPCTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitJSONRPCDeclarationCommand.Create
    ]);
end;

procedure FillWebModuleDFMTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TWebModuleDFMCommand.Create
    ]);
end;

procedure FillMustacheTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitMustacheHelpersDeclarationCommand.Create
    ]);
end;

procedure FillTemplateProTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitTemplateProHelpersDeclarationCommand.Create
    ]);
end;

procedure FillWebStencilsTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitWebStencilsHelpersDeclarationCommand.Create
    ]);
end;


procedure FillServicesTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitServicesDeclarationCommand.Create
    ]);
end;

procedure FillEntitiesTemplates(Gen: TMVCCodeGenerator);
begin
  Gen.Commands.AddRange([
    TUnitControllerEntityDeclarationCommand.Create
    ]);
end;


end.
