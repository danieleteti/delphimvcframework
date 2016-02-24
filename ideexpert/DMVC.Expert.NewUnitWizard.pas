{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DUnitX.Expert.NewUnitWizard;

interface

{$I DUnitX.inc}

uses
  ToolsApi,
  {$IFDEF USE_NS}
  VCL.Graphics;
  {$ELSE}
  Graphics;
  {$ENDIF}

type
  TDUnitXNewUnitWizard = class(TNotifierObject, IOTAWizard,
    IOTARepositoryWizard, IOTARepositoryWizard80, IOTAProjectWizard)
  protected
    FIcon: TIcon;
  public
    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    { Launch the AddIn }
    procedure Execute;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DUnitX.Expert.Forms.NewUnitWizard,
  DUnitX.Expert.CodeGen.NewTestUnit,
  Controls,
  Forms,
  Windows;

{ TNewBatchJobWizard }

constructor TDUnitXNewUnitWizard.Create;
begin
  FIcon := TIcon.Create;
  FIcon.LoadFromResourceName(HInstance, 'DUnitXNewUnitIcon');
end;

destructor TDUnitXNewUnitWizard.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TDUnitXNewUnitWizard.Execute;
var
  WizardForm     : TfrmDunitXNewUnit;
  ModuleServices : IOTAModuleServices;
  Project        : IOTAProject;
  TestUnit       : IOTAModule;
begin
  WizardForm := TfrmDunitXNewUnit.Create(Application);
  try
    if WizardForm.ShowModal = mrOk then
    begin
      ModuleServices := (BorlandIDEServices as IOTAModuleServices);
      Project :=  GetActiveProject;
      TestUnit := ModuleServices.CreateModule(
                       TNewTestUnit.Create(WizardForm.CreateSetupTearDownMethods,
                                           WizardForm.CreateSampleMethods,
                                           WizardForm.TestFixtureClasaName ));
      Project.AddFile(TestUnit.FileName,true);
    end;
  finally
    WizardForm.Free;
  end;
end;

function TDUnitXNewUnitWizard.GetAuthor: string;
begin
  result := 'DUnitX Team - https://github.com/VSoftTechnologies/DUnitX';
end;

function TDUnitXNewUnitWizard.GetComment: string;
begin
  result := 'Create New DUnitX Test Unit';
end;

function TDUnitXNewUnitWizard.GetDesigner: string;
begin
  result := dAny;
end;

function TDUnitXNewUnitWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  result := (BorlandIDEServices as IOTAGalleryCategoryManager)
    .FindCategory(sCategoryDelphiNewFiles);
end;

function TDUnitXNewUnitWizard.GetGlyph: Cardinal;
begin
  result := CopyIcon(FIcon.Handle);
end;

function TDUnitXNewUnitWizard.GetIDString: string;
begin
  result := 'DunitX.Wizard.NewUnitWizard';
end;

function TDUnitXNewUnitWizard.GetName: string;
begin
  result := 'DUnitX Unit';
end;

function TDUnitXNewUnitWizard.GetPage: string;
begin
  result := 'Delphi Files';
end;

function TDUnitXNewUnitWizard.GetPersonality: string;
begin
  result := sDelphiPersonality;
end;

function TDUnitXNewUnitWizard.GetState: TWizardState;
begin
  result := [wsEnabled];
end;

end.
