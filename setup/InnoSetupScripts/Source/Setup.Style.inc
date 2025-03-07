#ifndef UnitSetupStyle
#define UnitSetupStyle

[Code]
{************************************************************************}
{                                                                        }
{ InnoSetup Tools Library for Delphi Components                          }
{                                                                        }
{ Copyright (c) 2024-2025 Ethea S.r.l.                                   }
{                                                                        }
{ Original Code is Copyright (c) 2021-2025 Skia4Delphi Project.          }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
// unit Setup.Style;

// interface

// implementation

#ifdef VclStyle
{************************************************************************}
{                                                                        }
{                    VCL Styles Plugin for Inno Setup                    }
{               https://github.com/RRUZ/vcl-styles-plugins               }
{                                                                        }
{ Copyright (c) 2013-2025 Rodrigo Ruz V.                                 }
{                                                                        }
{ Use of this source code is governed by a MPL 1.1 license that can be   }
{ found at http://www.mozilla.org/MPL/                                   }
{                                                                        }
{************************************************************************}

// uses
  #include ".\Setup.Utils.inc"

#define _StylesDllName 'Setup.Styles.dll';
#define _StyleSubDir 'Style'

/// <summary> [External] Load a vcl style in setup </summary>
procedure _DllLoadVclStyle(AVclStyleFile: string); external 'LoadVCLStyleW@files:{#_StylesDllName} stdcall delayload setuponly';
/// <summary> [External] Load a vcl style in uninstaller </summary>
procedure _DllLoadVclStyleUninstall(AVclStyleFile: string); external 'LoadVCLStyleW@{tmp}\{#_StylesDllName} stdcall delayload uninstallonly';
/// <summary> [External] Unload vcl styles in setup </summary>
procedure _DllUnloadVclStyles; external 'UnLoadVCLStyles@files:{#_StylesDllName} stdcall setuponly delayload';
/// <summary> [External] Unload vcl styles in uninstaller </summary>
procedure _DllUnloadVclStylesUninstall; external 'UnLoadVCLStyles@{tmp}\{#_StylesDllName} stdcall delayload uninstallonly';

var
  _FIsUsingVclStyles: Boolean;

<event('DeinitializeSetup')>
procedure _SetupStyleDeinitializeSetup;
begin
  if _FIsUsingVclStyles then
    _DllUnloadVclStyles;
end;

<event('DeinitializeUninstall')>
procedure _SetupStyleDeinitializeUninstall;
begin
  if _FIsUsingVclStyles then
    _DllUnloadVclStylesUninstall;
end;

<event('InitializeSetup')>
function _SetupStyleInitializeSetup: Boolean;
begin
  _FIsUsingVclStyles := False;
  if not IsVerySilent then
  begin
    #ifdef FilesEmbedded
    ExtractTemporaryFile('{#VclStyle}');
    _DllLoadVclStyle(ExpandConstant('{tmp}\{#VclStyle}'));
    #else
    _DllLoadVclStyle(CombinePath(GetCurrentDir, '{#SetupFolder}\{#_StyleSubDir}\{#VclStyle}'));
    #endif
    _FIsUsingVclStyles := True;
  end;
  Result := True;
end;

<event('InitializeUninstall')>
function _SetupStyleInitializeUninstall: Boolean;
begin
  _FIsUsingVclStyles := False;
  if (not IsVerySilent) and
    FileCopy(ExpandConstant('{app}\{#SetupFolder}\{#_StyleSubDir}\{#_StylesDllName}'), ExpandConstant('{tmp}\{#_StylesDllName}'), False) and
    FileExists(CombinePath(ExpandConstant('{app}'), '{#SetupFolder}\{#_StyleSubDir}\{#VclStyle}')) then
  begin
    _DllLoadVclStyleUninstall(CombinePath(ExpandConstant('{app}'), '{#SetupFolder}\{#_StyleSubDir}\{#VclStyle}'));
    _FIsUsingVclStyles := True;
  end;
  Result := True;
end;

<event('InitializeWizard')>
procedure _SetupStyleInitializeWizard;
begin
  // Fix small issue in inno setup when styles enabled
  if _FIsUsingVclStyles then
    WizardForm.CancelButton.Top := WizardForm.NextButton.Top;
end;
#endif

// end.
#endif
