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

unit DMVC.Expert.CodeGen.TemplateEngine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  JsonDataObjects,
  TemplatePro;

type
  TDMVCTemplateEngine = class
  private
    class var FTemplatePath: string;
    class function LoadTemplateFromResource(const AName: string): string;
    class function LoadTemplateContent(const ATemplateName: string): string;
    class procedure PrepareModel(const ATemplate: ITProCompiledTemplate; AConfig: TJSONObject);
    class function GetScrambledAlphabet: string;
    class procedure ValidateConfig(AConfig: TJSONObject; const ATemplateName: string);
  public
    class function Render(const ATemplateName: string; AConfig: TJSONObject): string;
    class function GetTemplatePath: string; static;
    class procedure SetTemplatePath(const AValue: string); static;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.ShlObj;

{ TDMVCTemplateEngine }

class function TDMVCTemplateEngine.GetTemplatePath: string;
var
  LPublicDocsPath: array[0..MAX_PATH] of Char;
begin
  if FTemplatePath.IsEmpty then
  begin
    // Cerca nella cartella Documenti pubblici (C:\Users\Public\Documents\)
    if SHGetFolderPath(0, CSIDL_COMMON_DOCUMENTS, 0, SHGFP_TYPE_CURRENT, LPublicDocsPath) = S_OK then
      FTemplatePath := TPath.Combine(LPublicDocsPath, 'delphimvcframework_wizard_templates')
    else
      FTemplatePath := 'C:\Users\Public\Documents\delphimvcframework_wizard_templates';
  end;
  Result := FTemplatePath;
end;

class procedure TDMVCTemplateEngine.SetTemplatePath(const AValue: string);
begin
  FTemplatePath := AValue;
end;

class function TDMVCTemplateEngine.LoadTemplateContent(const ATemplateName: string): string;
var
  LExternalPath: string;
begin
  // Template che iniziano con "_" non possono essere sovrascritti (es. _license_header.tpro)
  if not ATemplateName.StartsWith('_') then
  begin
    // 1. Cerca file esterno (permette personalizzazione)
    LExternalPath := TPath.Combine(GetTemplatePath, ATemplateName);
    if TFile.Exists(LExternalPath) then
      Exit(TFile.ReadAllText(LExternalPath, TEncoding.UTF8));
  end;

  // 2. Fallback su risorsa embedded
  Result := LoadTemplateFromResource(ATemplateName);
end;

class function TDMVCTemplateEngine.LoadTemplateFromResource(const AName: string): string;
var
  LResName: string;
  LResStream: TResourceStream;
  LBytes: TBytes;
begin
  // Converti "program.dpr.tpro" -> "PROGRAM_DPR"
  LResName := ChangeFileExt(AName, '').Replace('.', '_').ToUpper;
  try
    LResStream := TResourceStream.Create(HInstance, LResName, RT_RCDATA);
    try
      SetLength(LBytes, LResStream.Size);
      if LResStream.Size > 0 then
        LResStream.ReadBuffer(LBytes[0], LResStream.Size);
      Result := TEncoding.UTF8.GetString(LBytes);
    finally
      LResStream.Free;
    end;
  except
    on E: EResNotFound do
      raise Exception.CreateFmt('Template resource "%s" not found for template "%s"', [LResName, AName]);
  end;
end;

class procedure TDMVCTemplateEngine.PrepareModel(const ATemplate: ITProCompiledTemplate; AConfig: TJSONObject);
var
  I: Integer;
  LName, LOriginalName: string;
begin
  // Passa tutte le chiavi JSON al template (converte i punti in underscore per compatibilità)
  for I := 0 to AConfig.Count - 1 do
  begin
    LOriginalName := AConfig.Names[I];
    LName := LOriginalName.Replace('.', '_');
    case AConfig.Types[LOriginalName] of
      jdtString:
        ATemplate.SetData(LName, AConfig.S[LOriginalName]);
      jdtBool:
        ATemplate.SetData(LName, AConfig.B[LOriginalName]);
      jdtInt:
        ATemplate.SetData(LName, AConfig.I[LOriginalName]);
      jdtFloat:
        ATemplate.SetData(LName, AConfig.F[LOriginalName]);
    end;
  end;

  // Valori calcolati dinamicamente
  ATemplate.SetData('current_year', YearOf(Now));
  ATemplate.SetData('new_guid', TGUID.NewGuid.ToString);
  ATemplate.SetData('scrambled_alphabet', GetScrambledAlphabet);
  // Form reference contains braces that conflict with TemplatePro syntax, so we compute it
  ATemplate.SetData('webmodule_form_reference',
    '{' + AConfig.S['webmodule.classname_short'] + ': TWebModule}');
end;

class procedure TDMVCTemplateEngine.ValidateConfig(AConfig: TJSONObject; const ATemplateName: string);
var
  I: Integer;
  LName, LValue: string;
  LMissingKeys: TArray<string>;
begin
  // Check for unset unit names - this helps debug timing issues
  LMissingKeys := [];
  for I := 0 to AConfig.Count - 1 do
  begin
    LName := AConfig.Names[I];
    if AConfig.Types[LName] = jdtString then
    begin
      LValue := AConfig.S[LName];
      if (LValue = 'TBA') or LValue.IsEmpty then
      begin
        // Only check for unit names
        if LName.EndsWith('_unit_name') or LName.EndsWith('.unit_name') then
          LMissingKeys := LMissingKeys + [LName + '=' + LValue.QuotedString('"')];
      end;
    end;
  end;
  {$IFDEF DEBUG}
  if Length(LMissingKeys) > 0 then
    OutputDebugString(PChar(Format(
      'DMVC Wizard Warning: Template "%s" has unset unit names: %s',
      [ATemplateName, String.Join(', ', LMissingKeys)])));
  {$ENDIF}
end;

class function TDMVCTemplateEngine.Render(const ATemplateName: string; AConfig: TJSONObject): string;
var
  LCompiler: TTProCompiler;
  LTemplate: ITProCompiledTemplate;
  LContent: string;
begin
  // Validate config (debug only - logs warnings for unset unit names)
  ValidateConfig(AConfig, ATemplateName);

  LContent := LoadTemplateContent(ATemplateName);
  LCompiler := TTProCompiler.Create;
  try
    // Usa OnGetIncludedTemplate per risolvere gli include da risorse embedded o file esterni
    LCompiler.OnGetIncludedTemplate :=
      procedure(const TemplateName: string; var TemplateContent: string; var Handled: Boolean)
      begin
        TemplateContent := LoadTemplateContent(TemplateName);
        Handled := True;
      end;

    LTemplate := LCompiler.Compile(LContent);
    PrepareModel(LTemplate, AConfig);
    Result := LTemplate.Render;
  finally
    LCompiler.Free;
  end;
end;

class function TDMVCTemplateEngine.GetScrambledAlphabet: string;
const
  DEFAULT_ALPHABET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  I, lIdx1, lIdx2, lSize: Integer;
  lTmp: Char;
begin
  Randomize;
  Result := DEFAULT_ALPHABET;
  lSize := Length(Result);
  for I := 1 to 100 do
  begin
    lIdx1 := Random(lSize) + 1;
    lIdx2 := Random(lSize) + 1;
    lTmp := Result[lIdx1];
    Result[lIdx1] := Result[lIdx2];
    Result[lIdx2] := lTmp;
  end;
end;

end.
