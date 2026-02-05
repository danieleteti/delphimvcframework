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

unit DMVC.Expert.ProjectGenerator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  System.Rtti,
  Vcl.Dialogs,
  JsonDataObjects,
  TemplatePro,
  DMVC.Expert.Commons;

type
  /// <summary>
  /// Generates a complete DMVC project to a specified folder.
  /// All unit names are known upfront, avoiding the timing issues
  /// of the IOTAModuleServices approach.
  /// </summary>
  TDMVCProjectGenerator = class
  private
    class function GetScrambledAlphabet: string;
    class function LoadTemplate(const ATemplateName: string): string;
    class function GetDMVCVersion: string;
    class procedure LogToFile(const AMessage: string);
  public
    /// <summary>
    /// Generates a complete project to the specified folder
    /// </summary>
    class procedure Generate(const AProjectFolder, AProjectName: string; AConfig: TJSONObject);
    /// <summary>
    /// Renders a template with the given configuration
    /// </summary>
    class function RenderTemplate(const ATemplateName: string; AConfig: TJSONObject): string;
  end;

implementation

uses
  Winapi.Windows;

{ TDMVCProjectGenerator }

class function TDMVCProjectGenerator.GetScrambledAlphabet: string;
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

class procedure TDMVCProjectGenerator.LogToFile(const AMessage: string);
var
  LLogFile: TextFile;
  LLogPath: string;
  LTimestamp: string;
begin
  try
    LLogPath := TPath.Combine(TPath.GetHomePath, 'dmvc_wizard.log');
    AssignFile(LLogFile, LLogPath);
    if TFile.Exists(LLogPath) then
      Append(LLogFile)
    else
      Rewrite(LLogFile);
    try
      LTimestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
      WriteLn(LLogFile, Format('[%s] %s', [LTimestamp, AMessage]));
    finally
      CloseFile(LLogFile);
    end;
  except
    // Ignore logging errors
  end;
end;

class function TDMVCProjectGenerator.GetDMVCVersion: string;
var
  LBuildConstsPath: string;
  LFileContent: string;
  LStartPos, LEndPos: Integer;
begin
  // Read version from sources/dmvcframeworkbuildconsts.inc
  LBuildConstsPath := TPath.Combine(
    TPath.GetDirectoryName(TPath.GetDirectoryName(ExtractFilePath(GetModuleName(HInstance)))),
    'sources\dmvcframeworkbuildconsts.inc');

  if TFile.Exists(LBuildConstsPath) then
  begin
    LFileContent := TFile.ReadAllText(LBuildConstsPath, TEncoding.UTF8);
    // Extract version from: DMVCFRAMEWORK_VERSION = '3.5.0-silicon-beta';
    LStartPos := Pos('''', LFileContent);
    if LStartPos > 0 then
    begin
      LEndPos := PosEx('''', LFileContent, LStartPos + 1);
      if LEndPos > LStartPos then
        Result := Copy(LFileContent, LStartPos + 1, LEndPos - LStartPos - 1)
      else
        Result := '3.x';
    end
    else
      Result := '3.x';
  end
  else
    Result := '3.x';
end;

class function TDMVCProjectGenerator.LoadTemplate(const ATemplateName: string): string;
var
  LResName: string;
  LResStream: TResourceStream;
  LBytes: TBytes;
begin
  // Load template from embedded resources only
  // Convert template path to resource name: "views\index.tpro" -> "VIEWS_INDEX"
  LResName := ChangeFileExt(ATemplateName, '').Replace('.', '_').Replace('\', '_').ToUpper;
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
      raise Exception.CreateFmt('Template "%s" not found in embedded resources (resource name: %s)',
        [ATemplateName, LResName]);
  end;
end;

class function TDMVCProjectGenerator.RenderTemplate(const ATemplateName: string; AConfig: TJSONObject): string;
var
  LCompiler: TTProCompiler;
  LTemplate: ITProCompiledTemplate;
  LContent: string;
  I: Integer;
  LName, LOriginalName: string;
  LErrorMsg: string;
begin
  LContent := LoadTemplate(ATemplateName);
  LogToFile('Compiling template: ' + ATemplateName);
  LCompiler := TTProCompiler.Create;
  try
    // Handle includes
    LCompiler.OnGetIncludedTemplate :=
      procedure(const TemplateName: string; var TemplateContent: string; var Handled: Boolean)
      begin
        LogToFile('Loading include: ' + TemplateName);
        TemplateContent := LoadTemplate(TemplateName);
        Handled := True;
      end;

    try
      LTemplate := LCompiler.Compile(LContent);
      LogToFile('Successfully compiled: ' + ATemplateName);
    except
      on E: Exception do
      begin
        LErrorMsg := Format('Template compilation error in "%s":'#13#10'%s'#13#10#13#10 +
          'Log file: %s\dmvc_wizard.log', [ATemplateName, E.Message, TPath.GetHomePath]);
        LogToFile('ERROR: ' + E.Message);
        ShowMessage(LErrorMsg);
        raise Exception.Create(LErrorMsg);
      end;
    end;

    // Pass all JSON keys to template (convert dots to underscores)
    for I := 0 to AConfig.Count - 1 do
    begin
      LOriginalName := AConfig.Names[I];
      LName := LOriginalName.Replace('.', '_');
      case AConfig.Types[LOriginalName] of
        jdtString:
          LTemplate.SetData(LName, AConfig.S[LOriginalName]);
        jdtBool:
          LTemplate.SetData(LName, AConfig.B[LOriginalName]);
        jdtInt:
          LTemplate.SetData(LName, AConfig.I[LOriginalName]);
        jdtFloat:
          LTemplate.SetData(LName, AConfig.F[LOriginalName]);
      end;
    end;

    // Computed values
    LTemplate.SetData('current_year', YearOf(Now));
    LTemplate.SetData('new_guid', TGUID.NewGuid.ToString);
    LTemplate.SetData('scrambled_alphabet', GetScrambledAlphabet);
    LTemplate.SetData('dmvc_version', GetDMVCVersion);
    LTemplate.SetData('webmodule_form_reference',
      '{' + AConfig.S[TConfigKey.webmodule_classname_short] + ': TWebModule}');

    // Handler for missing variables - log warning but don't crash
    // For boolean checks ({{if var}}), undefined vars should evaluate to false
    LTemplate.OnGetValue :=
      procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean)
      begin
        // Special handling for runtime variables (prefixed with "data_")
        // These are preserved as template variables for runtime processing
        if DataSource.StartsWith('data_') then
        begin
          // Remove "data_" prefix and output as template variable for runtime
          Value := '{{:' + DataSource.Substring(5) + '}}';
          Handled := True;
          Exit;
        end;

        // Return empty/false for undefined variables (graceful degradation)
        // This allows {{if undefined_var}} to work as expected (evaluates to false)
        Value := TValue.Empty;
        Handled := True;
        {$IFDEF DEBUG}
        // In debug mode, output to debug console for visibility
        OutputDebugString(PChar(Format(
          '[DMVC Wizard] Warning: Undefined template variable "%s%s" in template "%s"',
          [DataSource, IfThen(Members.IsEmpty, '', '.' + Members), ATemplateName])));
        {$ENDIF}
      end;

    Result := LTemplate.Render;
  finally
    LCompiler.Free;
  end;
end;

class procedure TDMVCProjectGenerator.Generate(const AProjectFolder, AProjectName: string; AConfig: TJSONObject);

  procedure SaveFile(const AFileName, AContent: string);
  begin
    TFile.WriteAllText(TPath.Combine(AProjectFolder, AFileName), AContent, TEncoding.UTF8);
  end;

const
  CONTROLLER_UNIT = 'ControllerU';
  WEBMODULE_UNIT = 'WebModuleU';
  ENTITY_UNIT = 'EntitiesU';
  SERVICES_UNIT = 'ServicesU';
  JSONRPC_UNIT = 'JSONRPCServiceU';
  AUTHENTICATION_UNIT = 'AuthenticationU';
  WEBSOCKET_UNIT = 'WebSocketServerU';
  MUSTACHE_HELPERS_UNIT = 'MustacheHelpersU';
  TEMPLATEPRO_HELPERS_UNIT = 'TemplateProHelpersU';
  WEBSTENCILS_HELPERS_UNIT = 'WebStencilsHelpersU';
var
  LBinPath: string;
  LWwwPath: string;
  LCssPath: string;
  LTemplatesPath: string;
  LTemplateExt: string;
begin
  LogToFile('=== Starting project generation ===');
  LogToFile('Project: ' + AProjectName);
  LogToFile('Folder: ' + AProjectFolder);

  // Create project folder
  if not TDirectory.Exists(AProjectFolder) then
    TDirectory.CreateDirectory(AProjectFolder);

  // Set ALL unit names BEFORE rendering any template (key advantage!)
  AConfig.S[TConfigKey.program_name] := AProjectName;
  AConfig.S[TConfigKey.controller_unit_name] := CONTROLLER_UNIT;
  AConfig.S[TConfigKey.webmodule_unit_name] := WEBMODULE_UNIT;
  AConfig.S[TConfigKey.entity_unit_name] := ENTITY_UNIT;
  AConfig.S[TConfigKey.program_service_container_unit_name] := SERVICES_UNIT;
  AConfig.S[TConfigKey.jsonrpc_unit_name] := JSONRPC_UNIT;
  AConfig.S[TConfigKey.authentication_unit_name] := AUTHENTICATION_UNIT;
  AConfig.S[TConfigKey.authentication_classname] := 'TAuthentication';
  AConfig.S[TConfigKey.websocket_unit_name] := WEBSOCKET_UNIT;
  AConfig.S[TConfigKey.mustache_helpers_unit_name] := MUSTACHE_HELPERS_UNIT;
  AConfig.S[TConfigKey.templatepro_helpers_unit_name] := TEMPLATEPRO_HELPERS_UNIT;
  AConfig.S[TConfigKey.webstencils_helpers_unit_name] := WEBSTENCILS_HELPERS_UNIT;

  // Server-side view engine configuration
  AConfig.B['program.ssv.any'] := AConfig.B[TConfigKey.program_ssv_mustache] or
                                   AConfig.B[TConfigKey.program_ssv_templatepro] or
                                   AConfig.B[TConfigKey.program_ssv_webstencils];

  // Always use .html extension for better editor support
  AConfig.S['template.extension'] := 'html';

  if AConfig.B[TConfigKey.program_ssv_templatepro] then
    AConfig.S['template.engine.name'] := 'TemplatePro'
  else if AConfig.B[TConfigKey.program_ssv_mustache] then
    AConfig.S['template.engine.name'] := 'Mustache'
  else if AConfig.B[TConfigKey.program_ssv_webstencils] then
    AConfig.S['template.engine.name'] := 'WebStencils';

  AConfig.S['dmvc.version'] := 'v3.x';

  // Generate all files (all names already set!)

  // Project files
  // Note: We only generate the .dpr file. Delphi will automatically create
  // the .dproj file when opening the project for the first time.
  SaveFile(AProjectName + '.dpr', RenderTemplate('program.dpr.tpro', AConfig));

  // Required units
  SaveFile(CONTROLLER_UNIT + '.pas', RenderTemplate('controller.pas.tpro', AConfig));
  SaveFile(WEBMODULE_UNIT + '.pas', RenderTemplate('webmodule.pas.tpro', AConfig));
  SaveFile(WEBMODULE_UNIT + '.dfm', RenderTemplate('webmodule.dfm.tpro', AConfig));

  // Optional units
  if AConfig.B[TConfigKey.entity_generate] then
    SaveFile(ENTITY_UNIT + '.pas', RenderTemplate('entity.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.program_service_container_generate] then
    SaveFile(SERVICES_UNIT + '.pas', RenderTemplate('services.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.jsonrpc_generate] then
    SaveFile(JSONRPC_UNIT + '.pas', RenderTemplate('jsonrpc.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.webmodule_middleware_jwt] then
    SaveFile(AUTHENTICATION_UNIT + '.pas', RenderTemplate('authentication.pas.tpro', AConfig));

  // Debug: show websocket config value
  OutputDebugString(PChar(Format('[DMVC Wizard] websocket_generate key="%s" value=%s',
    [TConfigKey.websocket_generate, BoolToStr(AConfig.B[TConfigKey.websocket_generate], True)])));

  if AConfig.B[TConfigKey.websocket_generate] then
    SaveFile(WEBSOCKET_UNIT + '.pas', RenderTemplate('websocketserver.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.program_ssv_mustache] then
    SaveFile(MUSTACHE_HELPERS_UNIT + '.pas', RenderTemplate('helpers_mustache.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.program_ssv_templatepro] then
    SaveFile(TEMPLATEPRO_HELPERS_UNIT + '.pas', RenderTemplate('helpers_templatepro.pas.tpro', AConfig));

  if AConfig.B[TConfigKey.program_ssv_webstencils] then
    SaveFile(WEBSTENCILS_HELPERS_UNIT + '.pas', RenderTemplate('helpers_webstencils.pas.tpro', AConfig));

  // Create bin folder for deployment (exe + all support files go here)
  LBinPath := TPath.Combine(AProjectFolder, 'bin');
  TDirectory.CreateDirectory(LBinPath);

  // Create www folder for static files middleware (in bin/, same level as executable)
  if AConfig.B['program.ssv.any'] or AConfig.B[TConfigKey.webmodule_middleware_staticfiles] then
  begin
    LWwwPath := TPath.Combine(LBinPath, 'www');
    TDirectory.CreateDirectory(LWwwPath);

    // Create css subfolder
    LCssPath := TPath.Combine(LWwwPath, 'css');
    TDirectory.CreateDirectory(LCssPath);

    // Create style.css with the application styles
    TFile.WriteAllText(
      TPath.Combine(LCssPath, 'style.css'),
      '* {' + sLineBreak +
      '    margin: 0;' + sLineBreak +
      '    padding: 0;' + sLineBreak +
      '    box-sizing: border-box;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      'body {' + sLineBreak +
      '    font-family: -apple-system, BlinkMacSystemFont, ''Segoe UI'', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;' + sLineBreak +
      '    line-height: 1.6;' + sLineBreak +
      '    color: #e2e8f0;' + sLineBreak +
      '    background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);' + sLineBreak +
      '    min-height: 100vh;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.container {' + sLineBreak +
      '    max-width: 1200px;' + sLineBreak +
      '    margin: 0 auto;' + sLineBreak +
      '    padding: 20px;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      'header {' + sLineBreak +
      '    background: rgba(30, 41, 59, 0.9);' + sLineBreak +
      '    backdrop-filter: blur(10px);' + sLineBreak +
      '    padding: 20px 0;' + sLineBreak +
      '    margin-bottom: 30px;' + sLineBreak +
      '    border-radius: 10px;' + sLineBreak +
      '    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);' + sLineBreak +
      '    border: 1px solid rgba(71, 85, 105, 0.3);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      'header h1 {' + sLineBreak +
      '    color: #06b6d4;' + sLineBreak +
      '    font-size: 2em;' + sLineBreak +
      '    font-weight: 700;' + sLineBreak +
      '    text-align: center;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      'header p {' + sLineBreak +
      '    text-align: center;' + sLineBreak +
      '    color: #94a3b8;' + sLineBreak +
      '    margin-top: 5px;' + sLineBreak +
      '    font-size: 0.9em;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.content {' + sLineBreak +
      '    background: rgba(30, 41, 59, 0.6);' + sLineBreak +
      '    backdrop-filter: blur(10px);' + sLineBreak +
      '    padding: 40px;' + sLineBreak +
      '    border-radius: 10px;' + sLineBreak +
      '    box-shadow: 0 10px 25px rgba(0, 0, 0, 0.3);' + sLineBreak +
      '    min-height: 400px;' + sLineBreak +
      '    border: 1px solid rgba(71, 85, 105, 0.2);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.hero {' + sLineBreak +
      '    text-align: center;' + sLineBreak +
      '    padding: 40px 20px;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.hero h2 {' + sLineBreak +
      '    font-size: 2.5em;' + sLineBreak +
      '    color: #f1f5f9;' + sLineBreak +
      '    margin-bottom: 20px;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.hero p {' + sLineBreak +
      '    font-size: 1.2em;' + sLineBreak +
      '    color: #cbd5e1;' + sLineBreak +
      '    margin-bottom: 30px;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.features {' + sLineBreak +
      '    display: grid;' + sLineBreak +
      '    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));' + sLineBreak +
      '    gap: 30px;' + sLineBreak +
      '    margin-top: 40px;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.feature-card {' + sLineBreak +
      '    background: rgba(51, 65, 85, 0.5);' + sLineBreak +
      '    padding: 30px;' + sLineBreak +
      '    border-radius: 8px;' + sLineBreak +
      '    border-left: 4px solid #06b6d4;' + sLineBreak +
      '    transition: transform 0.3s ease, box-shadow 0.3s ease;' + sLineBreak +
      '    border: 1px solid rgba(71, 85, 105, 0.3);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.feature-card:hover {' + sLineBreak +
      '    transform: translateY(-5px);' + sLineBreak +
      '    box-shadow: 0 5px 15px rgba(6, 182, 212, 0.2);' + sLineBreak +
      '    background: rgba(51, 65, 85, 0.7);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.feature-card h3 {' + sLineBreak +
      '    color: #06b6d4;' + sLineBreak +
      '    margin-bottom: 10px;' + sLineBreak +
      '    font-size: 1.3em;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.feature-card p {' + sLineBreak +
      '    color: #cbd5e1;' + sLineBreak +
      '    line-height: 1.6;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.btn {' + sLineBreak +
      '    display: inline-block;' + sLineBreak +
      '    padding: 12px 30px;' + sLineBreak +
      '    background: linear-gradient(135deg, #0891b2 0%, #0e7490 100%);' + sLineBreak +
      '    color: white;' + sLineBreak +
      '    text-decoration: none;' + sLineBreak +
      '    border-radius: 5px;' + sLineBreak +
      '    font-weight: 600;' + sLineBreak +
      '    transition: transform 0.2s ease, box-shadow 0.2s ease;' + sLineBreak +
      '    border: none;' + sLineBreak +
      '    cursor: pointer;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.btn:hover {' + sLineBreak +
      '    transform: translateY(-2px);' + sLineBreak +
      '    box-shadow: 0 5px 15px rgba(6, 182, 212, 0.4);' + sLineBreak +
      '    background: linear-gradient(135deg, #06b6d4 0%, #0891b2 100%);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.info-box {' + sLineBreak +
      '    background: rgba(30, 41, 59, 0.8);' + sLineBreak +
      '    border-left: 4px solid #06b6d4;' + sLineBreak +
      '    padding: 15px 20px;' + sLineBreak +
      '    margin: 20px 0;' + sLineBreak +
      '    border-radius: 4px;' + sLineBreak +
      '    border: 1px solid rgba(71, 85, 105, 0.3);' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      '.info-box strong {' + sLineBreak +
      '    color: #06b6d4;' + sLineBreak +
      '}' + sLineBreak + sLineBreak +
      'code {' + sLineBreak +
      '    background: rgba(51, 65, 85, 0.6);' + sLineBreak +
      '    padding: 2px 6px;' + sLineBreak +
      '    border-radius: 3px;' + sLineBreak +
      '    font-family: ''Courier New'', monospace;' + sLineBreak +
      '    font-size: 0.9em;' + sLineBreak +
      '    color: #22d3ee;' + sLineBreak +
      '}' + sLineBreak,
      TEncoding.UTF8);

    // Create a sample index.html
    TFile.WriteAllText(
      TPath.Combine(LWwwPath, 'index.html'),
      '<!DOCTYPE html>' + sLineBreak +
      '<html>' + sLineBreak +
      '<head>' + sLineBreak +
      '  <title>Welcome</title>' + sLineBreak +
      '</head>' + sLineBreak +
      '<body>' + sLineBreak +
      '  <h1>Static Files Work!</h1>' + sLineBreak +
      '  <p>This file is served from the <code>/static</code> path.</p>' + sLineBreak +
      '  <p>Try: <a href="/static/index.html">/static/index.html</a></p>' + sLineBreak +
      '</body>' + sLineBreak +
      '</html>',
      TEncoding.UTF8);
  end;

  // Create templates folder for server-side views (in bin/, same level as executable)
  if AConfig.B[TConfigKey.program_ssv_mustache] or
     AConfig.B[TConfigKey.program_ssv_templatepro] or
     AConfig.B[TConfigKey.program_ssv_webstencils] then
  begin
    LTemplatesPath := TPath.Combine(LBinPath, 'templates');
    TDirectory.CreateDirectory(LTemplatesPath);

    // Determine template file extension
    LTemplateExt := AConfig.S['template.extension'];

    // Generate complete index template (includes header and footer inline)
    // This shows the complete page structure without needing CommonHeaders/Footers
    TFile.WriteAllText(
      TPath.Combine(LTemplatesPath, 'index.' + LTemplateExt),
      RenderTemplate('views\index_complete_view.tpro', AConfig),
      TEncoding.UTF8);
  end;

  // Create .gitignore file
  TFile.WriteAllText(
    TPath.Combine(AProjectFolder, '.gitignore'),
    '# Delphi compiled files' + sLineBreak +
    '*.exe' + sLineBreak +
    '*.dll' + sLineBreak +
    '*.bpl' + sLineBreak +
    '*.dcp' + sLineBreak +
    '*.dcu' + sLineBreak +
    '*.obj' + sLineBreak +
    '*.o' + sLineBreak +
    '*.res' + sLineBreak +
    '*.rsm' + sLineBreak +
    '*.map' + sLineBreak +
    '*.drc' + sLineBreak +
    '*.local' + sLineBreak +
    '*.identcache' + sLineBreak +
    '*.projdata' + sLineBreak +
    '*.tvsconfig' + sLineBreak +
    '*.dsk' + sLineBreak +
    '*.stat' + sLineBreak +
    sLineBreak +
    '# Delphi autogenerated files' + sLineBreak +
    '__history/' + sLineBreak +
    '__recovery/' + sLineBreak +
    '*.~*' + sLineBreak +
    sLineBreak +
    '# Build output' + sLineBreak +
    'bin/' + sLineBreak +
    'Win32/' + sLineBreak +
    'Win64/' + sLineBreak +
    'Debug/' + sLineBreak +
    'Release/' + sLineBreak +
    sLineBreak +
    '# Environment files' + sLineBreak +
    '.env' + sLineBreak +
    '*.env.local' + sLineBreak,
    TEncoding.UTF8);

  // Create sample .env file when dotenv is enabled (inside bin)
  if AConfig.B[TConfigKey.program_dotenv] then
  begin
    TFile.WriteAllText(
      TPath.Combine(AProjectFolder, 'bin' + PathDelim + '.env.sample'),
      '# Sample environment configuration' + sLineBreak +
      '# Copy this file to .env and modify the values' + sLineBreak +
      sLineBreak +
      '# Server configuration' + sLineBreak +
      'SERVER_PORT=' + AConfig.S[TConfigKey.program_default_server_port] + sLineBreak +
      sLineBreak +
      '# Database configuration (if using ActiveRecord)' + sLineBreak +
      '# DB_CONNECTION_DEF_NAME=MyConnection' + sLineBreak +
      '# DB_HOST=localhost' + sLineBreak +
      '# DB_PORT=5432' + sLineBreak +
      '# DB_NAME=mydb' + sLineBreak +
      '# DB_USER=user' + sLineBreak +
      '# DB_PASSWORD=password' + sLineBreak +
      sLineBreak +
      '# JWT configuration (if using JWT middleware)' + sLineBreak +
      '# JWT_SECRET=your-secret-key-change-this-in-production' + sLineBreak +
      '# JWT_EXPIRES_HOURS=24' + sLineBreak,
      TEncoding.UTF8);
  end;

  // Create sample FDConnectionDefs.ini when ActiveRecord middleware is enabled (inside bin)
  if AConfig.B[TConfigKey.webmodule_middleware_activerecord] then
  begin
    TFile.WriteAllText(
      TPath.Combine(AProjectFolder, 'bin' + PathDelim + AConfig.S[TConfigKey.con_def_filename]),
      '; FireDAC Connection Definitions' + sLineBreak +
      '; https://docwiki.embarcadero.com/RADStudio/en/Defining_Connection_(FireDAC)' + sLineBreak +
      sLineBreak +
      '[' + AConfig.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] + ']' + sLineBreak +
      '; PostgreSQL example' + sLineBreak +
      '; DriverID=PG' + sLineBreak +
      '; Server=localhost' + sLineBreak +
      '; Port=5432' + sLineBreak +
      '; Database=mydb' + sLineBreak +
      '; User_Name=postgres' + sLineBreak +
      '; Password=password' + sLineBreak +
      sLineBreak +
      '; SQLite example' + sLineBreak +
      'DriverID=SQLite' + sLineBreak +
      'Database=.\data\database.db' + sLineBreak +
      sLineBreak +
      '; InterBase/Firebird example' + sLineBreak +
      '; DriverID=IB' + sLineBreak +
      '; Server=localhost' + sLineBreak +
      '; Port=3050' + sLineBreak +
      '; Database=C:\databases\mydb.fdb' + sLineBreak +
      '; User_Name=SYSDBA' + sLineBreak +
      '; Password=masterkey' + sLineBreak +
      sLineBreak +
      '; MySQL/MariaDB example' + sLineBreak +
      '; DriverID=MySQL' + sLineBreak +
      '; Server=localhost' + sLineBreak +
      '; Port=3306' + sLineBreak +
      '; Database=mydb' + sLineBreak +
      '; User_Name=root' + sLineBreak +
      '; Password=password' + sLineBreak +
      sLineBreak +
      '; SQL Server example' + sLineBreak +
      '; DriverID=MSSQL' + sLineBreak +
      '; Server=localhost' + sLineBreak +
      '; Database=mydb' + sLineBreak +
      '; User_Name=sa' + sLineBreak +
      '; Password=password' + sLineBreak,
      TEncoding.UTF8);
  end;
end;

end.
