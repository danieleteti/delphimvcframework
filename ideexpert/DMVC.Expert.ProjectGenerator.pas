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
    class procedure SaveResourceToFile(const AResName, AFilePath: string);
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

class procedure TDMVCProjectGenerator.SaveResourceToFile(const AResName, AFilePath: string);
var
  LResStream: TResourceStream;
  LFileStream: TFileStream;
begin
  LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    LFileStream := TFileStream.Create(AFilePath, fmCreate);
    try
      LFileStream.CopyFrom(LResStream, LResStream.Size);
    finally
      LFileStream.Free;
    end;
  finally
    LResStream.Free;
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
    // Form reference for Windows Service
    LTemplate.SetData('service_form_reference',
      '{DMVCFrameworkWindowsService: TDMVCFrameworkWindowsService}');

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
    // Normalize line endings to CRLF (required by Delphi IDE)
    Result := Result.Replace(#13#10, #10).Replace(#10, #13#10);
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

  // Log key configuration values for debugging
  LogToFile('--- Configuration ---');
  LogToFile('program.server_engine: ' + AConfig.S[TConfigKey.program_server_engine]);
  LogToFile('program.type: ' + AConfig.S[TConfigKey.program_type]);
  if AConfig.Contains('program.server.protocol') then
    LogToFile('program.server.protocol: ' + AConfig.S['program.server.protocol']);
  LogToFile('program.default_server_port: ' + AConfig.S[TConfigKey.program_default_server_port]);
  LogToFile('controller.classname: ' + AConfig.S[TConfigKey.controller_classname]);
  LogToFile('webmodule.classname: ' + AConfig.S[TConfigKey.webmodule_classname]);
  if AConfig.Contains(TConfigKey.websocket_generate) then
    LogToFile('websocketserver.generate: ' + BoolToStr(AConfig.B[TConfigKey.websocket_generate], True));
  if AConfig.Contains(TConfigKey.jsonrpc_generate) then
    LogToFile('jsonrpc.generate: ' + BoolToStr(AConfig.B[TConfigKey.jsonrpc_generate], True));
  LogToFile('---------------------');

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

  // Determine server engine (default to 'webbroker' if not specified)
  if AConfig.S[TConfigKey.program_server_engine] = '' then
    AConfig.S[TConfigKey.program_server_engine] := 'webbroker';

  // Project files - branching on server engine + program type combinations
  if AConfig.S[TConfigKey.program_server_engine] = 'indydirect' then
  begin
    // Indy Direct: no WebBroker, no WebModule - uses EngineConfigU instead
    if AConfig.S[TConfigKey.program_type] = TProgramTypes.WINDOWS_SERVICE then
    begin
      SaveFile(AProjectName + '.dpr', RenderTemplate('program_service_indydirect.dpr.tpro', AConfig));
      SaveFile('ServiceU.pas', RenderTemplate('service_indydirect.pas.tpro', AConfig));
      SaveFile('ServiceU.dfm', RenderTemplate('service.dfm.tpro', AConfig));
    end
    else
      SaveFile(AProjectName + '.dpr', RenderTemplate('program_indydirect.dpr.tpro', AConfig));
    SaveFile('EngineConfigU.pas', RenderTemplate('engineconfig.pas.tpro', AConfig));
  end
  else if AConfig.S[TConfigKey.program_server_engine] = 'httpsys' then
  begin
    // HTTP.sys: no WebBroker, no WebModule - uses EngineConfigU instead
    if AConfig.S[TConfigKey.program_type] = TProgramTypes.WINDOWS_SERVICE then
    begin
      SaveFile(AProjectName + '.dpr', RenderTemplate('program_service_httpsys.dpr.tpro', AConfig));
      SaveFile('ServiceU.pas', RenderTemplate('service_httpsys.pas.tpro', AConfig));
      SaveFile('ServiceU.dfm', RenderTemplate('service.dfm.tpro', AConfig));
    end
    else
      SaveFile(AProjectName + '.dpr', RenderTemplate('program_httpsys.dpr.tpro', AConfig));
    SaveFile('EngineConfigU.pas', RenderTemplate('engineconfig.pas.tpro', AConfig));
  end
  else if AConfig.S[TConfigKey.program_type] = TProgramTypes.WINDOWS_SERVICE then
  begin
    // Windows Service (WebBroker) uses different program template and adds ServiceU unit
    SaveFile(AProjectName + '.dpr', RenderTemplate('program_service.dpr.tpro', AConfig));
    SaveFile('ServiceU.pas', RenderTemplate('service.pas.tpro', AConfig));
    SaveFile('ServiceU.dfm', RenderTemplate('service.dfm.tpro', AConfig));
  end
  else if AConfig.S[TConfigKey.program_type] = TProgramTypes.ISAPI then
    // ISAPI library (WebBroker, no main loop)
    SaveFile(AProjectName + '.dpr', RenderTemplate('program_isapi.dpr.tpro', AConfig))
  else if AConfig.S[TConfigKey.program_type] = TProgramTypes.APACHE then
    // Apache module library (WebBroker)
    SaveFile(AProjectName + '.dpr', RenderTemplate('program_apache.dpr.tpro', AConfig))
  else
  begin
    // Console/FastCGI use standard program template
    SaveFile(AProjectName + '.dpr', RenderTemplate('program.dpr.tpro', AConfig));
  end;

  // Generate .dproj with correct output paths (exe -> .\bin, dcu -> .\$(Platform)\$(Config))
  SaveFile(AProjectName + '.dproj', RenderTemplate('project.dproj.tpro', AConfig));

  // Required units
  SaveFile(CONTROLLER_UNIT + '.pas', RenderTemplate('controller.pas.tpro', AConfig));

  // WebModule is only generated for WebBroker server engine
  if (AConfig.S[TConfigKey.program_server_engine] = 'webbroker') or
     (AConfig.S[TConfigKey.program_server_engine] = '') then
  begin
    SaveFile(WEBMODULE_UNIT + '.pas', RenderTemplate('webmodule.pas.tpro', AConfig));
    SaveFile(WEBMODULE_UNIT + '.dfm', RenderTemplate('webmodule.dfm.tpro', AConfig));
  end;

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

    // Create style.css from template
    SaveFile('bin' + PathDelim + 'www' + PathDelim + 'css' + PathDelim + 'style.css',
      LoadTemplate('views\style_css.tpro'));

    // Favicon and web app icons
    SaveResourceToFile('WEBAPP_FAVICON_ICO', TPath.Combine(LWwwPath, 'favicon.ico'));
    SaveResourceToFile('WEBAPP_FAVICON_16', TPath.Combine(LWwwPath, 'favicon-16x16.png'));
    SaveResourceToFile('WEBAPP_FAVICON_32', TPath.Combine(LWwwPath, 'favicon-32x32.png'));
    SaveResourceToFile('WEBAPP_APPLE_TOUCH', TPath.Combine(LWwwPath, 'apple-touch-icon.png'));
    SaveResourceToFile('WEBAPP_ANDROID_192', TPath.Combine(LWwwPath, 'android-chrome-192x192.png'));
    SaveResourceToFile('WEBAPP_ANDROID_512', TPath.Combine(LWwwPath, 'android-chrome-512x512.png'));

    // Web app manifest
    SaveFile('bin' + PathDelim + 'www' + PathDelim + 'site.webmanifest',
      LoadTemplate('views\site_webmanifest.tpro')
        .Replace('{{:program_name}}', AProjectName));

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

    if AConfig.B[TConfigKey.program_ssv_templatepro] then
    begin
      // TemplatePro: use template inheritance pattern
      // These view files contain runtime TemplatePro directives (extends, block)
      // so they must NOT be processed by TemplatePro at wizard time.
      // We load them raw and only replace wizard-time placeholders.
      if AConfig.B[TConfigKey.program_htmx] then
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'baselayout.' + LTemplateExt,
          LoadTemplate('views\baselayout.tpro')
            .Replace('{{:program_name}}', AProjectName)
            .Replace('<!--HTMX_SCRIPT-->',
              '<script src="https://unpkg.com/htmx.org@2/dist/htmx.min.js"></script>'))
      else
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'baselayout.' + LTemplateExt,
          LoadTemplate('views\baselayout.tpro')
            .Replace('{{:program_name}}', AProjectName)
            .Replace('  <!--HTMX_SCRIPT-->' + #13#10, '')
            .Replace('  <!--HTMX_SCRIPT-->' + #10, ''));

      TDirectory.CreateDirectory(TPath.Combine(LTemplatesPath, 'home'));
      if AConfig.B[TConfigKey.program_htmx] then
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'home' + PathDelim + 'index.' + LTemplateExt,
          LoadTemplate('views\home_index_htmx.tpro')
            .Replace('{{:program_name}}', AProjectName)
            .Replace('{{:controller_unit_name}}', CONTROLLER_UNIT))
      else
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'home' + PathDelim + 'index.' + LTemplateExt,
          LoadTemplate('views\home_index.tpro')
            .Replace('{{:program_name}}', AProjectName)
            .Replace('{{:controller_unit_name}}', CONTROLLER_UNIT));

      TDirectory.CreateDirectory(TPath.Combine(LTemplatesPath, 'about'));
      if AConfig.B[TConfigKey.program_htmx] then
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'about' + PathDelim + 'index.' + LTemplateExt,
          LoadTemplate('views\about_index_htmx.tpro')
            .Replace('{{:program_name}}', AProjectName))
      else
        SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'about' + PathDelim + 'index.' + LTemplateExt,
          LoadTemplate('views\about_index.tpro')
            .Replace('{{:program_name}}', AProjectName));

      SaveFile('bin' + PathDelim + 'templates' + PathDelim + 'error.' + LTemplateExt,
        LoadTemplate('views\error_view.tpro')
          .Replace('{{:program_name}}', AProjectName));
    end
    else
    begin
      // Mustache/WebStencils: single-file template (no inheritance support)
      TFile.WriteAllText(
        TPath.Combine(LTemplatesPath, 'index.' + LTemplateExt),
        RenderTemplate('views\index_complete_view.tpro', AConfig),
        TEncoding.UTF8);
    end;
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

  // Create .env file in bin/ with all default values
  SaveFile('bin' + PathDelim + '.env', RenderTemplate('dotenv.tpro', AConfig));

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
