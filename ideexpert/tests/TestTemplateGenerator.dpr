// ***************************************************************************
//
// Delphi MVC Framework - Template Generator Test Tool
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// ***************************************************************************
//
// This tool tests the TemplatePro-based code generation by:
// 1. Creating test configurations (like from the wizard UI)
// 2. Generating projects to a test folder
// 3. Reporting results
//
// Usage: TestTemplateGenerator.exe [options]
//   --output-dir=<path>   Output directory for generated projects (default: .\output)
//   --verbose             Show detailed output
//
// ***************************************************************************

program TestTemplateGenerator;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.Rtti,
  Winapi.Windows,
  Winapi.ShellAPI,
  JsonDataObjects,
  TemplatePro;

type
  // Config keys (duplicated from DMVC.Expert.Commons to avoid ToolsAPI dependency)
  TConfigKey = class sealed
  public const
    program_name = 'program.name';
    program_default_server_port = 'program.default_server_port';
    program_msheap = 'program.msheap';
    program_sqids = 'program.sqids';
    program_dotenv = 'program.dotenv';
    program_ssv_templatepro = 'program.ssv.templatepro';
    program_ssv_webstencils = 'program.ssv.webstencils';
    program_ssv_mustache = 'program.ssv.mustache';
    program_type = 'program.type';
    program_service_container_generate = 'program.service.container.generate';
    program_service_container_unit_name = 'program.service.container.unit_name';
    mustache_helpers_unit_name = 'mustache.helpers_unit_name';
    templatepro_helpers_unit_name = 'templatepro.helpers_unit_name';
    webstencils_helpers_unit_name = 'webstencils.helpers_unit_name';
    controller_unit_name = 'controller.unit_name';
    controller_classname = 'controller.classname';
    controller_index_methods_generate = 'controller.index_methods.generate';
    controller_action_filters_generate = 'controller.action_filters.generate';
    controller_crud_methods_generate = 'controller.crud_methods.generate';
    controller_actions_profiling_generate = 'controller.actions.profiling.generate';
    entity_generate = 'entity.generate';
    entity_classname = 'entity.classname';
    entity_unit_name = 'entity.unit_name';
    jsonrpc_generate = 'jsonrpc.generate';
    jsonrpc_classname = 'jsonrpc.classname';
    jsonrpc_unit_name = 'jsonrpc.unit_name';
    authentication_unit_name = 'authentication.unit_name';
    authentication_classname = 'authentication.classname';
    websocket_unit_name = 'websocketserver.unit_name';
    websocket_generate = 'websocketserver.generate';
    serializer_name_case = 'serializer.name_case';
    webmodule_classname = 'webmodule.classname';
    webmodule_unit_name = 'webmodule.unit_name';
    webmodule_classname_short = 'webmodule.classname_short';
    default_media_type = 'default_media_type';
    webmodule_middleware_analytics = 'webmodule.middleware.analytics';
    webmodule_middleware_staticfiles = 'webmodule.middleware.staticfiles';
    webmodule_middleware_trace = 'webmodule.middleware.trace';
    webmodule_middleware_compression = 'webmodule.middleware.compression';
    webmodule_middleware_etag = 'webmodule.middleware.etag';
    webmodule_middleware_cors = 'webmodule.middleware.cors';
    webmodule_middleware_ratelimit = 'webmodule.middleware.ratelimit';
    webmodule_middleware_jwt = 'webmodule.middleware.jwt';
    webmodule_middleware_activerecord = 'webmodule.middleware.activerecord';
    webmodule_middleware_activerecord_con_def_name = 'webmodule.middleware.activerecord.con_def_name';
    webmodule_middleware_activerecord_con_def_filename = 'webmodule.middleware.activerecord.con_def_filename';
    con_def_filename = 'con_def_filename';
    webmodule_middleware_session_memory = 'webmodule.middleware.session.memory';
    webmodule_middleware_session_file = 'webmodule.middleware.session.file';
    webmodule_middleware_session_database = 'webmodule.middleware.session.database';
    webmodule_middleware_session_timeout = 'webmodule.middleware.session.timeout';
  end;

  TProgramTypes = record
  public const
    HTTP_CONSOLE = 'http.console';
    HTTPS_CONSOLE = 'https.console';
    FASTCGI_CONSOLE = 'fastcgi.console';
  end;

  TTestCase = record
    Name: string;
    Config: TJSONObject;
  end;

  TTestResult = record
    TestName: string;
    GenerationOK: Boolean;
    CompilationOK: Boolean;
    ErrorMessage: string;
  end;

  // Simplified template engine for testing
  TTestTemplateEngine = class
  private
    class var FTemplatePath: string;
    class function GetScrambledAlphabet: string;
  public
    class function Render(const ATemplateName: string; AConfig: TJSONObject): string;
    class function GetTemplatePath: string;
    class procedure SetTemplatePath(const AValue: string);
  end;

var
  GOutputDir: string;
  GVerbose: Boolean;
  GTestResults: TList<TTestResult>;
  GDelphiPath: string;
  GSkipCompile: Boolean;

procedure Log(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure LogVerbose(const AMsg: string);
begin
  if GVerbose then
    WriteLn('  ' + AMsg);
end;

function FindDelphiPath: string;
const
  // Studio version numbers: 37=D13, 24=D12, 23=D11.3, 22=D11, 21=D10.4, 20=D10.3, 19=D10.2, 18=D10.1
  DELPHI_VERSIONS: array[0..7] of string = ('37.0', '24.0', '23.0', '22.0', '21.0', '20.0', '19.0', '18.0');
var
  LVersion: string;
  LPath: string;
begin
  Result := '';
  // Try standard paths for Delphi installations
  for LVersion in DELPHI_VERSIONS do
  begin
    LPath := Format('C:\Program Files (x86)\Embarcadero\Studio\%s\bin\rsvars.bat', [LVersion]);
    if TFile.Exists(LPath) then
      Exit(ExtractFilePath(LPath));
  end;
end;

function CompileProject(const AProjectDir, AProjectName: string; out AErrorOutput: string): Boolean;
var
  LDprPath: string;
  LBatFile: string;
  LOutputFile: string;
  LBatContent: string;
  LStartInfo: TStartupInfo;
  LProcInfo: TProcessInformation;
  LExitCode: DWORD;
  LCmdLine: string;
  LAbsProjectDir: string;
begin
  Result := False;
  AErrorOutput := '';

  if GDelphiPath.IsEmpty then
  begin
    AErrorOutput := 'Delphi not found';
    Exit;
  end;

  // Use absolute paths
  LAbsProjectDir := TPath.GetFullPath(AProjectDir);
  LDprPath := TPath.Combine(LAbsProjectDir, AProjectName + '.dpr');
  LOutputFile := TPath.Combine(LAbsProjectDir, 'compile_output.txt');
  LBatFile := TPath.Combine(LAbsProjectDir, 'compile.bat');

  // Create output folder (matches IDE wizard output configuration: .\$(Platform)\$(Config))
  ForceDirectories(TPath.Combine(LAbsProjectDir, 'Win32' + PathDelim + 'Debug'));

  // Create a minimal resource file if not exists
  if not TFile.Exists(TPath.Combine(LAbsProjectDir, AProjectName + '.res')) then
  begin
    // Create a minimal .rc file and compile it
    TFile.WriteAllText(TPath.Combine(LAbsProjectDir, AProjectName + '.rc'),
      '// Minimal resource file' + sLineBreak, TEncoding.ASCII);
  end;

  // Create batch file to compile
  // Include all DMVC search paths (sources and lib folders)
  // Output to bin folder (-E.\bin) to match IDE wizard configuration
  LBatContent :=
    '@echo off' + sLineBreak +
    'cd /d "' + LAbsProjectDir + '"' + sLineBreak +
    'call "' + GDelphiPath + 'rsvars.bat"' + sLineBreak +
    'brcc32 "' + AProjectName + '.rc" > nul 2>&1' + sLineBreak +  // Compile RC to RES
    'dcc32 -B -Q ' +
    '-E".\Win32\Debug" ' +  // Output exe to Win32\Debug folder
    '-N".\Win32\Debug" ' +  // Output dcu to Win32\Debug folder
    '-U"C:\DEV\dmvcframework\sources" ' +
    '-U"C:\DEV\dmvcframework\lib\loggerpro" ' +
    '-U"C:\DEV\dmvcframework\lib\dmustache" ' +
    '-U"C:\DEV\dmvcframework\lib\swagdoc\Source" ' +
    '-U"C:\DLib\indy_extras\TaurusTLS\Source" ' +
    '-U"C:\DLib\indy_extras\TaurusTLS\Source\Extra" ' +
    '-NSSystem;Winapi;System.Win;Vcl;Data;Data.Win;Web;Soap;Xml ' +
    '"' + LDprPath + '" > "' + LOutputFile + '" 2>&1' + sLineBreak +
    'exit /b %errorlevel%';
  TFile.WriteAllText(LBatFile, LBatContent, TEncoding.ASCII);

  // Run the batch file
  FillChar(LStartInfo, SizeOf(LStartInfo), 0);
  LStartInfo.cb := SizeOf(LStartInfo);
  LStartInfo.dwFlags := STARTF_USESHOWWINDOW;
  LStartInfo.wShowWindow := SW_HIDE;

  FillChar(LProcInfo, SizeOf(LProcInfo), 0);

  LCmdLine := 'cmd.exe /c "' + LBatFile + '"';

  if CreateProcess(nil, PChar(LCmdLine), nil, nil, False,
    CREATE_NO_WINDOW, nil, PChar(AProjectDir), LStartInfo, LProcInfo) then
  begin
    WaitForSingleObject(LProcInfo.hProcess, 120000); // 2 minute timeout
    GetExitCodeProcess(LProcInfo.hProcess, LExitCode);
    CloseHandle(LProcInfo.hProcess);
    CloseHandle(LProcInfo.hThread);

    Result := (LExitCode = 0);

    if TFile.Exists(LOutputFile) then
    begin
      AErrorOutput := TFile.ReadAllText(LOutputFile);
      if not Result and (AErrorOutput.Length > 500) then
        AErrorOutput := AErrorOutput.Substring(0, 500) + '...';
    end;
  end
  else
    AErrorOutput := 'Failed to start compiler process';
end;

{ TTestTemplateEngine }

class function TTestTemplateEngine.GetTemplatePath: string;
begin
  Result := FTemplatePath;
end;

class procedure TTestTemplateEngine.SetTemplatePath(const AValue: string);
begin
  FTemplatePath := AValue;
end;

class function TTestTemplateEngine.GetScrambledAlphabet: string;
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

class function TTestTemplateEngine.Render(const ATemplateName: string; AConfig: TJSONObject): string;
var
  LCompiler: TTProCompiler;
  LTemplate: ITProCompiledTemplate;
  LContent: string;
  LTemplateFullPath: string;
  I: Integer;
  LName: string;
begin
  LTemplateFullPath := TPath.Combine(FTemplatePath, ATemplateName);
  if not TFile.Exists(LTemplateFullPath) then
    raise Exception.CreateFmt('Template file not found: %s', [LTemplateFullPath]);

  LContent := TFile.ReadAllText(LTemplateFullPath, TEncoding.UTF8);
  LCompiler := TTProCompiler.Create;
  try
    LTemplate := LCompiler.Compile(LContent, LTemplateFullPath);

    // Pass all JSON keys to template (convert dots to underscores for template compatibility)
    for I := 0 to AConfig.Count - 1 do
    begin
      LName := AConfig.Names[I].Replace('.', '_');
      case AConfig.Types[AConfig.Names[I]] of
        jdtString:
          LTemplate.SetData(LName, AConfig.S[AConfig.Names[I]]);
        jdtBool:
          LTemplate.SetData(LName, AConfig.B[AConfig.Names[I]]);
        jdtInt:
          LTemplate.SetData(LName, AConfig.I[AConfig.Names[I]]);
      end;
    end;

    // Computed values
    LTemplate.SetData('current_year', YearOf(Now));
    LTemplate.SetData('new_guid', TGUID.NewGuid.ToString);
    LTemplate.SetData('scrambled_alphabet', GetScrambledAlphabet);
    // Form reference contains braces that conflict with TemplatePro syntax, so we compute it
    LTemplate.SetData('webmodule_form_reference',
      '{' + AConfig.S[TConfigKey.webmodule_classname_short] + ': TWebModule}');

    // Handler for missing variables - helps catch template errors early
    LTemplate.OnGetValue :=
      procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean)
      var
        LVarName: string;
      begin
        // Build the full variable name
        if Members.IsEmpty then
          LVarName := DataSource
        else
          LVarName := DataSource + '.' + Members;
        // Raise exception for undefined variables
        raise Exception.CreateFmt(
          'Undefined template variable "%s" in template "%s". ' +
          'Check that the variable name matches a config key (with dots replaced by underscores).',
          [LVarName, ATemplateName]);
      end;

    Result := LTemplate.Render;
  finally
    LCompiler.Free;
  end;
end;

function CreateBaseConfig: TJSONObject;
begin
  Result := TJSONObject.Create;

  // Program defaults
  Result.S[TConfigKey.program_name] := 'TestProject';
  Result.S[TConfigKey.program_default_server_port] := '8080';
  Result.B[TConfigKey.program_msheap] := False;
  Result.B[TConfigKey.program_sqids] := False;
  Result.B[TConfigKey.program_dotenv] := True;
  Result.B[TConfigKey.program_ssv_templatepro] := False;
  Result.B[TConfigKey.program_ssv_webstencils] := False;
  Result.B[TConfigKey.program_ssv_mustache] := False;
  Result.B['program.ssv.any'] := False;  // Will be set to True in specific test cases
  Result.S[TConfigKey.program_type] := TProgramTypes.HTTP_CONSOLE;
  Result.B[TConfigKey.program_service_container_generate] := False;
  Result.S[TConfigKey.program_service_container_unit_name] := 'ServicesU';

  // Controller defaults
  Result.S[TConfigKey.controller_unit_name] := 'MainControllerU';
  Result.S[TConfigKey.controller_classname] := 'TMainController';
  Result.B[TConfigKey.controller_index_methods_generate] := True;
  Result.B[TConfigKey.controller_action_filters_generate] := False;
  Result.B[TConfigKey.controller_crud_methods_generate] := False;
  Result.B[TConfigKey.controller_actions_profiling_generate] := False;

  // Entity defaults
  Result.B[TConfigKey.entity_generate] := False;
  Result.S[TConfigKey.entity_classname] := 'TPerson';
  Result.S[TConfigKey.entity_unit_name] := 'EntitiesU';

  // JSON-RPC defaults
  Result.B[TConfigKey.jsonrpc_generate] := False;
  Result.S[TConfigKey.jsonrpc_classname] := 'TMyJSONRPCService';
  Result.S[TConfigKey.jsonrpc_unit_name] := 'JSONRPCServiceU';

  // Authentication defaults
  Result.S[TConfigKey.authentication_classname] := 'TAuthentication';
  Result.S[TConfigKey.authentication_unit_name] := 'AuthenticationU';

  // WebSocket defaults
  Result.B[TConfigKey.websocket_generate] := False;
  Result.S[TConfigKey.websocket_unit_name] := 'WebSocketServerU';

  // Serializer
  Result.S[TConfigKey.serializer_name_case] := 'ncLowerCase';

  // WebModule defaults
  Result.S[TConfigKey.webmodule_classname] := 'TMyWebModule';
  Result.S[TConfigKey.webmodule_unit_name] := 'WebModuleU';
  Result.S[TConfigKey.webmodule_classname_short] := 'MyWebModule';
  Result.S[TConfigKey.default_media_type] := 'TMVCConstants.DEFAULT_CONTENT_TYPE';

  // Middleware defaults
  Result.B[TConfigKey.webmodule_middleware_analytics] := False;
  Result.B[TConfigKey.webmodule_middleware_staticfiles] := False;
  Result.B[TConfigKey.webmodule_middleware_trace] := False;
  Result.B[TConfigKey.webmodule_middleware_compression] := False;
  Result.B[TConfigKey.webmodule_middleware_etag] := False;
  Result.B[TConfigKey.webmodule_middleware_cors] := False;
  Result.B[TConfigKey.webmodule_middleware_ratelimit] := False;
  Result.B[TConfigKey.webmodule_middleware_jwt] := False;
  Result.B[TConfigKey.webmodule_middleware_activerecord] := False;
  Result.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] := 'MyConnection';
  Result.S[TConfigKey.webmodule_middleware_activerecord_con_def_filename] := '$(AppPath)FDConnectionDefs.ini';
  Result.S[TConfigKey.con_def_filename] := 'FDConnectionDefs.ini';

  // Session middleware defaults
  Result.B[TConfigKey.webmodule_middleware_session_memory] := False;
  Result.B[TConfigKey.webmodule_middleware_session_file] := False;
  Result.B[TConfigKey.webmodule_middleware_session_database] := False;
  Result.I[TConfigKey.webmodule_middleware_session_timeout] := 0;

  // Template helpers
  Result.S[TConfigKey.mustache_helpers_unit_name] := 'MustacheHelpersU';
  Result.S[TConfigKey.templatepro_helpers_unit_name] := 'TemplateProHelpersU';
  Result.S[TConfigKey.webstencils_helpers_unit_name] := 'WebStencilsHelpersU';
end;

function GenerateProject(const AConfig: TJSONObject; const AOutputDir: string): Boolean;
var
  LSource: string;
begin
  Result := True;
  try
    ForceDirectories(AOutputDir);

    // Set server-side view engine metadata BEFORE generating any files
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

    // Generate program.dpr
    // Note: We only generate the .dpr file. Delphi will automatically create
    // the .dproj file when opening/compiling the project for the first time.
    LogVerbose('Generating program.dpr...');
    LSource := TTestTemplateEngine.Render('program.dpr.tpro', AConfig);
    TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.program_name] + '.dpr'), LSource);

    // Generate controller
    LogVerbose('Generating controller...');
    LSource := TTestTemplateEngine.Render('controller.pas.tpro', AConfig);
    TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.controller_unit_name] + '.pas'), LSource);

    // Generate webmodule
    LogVerbose('Generating webmodule...');
    LSource := TTestTemplateEngine.Render('webmodule.pas.tpro', AConfig);
    TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.webmodule_unit_name] + '.pas'), LSource);

    LogVerbose('Generating webmodule.dfm...');
    LSource := TTestTemplateEngine.Render('webmodule.dfm.tpro', AConfig);
    TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.webmodule_unit_name] + '.dfm'), LSource);

    // Generate optional units
    if AConfig.B[TConfigKey.entity_generate] then
    begin
      LogVerbose('Generating entity...');
      LSource := TTestTemplateEngine.Render('entity.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.entity_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.program_service_container_generate] then
    begin
      LogVerbose('Generating services...');
      LSource := TTestTemplateEngine.Render('services.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.program_service_container_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.jsonrpc_generate] then
    begin
      LogVerbose('Generating jsonrpc...');
      LSource := TTestTemplateEngine.Render('jsonrpc.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.jsonrpc_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.webmodule_middleware_jwt] then
    begin
      LogVerbose('Generating authentication handler...');
      LSource := TTestTemplateEngine.Render('authentication.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.authentication_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.websocket_generate] then
    begin
      LogVerbose('Generating websocket server...');
      LSource := TTestTemplateEngine.Render('websocketserver.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.websocket_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.program_ssv_mustache] then
    begin
      LogVerbose('Generating mustache helpers...');
      LSource := TTestTemplateEngine.Render('helpers_mustache.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.mustache_helpers_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.program_ssv_templatepro] then
    begin
      LogVerbose('Generating templatepro helpers...');
      LSource := TTestTemplateEngine.Render('helpers_templatepro.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.templatepro_helpers_unit_name] + '.pas'), LSource);
    end;

    if AConfig.B[TConfigKey.program_ssv_webstencils] then
    begin
      LogVerbose('Generating webstencils helpers...');
      LSource := TTestTemplateEngine.Render('helpers_webstencils.pas.tpro', AConfig);
      TFile.WriteAllText(TPath.Combine(AOutputDir, AConfig.S[TConfigKey.webstencils_helpers_unit_name] + '.pas'), LSource);
    end;

    // Create www folder for static files middleware
    if AConfig.B[TConfigKey.webmodule_middleware_staticfiles] then
    begin
      LogVerbose('Creating www folder...');
      ForceDirectories(TPath.Combine(AOutputDir, 'www'));
      TFile.WriteAllText(
        TPath.Combine(AOutputDir, 'www\index.html'),
        '<!DOCTYPE html>' + sLineBreak +
        '<html>' + sLineBreak +
        '<head>' + sLineBreak +
        '  <title>Welcome</title>' + sLineBreak +
        '</head>' + sLineBreak +
        '<body>' + sLineBreak +
        '  <h1>Static Files Work!</h1>' + sLineBreak +
        '  <p>This file is served from the <code>/static</code> path.</p>' + sLineBreak +
        '</body>' + sLineBreak +
        '</html>');
    end;

    // Create templates folder for server-side views
    // Generate in project root (not in bin/) so they're accessible during development
    if AConfig.B[TConfigKey.program_ssv_mustache] or
       AConfig.B[TConfigKey.program_ssv_templatepro] or
       AConfig.B[TConfigKey.program_ssv_webstencils] then
    begin
      LogVerbose('Creating templates folder with header, footer and index...');
      ForceDirectories(TPath.Combine(AOutputDir, 'templates'));

      // Template engine metadata is already set at the beginning of GenerateProject
      var LTemplateExt := AConfig.S['template.extension'];

      // Generate complete index template
      TFile.WriteAllText(
        TPath.Combine(AOutputDir, 'templates\index.' + LTemplateExt),
        TTestTemplateEngine.Render('views\index_complete_view.tpro', AConfig));
    end;

    // Create .gitignore file
    LogVerbose('Creating .gitignore...');
    TFile.WriteAllText(
      TPath.Combine(AOutputDir, '.gitignore'),
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
      '*.env.local' + sLineBreak);

    // Create sample .env file when dotenv is enabled
    if AConfig.B[TConfigKey.program_dotenv] then
    begin
      LogVerbose('Creating .env.sample...');
      TFile.WriteAllText(
        TPath.Combine(AOutputDir, '.env.sample'),
        '# Sample environment configuration' + sLineBreak +
        '# Copy this file to .env and modify the values' + sLineBreak +
        sLineBreak +
        '# Application base path (relative path from exe to project root)' + sLineBreak +
        '# Development default: ..\.. (exe in Win32\Debug)' + sLineBreak +
        '# Production: . (exe alongside templates/www/etc.)' + sLineBreak +
        '# APP_BASE_PATH=.' + sLineBreak +
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
        '# JWT_EXPIRES_HOURS=24' + sLineBreak);
    end;

    // Create sample FDConnectionDefs.ini when ActiveRecord middleware is enabled
    if AConfig.B[TConfigKey.webmodule_middleware_activerecord] then
    begin
      LogVerbose('Creating FDConnectionDefs.ini and data folder...');
      TFile.WriteAllText(
        TPath.Combine(AOutputDir, 'FDConnectionDefs.ini'),
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
        '; Password=password' + sLineBreak);

      // Create data folder for SQLite database
      ForceDirectories(TPath.Combine(AOutputDir, 'data'));
    end;

  except
    on E: Exception do
    begin
      Result := False;
      Log('ERROR: ' + E.Message);
    end;
  end;
end;

procedure RunTest(const ATestName: string; const AConfig: TJSONObject);
var
  LResult: TTestResult;
  LOutputDir: string;
  LCompileError: string;
begin
  Log('');
  Log('=== Test: ' + ATestName + ' ===');

  LResult.TestName := ATestName;
  LResult.GenerationOK := False;
  LResult.CompilationOK := False;
  LResult.ErrorMessage := '';

  LOutputDir := TPath.Combine(GOutputDir, ATestName);

  try
    LResult.GenerationOK := GenerateProject(AConfig, LOutputDir);
    if LResult.GenerationOK then
    begin
      Log('Generation: OK');

      // Try to compile if not skipped
      if not GSkipCompile then
      begin
        LogVerbose('Compiling...');
        LResult.CompilationOK := CompileProject(LOutputDir, AConfig.S[TConfigKey.program_name], LCompileError);
        if LResult.CompilationOK then
          Log('Compilation: OK')
        else
        begin
          Log('Compilation: FAILED');
          LResult.ErrorMessage := LCompileError;
          LogVerbose(LCompileError);
        end;
      end
      else
      begin
        Log('Compilation: SKIPPED');
        LResult.CompilationOK := True; // Mark as OK when skipped
      end;
    end
    else
    begin
      Log('Generation: FAILED');
      LResult.ErrorMessage := 'Generation failed';
    end;
  except
    on E: Exception do
    begin
      LResult.ErrorMessage := E.Message;
      Log('ERROR: ' + E.Message);
    end;
  end;

  GTestResults.Add(LResult);
end;

procedure CreateTestCases(ATestCases: TList<TTestCase>);
var
  LTestCase: TTestCase;
begin
  // Test 1: Minimal HTTP Console
  LTestCase.Name := 'minimal_http';
  LTestCase.Config := CreateBaseConfig;
  ATestCases.Add(LTestCase);

  // Test 2: HTTP Console with CRUD
  LTestCase.Name := 'http_with_crud';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 3: HTTP Console with all middleware
  LTestCase.Name := 'http_all_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_analytics] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_trace] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_ratelimit] := True;
  ATestCases.Add(LTestCase);

  // Test 4: HTTPS Console
  LTestCase.Name := 'https_console';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPS_CONSOLE;
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // Test 5: FastCGI Console
  LTestCase.Name := 'fastcgi_console';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.FASTCGI_CONSOLE;
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '9000';
  ATestCases.Add(LTestCase);

  // Test 6: With Service Container
  LTestCase.Name := 'with_service_container';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_service_container_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 7: With JSON-RPC
  LTestCase.Name := 'with_jsonrpc';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 8: With WebSocket
  LTestCase.Name := 'with_websocket';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.websocket_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 9: With TemplatePro SSV
  LTestCase.Name := 'with_templatepro_ssv';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  ATestCases.Add(LTestCase);

  // Test 10: With Mustache SSV
  LTestCase.Name := 'with_mustache_ssv';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_mustache] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  ATestCases.Add(LTestCase);

  // Test 11: With WebStencils SSV
  LTestCase.Name := 'with_webstencils_ssv';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_webstencils] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  ATestCases.Add(LTestCase);

  // Test 12: With Action Filters and Profiling
  LTestCase.Name := 'with_filters_profiling';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.controller_action_filters_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_actions_profiling_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 13: With SQIDS
  LTestCase.Name := 'with_sqids';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 14: With ActiveRecord Middleware
  LTestCase.Name := 'with_activerecord';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  ATestCases.Add(LTestCase);

  // Test 15: Full featured (all options)
  LTestCase.Name := 'full_featured';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_msheap] := True;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  LTestCase.Config.B[TConfigKey.program_service_container_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_action_filters_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_actions_profiling_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  LTestCase.Config.B[TConfigKey.websocket_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_analytics] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_trace] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  ATestCases.Add(LTestCase);

  // Test 16: HTTPS with WebSocket
  LTestCase.Name := 'https_with_websocket';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPS_CONSOLE;
  LTestCase.Config.B[TConfigKey.websocket_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 17: With Memory Session
  LTestCase.Name := 'with_memory_session';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_memory] := True;
  ATestCases.Add(LTestCase);

  // Test 18: With File Session
  LTestCase.Name := 'with_file_session';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_file] := True;
  ATestCases.Add(LTestCase);

  // Test 19: With Database Session (requires ActiveRecord)
  LTestCase.Name := 'with_database_session';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_database] := True;
  ATestCases.Add(LTestCase);

  // Test 20: With JWT Cookie Authentication
  LTestCase.Name := 'with_jwt';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  ATestCases.Add(LTestCase);
end;

procedure PrintSummary;
var
  LResult: TTestResult;
  LPassed, LFailed: Integer;
begin
  Log('');
  Log('========================================');
  Log('TEST SUMMARY');
  Log('========================================');

  LPassed := 0;
  LFailed := 0;

  for LResult in GTestResults do
  begin
    if LResult.GenerationOK and LResult.CompilationOK then
    begin
      Inc(LPassed);
      Log('[PASS] ' + LResult.TestName);
    end
    else
    begin
      Inc(LFailed);
      if not LResult.GenerationOK then
        Log('[FAIL] ' + LResult.TestName + ' - Generation failed: ' + LResult.ErrorMessage)
      else
        Log('[FAIL] ' + LResult.TestName + ' - Compilation failed: ' + LResult.ErrorMessage);
    end;
  end;

  Log('');
  Log('Total: ' + IntToStr(LPassed + LFailed) + ' tests');
  Log('Passed: ' + IntToStr(LPassed));
  Log('Failed: ' + IntToStr(LFailed));

  if LFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

procedure ParseCommandLine;
var
  I: Integer;
  LArg: string;
begin
  GOutputDir := '.\output';
  GVerbose := False;
  GSkipCompile := False;

  for I := 1 to ParamCount do
  begin
    LArg := ParamStr(I);
    if LArg.StartsWith('--output-dir=') then
      GOutputDir := LArg.Substring(13)
    else if LArg = '--verbose' then
      GVerbose := True
    else if LArg = '--skip-compile' then
      GSkipCompile := True
    else if LArg.StartsWith('--delphi=') then
      GDelphiPath := LArg.Substring(9);
  end;

  // Auto-detect Delphi if not specified
  if GDelphiPath.IsEmpty and not GSkipCompile then
    GDelphiPath := FindDelphiPath;
end;

var
  LTestCases: TList<TTestCase>;
  LTestCase: TTestCase;
begin
  try
    Log('DMVCFramework Template Generator Test Tool');
    Log('==========================================');
    Log('');

    ParseCommandLine;

    // Set template path to the ideexpert/templates folder
    TTestTemplateEngine.SetTemplatePath(TPath.Combine(
      TPath.GetDirectoryName(TPath.GetDirectoryName(ParamStr(0))),
      'templates'
    ));

    // Try current directory if not found
    if not TDirectory.Exists(TTestTemplateEngine.GetTemplatePath) then
      TTestTemplateEngine.SetTemplatePath(TPath.Combine(GetCurrentDir, 'templates'));

    // Try relative to exe
    if not TDirectory.Exists(TTestTemplateEngine.GetTemplatePath) then
      TTestTemplateEngine.SetTemplatePath(TPath.Combine(ExtractFilePath(ParamStr(0)), 'templates'));

    Log('Template path: ' + TTestTemplateEngine.GetTemplatePath);
    Log('Output path: ' + TPath.GetFullPath(GOutputDir));
    if GSkipCompile then
      Log('Compilation: SKIPPED (--skip-compile)')
    else if GDelphiPath.IsEmpty then
    begin
      Log('Delphi path: NOT FOUND (compilation will be skipped)');
      GSkipCompile := True;
    end
    else
      Log('Delphi path: ' + GDelphiPath);
    Log('');

    if not TDirectory.Exists(TTestTemplateEngine.GetTemplatePath) then
    begin
      Log('ERROR: Template directory not found!');
      Log('Expected: ' + TTestTemplateEngine.GetTemplatePath);
      ExitCode := 1;
      Exit;
    end;

    GTestResults := TList<TTestResult>.Create;
    LTestCases := TList<TTestCase>.Create;
    try
      CreateTestCases(LTestCases);

      for LTestCase in LTestCases do
      begin
        RunTest(LTestCase.Name, LTestCase.Config);
        LTestCase.Config.Free;
      end;

      PrintSummary;
    finally
      LTestCases.Free;
      GTestResults.Free;
    end;

  except
    on E: Exception do
    begin
      Writeln('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
