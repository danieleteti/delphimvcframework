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
{$WARN SYMBOL_PLATFORM OFF} // Windows-only tool (TFileAttribute in the SwaggerUI tests)

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.Rtti,
  System.Hash,
  System.Zip,
  System.Diagnostics,
  IdContext,
  IdTCPServer,
  Winapi.Windows,
  Winapi.ShellAPI,
  JsonDataObjects,
  TemplatePro,
  DMVC.Expert.Commons in '..\DMVC.Expert.Commons.pas',
  DMVC.Expert.ProjectGenerator in '..\DMVC.Expert.ProjectGenerator.pas',
  DMVC.Expert.SwaggerUI in '..\DMVC.Expert.SwaggerUI.pas';

type
  TTestCase = record
    Name: string;
    Config: TJSONObject;
    // Optional file-presence assertions evaluated after GenerateProject.
    // Paths are relative to the per-case output folder, use '/' separators.
    // ExpectedFiles must all exist; ForbiddenFiles must all be absent.
    // Leave empty to skip the file-check phase for the case.
    ExpectedFiles: TArray<string>;
    ForbiddenFiles: TArray<string>;
    // Optional content assertions, each entry 'relative/path|needle'.
    // A missing file fails both lists: an assertion that passes because nothing
    // was generated is worse than no assertion at all.
    MustContain: TArray<string>;
    MustNotContain: TArray<string>;
  end;

  TTestResult = record
    TestName: string;
    GenerationOK: Boolean;
    CompilationOK: Boolean;
    FileCheckOK: Boolean; // True when no expected/forbidden lists are configured
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
  GSwaggerUIZip: TBytes; // one real download, reused by every *_openapi case
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
  Result.S[TConfigKey.program_server_engine] := 'webbroker';
  Result.B[TConfigKey.program_uses_webmodule] := False;
  Result.S[TConfigKey.program_server_protocol] := 'http';  // http or https
  Result.B[TConfigKey.program_service_container_generate] := False;
  Result.S[TConfigKey.program_service_container_unit_name] := 'ServicesU';
  Result.B[TConfigKey.program_minimal_api] := False;

  // Controller defaults
  Result.S[TConfigKey.controller_unit_name] := 'Controllers.HomeU';
  Result.S[TConfigKey.controller_classname] := 'THomeController';
  Result.B[TConfigKey.controller_index_methods_generate] := True;
  Result.B[TConfigKey.controller_action_filters_generate] := False;
  Result.B[TConfigKey.controller_crud_methods_generate] := False;
  Result.B[TConfigKey.controller_actions_profiling_generate] := False;
  // Derived flag mirrored by GenerateProject; declared here too so templates
  // rendered directly (RunDProjAppTypeTest) don't trip the strict engine.
  Result.B['controller.main.generate'] := True;

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
  Result.B[TConfigKey.webmodule_middleware_jwt_asymmetric] := False;
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
begin
  // The real thing. This used to be a 490-line copy of TDMVCProjectGenerator:
  // the two drifted by construction, and every file the copy did not write
  // (the .env, loggerpro.json, half the view templates) was a template nothing
  // ever rendered. TemplateFolder makes the generator read the .tpro files on
  // disk instead of the copies embedded in the BPL, so this suite tests the
  // templates in the working tree.
  Result := True;
  try
    ForceDirectories(AOutputDir);
    TDMVCProjectGenerator.TemplateFolder := TTestTemplateEngine.GetTemplatePath;
    TDMVCProjectGenerator.Generate(AOutputDir,
      AConfig.S[TConfigKey.program_name], AConfig);
    // SQLite needs the folder to exist before the first connection.
    if AConfig.B[TConfigKey.webmodule_middleware_activerecord] then
      ForceDirectories(TPath.Combine(AOutputDir, 'bin' + PathDelim + 'data'));
  except
    on E: Exception do
    begin
      Result := False;
      Log('ERROR: ' + E.Message);
    end;
  end;
end;

function VerifyExpectedFiles(const AOutputDir: string;
  const AExpected, AForbidden: TArray<string>; out AError: string): Boolean;
var
  LRel: string;
  LAbs: string;
  LMissing, LLeaked: TArray<string>;
begin
  Result := True;
  AError := '';
  LMissing := nil;
  LLeaked := nil;

  for LRel in AExpected do
  begin
    LAbs := TPath.Combine(AOutputDir, LRel.Replace('/', PathDelim));
    if not (TFile.Exists(LAbs) or TDirectory.Exists(LAbs)) then
      LMissing := LMissing + [LRel];
  end;

  for LRel in AForbidden do
  begin
    LAbs := TPath.Combine(AOutputDir, LRel.Replace('/', PathDelim));
    if TFile.Exists(LAbs) or TDirectory.Exists(LAbs) then
      LLeaked := LLeaked + [LRel];
  end;

  if Length(LMissing) > 0 then
  begin
    Result := False;
    AError := 'Missing expected: ' + String.Join(', ', LMissing);
  end;
  if Length(LLeaked) > 0 then
  begin
    Result := False;
    if AError <> '' then
      AError := AError + ' | ';
    AError := AError + 'Unexpected present: ' + String.Join(', ', LLeaked);
  end;
end;

function VerifyContent(const AOutputDir: string;
  const AMustContain, AMustNotContain: TArray<string>; out AError: string): Boolean;
var
  LEntry, LRel, LNeedle, LAbs, LText: string;
  LSep: Integer;
  LFailures: TArray<string>;
begin
  AError := '';
  LFailures := nil;

  for LEntry in AMustContain do
  begin
    LSep := LEntry.IndexOf('|');
    LRel := LEntry.Substring(0, LSep);
    LNeedle := LEntry.Substring(LSep + 1);
    LAbs := TPath.Combine(AOutputDir, LRel.Replace('/', PathDelim));
    if not TFile.Exists(LAbs) then
      LFailures := LFailures + ['no ' + LRel]
    else
    begin
      LText := TFile.ReadAllText(LAbs);
      if not LText.Contains(LNeedle) then
        LFailures := LFailures + [LRel + ' lacks "' + LNeedle + '"'];
    end;
  end;

  for LEntry in AMustNotContain do
  begin
    LSep := LEntry.IndexOf('|');
    LRel := LEntry.Substring(0, LSep);
    LNeedle := LEntry.Substring(LSep + 1);
    LAbs := TPath.Combine(AOutputDir, LRel.Replace('/', PathDelim));
    if not TFile.Exists(LAbs) then
      LFailures := LFailures + ['no ' + LRel]
    else
    begin
      LText := TFile.ReadAllText(LAbs);
      if LText.Contains(LNeedle) then
        LFailures := LFailures + [LRel + ' still has "' + LNeedle + '"'];
    end;
  end;

  Result := Length(LFailures) = 0;
  if not Result then
    AError := 'Content check: ' + String.Join(' | ', LFailures);
end;

procedure RunTest(const ATestCase: TTestCase);
var
  LResult: TTestResult;
  LOutputDir: string;
  LCompileError: string;
  LFileError: string;
begin
  Log('');
  Log('=== Test: ' + ATestCase.Name + ' ===');

  LResult.TestName := ATestCase.Name;
  LResult.GenerationOK := False;
  LResult.CompilationOK := False;
  LResult.FileCheckOK := True; // True by default; flipped on a real check failure
  LResult.ErrorMessage := '';

  LOutputDir := TPath.Combine(GOutputDir, ATestCase.Name);
  // Wipe first: a file left by a previous run satisfies an ExpectedFiles
  // assertion the current templates no longer produce.
  if TDirectory.Exists(LOutputDir) then
    TDirectory.Delete(LOutputDir, True);

  try
    LResult.GenerationOK := GenerateProject(ATestCase.Config, LOutputDir);
    if LResult.GenerationOK then
    begin
      Log('Generation: OK');

      // File-presence assertions (only when the case configured at least one).
      if (Length(ATestCase.ExpectedFiles) > 0) or
         (Length(ATestCase.ForbiddenFiles) > 0) then
      begin
        LResult.FileCheckOK := VerifyExpectedFiles(LOutputDir,
          ATestCase.ExpectedFiles, ATestCase.ForbiddenFiles, LFileError);
        if LResult.FileCheckOK then
          Log('File check: OK')
        else
        begin
          Log('File check: FAILED - ' + LFileError);
          LResult.ErrorMessage := LFileError;
        end;
      end;

      if LResult.FileCheckOK and
         ((Length(ATestCase.MustContain) > 0) or
          (Length(ATestCase.MustNotContain) > 0)) then
      begin
        LResult.FileCheckOK := VerifyContent(LOutputDir,
          ATestCase.MustContain, ATestCase.MustNotContain, LFileError);
        if LResult.FileCheckOK then
          Log('Content check: OK')
        else
        begin
          Log('Content check: FAILED - ' + LFileError);
          LResult.ErrorMessage := LFileError;
        end;
      end;

      // Try to compile if not skipped AND file check passed
      if (not GSkipCompile) and LResult.FileCheckOK then
      begin
        LogVerbose('Compiling...');
        LResult.CompilationOK := CompileProject(LOutputDir, ATestCase.Config.S[TConfigKey.program_name], LCompileError);
        if LResult.CompilationOK then
          Log('Compilation: OK')
        else
        begin
          Log('Compilation: FAILED');
          LResult.ErrorMessage := LCompileError;
          LogVerbose(LCompileError);
        end;
      end
      else if GSkipCompile then
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
  // The People page is the table example: its views and its action
  LTestCase.ExpectedFiles := ['bin/templates/people/index.html', 'bin/templates/people/table.html'];
  LTestCase.MustContain := ['Controllers.HomeU.pas|function THomeController.People(',
    'bin/templates/people/index.html|{{include "table.html"}}',
    'bin/templates/people/index.html|Controllers.HomeU.People',
    'bin/templates/people/table.html|{{for p in people}}',
    'bin/templates/baselayout.html|href="/web/people"'];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

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

  // Test: JWT with asymmetric signing (RS256) - Bearer
  LTestCase.Name := 'with_jwt_asymmetric';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt_asymmetric] := True;
  ATestCases.Add(LTestCase);

  // Test: JWT with asymmetric signing (RS256) - Cookie (with SSV)
  LTestCase.Name := 'with_jwt_asymmetric_cookie';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt_asymmetric] := True;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.B['program.ssv.any'] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  ATestCases.Add(LTestCase);

  // === Windows Service Tests ===

  // Test 21: Windows Service HTTP Minimal
  LTestCase.Name := 'winservice_http_minimal';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '8080';
  ATestCases.Add(LTestCase);

  // Test 22: Windows Service HTTPS
  LTestCase.Name := 'winservice_https';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // Test 23: Windows Service HTTP with CRUD
  LTestCase.Name := 'winservice_http_crud';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 24: Windows Service HTTPS with all middleware
  LTestCase.Name := 'winservice_https_all_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  LTestCase.Config.B[TConfigKey.webmodule_middleware_analytics] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_trace] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_ratelimit] := True;
  ATestCases.Add(LTestCase);

  // Test 25: Windows Service with Memory Session
  LTestCase.Name := 'winservice_memory_session';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_memory] := True;
  ATestCases.Add(LTestCase);

  // Test 26: Windows Service with ActiveRecord
  LTestCase.Name := 'winservice_activerecord';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  ATestCases.Add(LTestCase);

  // Test 27: Windows Service Full Featured
  LTestCase.Name := 'winservice_full_featured';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  LTestCase.Config.B[TConfigKey.program_msheap] := True;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.program_service_container_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_action_filters_generate] := True;
  LTestCase.Config.B[TConfigKey.controller_actions_profiling_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_analytics] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_memory] := True;
  ATestCases.Add(LTestCase);

  // === Indy Direct Tests ===

  // Test 28: Indy Direct minimal
  LTestCase.Name := 'indydirect_minimal';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  ATestCases.Add(LTestCase);

  // Test 29: Indy Direct with CRUD
  LTestCase.Name := 'indydirect_with_crud';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 30: Indy Direct with all middleware
  LTestCase.Name := 'indydirect_all_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_trace] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_analytics] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_ratelimit] := True;
  ATestCases.Add(LTestCase);

  // Test 31: Indy Direct with JSON-RPC
  LTestCase.Name := 'indydirect_jsonrpc';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  ATestCases.Add(LTestCase);

  // Test 32: Indy Direct with session (memory)
  LTestCase.Name := 'indydirect_session';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_session_memory] := True;
  ATestCases.Add(LTestCase);

  // Test 33: Indy Direct full featured
  LTestCase.Name := 'indydirect_full_featured';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  ATestCases.Add(LTestCase);

  // === HTTP.sys Tests ===

  // Test 34: HTTP.sys minimal
  LTestCase.Name := 'httpsys_minimal';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPSYS;
  ATestCases.Add(LTestCase);

  // Test 35: HTTP.sys with CRUD and middleware
  LTestCase.Name := 'httpsys_with_crud_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPSYS;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  ATestCases.Add(LTestCase);

  // Test 36: HTTP.sys full featured (no websocket - incompatible)
  LTestCase.Name := 'httpsys_full_featured';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPSYS;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.jsonrpc_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  ATestCases.Add(LTestCase);

  // === Indy Direct + Windows Service Tests ===

  // Test 37: Indy Direct as Windows Service (HTTP)
  LTestCase.Name := 'winservice_indydirect_http';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  ATestCases.Add(LTestCase);

  // Test 38: Indy Direct Service with CRUD + middleware
  LTestCase.Name := 'winservice_indydirect_crud_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  ATestCases.Add(LTestCase);

  // Test 39: Indy Direct as Windows Service (HTTPS via TaurusTLS)
  LTestCase.Name := 'winservice_indydirect_https';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // === HTTP.sys + Windows Service Tests ===

  // Test 40: HTTP.sys as Windows Service (HTTP)
  LTestCase.Name := 'winservice_httpsys_http';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  ATestCases.Add(LTestCase);

  // Test 41: HTTP.sys Service with CRUD + middleware
  LTestCase.Name := 'winservice_httpsys_crud_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'http';
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  ATestCases.Add(LTestCase);

  // Test 42: HTTP.sys as Windows Service (HTTPS - kernel SSL)
  LTestCase.Name := 'winservice_httpsys_https';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.WINDOWS_SERVICE;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // === Console + HTTPS Tests for new engines ===

  // Test 43: Indy Direct console HTTPS
  LTestCase.Name := 'indydirect_https';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // Test 44: HTTP.sys console HTTPS
  LTestCase.Name := 'httpsys_https';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPSYS;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  ATestCases.Add(LTestCase);

  // === ISAPI tests ===

  // Test 45: ISAPI minimal
  LTestCase.Name := 'isapi_minimal';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'webbroker';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.ISAPI;
  ATestCases.Add(LTestCase);

  // Test 46: ISAPI with CRUD + middleware
  LTestCase.Name := 'isapi_crud_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'webbroker';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.ISAPI;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  ATestCases.Add(LTestCase);

  // === Apache module tests ===

  // Test 47: Apache minimal
  LTestCase.Name := 'apache_minimal';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'webbroker';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.APACHE;
  ATestCases.Add(LTestCase);

  // Test 48: Apache with CRUD + middleware
  LTestCase.Name := 'apache_crud_middleware';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'webbroker';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.APACHE;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  ATestCases.Add(LTestCase);

  // === Minimal API tests (Indy Direct / HTTP.sys / WebBroker console) ===

  // Test 49: Indy Direct + Minimal API + CRUD
  LTestCase.Name := 'indydirect_minimal_api';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  // Minimal mode disables index/action-filters/profile; main controller is skipped.
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  ATestCases.Add(LTestCase);

  // Test 50: HTTP.sys + Minimal API + CRUD
  LTestCase.Name := 'httpsys_minimal_api';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'httpsys';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTPSYS;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  ATestCases.Add(LTestCase);

  // Test 51: WebBroker console + Minimal API + CRUD
  LTestCase.Name := 'webbroker_minimal_api';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'webbroker';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.HTTP_CONSOLE;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  ATestCases.Add(LTestCase);

  // Test 52: Indy Direct + Minimal API + CRUD + Service Container
  LTestCase.Name := 'indydirect_minimal_api_services';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_service_container_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  ATestCases.Add(LTestCase);

  // Test 53: Indy Direct + Minimal API WebApp (TemplatePro + HTMX)
  LTestCase.Name := 'indydirect_minimal_api_web';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.B[TConfigKey.program_htmx] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  LTestCase.ExpectedFiles := ['bin/templates/baselayout.html', 'bin/templates/error.html',
    'bin/templates/pages/home.html', 'bin/templates/pages/login.html',
    'bin/templates/pages/admin_home.html', 'bin/templates/pages/time.html'];
  LTestCase.ForbiddenFiles := [];
  ATestCases.Add(LTestCase);

  // Test 54: Indy Direct + Minimal API + full HTTPFilter stack.
  // Verifies the engineconfig.pas.tpro minimal-API block emits
  // UseHTTPFilter(...) calls for every supported helper and that the
  // generated code compiles.
  LTestCase.Name := 'indydirect_minimal_api_httpfilters';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  // HTTPFilter helpers — each generates a UseHTTPFilter line in
  // EngineConfigU.ConfigureEngine.
  LTestCase.Config.B[TConfigKey.webmodule_middleware_cors] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_ratelimit] := True;
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  ATestCases.Add(LTestCase);

  // Test 55: Indy Direct + Minimal API WebApp + StaticFiles + ETag +
  // Compression. Reflects the updated ppMinimalAPIWebApp preset which
  // now opts into StaticFiles + ETag via the HTTPFilter wiring.
  LTestCase.Name := 'indydirect_minimal_api_web_full_filters';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.B[TConfigKey.program_htmx] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_compression] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_etag] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.ExpectedFiles := ['bin/templates/baselayout.html', 'bin/templates/error.html',
    'bin/templates/pages/home.html', 'bin/templates/pages/login.html',
    'bin/templates/pages/admin_home.html', 'bin/templates/pages/time.html'];
  LTestCase.ForbiddenFiles := [];
  ATestCases.Add(LTestCase);

  // From here on the case record is reset first: the four assertion arrays are
  // sticky otherwise, and a case that forgets to clear them inherits the previous
  // one's expectations.
  LTestCase := Default(TTestCase);

  // Test 56: ISAPI + TemplatePro views. The WebModule flavor (ISAPI, Apache,
  // Windows Service, FastCGI) must render errors through UseExceptionHandler,
  // not through a hand-rolled handler that puts E.Message on the page.
  LTestCase.Name := 'isapi_webapp_error_handler';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.ISAPI;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := ['WebModuleU.pas|UseExceptionHandler'];
  LTestCase.MustNotContain := ['WebModuleU.pas|lError := E.Message'];
  ATestCases.Add(LTestCase);

  // Test 57: Indy Direct + Minimal API + JWT. The combination was never
  // exercised: the JWT units are emitted only for the non-minimal flavor while
  // the middleware call is emitted always. Also pins the two things a minimal
  // API project silently lost: the engine config callback (so the dmvc.* keys
  // in .env are actually read) and a login handler that does not hand out a
  // token to admin/admin.
  LTestCase.Name := 'indydirect_minimal_api_jwt';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := [
    'RoutesU.pas|Result := JWT(',
    'TestProject.dpr|expose_server_signature',
    'bin/.env|dmvc.expose_x_powered_by=false'];
  LTestCase.MustNotContain := ['AuthenticationU.pas|UserName.Equals(Password)'];
  ATestCases.Add(LTestCase);

  // Test 58: Indy Direct over HTTPS. The .env must carry the certificate keys
  // the generated .dpr reads, otherwise the project cannot start.
  LTestCase.Name := 'indydirect_https_env';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.S[TConfigKey.program_server_protocol] := 'https';
  LTestCase.Config.S[TConfigKey.program_default_server_port] := '443';
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := [
    'bin/.env|https.cert.privkey',
    'bin/.env|https.cert.cacert'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);

  // Test 59: TemplatePro views. Dynamic includes ({{include @(expr)}}) must be
  // confined to the views folder, resolved with the same key and rule the
  // framework uses for TMVCConfigKey.ViewPath, so that a file name built from
  // request data cannot read files outside it.
  LTestCase.Name := 'templatepro_include_root';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := [
    'TemplateProHelpersU.pas|dotEnv.Env(''dmvc.view_path'', TPath.Combine(AppPath, ''templates''))',
    'TemplateProHelpersU.pas|CompiledTemplate.IncludeRootPath := lViewPath;'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 60: TemplatePro views get the forms library (bin/templates/lib), copied
  // verbatim: it is a runtime TemplatePro file, not a wizard template.
  LTestCase.Name := 'templatepro_forms_library';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  LTestCase.ExpectedFiles := ['bin/templates/lib/forms_bootstrap5.tpro'];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := [
    'bin/templates/lib/forms_bootstrap5.tpro|{{macro input(',
    'bin/templates/lib/forms_bootstrap5.tpro|{{:model|attr,f.FieldName}}'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 61: ActiveRecord + TemplatePro views: the ActiveRecord field metadata
  // unit is used, so {{for f in entity.@@fields}} and the forms library follow the mapping.
  LTestCase.Name := 'templatepro_activerecord_fields';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.B[TConfigKey.program_ssv_templatepro] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_activerecord] := True;
  LTestCase.Config.S[TConfigKey.default_media_type] := 'TMVCMediaType.TEXT_HTML';
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := [
    'EngineConfigU.pas|MVCFramework.View.Renderers.TemplatePro.ActiveRecord,'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // === API documentation option (OpenAPI 3 + Swagger UI downloaded at generation) ===

  // Test 62: controller project, option ON. Swagger middleware with OpenAPI 3,
  // MVCSwag attributes on the sample controller, Swagger UI in bin\www\swagger
  // with the initializer pointing at /openapi.json. Real download.
  LTestCase.Name := 'indydirect_openapi_controllers';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_jwt] := True;
  LTestCase.Config.B[TConfigKey.program_openapi] := True;
  LTestCase.ExpectedFiles := ['bin/www/swagger/index.html', 'bin/www/swagger/index.css',
    'bin/www/swagger/swagger-ui-bundle.js', 'bin/www/swagger/swagger-ui-standalone-preset.js',
    'bin/www/swagger/swagger-ui.css', 'bin/www/swagger/swagger-initializer.js',
    'bin/www/swagger/LICENSE', 'bin/www/swagger/NOTICE'];
  LTestCase.ForbiddenFiles := ['bin/www/swagger/README-swagger-ui.txt',
    'bin/www/swagger/swagger-ui.js', 'bin/www/swagger/swagger-ui-es-bundle.js',
    'bin/www/swagger/swagger-ui-es-bundle-core.js'];
  LTestCase.MustContain := [
    'EngineConfigU.pas|TMVCSwaggerMiddleware.Create(AEngine, LSwaggerInfo, ''/openapi.json''',
    'TestProject.dpr|LogI(''API documentation (Swagger UI): http://localhost:'' + APort.ToString + ''/swagger/'');',
    'EngineConfigU.pas|ssvOpenAPI3',
    'EngineConfigU.pas|TMVCStaticFilesMiddleware.Create(''/swagger''',
    'EngineConfigU.pas|if dotEnv.Env(''dmvc.openapi.enabled'', False) then',
    'bin/.env|dmvc.openapi.enabled=true',
    'Controllers.PeopleU.pas|[MVCSWAGDefaultModel(TPerson, ''Person'', ''People'')]',
    'bin/www/swagger/swagger-initializer.js|url: "/openapi.json"'];
  LTestCase.MustNotContain := ['bin/www/swagger/swagger-initializer.js|petstore',
    'bin/.env|JWT_SECRET='#13#10]; // the wizard writes a generated key
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 63: same controller project, option OFF: nothing of the above.
  LTestCase.Name := 'indydirect_openapi_off';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_openapi] := False;
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := ['bin/www/swagger'];
  LTestCase.MustContain := [];
  LTestCase.MustNotContain := ['EngineConfigU.pas|Swagger', 'Controllers.PeopleU.pas|MVCSwag',
    'bin/.env|dmvc.openapi'];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 64: WebModule flavor (ISAPI), option ON: the same wiring in WebModuleU.
  LTestCase.Name := 'isapi_openapi';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.ISAPI;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.webmodule_middleware_staticfiles] := True;
  LTestCase.Config.B[TConfigKey.program_openapi] := True;
  LTestCase.ExpectedFiles := ['bin/www/swagger/index.html'];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := ['WebModuleU.pas|ssvOpenAPI3',
    'WebModuleU.pas|TMVCStaticFilesMiddleware.Create(''/swagger''',
    'WebModuleU.pas|if dotEnv.Env(''dmvc.openapi.enabled'', False) then',
    'bin/.env|dmvc.openapi.enabled=true'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 65: Minimal API, option ON: native OpenAPI() filter, route metadata,
  // no Swagger middleware.
  LTestCase.Name := 'indydirect_minimal_api_openapi';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  LTestCase.Config.B[TConfigKey.program_openapi] := True;
  LTestCase.ExpectedFiles := ['bin/www/swagger/index.html', 'bin/www/swagger/swagger-initializer.js'];
  LTestCase.ForbiddenFiles := ['bin/www/swagger/README-swagger-ui.txt'];
  LTestCase.MustContain := [
    'EngineConfigU.pas|AEngine.UseHTTPFilter(OpenAPI(AEngine, LOpenAPIInfo, ''/openapi.json''))',
    'TestProject.dpr|LogI(''API documentation (Swagger UI): http://localhost:'' + APort.ToString + ''/swagger/'');',
    'EngineConfigU.pas|AEngine.UseHTTPFilter(StaticFiles(LSwaggerUIOptions))',
    'EngineConfigU.pas|if dotEnv.Env(''dmvc.openapi.enabled'', False) then',
    'bin/.env|dmvc.openapi.enabled=true',
    'RoutesU.pas|.Produces<TArray<TPerson>>',
    'RoutesU.pas|.WithTags(''People'')',
    'bin/www/swagger/swagger-initializer.js|url: "/openapi.json"'];
  LTestCase.MustNotContain := ['EngineConfigU.pas|TMVCSwaggerMiddleware'];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 66: controller project with Sqids and the option ON: the ($ID:sqids)
  // path must compile and be documented.
  LTestCase.Name := 'indydirect_openapi_sqids';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_sqids] := True;
  LTestCase.Config.B[TConfigKey.program_openapi] := True;
  LTestCase.ExpectedFiles := ['bin/www/swagger/index.html'];
  LTestCase.ForbiddenFiles := [];
  LTestCase.MustContain := ['Controllers.PeopleU.pas|[MVCPath(''/($ID:sqids)'')]',
    'Controllers.PeopleU.pas|"data":{"type":"array"'];
  LTestCase.MustNotContain := [];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);

  // Test 67: Minimal API, option OFF.
  LTestCase.Name := 'indydirect_minimal_api_openapi_off';
  LTestCase.Config := CreateBaseConfig;
  LTestCase.Config.S[TConfigKey.program_server_engine] := 'indydirect';
  LTestCase.Config.S[TConfigKey.program_type] := TProgramTypes.INDY_DIRECT;
  LTestCase.Config.B[TConfigKey.controller_crud_methods_generate] := True;
  LTestCase.Config.B[TConfigKey.entity_generate] := True;
  LTestCase.Config.B[TConfigKey.program_minimal_api] := True;
  LTestCase.Config.B[TConfigKey.controller_index_methods_generate] := False;
  LTestCase.Config.B['controller.main.generate'] := False;
  LTestCase.Config.B[TConfigKey.program_openapi] := False;
  LTestCase.ExpectedFiles := [];
  LTestCase.ForbiddenFiles := ['bin/www/swagger'];
  LTestCase.MustContain := [];
  LTestCase.MustNotContain := ['EngineConfigU.pas|OpenAPI', 'RoutesU.pas|WithSummary',
    'bin/.env|dmvc.openapi'];
  ATestCases.Add(LTestCase);
  LTestCase := Default(TTestCase);
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
    if LResult.GenerationOK and LResult.CompilationOK and LResult.FileCheckOK then
    begin
      Inc(LPassed);
      Log('[PASS] ' + LResult.TestName);
    end
    else
    begin
      Inc(LFailed);
      if not LResult.GenerationOK then
        Log('[FAIL] ' + LResult.TestName + ' - Generation failed: ' + LResult.ErrorMessage)
      else if not LResult.FileCheckOK then
        Log('[FAIL] ' + LResult.TestName + ' - File check failed: ' + LResult.ErrorMessage)
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

function RunDProjAppTypeTest(const AProgramType, AExpectedAppType: string): Boolean;
var
  LConfig: TJSONObject;
  LSource: string;
  LExpectedTag: string;
  LUnexpectedTag: string;
begin
  LConfig := CreateBaseConfig;
  try
    LConfig.S[TConfigKey.program_type] := AProgramType;
    LSource := TTestTemplateEngine.Render('project.dproj.tpro', LConfig);

    LExpectedTag := '<AppType>' + AExpectedAppType + '</AppType>';
    if AExpectedAppType = 'Console' then
      LUnexpectedTag := '<AppType>Library</AppType>'
    else
      LUnexpectedTag := '<AppType>Console</AppType>';

    Result := LSource.Contains(LExpectedTag) and not LSource.Contains(LUnexpectedTag);
    if Result then
      Log(Format('  [PASS] program_type="%s" -> AppType=%s', [AProgramType, AExpectedAppType]))
    else
      Log(Format('  [FAIL] program_type="%s" -> expected AppType=%s, got: %s',
        [AProgramType, AExpectedAppType,
         Copy(LSource, Pos('<AppType>', LSource),
              Pos('</AppType>', LSource) + Length('</AppType>') - Pos('<AppType>', LSource))]));
  finally
    LConfig.Free;
  end;
end;

function RunAllDProjAppTypeTests: Boolean;
begin
  Log('');
  Log('=== project.dproj.tpro AppType tests ===');
  Result := True;
  Result := RunDProjAppTypeTest(TProgramTypes.HTTP_CONSOLE, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.HTTPS_CONSOLE, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.FASTCGI_CONSOLE, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.WINDOWS_SERVICE, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.INDY_DIRECT, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.HTTPSYS, 'Console') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.ISAPI, 'Library') and Result;
  Result := RunDProjAppTypeTest(TProgramTypes.APACHE, 'Library') and Result;
end;

function CraftZip(const AEntries: TArray<string>): TBytes;
var
  LStream: TBytesStream;
  LZip: TZipFile;
  LName: string;
begin
  LStream := TBytesStream.Create;
  try
    LZip := TZipFile.Create;
    try
      LZip.Open(LStream, zmWrite);
      for LName in AEntries do
        LZip.Add(TEncoding.UTF8.GetBytes('// ' + LName), LName);
      LZip.Close;
    finally
      LZip.Free;
    end;
    Result := Copy(LStream.Bytes, 0, LStream.Size);
  finally
    LStream.Free;
  end;
end;

function SHA256Of(const ABytes: TBytes): string;
var
  LHash: THashSHA2;
begin
  LHash := THashSHA2.Create;
  LHash.Update(ABytes);
  Result := LHash.HashAsString;
end;

const
  STALLED_PORT = 8899;

type
  { Accepts the connection and never answers: the download must give up at its deadline }
  TStalledServer = class
    procedure Execute(AContext: TIdContext);
  end;

procedure TStalledServer.Execute(AContext: TIdContext);
begin
  Sleep(100);
end;

{ DMVC.Expert.SwaggerUI without the network: the failure paths must extract
  nothing and leave the README. The success path (real download) is covered
  by the *_openapi generation cases. }
function RunSwaggerUITests: Boolean;
var
  LDir, LErr, LReadme: string;
  LZip: TBytes;
  LRelease: TSwaggerUIRelease;
  LServer: TIdTCPServer;
  LStalled: TStalledServer;
  LWatch: TStopwatch;

  function Check(const AName: string; ACondition: Boolean; const ADetail: string): Boolean;
  begin
    Result := ACondition;
    if Result then
      Log('  [PASS] ' + AName)
    else
      Log('  [FAIL] ' + AName + ' - ' + ADetail);
  end;

begin
  Log('');
  Log('=== DMVC.Expert.SwaggerUI tests ===');
  Result := True;
  LZip := CraftZip(['swagger-ui-9.9.9/dist/index.html', 'swagger-ui-9.9.9/dist/swagger-ui-bundle.js',
    'swagger-ui-9.9.9/dist/swagger-ui.js.map']);

  // wrong hash: nothing extracted, README written
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_hash');
  if TDirectory.Exists(LDir) then
    TDirectory.Delete(LDir, True);
  LRelease.Version := '9.9.9';
  LRelease.SHA256 := StringOfChar('0', 64);
  LErr := InstallSwaggerUIFromZip(LZip, LRelease, LDir, '/openapi.json');
  Result := Check('hash mismatch is refused', LErr.Contains('SHA-256 mismatch'), LErr) and Result;
  Result := Check('hash mismatch extracts nothing',
    not TFile.Exists(TPath.Combine(LDir, 'index.html')) and
    not TFile.Exists(TPath.Combine(LDir, 'swagger-initializer.js')), 'files found') and Result;
  Result := Check('hash mismatch writes the README',
    TFile.Exists(TPath.Combine(LDir, SWAGGER_UI_README)) and
    TFile.ReadAllText(TPath.Combine(LDir, SWAGGER_UI_README)).Contains(SwaggerUIDownloadURL('9.9.9')),
    'README missing or without the URL') and Result;
  { the README is served with the UI: the reason and local paths stay in the IDE warning }
  LReadme := TFile.ReadAllText(TPath.Combine(LDir, SWAGGER_UI_README));
  Result := Check('the README carries no reason and no local path',
    not LReadme.Contains('mismatch') and not LReadme.Contains(LDir) and
    not LReadme.Contains('es-bundle'), LReadme) and Result;

  // right hash: the crafted dist is extracted, .map skipped, initializer written
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_ok');
  if TDirectory.Exists(LDir) then
    TDirectory.Delete(LDir, True);
  LRelease.SHA256 := SHA256Of(LZip);
  LErr := InstallSwaggerUIFromZip(LZip, LRelease, LDir, '/api/doc.json');
  Result := Check('matching hash is extracted', (LErr = '') and
    TFile.Exists(TPath.Combine(LDir, 'index.html')) and
    not TFile.Exists(TPath.Combine(LDir, 'swagger-ui.js.map')) and
    not TFile.Exists(TPath.Combine(LDir, SWAGGER_UI_README)) and
    TFile.ReadAllText(TPath.Combine(LDir, 'swagger-initializer.js')).Contains('url: "/api/doc.json"'), LErr) and Result;

  // zip-slip: one entry escapes the target folder, so nothing is written at all
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_slip');
  if TDirectory.Exists(LDir) then
    TDirectory.Delete(LDir, True);
  if TFile.Exists(TPath.Combine(TPath.GetFullPath(GOutputDir), 'evil.js')) then
    TFile.Delete(TPath.Combine(TPath.GetFullPath(GOutputDir), 'evil.js'));
  LZip := CraftZip(['swagger-ui-9.9.9/dist/index.html', 'swagger-ui-9.9.9/dist/swagger-ui-bundle.js',
    'swagger-ui-9.9.9/dist/../../evil.js']);
  LRelease.SHA256 := SHA256Of(LZip);
  LErr := InstallSwaggerUIFromZip(LZip, LRelease, LDir, '/openapi.json');
  Result := Check('zip-slip entry is rejected', LErr.Contains('Unsafe archive entry'), LErr) and Result;
  Result := Check('zip-slip extracts nothing',
    not TFile.Exists(TPath.Combine(TPath.GetFullPath(GOutputDir), 'evil.js')) and
    not TFile.Exists(TPath.Combine(LDir, 'index.html')) and
    TFile.Exists(TPath.Combine(LDir, SWAGGER_UI_README)), 'files found or README missing') and Result;

  LZip := CraftZip(['swagger-ui-9.9.9/dist/index.html', 'swagger-ui-9.9.9/dist/swagger-ui-bundle.js']);
  LRelease.SHA256 := SHA256Of(LZip);

  // an old README that cannot be deleted does not turn a good install into a failure
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_readonly_readme');
  if TDirectory.Exists(LDir) then
  begin
    if TFile.Exists(TPath.Combine(LDir, SWAGGER_UI_README)) then
      TFile.SetAttributes(TPath.Combine(LDir, SWAGGER_UI_README), [TFileAttribute.faNormal]);
    TDirectory.Delete(LDir, True);
  end;
  TDirectory.CreateDirectory(LDir);
  TFile.WriteAllText(TPath.Combine(LDir, SWAGGER_UI_README), 'old');
  TFile.SetAttributes(TPath.Combine(LDir, SWAGGER_UI_README), [TFileAttribute.faReadOnly]);
  try
    LErr := InstallSwaggerUIFromZip(LZip, LRelease, LDir, '/openapi.json');
  finally
    TFile.SetAttributes(TPath.Combine(LDir, SWAGGER_UI_README), [TFileAttribute.faNormal]);
  end;
  Result := Check('an undeletable old README is not fatal',
    (LErr = '') and TFile.Exists(TPath.Combine(LDir, 'index.html')), LErr) and Result;

  // a write failure after the extraction started says so
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_incomplete');
  if TDirectory.Exists(LDir) then
  begin
    if TFile.Exists(TPath.Combine(LDir, 'swagger-ui-bundle.js')) then
      TFile.SetAttributes(TPath.Combine(LDir, 'swagger-ui-bundle.js'), [TFileAttribute.faNormal]);
    TDirectory.Delete(LDir, True);
  end;
  TDirectory.CreateDirectory(LDir);
  TFile.WriteAllText(TPath.Combine(LDir, 'swagger-ui-bundle.js'), 'locked');
  TFile.SetAttributes(TPath.Combine(LDir, 'swagger-ui-bundle.js'), [TFileAttribute.faReadOnly]);
  try
    LErr := InstallSwaggerUIFromZip(LZip, LRelease, LDir, '/openapi.json');
  finally
    TFile.SetAttributes(TPath.Combine(LDir, 'swagger-ui-bundle.js'), [TFileAttribute.faNormal]);
  end;
  Result := Check('a write failure says the installation is incomplete',
    LErr.StartsWith('Swagger UI installation incomplete'), LErr) and Result;

  // a server that accepts and never answers: the download must stop at the deadline
  LDir := TPath.Combine(TPath.GetFullPath(GOutputDir), '_swaggerui_stalled');
  if TDirectory.Exists(LDir) then
    TDirectory.Delete(LDir, True);
  LStalled := TStalledServer.Create;
  LServer := TIdTCPServer.Create(nil);
  try
    LServer.Bindings.Add.SetBinding('127.0.0.1', STALLED_PORT);
    LServer.OnExecute := LStalled.Execute;
    LServer.Active := True;

    LWatch := TStopwatch.StartNew;
    LErr := InstallSwaggerUI(LDir, '/openapi.json',
      Format('http://127.0.0.1:%d/swagger-ui.zip', [STALLED_PORT]), 2000, nil);
    Result := Check('a stalled server is dropped at the deadline',
      LErr.Contains('no answer within') and (LWatch.ElapsedMilliseconds < 6000) and
      TFile.Exists(TPath.Combine(LDir, SWAGGER_UI_README)),
      Format('%s (after %d ms)', [LErr, LWatch.ElapsedMilliseconds])) and Result;

    LWatch := TStopwatch.StartNew;
    LErr := InstallSwaggerUI(LDir, '/openapi.json',
      Format('http://127.0.0.1:%d/swagger-ui.zip', [STALLED_PORT]), 60000,
      function: Boolean
      begin
        Result := LWatch.ElapsedMilliseconds > 500;
      end);
    Result := Check('cancel stops a stalled download',
      LErr.Contains('cancelled') and (LWatch.ElapsedMilliseconds < 5000),
      Format('%s (after %d ms)', [LErr, LWatch.ElapsedMilliseconds])) and Result;
  finally
    LServer.Active := False;
    LServer.Free;
    LStalled.Free;
  end;
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
  LDownloadsBefore: Integer;
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
      if not RunAllDProjAppTypeTests then
      begin
        Log('');
        Log('FAIL: project.dproj.tpro AppType tests did not pass.');
        ExitCode := 1;
        Exit;
      end;

      if not RunSwaggerUITests then
      begin
        Log('');
        Log('FAIL: DMVC.Expert.SwaggerUI tests did not pass.');
        ExitCode := 1;
        Exit;
      end;

      // One real download of the release for the whole run: every case with the
      // option on extracts it through the same InstallSwaggerUIFromZip the
      // wizard uses after its own download.
      LDownloadsBefore := SwaggerUIDownloadCount;
      try
        GSwaggerUIZip := DownloadSwaggerUIZip(SwaggerUIDownloadURL(SWAGGER_UI_RELEASE.Version),
          SWAGGER_UI_DEADLINE_MS, nil);
      except
        on E: Exception do
          Log('Swagger UI download failed: ' + E.Message); // the *_openapi cases then fail
      end;
      TDMVCProjectGenerator.SwaggerUIInstaller :=
        function(ATargetFolder, ADocumentURL: string): string
        begin
          Result := InstallSwaggerUIFromZip(GSwaggerUIZip, SWAGGER_UI_RELEASE,
            ATargetFolder, ADocumentURL);
        end;

      CreateTestCases(LTestCases);

      for LTestCase in LTestCases do
      begin
        RunTest(LTestCase);
        LTestCase.Config.Free;
      end;

      Log(Format('Swagger UI release downloads during the generation cases: %d',
        [SwaggerUIDownloadCount - LDownloadsBefore]));
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
