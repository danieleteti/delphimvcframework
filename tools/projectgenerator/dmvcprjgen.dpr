program dmvcprjgen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  MVCFramework.Rtti.Utils,
  ProjectGeneratorU in 'ProjectGeneratorU.pas',
  CommandsU in 'CommandsU.pas',
  CommonsU in 'CommonsU.pas',
  MVCFramework.Commons,
  Commands.TemplatesU in 'Commands.TemplatesU.pas',
  System.IOUtils,
  JsonDataObjects;

function GenerateSource(JSON: TJSONObject; FillerProc: TProc<TMVCCodeGenerator>): String;
var
  lGenerator: TMVCCodeGenerator;
begin
  lGenerator := TMVCCodeGenerator.Create;
  try
    lGenerator.Commands.Clear;
    FillerProc(lGenerator);
    lGenerator.Execute(JSON);
    Result := lGenerator.Source;
  finally
    lGenerator.Free;
  end;
end;

procedure Main;
begin
  var lJSON := TJSONObject.Create;
  try
    lJSON.S[TConfigKey.program_name] :=  'MyProgram';
    lJSON.S[TConfigKey.program_default_server_port] :=  '8080';
    lJSON.B[TConfigKey.program_msheap] := true;
    lJSON.S[TConfigKey.controller_unit_name] := 'MyControllerU';
    lJSON.S[TConfigKey.controller_classname] :=  'TMyController';
    lJSON.B[TConfigKey.controller_index_methods_generate] :=  true;
    lJSON.B[TConfigKey.controller_action_filters_generate] :=  true;
    lJSON.B[TConfigKey.controller_crud_methods_generate] :=  true;
    lJSON.B[TConfigKey.entity_generate] :=  true;
    lJSON.S[TConfigKey.entity_classname] :=  'TPerson';
    lJSON.B[TConfigKey.jsonrpc_generate] :=  true;
    lJSON.S[TConfigKey.jsonrpc_classname] :=  'TMyJSONRPC';
    lJSON.S[TConfigKey.jsonrpc_unit_name] := 'MyJSONRPCU';

    //webmodule

    lJSON.S[TConfigKey.webmodule_classname] :=  'TMyWebModule';
    lJSON.B[TConfigKey.webmodule_middleware_analytics] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_staticfiles] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_trace] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_compression] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_etag] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_cors] :=  true;
    lJSON.B[TConfigKey.webmodule_middleware_activerecord] :=  true;
    lJSON.S[TConfigKey.webmodule_middleware_activerecord_con_def_name] :=  'MyConnection';
    lJSON.S[TConfigKey.webmodule_middleware_activerecord_con_def_filename] :=  '';

    //webmodule - end

    TFile.WriteAllText(lJSON.S[TConfigKey.program_name] + '.dpr', GenerateSource(lJSON,
                                                  procedure (Gen: TMVCCodeGenerator)
                                                  begin
                                                    FillProgramTemplates(Gen)
                                                  end));
    TFile.WriteAllText('MyControllerU.pas', GenerateSource(lJSON,
                                                  procedure (Gen: TMVCCodeGenerator)
                                                  begin
                                                    FillControllerTemplates(Gen)
                                                  end));
    TFile.WriteAllText('MyWebModuleU.pas', GenerateSource(lJSON,
                                                  procedure (Gen: TMVCCodeGenerator)
                                                  begin
                                                    FillWebModuleTemplates(Gen)
                                                  end));
    TFile.WriteAllText('MyWebModuleU.dfm', GenerateSource(lJSON,
                                                  procedure (Gen: TMVCCodeGenerator)
                                                  begin
                                                    FillWebModuleDFMTemplates(Gen)
                                                  end));
    if lJSON.B[TConfigKey.jsonrpc_generate] then
    begin
      TFile.WriteAllText('MyJSONRPCU.pas', GenerateSource(lJSON,
                                                    procedure (Gen: TMVCCodeGenerator)
                                                    begin
                                                      FillJSONRPCTemplates(Gen)
                                                    end));
    end;

  finally
    lJSON.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
