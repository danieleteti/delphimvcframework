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
    lJSON.S['program.name'] :=  'MyProgram';
    lJSON.S['program.default_server_port'] :=  '8080';
    lJSON.S['controller.unit_name'] := 'MyControllerU';
    lJSON.S['controller.classname'] :=  'TMyController';
    lJSON.B['controller.index_methods.generate'] :=  true;
    lJSON.B['controller.action_filters.generate'] :=  true;
    lJSON.B['controller.crud_methods.generate'] :=  true;
    lJSON.B['entity.generate'] :=  true;
    lJSON.S['entity.classname'] :=  'TPerson';
    lJSON.B['jsonrpc.generate'] :=  true;
    lJSON.S['jsonrpc.classname'] :=  'TMyJSONRPC';
    lJSON.S['jsonrpc.unit_name'] := 'MyJSONRPCU';

    //webmodule

    lJSON.S['webmodule.classname'] :=  'TMyWebModule';
    lJSON.B['webmodule.middleware.analytics'] :=  true;
    lJSON.B['webmodule.middleware.staticfiles'] :=  true;
    lJSON.B['webmodule.middleware.trace'] :=  true;
    lJSON.B['webmodule.middleware.compression'] :=  true;
    lJSON.B['webmodule.middleware.etag'] :=  true;
    lJSON.B['webmodule.middleware.cors'] :=  true;
    lJSON.B['webmodule.middleware.activerecord'] :=  true;
    lJSON.S['webmodule.middleware.activerecord.con_def_name'] :=  'MyConnection';
    lJSON.S['webmodule.middleware.activerecord.con_def_filename'] :=  '';

    //webmodule - end


    lJSON.B['msheap'] := true;
    TFile.WriteAllText(lJSON.S['program.name'] + '.dpr', GenerateSource(lJSON,
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
    if lJSON.B['jsonrpc.generate'] then
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
