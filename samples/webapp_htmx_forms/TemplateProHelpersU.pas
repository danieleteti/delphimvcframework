// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit TemplateProHelpersU;

interface

procedure TemplateProContextConfigure;

implementation

uses
  System.SysUtils, System.IOUtils, TemplatePro, MVCFramework.Commons;

procedure TemplateProContextConfigure;
var
  lViewPath: string;
begin
  // Dynamic includes (include with an expression) can only load files from the views
  // folder: a file name built from request data cannot escape it with "..\" or an
  // absolute path. Resolved with the same key and rule the framework uses for
  // Config[TMVCConfigKey.ViewPath]: keep the two in sync if you change either.
  lViewPath := dotEnv.Env('dmvc.view_path', TPath.Combine(AppPath, 'templates'));
  if not TDirectory.Exists(lViewPath) then
    lViewPath := TPath.Combine(AppPath, lViewPath);
  lViewPath := TPath.GetFullPath(lViewPath);

  TTProConfiguration.OnContextConfiguration := procedure(const CompiledTemplate: ITProCompiledTemplate)
  begin
    CompiledTemplate.IncludeRootPath := lViewPath;
  end;
end;


end.
